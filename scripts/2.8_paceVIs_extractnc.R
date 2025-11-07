
#Note this script was written to work using 6 cores of parallel processing!

rm(list=ls())

library(terra)
library(ncdf4)
library(tidyverse)
library(parallel)

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"
boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"

raw_dir <- paste0(wd, "/pace_vi_data/raw")
csv_dir <- paste0(wd, "/pace_vi_data/clean_csvs")

clean_dir <- paste0(boxwd, "/compiled_rasters", "/pace_clean_rasters")

amz_vect <- vect(paste0(boxwd, "/amz_shps/amz_geo_agg_extended.shp"))

# Landcover mask
#Need to process with landcover mask!!
lc_tree <- rast(paste0(wd, "/pace_vi_data/mcd12c1_2019_lc_paceext_amz.tif"))

pacefiles <- unlist(list.files(raw_dir, pattern="*nc", full.names=T))

#Threshold for NDVI filter
ndvi_thresh <- 0.5

process_nc <- function(ncfile) {
#for (i in seq_along(pacefiles)) {
  
  # Setup
  pacefile <- basename(ncfile)
  pacedate <- str_match(pacefile, "\\.(\\d{8})\\_")[,2]
  
  nc <- nc_open(ncfile)
  
  #pacefile <- basename(pacefiles[i])
  #pacedate <- str_match(pacefile, "\\.(\\d{8})\\_")[,2]
  
  #nc <- nc_open(pacefiles[i])
  
  # Get dimensions
  lat <- ncvar_get(nc, "lat")
  lon <- ncvar_get(nc, "lon")
  
  # Step size (can also use metadata constant if available)
  lat_step <- abs(lat[2] - lat[1])
  lon_step <- abs(lon[2] - lon[1])
  
  # Extract variables
  getvar <- function(nc, varname) {
    v <- ncvar_get(nc, varname)
    v[v == -32767] <- NA
    return(v)
  }
  
  ndvi  <- getvar(nc, "ndvi")
  cci   <- getvar(nc, "cci")
  pri   <- getvar(nc, "pri")
  cire  <- getvar(nc, "cire")
  car   <- getvar(nc, "car")
  
  nc_close(nc)
  
  # Define extent: global from bottom-left origin
  global_ext <- ext(
    min(lon) - lon_step / 2,
    max(lon) + lon_step / 2,
    min(lat) - lat_step / 2,
    max(lat) + lat_step / 2
  )
  
  ras_stack <- c(
    flip(rast(t(ndvi)[nrow(t(ndvi)):1, ], extent = global_ext, crs = "EPSG:4326")),
    flip(rast(t(cci)[nrow(t(cci)):1, ], extent = global_ext, crs = "EPSG:4326")),
    flip(rast(t(pri)[nrow(t(pri)):1, ], extent = global_ext, crs = "EPSG:4326")),
    flip(rast(t(cire)[nrow(t(cire)):1, ], extent = global_ext, crs = "EPSG:4326")),
    flip(rast(t(car)[nrow(t(car)):1, ], extent = global_ext, crs = "EPSG:4326"))
  )
  
  names(ras_stack) <- c("ndvi", "cci", "pri", "cire", "car")
  
  # Crop to Amazon vector (already in EPSG:4326)
  ras_crop <- crop(ras_stack, amz_vect, mask = TRUE)
  
  # Calculate a chlorophyll:carotenoid ratio
  chlcar <- ras_crop$cire / ras_crop$car
  
  names(chlcar) <- "chlcar"
  
  ras_crop$chlcar <- chlcar
  
  #Add a 0.5 ndvi threshold
  ndvi_mask <- clamp(ras_crop$ndvi, lower = ndvi_thresh, upper = 1, values = FALSE)
  ras_ndvimsk <- mask(ras_crop, ndvi_mask)
  
  #Mask by landcover type
  ras_lcmsk <- mask(ras_ndvimsk, lc_tree)
  
  geo_grid <- rasterize(amz_vect, ras_lcmsk$cci, field = "region")
  names(geo_grid) <- "georeg"
  
  pace_comb <- c(ras_lcmsk, geo_grid)
  
  pace_df <- terra::as.data.frame(pace_comb, xy = TRUE, na.rm = TRUE)
  
  # Save output
  writeRaster(pace_comb, file.path(clean_dir, paste0("pace_amz_4km_", pacedate, ".tif")), overwrite = TRUE, datatype = "FLT4S")
  
  if (nrow(pace_df) == 0) {
    # Create a 0-row version of the same column structure
    empty_df <- pace_df[0, ]
    write.csv(empty_df, file.path(csv_dir, paste0("pace_amz_df_", pacedate, ".csv")), row.names = FALSE)
  } else {
    write.csv(pace_df, file.path(csv_dir, paste0("pace_amz_df_", pacedate, ".csv")), row.names = FALSE)
  }
  outpath <- file.path(clean_dir, paste0("pace_amz_4km_", pacedate, ".tif"))
  
  gc()
  
  return(outpath)
}


num_cores <- 6
# Use mclapply to run in parallel, or lapply to not
results <- mclapply(pacefiles, process_nc, mc.cores = num_cores)
#results <- lapply(pacefiles, process_nc)




# 
# #Quick Test Plot based on Na Wang's DESIS code
# 
# spec_df <- terra::as.data.frame(amz_pace)
# spec_mat <- data.matrix(spec_df)
# wl_mat <- data.matrix(wl)
# 
# set.seed(304) # Set the seed for reproducibility
# # Select a random sample of rows from spec_mat
# samp_sel <- min(2000, nrow(spec_mat)) # Define the sample size (change as needed)
# samp_ind <- sample(nrow(spec_mat), samp_sel)
# rando_samp <- spec_mat[samp_ind, ]
# 
# # Plot the random sample
# plot(rando_samp[1, ] ~ wl_mat, type = "l", xlab = "Wavelength (nm)", ylab = "Reflectance", ylim = c(-0.1, 1), xlim = c(400, 2400))
# for (j in 2:nrow(rando_samp)) {
#   lines(rando_samp[j, ] ~ wl_mat, col = j)
# }
# 
