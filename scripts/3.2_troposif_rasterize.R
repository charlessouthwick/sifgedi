# Rasterize TROPOSIF data

rm(list=ls())

library(tidyverse)
library(terra)
library(parallel)

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"

yearid <- "2021"

ncpath <- paste0(wd, "/troposif_data/", yearid)

amz_shp <- vect(paste0(wd, "/amz_shps/amz_biome.shp"))

#NOTE! If a more strict CF or SZA is to be used, need to filter before this rasterizing step!
cf_thresh <- 0.2
sza_thresh <- 45
vza_thresh <- 40

#This assumes that 'point' SIF data are dispersed evenly
#New Grid Structure
sifgrid <- rast(nrow = 600, ncol = 744, resolution = 0.05, extent = c(-80.5, -43.3, -21, 9), crs = "EPSG:4326")

# Raster parent directory
parent_dir_rast <- paste0(ncpath, "/complete_rast_", yearid)

# Shapefile parent directory
parent_dir_vect <- paste0(ncpath, "/complete_vect_", yearid)

# List files with ".shp" format in the parent directory
shp_files <- list.files(parent_dir_vect, pattern = "\\.shp$", full.names = TRUE)

num_cores <- 12 #This should work for the Albert Lab Mac with no memory issues

# Define the processing function
process_shapefile <- function(shp_file) {
  
  vectname <- gsub("\\.shp", "", basename(shp_file))
  
  # Convert "vect" to "rast"
  rastname <- gsub("vect", "rast", vectname)
  
  # Read in vector
  sifvect <- vect(shp_file)
  
  # Filter by Cloud Fraction and SZA
  sif_sub <- sifvect[sifvect$cf < cf_thresh & sifvect$sza < sza_thresh, ]
  
  sif_sub$sif_csza <- sif_sub$sif743 / cos(sif_sub$sza * pi / 180)
  sif_sub$sifcor_csza <- sif_sub$sif743_cor / cos(sif_sub$sza * pi / 180)
  
  # Filter out 'cf' and 'sza' layers
  siffilt <- sif_sub[, !names(sif_sub) %in% c("cf", "sza", "vza")]
  
  # Rasterize each element
  sifrast <- rasterize(siffilt, sifgrid, field = c(names(siffilt)), fun = mean)
  #temp
  #sifrast2 <- rasterize(sifvect, sifgrid, field = c(names(sifvect)), fun = mean)
  
  # Give each element a proper name
  names(sifrast) <- names(siffilt)
  #temp
  #names(sifrast2) <- names(sifvect)
  
  # Crop to AMZ extent
  sifamz <- terra::crop(sifrast, amz_shp, mask = TRUE)
  #temp
 # sifamz2 <- terra::crop(sifrast2, amz_shp, mask = TRUE)
  
  # Save each raster to parent_dir_rast
  writeRaster(sifamz, file = file.path(parent_dir_rast, paste0(rastname, ".tif")), overwrite = TRUE)
}

start_time <-  Sys.time()

# Use mclapply for parallel processing
sif_rast <- mclapply(shp_files, process_shapefile, mc.cores = num_cores)

# Print the times for each file
end_time <- Sys.time()

total_time <- difftime(end_time, start_time, units = "mins")
cat(length(shp_files), "rasterized in ", total_time, "minutes\n")

# Optional: plot the last processed raster if needed
# plot(sif_rast[[length(sif_rast)]])

