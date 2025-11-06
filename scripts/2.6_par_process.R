#Process NCEP Reanlysis PAR data

rm(list=ls())

library(tidyverse)
library(terra)
library(tidyterra) # To filter the spatraster by coordinates


#Data accessed from: https://psl.noaa.gov/data/gridded/data.ncep.reanalysis2.html

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"

yearid <- "2021"

amz_vect <- vect(paste0(wd, "/amz_shps/amz_biome.shp"))

in_data <- paste0(wd, "/par_data/raw")
out_data <- paste0(wd, "/par_data/processed")

#'Nominal top of atmosphere' = TOA, Downward shortwave radiation flux, daily
dswrf_toa <- rast(paste0(in_data, "/dswrf.ntat.gauss.", yearid,".nc"))
#'Surface' = TOC, Downward surface radiation flux, daily 
dswrf_toc <- rast(paste0(in_data, "/dswrf.sfc.gauss.", yearid, ".nc"))

# Generate the index vector to be used with the R terra 'tapp' function
generate_16_day_index <- function(num_layers) {
  # Each element in the sequence gets an integer division by 16, plus 1,
  rep(1:(num_layers %/% 16 + ifelse(num_layers %% 16 > 0, 1, 0)), each=16, length.out = num_layers)
}

# Create the index vector
toa_16dayind <- generate_16_day_index(nlyr(dswrf_toa))
toc_16dayind <- generate_16_day_index(nlyr(dswrf_toc))

# Apply the mean function for each 16-day chunk
toa_16day <- tapp(dswrf_toa, index = toa_16dayind, fun = mean)
toc_16day <- tapp(dswrf_toc, index = toc_16dayind, fun = mean) 

#change the names to be more meaningful
names(toa_16day) <- paste0("par_toa_doy", (seq_along(1:nlyr(toa_16day)) - 1) * 16 + 1)
names(toc_16day) <- paste0("par_toc_doy", (seq_along(1:nlyr(toc_16day)) - 1) * 16 + 1)

#Delta-PAR is TOA - TOC
deltapar_16day <- toa_16day - toc_16day

# Now, rename the layers of deltapar_16day to reflect the 'deltapar_doy*' pattern
names(deltapar_16day) <- paste0("deltapar_doy", (seq_along(1:nlyr(deltapar_16day)) - 1) * 16 + 1)


#Now re-organize geometry away from NOAA format ----------------------

small_toa <- toa_16day %>% tidyterra::filter(x > 270, .keep_extent = FALSE)
small_toc <- toc_16day %>% tidyterra::filter(x > 270, .keep_extent = FALSE)
small_delta <- deltapar_16day %>% tidyterra::filter(x > 270, .keep_extent = FALSE)

toa_old_ext <- ext(small_toa)
toc_old_ext <- ext(small_toc)
delta_old_ext <- ext(small_delta)

convert_lon <- function(old_ext) {
  
  if (old_ext[1] > 180) {
    old_ext[1] <- old_ext[1] - 360
  }
  if (old_ext[2] > 180) {
    old_ext[2] <- old_ext[2] - 360
  }
  
  old_ext
}

# Convert longitude values
toa_new_ext <- convert_lon(toa_old_ext)
toc_new_ext <- convert_lon(toc_old_ext)
delta_new_ext <- convert_lon(delta_old_ext)

# Change the extent
ext(small_toa) <- toa_new_ext
ext(small_toc) <- toc_new_ext
ext(small_delta) <- delta_new_ext

plot(small_toc[[1]])
plot(amz_vect, add = T)

sifgrid <- rast(nrow = 600, ncol = 744, resolution = 0.05, extent = c(-80.5, -43.3, -21, 9), crs = "EPSG:4326")

toa_resamp <- resample(small_toa, sifgrid)
toc_resamp <- resample(small_toc, sifgrid)
delta_resamp <- resample(small_delta, sifgrid)

plot(toc_resamp[[1]])
plot(amz_vect, add = T)

toa_proj <- terra::project(toa_resamp, "epsg:4326")
toc_proj <- terra::project(toc_resamp, "epsg:4326")
delta_proj <- terra::project(delta_resamp, "epsg:4326")

toa_crop <- crop(toa_proj, amz_vect)
toc_crop <- crop(toc_proj, amz_vect)
delta_crop <- crop(delta_proj, amz_vect)

plot(toc_crop[[1]])
plot(amz_vect, add = T)


#Save rasters
writeRaster(toa_crop, paste0(out_data, "/toa_par_amz_", yearid, ".tif"), overwrite = T)
writeRaster(toc_crop, paste0(out_data, "/toc_par_amz_", yearid, ".tif"), overwrite = T)
writeRaster(delta_crop, paste0(out_data, "/delta_par_amz_", yearid, ".tif"), overwrite = T)

