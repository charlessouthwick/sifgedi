# Regrid the MOD15A2H data to the SIF extent


#troposif rasterize

rm(list=ls())
gc()

library(tidyverse)
library(terra)
library(parallel)

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"

yearid <- "2021"

tifpath <- paste0(wd, "/mod15a2h_fpar_data_", yearid, "/final_process")

amz_shp <- vect(paste0(wd, "/amz_shps/amz_biome.shp"))

# Raster parent directory
out_dir <- paste0(wd, "/mod15a2h_fpar_data_", yearid, "/resampled_rast")


sifgrid <- rast(paste0(wd, "/troposif_data/2020/complete_rast_2020/amz_troposif_rast_doy1.tif"))

# List files
rast_list <- list.files(tifpath, pattern = "\\.tif$", full.names = TRUE)

# Read only the 1st and 4th layers from each raster, corresponding to fpar and LAI
rast_files <- lapply(rast_list, function(file) {
  rast_file <- rast(file)
  subset(rast_file, c(1, 4)) # Extract layers 1 and 4
})

# Resample each raster in the list to match the target raster dimensions ---------------------

start_time <-  Sys.time()

resamplist <- lapply(rast_files, function(r) {
  resampled <- resample(r, sifgrid, method = "bilinear", threads = TRUE)
  # Rename the remaining layers
  names(resampled) <- c('fpar', 'modis_lai')  # Name the first layer 'fpar' and the second (originally fourth) 'modis_lai'
  
  return(resampled)
})

end_time <- Sys.time()

total_time <- difftime(end_time, start_time, units = "mins")
cat(length(rast_files), " rasters resampled in ", total_time, "minutes\n")

# Save each resampled raster to a new file
output_paths <- file.path(out_dir, paste0("resampled_", basename(rast_list)))
mapply(writeRaster, resamplist, output_paths, overwrite = TRUE)

