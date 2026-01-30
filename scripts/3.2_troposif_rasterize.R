# Rasterize TROPOSIF data


rm(list=ls())
gc()

library(tidyverse)
library(terra)
library(parallel)


wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"

yearid <- "2021"

#Thresholds for filtering
cf_thresh <- 0.2
sza_thresh <- 45 #Eliminates some high outliers at solstices
vza_thresh <- 35 #Retains ~ 45-60% of swath

vza_wide <- 45 #alternatives for comparison
vza_nar <- 25 #alternatives for comparison

tropopath <- paste0(wd, "/troposif_data")
ncpath <- paste0(wd, "/troposif_data/", yearid)
parent_dir_rast <- paste0(ncpath, "/complete_rast_", yearid)

# List all date folders within ncpath and filter by name pattern
date_pattern <- "^\\d{4}\\.\\d{2}\\.\\d{2}$"
date_folders <- list.dirs(ncpath, full.names = TRUE, recursive = FALSE)
date_folders <- date_folders[grep(date_pattern, basename(date_folders))]

#This assumes that 'point' SIF data are dispersed evenly
#New Grid Structure
sifgrid <- rast(nrow = 600, ncol = 744, resolution = 0.05, extent = c(-80.5, -43.3, -21, 9), crs = "EPSG:4326")
amz_shp <- vect(paste0(wd, "/amz_shps/amz_biome.shp"))

num_cores <- 8 # ~4 minutes on Albert Lab Mac with no memory issues

process_date_folder <- function(date_folder) {
  
  folder_doy <- yday(as.Date(basename(date_folder), "%Y.%m.%d"))
  csv_files <- list.files(date_folder, pattern = "\\.csv$", full.names = TRUE)
  
  df <- do.call(rbind, lapply(csv_files, read.csv))
  
  v <- vect(df, geom = c("lon", "lat"), crs = "EPSG:4326")
  
  #Filter for thresholds
  v2 <- v[v$cf < cf_thresh & v$sza < sza_thresh & v$vza < vza_thresh, ]
  v2 <- v2[, !names(v2) %in% c("cf", "sza", "vza", "doy")]
  
  vnar <- v[v$cf < cf_thresh & v$sza < sza_thresh & v$vza < vza_nar, ]
  vnar <- vnar[, !names(vnar) %in% c("cf", "sza", "vza", "doy")]
  
  vwide <- v[v$cf < cf_thresh & v$sza < sza_thresh & v$vza < vza_wide, ]
  vwide <- vwide[, !names(vwide) %in% c("cf", "sza", "vza", "doy")]
  
  rm(df, v)
  
  #Rasterize, rename, and crop
  r_mean <- rasterize(v2, sifgrid, field = names(v2), fun = mean)
  r_n   <- rasterize(v2, sifgrid, field = "sif743", fun = length)
  
  r_nar <- rasterize(vnar, sifgrid, field = "sif743_cor", fun = mean)
  r_wide <- rasterize(vwide, sifgrid, field = "sif743_cor", fun = mean)
  
  names(r_mean) <- names(v2)
  names(r_n) <- "nsifobs"
  
  r_mid <- r_mean$sif743_cor
  names(r_mid) <- "sif743_cor_vza35"
  names(r_nar) <- "sif743_cor_vza25"
  names(r_wide) <- "sif743_cor_vza45"
  
  r_out <- c(r_mean, r_n)
  r_c <- crop(r_out, amz_shp, mask = TRUE)
  #r$doy <- as.numeric(folder_doy)
  r_vza <- c(r_nar, r_mid, r_wide)
  r_vza_c <- crop(r_vza, amz_shp, mask = TRUE)

  #Write raster
  writeRaster(
    r_c,
    file.path(parent_dir_rast, paste0("amz_troposif_rast_doy", folder_doy, ".tif")),
    overwrite = TRUE
  )
  
  writeRaster(
    r_vza_c,
    file.path(tropopath, paste0("vza_testing/troposif_vzatest_", yearid, "_doy", folder_doy, ".tif")),
    overwrite = TRUE
  )
  
  invisible(NULL) #this makes sure the function doesn't print/return anything
}

start_time <-  Sys.time()
mclapply(date_folders, process_date_folder, mc.cores = num_cores)
end_time <- Sys.time()

total_time <- difftime(end_time, start_time, units = "mins")
cat(length(date_folders), "processed in ", total_time, "minutes\n")


#Testing-------
testlist <- list.files(parent_dir_rast, pattern = ".tif", full.names = TRUE)

r_list <- vector("list", length(testlist))

for (i in seq_along(testlist)) {
  r_list[[i]] <- rast(testlist[i])[[8]]
}

r <- rast(r_list)

plot(r[[1:16]])
#plot(r[[16:23]])


#Pre- Jan 2026

# 
# 
# ##
# 
# 
# rm(list=ls())
# gc()
# 
# library(tidyverse)
# library(terra)
# library(parallel)
# 
# wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"
# 
# yearid <- "2019"
# 
# ncpath <- paste0(wd, "/troposif_data/", yearid)
# 
# amz_shp <- vect(paste0(wd, "/amz_shps/amz_biome.shp"))
# 
# #NOTE! If a more strict CF or SZA is to be used, need to filter before this rasterizing step!
# cf_thresh <- 0.2
# sza_thresh <- 45
# vza_thresh <- 40
# 
# #This assumes that 'point' SIF data are dispersed evenly
# #New Grid Structure
# sifgrid <- rast(nrow = 600, ncol = 744, resolution = 0.05, extent = c(-80.5, -43.3, -21, 9), crs = "EPSG:4326")
# 
# # Raster parent directory
# parent_dir_rast <- paste0(ncpath, "/complete_rast_", yearid)
# 
# # Shapefile parent directory
# parent_dir_vect <- paste0(ncpath, "/complete_vect_", yearid)
# 
# # List files with ".shp" format in the parent directory
# shp_files <- list.files(parent_dir_vect, pattern = "\\.shp$", full.names = TRUE)
# 
# num_cores <- 12 #This should work for the Albert Lab Mac with no memory issues
# 
# # Define the processing function
# process_shapefile <- function(shp_file) {
#   
#   vectname <- gsub("\\.shp", "", basename(shp_file))
#   
#   # Convert "vect" to "rast"
#   rastname <- gsub("vect", "rast", vectname)
#   
#   # Read in vector
#   sifvect <- vect(shp_file)
#   
#   # Filter by Cloud Fraction and SZA
#   sif_sub <- sifvect[sifvect$cf < cf_thresh & sifvect$sza < sza_thresh, ]
#   
#   #Doing this in an earlier step
#   #sif_sub$sif_csza <- sif_sub$sif743 / cos(sif_sub$sza * pi / 180)
#   #sif_sub$sifcor_csza <- sif_sub$sif743_cor / cos(sif_sub$sza * pi / 180)
#   
#   # Filter out 'cf' and 'sza' layers
#   siffilt <- sif_sub[, !names(sif_sub) %in% c("cf", "sza", "vza")]
#   
#   # Rasterize each element
#   sifrast <- rasterize(siffilt, sifgrid, field = c(names(siffilt)), fun = mean)
#   #temp
#   #sifrast2 <- rasterize(sifvect, sifgrid, field = c(names(sifvect)), fun = mean)
#   
#   # Give each element a proper name
#   names(sifrast) <- names(siffilt)
#   #temp
#   #names(sifrast2) <- names(sifvect)
#   
#   # Crop to AMZ extent
#   sifamz <- terra::crop(sifrast, amz_shp, mask = TRUE)
#   #temp
#  # sifamz2 <- terra::crop(sifrast2, amz_shp, mask = TRUE)
#   
#   # Save each raster to parent_dir_rast
#   writeRaster(sifamz, file = file.path(parent_dir_rast, paste0(rastname, ".tif")), overwrite = TRUE)
# }
# 
# start_time <-  Sys.time()
# 
# # Use mclapply for parallel processing
# sif_rast <- mclapply(shp_files, process_shapefile, mc.cores = num_cores)
# 
# # Print the times for each file
# end_time <- Sys.time()
# 
# total_time <- difftime(end_time, start_time, units = "mins")
# cat(length(shp_files), "rasterized in ", total_time, "minutes\n")
# 
# # Optional: plot the last processed raster if needed
# # plot(sif_rast[[length(sif_rast)]])

