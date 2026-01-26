

### Initial processing of TROPOSIF data
library(tidyverse)
library(ncdf4)
#library(ncdf4.helpers)
library(terra)
library(lubridate)
library(fs)
library(parallel)

rm(list=ls())
gc()

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"

#Choose one of 2019, 2020, 2021
yearid <- "2020"

ncpath <- paste0(wd, "/troposif_data/", yearid)

siffiles <- unlist(list.files(ncpath, pattern="*nc", full.names=T))

#play with this to make sure the ncname is saving correctly (change the number 8 accordingly). I'll mess with this another time:
ncname <- unlist(strsplit(unlist(lapply(strsplit(siffiles, "/"), function(x) x[9])),".nc"))
ncdate <- as.numeric(yday(as.Date(str_match(ncname, "\\d{4}-\\d{2}-\\d{2}"))))

amz_ext <- ext(-80.5, -43.3, -21, 9)

amz <- vect(paste0(wd, "/amz_shps/amz_biome.shp"))

num_cores <- 10

##
#Extract NC file data and process ------------------------------------
##

process_troposif <- function(i) {
  sifdf <- NULL
  ncname <- unlist(strsplit(unlist(lapply(strsplit(siffiles[i], "/"), function(x) x[9])), ".nc")) # adjust "9" as needed for the pathname!
  ncdate <- as.numeric(yday(as.Date(str_match(ncname, "\\d{4}-\\d{2}-\\d{2}"))))
  
  nc <- nc_open(siffiles[i])
  
  # Extract variables of interest
  sif743 <- ncvar_get(nc, "PRODUCT/SIF_743")
  sif743_corr <- ncvar_get(nc, "PRODUCT/SIF_Corr_743")
  sif743_err <- ncvar_get(nc, "PRODUCT/SIF_ERROR_743")
  cf <- ncvar_get(nc, "PRODUCT/SUPPORT_DATA/INPUT_DATA/cloud_fraction_L2")
  lat <- ncvar_get(nc, "PRODUCT/latitude")
  lon <- ncvar_get(nc, "PRODUCT/longitude")
  toarad743 <- ncvar_get(nc, "PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/Mean_TOA_RAD_743")
  toa_rfl <- ncvar_get(nc, "PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/TOA_RFL")
  rfl_665 <- toa_rfl[1, ]
  rfl_781 <- toa_rfl[7, ]
  sza <- ncvar_get(nc, "PRODUCT/SUPPORT_DATA/GEOLOCATIONS/solar_zenith_angle")
  vza <- ncvar_get(nc, "PRODUCT/SUPPORT_DATA/GEOLOCATIONS/viewing_zenith_angle")
  
  # Fill data frame
  sifdf <- data.frame(lon = as.vector(lon),
                      lat = as.vector(lat),
                      sif743 = as.vector(sif743),
                      sif743_corr = as.vector(sif743_corr),
                      sif743_err = as.vector(sif743_err),
                      toarad743 = as.vector(toarad743),
                      rfl_665 = as.vector(rfl_665),
                      rfl_781 = as.vector(rfl_781),
                      cf = as.vector(cf),
                      sza = as.vector(sza),
                      vza = as.vector(vza))
  
  sifdf$doy <- ncdate
  
  # Subset by Amazon coordinates
  sif_sub_amz <- subset(sifdf, lat > amz_ext[3] & lat < amz_ext[4] & lon > amz_ext[1] & lon < amz_ext[2])
  
  # Filter by cloud fraction
  sifdf_cf <- subset(sif_sub_amz, cf <= 0.3)
  
  # Save CSV file
  write.csv(sifdf_cf, file.path(ncpath, paste0("amz_", ncname, ".csv")), row.names = FALSE)
  
  # Close NetCDF file
  nc_close(nc)
}

start_time <-  Sys.time()

# Use mclapply to run in parallel
mclapply(seq_along(siffiles), process_troposif, mc.cores = num_cores)

# Print the times for each file
end_time <- Sys.time()

total_time <- difftime(end_time, start_time, units = "mins")
cat(length(siffiles), "processed in ", total_time, "minutes\n")




##--------------------------------------------------------


parent_folder <- ncpath

# List all .csv files in the directory
csv_files <- list.files(parent_folder, pattern = "\\.csv$", full.names = TRUE)

file_dates <- str_match(csv_files, "\\d{4}-\\d{2}-\\d{2}") %>% as.Date(.)

# Function to extract 16-day chunks and save to a new folder
extract_and_save <- function(start_date) {
  end_date <- start_date + 15
  
  selected_files <- csv_files[file_dates >= start_date & file_dates <= end_date]
  
  folder_name <- format(start_date, "%Y.%m.%d")
  proc_name <- "processed"
  dir_create(fs::path(parent_folder, folder_name))
  
  # Move selected files to the new folder
  for (file in selected_files) {
    file.rename(file, fs::path(parent_folder, folder_name, basename(file)))
  }
}

# Loop through the dates in 16-day intervals
start_date <- as.Date(paste0(yearid, "-01-01"))
end_date <- max(file_dates)

#Run the function
while (start_date <= end_date) {
  extract_and_save(start_date)
  start_date <- start_date + 16
}

##
# Combine the csvs in the list, vectorize, rasterize -----------------------
##

num_cores <- 14 #Tinker according to computer specs

parent_dir_vect <- paste0(ncpath, "/complete_vect_", yearid)

date_pattern <- "^\\d{4}\\.\\d{2}\\.\\d{2}$"

# List all date folders within ncpath and filter by name pattern
date_folders <- list.dirs(ncpath, full.names = TRUE, recursive = FALSE)
date_folders <- date_folders[grep(date_pattern, basename(date_folders))]

# Function to read and combine CSV files in a folder, convert to vect, and save
process_date_folder <- function(date_folder) {

  folder_doy <- yday(as.Date(basename(date_folder), "%Y.%m.%d"))

  csv_files <- list.files(date_folder, pattern = "\\.csv$", full.names = TRUE)

  combined_data <- do.call(rbind, lapply(csv_files, read.csv))
  
  # Convert combined data to terra spatvector object
  vect_data <- vect(combined_data, geom = c("lon", "lat"), crs = "epsg:4326")
  
  # Construct the file path for saving the vector with DOY in the filename
  output_file <- file.path(parent_dir_vect, paste0("amz_troposif_vect_doy", folder_doy, ".shp"))
  
  writeVector(vect_data, file = output_file, overwrite = TRUE)
}


start_time <-  Sys.time()

# Parallelize the processing
mclapply(date_folders, process_date_folder, mc.cores = num_cores)

# Print the times for each file
end_time <- Sys.time()

total_time <- difftime(end_time, start_time, units = "mins")
cat(length(date_folders), "processed in ", total_time, "minutes\n")

