#Process mcd15a2h data

library(terra)
library(tidyverse)
library(tidyterra)
library(parallel)

rm(list=ls())
gc()

num_cores <- 12 #For running this process in parallel

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"

yearid <- "2021"

data_dir <- paste0(wd, "/mod15a2h_fpar_data_", yearid)

in_dir <- paste0(data_dir, "/raw_tif")
out_dir <- paste0(data_dir, "/initial_process")
proc_dir <- paste0(data_dir, "/second_process")
final_dir <- paste0(data_dir, "/final_process")

amz_vect <- vect(paste0(wd, "/amz_shps/amz_biome.shp"))

# Initial data processsing ------------------------

tiflist <- list.files(in_dir)

# Function to extract doy from filenames
extract_doy <- function(filename) {
  doy <- sub(".*_doy(\\d{7}).*", "\\1", filename)
  return(doy)
}

# Function to extract short name (type) from filename
extract_short_name <- function(filename) {
  short_name <- sub("MOD15A2H\\.061_(.*?)_doy.*", "\\1", filename)
  return(short_name)
}

# Extract the doy for each file
file_info <- data.frame(
  filename = tiflist,
  doy = sapply(tiflist, extract_doy),
  short_name = sapply(tiflist, extract_short_name)
)

# Group files by doy
grouped_files <- file_info %>% group_by(doy) %>% group_split()

# Define the function to process each group of files
process_group <- function(group, in_dir, out_dir) {
  # Create full paths to the files
  full_paths <- file.path(in_dir, group$filename)
  
  # Read the rasters corresponding to this doy
  rasters <- lapply(full_paths, rast)
  
  # Stack them into one multilayer raster
  stacked_raster <- rast(rasters)
  
  # Rename the layers based on the short names
  names(stacked_raster) <- group$short_name
  
  # Define the output filename
  output_filename <- file.path(out_dir, paste0("proc_MOD15A2H_doy", group$doy[1], ".tif"))
  
  # Save the stacked raster to the output directory
  writeRaster(stacked_raster, output_filename, overwrite = TRUE)
}

# Run the process in parallel using mclapply
mclapply(grouped_files, process_group, in_dir = in_dir, out_dir = out_dir, mc.cores = num_cores)

# Run the process without parallel processing, if preferred
#lapply(grouped_files, process_group, in_dir = in_dir, out_dir = out_dir)



