## Process MCD19A1CMGO v061 data once it is in .tif form (following MATLAB conversion)
rm(list=ls())

#Code for processing MODIS data

library(tidyverse)
library(terra)
library(parallel)
library(gtools)


gc()

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"

yearid <- "2021"

amz_vect <- vect(paste0(wd, "/amz_shps/amz_biome.shp"))

modpath <- paste0(wd, "/mcd19a1cmgo_data_", yearid)

proc1path <- paste0(modpath, "/initial_process")
proc2path <- paste0(modpath, "/second_process")
compiledpath <- paste0(modpath, "/compiled")

num_cores <- 10

# List all .tif files
nbarfiles <- mixedsort(list.files(proc1path, pattern = ".tif", recursive = TRUE, full.names = TRUE))

# Define the processing function with error handling
process_nbar <- function(nbarfile) {
  tryCatch({
    nbarname <- gsub(".tif", "", basename(nbarfile))
    shortname <- paste0("nbarpri_", as.numeric(substr(nbarname, 28, 30)))
    
    nbar_r <- rast(nbarfile)
    
    names(nbar_r) <- c("nbar_refl_b11", "nbar_refl_b12", "qa_qc")
    
    nbar_crop <- crop(nbar_r, amz_vect)
    
    clamp_b11 <- terra::clamp(nbar_crop$nbar_refl_b11, lower = 0, values = FALSE)
    clamp_b12 <- terra::clamp(nbar_crop$nbar_refl_b12, lower = 0, values = FALSE)
    clamp_qa <- terra::clamp(nbar_crop$qa_qc, upper = 2, values = FALSE) #Exclude High AOD values
    
    nbar_clamp <- c(clamp_b11, clamp_b12)
    
    nbar_mask <- mask(nbar_clamp, clamp_qa)
    
    nbar_mask$nbar_refl_b11 <- nbar_mask$nbar_refl_b11 * 0.0001
    nbar_mask$nbar_refl_b12 <- nbar_mask$nbar_refl_b12 * 0.0001
    
    #Function for 'narrow' PRI from De Sousa et al. 2017
    calc_prinar <- function(band11, band12) { (band11 - band12) / (band11 + band12) }
    
    # Calculate NDVI
    prinar <- lapp(c(nbar_mask$nbar_refl_b11, nbar_mask$nbar_refl_b12), fun = calc_prinar)
    
    # Add NDVI and NIRV as new layers to the original SpatRaster
    nbar_clamp <- c(nbar_mask, prinar)
    
    names(nbar_clamp) <- c("refl_b11", "refl_b12", "pri_nar")
    
    # Write the output raster
    writeRaster(nbar_clamp, paste0(proc2path, "/", "amz_", shortname, ".tif"), overwrite = TRUE)
    
  }, error = function(e) {
    cat("Error processing file:", nbarfile, "\n", e$message, "\n")
  })
}

# Record start time
start_time <- Sys.time()

# Process the data in parallel using mclapply
results <- mclapply(nbarfiles, process_nbar, mc.cores = num_cores)

end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "mins")
cat("Total time taken: ", total_time, "minutes\n")



#### Process into 16 day chunks ----------------------------

# List the processed nbar files
nbar_procfiles <- mixedsort(list.files(proc2path, pattern = "amz_nbarpri", recursive = TRUE, full.names = TRUE))

# Define the processing function
process_raster_subset <- function(files_subset) {
  tryCatch({
    # Read rasters into a list
    rlist <- lapply(files_subset, function(file) {
      r <- rast(file)
      if (is.null(r) || !inherits(r, "SpatRaster")) {
        stop(paste("Failed to read raster from file:", file))
      }
      return(r)
    })
    
    # Create a stack of the rasters
    s <- sds(rlist)
    
    # Calculate the mean raster
    mean_raster <- app(s, mean, na.rm = T)
    
    # Generate the output filename based on the first file's number
    first_number <- as.numeric(substr(gsub(".tif", "", basename(files_subset[1])), 13, 15))
    output_file <- paste0("nbarpri_16day_doy", first_number, ".tif")
    
    # Save the mean raster
    writeRaster(mean_raster, paste0(compiledpath, "/", output_file), overwrite = TRUE)
    
  }, error = function(e) {
    cat("Error processing files:", files_subset, "\n", e$message, "\n")
  })
}

# Prepare the file subsets for parallel processing
file_subsets <- split(nbar_procfiles, ceiling(seq_along(nbar_procfiles) / 16))

# Start parallel processing with mclapply
start_time <- Sys.time()

# Use mclapply to process file subsets in parallel
results <- mclapply(file_subsets, process_raster_subset, mc.cores = num_cores)

end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "mins")
cat("Total time taken: ", total_time, "minutes\n")

