#Process mcd15a2h data

library(terra)
library(tidyverse)
library(tidyterra)
library(parallel)

rm(list=ls())
gc()

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"

#Select one of 2019, 2020, 2021
yearid <- "2021"

data_dir <- paste0(wd, "/mod15a2h_fpar_data_", yearid)

in_dir <- paste0(data_dir, "/raw_tif")
out_dir <- paste0(data_dir, "/initial_process")
proc_dir <- paste0(data_dir, "/second_process")
final_dir <- paste0(data_dir, "/final_process")

amz_vect <- vect(paste0(wd, "/amz_shps/amz_biome.shp"))


#Functions to parse the MODIS QA bits --------------------------------

# Function to extract specific bits and return as numeric
parse_fparlai_qc <- function(x) {
  # Extract individual quality fields
  modland_qc <- bitwAnd(x, 1)                     # Bit 0: MODLAND_QC
  sensor <- bitwShiftR(bitwAnd(x, 2), 1)          # Bit 1: Sensor
  dead_detector <- bitwShiftR(bitwAnd(x, 4), 2)   # Bit 2: DeadDetector
  cloud_state <- bitwShiftR(bitwAnd(x, 24), 3)    # Bits 3-4: CloudState
  scf_qc <- bitwShiftR(bitwAnd(x, 224), 5)        # Bits 5-7: SCF_QC
  
  # Apply quality control criteria
  good_qual <- as.numeric(
    (modland_qc == 0) &                         # MODLAND_QC must be 0
      (dead_detector == 0) &                     # DeadDetector must be 0
      (cloud_state %in% c(0, 3)) &               # CloudState must be 0 or 3
      (scf_qc %in% c(0, 1, 2))                   # SCF_QC must be 0, 1, or 2
  )
  
  return(good_qual) # Must return numeric value
}

# Function to parse and filter the FparExtra_QC layer
parse_fparextra_qc <- function(x) {
  # Extract individual quality fields
  land_sea <- bitwAnd(x, 3)                          # Bits 0-1: LandSea
  snow_ice <- bitwShiftR(bitwAnd(x, 4), 2)           # Bit 2: Snow_Ice
  aerosol <- bitwShiftR(bitwAnd(x, 8), 3)            # Bit 3: Aerosol
  cirrus <- bitwShiftR(bitwAnd(x, 16), 4)            # Bit 4: Cirrus
  internal_cloud_mask <- bitwShiftR(bitwAnd(x, 32), 5) # Bit 5: Internal_CloudMask
  cloud_shadow <- bitwShiftR(bitwAnd(x, 64), 6)      # Bit 6: Cloud_Shadow
  scf_biome_mask <- bitwShiftR(bitwAnd(x, 128), 7)   # Bit 7: SCF_Biome_Mask
  
  # Apply quality control criteria:
  good_qual <- as.numeric(
    (land_sea %in% c(0, 1)) &    # - LandSea: Keep only 0 (LAND) or 1 (SHORE)
      (snow_ice == 0) &          # - Snow_Ice: Keep only 0 (No snow/ice detected)
      #(aerosol == 0) &          # - Aerosol: Keep only 0 (No or low aerosol levels)
      (cirrus == 0) &            # - Cirrus: Keep only 0 (No cirrus detected)
      (internal_cloud_mask == 0) #&  # - Internal_CloudMask: Keep only 0 (No clouds)
    #(cloud_shadow == 0) &       # - Cloud_Shadow: Keep only 0 (No cloud shadow detected)
    #(scf_biome_mask == 0)       # - SCF_Biome_Mask: Keep only 0 (Biome outside interval <1,4>)
  )
  
  return(good_qual) # Return numeric result
}

#### Second round processing ---------------------------------------

#This loop is quite memory intensive! Recommended to have at least 64GB free!

proclist <- list.files(out_dir)

proc_ext <- terra::ext(rast(file.path(out_dir, proclist[[1]])))

start_time <-  Sys.time()

# Iterate through each group of files for a specific doy
for (i in proclist) {
  
  fullpath <- file.path(out_dir, i)
  
  r_file <- rast(fullpath)
  
  #Pull out the Day of Year for saving clean file
  doy <- sub(".*_doy(\\d{7}).*", "\\1", i)
  
  # Crop by the extent of the Amazon
  #r_file <- crop(r_file, amz_vect, mask = TRUE)
  
  # Extract layers
  fpar_lyr <- r_file[["Fpar_500m"]]
  lai_lyr <- r_file[["Lai_500m"]]
  fparextraqc_lyr <- r_file[["FparExtra_QC"]]
  fparlaiqc_lyr <- r_file[["FparLai_QC"]]
  
  # Apply quality control and filtering thresholds
  good_fparlaiqc <- app(fparlaiqc_lyr, parse_fparlai_qc) # QC for FparLai
  good_fextraqc <- app(fparextraqc_lyr, parse_fparextra_qc) # QC for FparExtra
  
  # Mask Fpar and Lai layers based on thresholds and quality control
  fpar_filt <- mask(ifel(fpar_lyr >= 2.49, NA, fpar_lyr), good_fparlaiqc, maskvalue = 0)
  lai_filt <- mask(ifel(lai_lyr >= 24.9, NA, lai_lyr), good_fparlaiqc, maskvalue = 0)
  
  # Mask the Fpar and LAI data further with FparExtra QC
  fpar_filt <- mask(fpar_filt, good_fextraqc, maskvalue = 0)
  lai_filt <- mask(lai_filt, good_fextraqc, maskvalue = 0)
  
  # Update the raster stack
  r_file[["Fpar_500m"]] <- fpar_filt
  r_file[["Lai_500m"]] <- lai_filt
  r_file[["FparLai_QC"]] <- good_fparlaiqc
  r_file[["FparExtra_QC"]] <- good_fextraqc
  
  #Better not to crop to leave a buffer for resampling later on.
  #r_crop <- crop(r_file, amz_vect, mask = TRUE)
  
  # Extend the spatraster to match the original extent
  #r_extended <- terra::extend(r_file, proc_ext)
  
  # Save the filtered raster to a new file
  writeRaster(r_file, filename = paste0(proc_dir, "/amz_filt_FPAR_LAI_", doy, ".tif"), overwrite = TRUE)
  
  # Clear memory
  rm(fpar_lyr, lai_lyr, fpar_filt, lai_filt, fparextraqc_lyr, fparlaiqc_lyr, fparextraqc_filt)
  gc()  # Trigger garbage collection
}

end_time <- Sys.time()

total_time <- difftime(end_time, start_time, units = "mins")
cat(length(proclist), " rasters cleaned in ", total_time, "minutes\n")


#Now merge the files by date to give 16-day averages -------------

rm(proclist)
gc()

proc2list <- list.files(proc_dir)

start_time <-  Sys.time()

# Loop through the list and process date pairs
for (i in seq(1, length(proc2list) - 1, by = 2)) {
  # Load the two rasters
  raster1 <- rast(paste0(proc_dir, "/", proc2list[i]))
  raster2 <- rast(paste0(proc_dir, "/", proc2list[i + 1]))
  
  r_sds <- sds(raster1, raster2)
  
  mean_raster <- terra::app(r_sds, mean, na.rm = TRUE)
  
  output_filename <- file.path(final_dir, 
                               sub("amz_filt_FPAR_LAI_", "amz_clean_FPAR_LAI_", proc2list[i]))
  
  # Save the new raster
  writeRaster(mean_raster, filename = output_filename, overwrite = TRUE)
  
  rm(raster1, raster2, r_sds)
  gc()
}

end_time <- Sys.time()

total_time <- difftime(end_time, start_time, units = "mins")
cat(length(proc2list), " rasters resampled in ", total_time, "minutes\n")

# testrast <- rast(paste0(final_dir, "/amz_clean_FPAR_LAI_", yearid, "161.tif"))
# plot(testrast)
