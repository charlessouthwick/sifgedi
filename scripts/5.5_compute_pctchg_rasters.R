# Processing % change maps

#Charles Southwick


rm(list=ls())
gc()

library(tidyverse)
library(terra)
library(tidyterra)
library(gtools)
library(viridis)

par(mfrow = c(1, 1))

#wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"
boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"
compiled_dir <- paste0(boxwd, "/compiled_rasters")
pacedir <- paste0(compiled_dir, "/pace_clean_rasters")
#paceout <- paste0(boxwd, "/pace_vi_data")

# Now we'll calculate Annual Relative Amplitude % change for a few of our variables ----------------------------------

# Set working directory to the parent folder
setwd(compiled_dir)

# Years to process
yearids <- 2019:2021

# Variables of interest
# varids <- c("sif_par", "cci", "fesc", "pai_toc", "modis_lai", "pri_nar")
varids <- c("sif_parm", "cci", "fesc", "fesc_tropo_refl", "pai_toc", "modis_lai", "phifm_tropo_rad")

all_files <- list()

for (year in yearids){
  filestruct <- file.path(compiled_dir, year)
  filelist <- gtools::mixedsort(list.files(filestruct, pattern = "\\.tif$", full.names = TRUE))
  all_files <- c(all_files, filelist)
}

# collect all file paths
all_files <- unlist(all_files)

# stack em up
all_stack <- rast(all_files)

# Extract dates from filenames (assuming YYYYMMDD appears in filenames)
file_dates <- as.Date(
  strptime(gsub(".tif", "", gsub("complete_rast_", "", basename(all_files))), format = "%Y%j")
)
file_years <- as.integer(format(file_dates, "%Y"))
file_doys  <- as.integer(format(file_dates, "%j"))

#Create filtering mask
phifmtr_layers <- all_stack[[grep("^phifm_tropo_rad$", names(all_stack))]]
sifreltr_layers <- all_stack[[grep("^sif_rel_tropo$", names(all_stack))]]
phifmtr_mask <- (phifmtr_layers < 7e-08) & (phifmtr_layers > -1e-08)
sifreltr_mask <- (sifreltr_layers < 0.03) & (sifreltr_layers > 0)

rel_ampl_list <- list()

for (var in varids) {
  message("Processing variable: ", var)
  
  # Extract all layers for this variable across all dates
  v_layers <- all_stack[[grep(paste0("^", var, "$"), names(all_stack))]]
  
  for (i in seq_len(nlyr(v_layers))) {
    # Apply mask to this layer
    v_layers[[i]] <- mask(v_layers[[i]], phifmtr_mask[[i]], maskvalues = FALSE)
    v_layers[[i]] <- mask(v_layers[[i]], sifreltr_mask[[i]], maskvalues = FALSE)
  }
  
  # Associate each raster layer with its DOY
  if (nlyr(v_layers) != length(file_doys)) {
    stop("Mismatch between number of layers and number of file dates — check file naming or stacking order.")
  }
  
  # Group by DOY (across all years)
  idx_by_doy <- split(seq_along(file_doys), file_doys)
  
  # Compute mean raster per DOY across any available rasters (some years will have 1, others >1)
  doy_means <- lapply(idx_by_doy, function(idxs) {
    layers_for_doy <- v_layers[[idxs]]
    app(layers_for_doy, mean, na.rm = TRUE)
  })
  
  # Stack them in chronological DOY order
  doy_order <- sort(as.integer(names(idx_by_doy)))
  doy_stack <- rast(doy_means[order(doy_order)])
  
  # Compute min, max, relative amplitude across DOY-mean stack
  v_min <- app(doy_stack, fun = min, na.rm = TRUE)
  v_max <- app(doy_stack, fun = max, na.rm = TRUE)
  
  denom <- (abs(v_max) + abs(v_min)) / 2
  denom[denom == 0] <- NA
  
  v_rel_ampl <- (abs(v_max - v_min) / denom) * 100
  names(v_rel_ampl) <- paste0(var, "_rel_ampl")
  
  
  rel_ampl_list[[var]] <- v_rel_ampl
}

# Combine all variables
rs_rel_ampl <- rast(rel_ampl_list)
plot(rs_rel_ampl)

writeRaster(rs_rel_ampl, paste0(compiled_dir, "/rs_rel_ampl_across_years.tif"), overwrite = TRUE)

#
# Let's process the PACE data in the same way ----------------------------------
#Process PACE

pacevars <- c("cci", "cire", "car", "pri", "chlcar")
n_vars <- length(pacevars)
#Need to add car variable!

pace_files <- gtools::mixedsort(list.files(pacedir, pattern = "\\.tif$", full.names = TRUE))

# Function to read and filter each file
read_and_filter <- function(f) {
  r <- rast(f)

  # Extract filtering variables
  chlcar <- r$chlcar
  car    <- r$car
  cire   <- r$cire

  # Build mask: valid if all conditions met
  valid_mask <- (chlcar > 0 & chlcar < 7) &
    (car < 15) &
    (cire < 6)

  # Mask all layers
  r_filtered <- mask(r, valid_mask, maskvalues = FALSE)

  return(r_filtered)
}

# Apply to all files
filtered_pace <- lapply(pace_files, read_and_filter)

paceyears <- 2024:2025

#Stack em up
pace_stack <- rast(filtered_pace)

# Create a new list, and process each variable in the list
pace_rel_ampl_list <- list()

n_files <- length(filtered_pace)

# extract file dates (assumes yyyymmdd appears in filename, e.g. pace_amz_4km_20240305)
file_dates <- as.Date(gsub(".*(\\d{8}).*", "\\1", basename(pace_files)), format = "%Y%m%d")
file_years <- as.integer(format(file_dates, "%Y"))

pace_rel_ampl_list <- list()

#var is one of the VIs
for (var in pacevars) {
  message("Processing variable: ", var)
  
  # Build a per-file list of the variable layer
  file_idx_with_var <- which(sapply(filtered_pace, function(r) var %in% names(r)))

  # Extract the single-layer SpatRaster for the variable from each file that has it
  var_layer_list <- lapply(file_idx_with_var, function(i) {
    r <- filtered_pace[[i]]
    idx <- which(names(r) == var)
    # if multiple layers of same name in a file, take the first
    r[[idx[1]]]
  })
  
  # Stack these per-file variable layers into a single SpatRaster (time series). This is many layers of a single variable pulled from a given 8-day period.
  v_layers <- rast(var_layer_list)   # length = number of files that contained `var`
  
  # Associate each raster with its date and DOY
  dates_for_var <- file_dates[file_idx_with_var]
  doys <- as.integer(format(dates_for_var, "%j"))
  
  # Group indices by DOY
  idx_by_doy <- split(seq_along(doys), doys)
  
  # Compute mean across all rasters (any year) for each DOY
  doy_means <- lapply(idx_by_doy, function(idxs) {
    layers_for_doy <- v_layers[[idxs]]
    app(layers_for_doy, mean, na.rm = TRUE)
  })
  
  # Stack in chronological DOY order
  doy_order <- sort(as.integer(names(idx_by_doy)))
  doy_stack <- rast(doy_means[order(doy_order)])
  
  # Compute min/max and relative amplitude across the period-mean stack
  v_min <- app(doy_stack, min, na.rm = TRUE)
  v_max <- app(doy_stack, max, na.rm = TRUE)
  
  # avoid divide-by-zero: use mean of abs(max,min) denominator but guard zeros
  denom <- (abs(v_max) + abs(v_min)) / 2
  denom[denom == 0] <- NA  # or small number if you prefer
  
  v_rel_ampl <- (abs(v_max - v_min) / denom) * 100
  
  names(v_rel_ampl) <- paste0(var, "_rel_ampl")
  pace_rel_ampl_list[[var]] <- v_rel_ampl
}

# Combine into one multi-layer SpatRaster
pace_rel_ampl <- rast(pace_rel_ampl_list)

writeRaster(pace_rel_ampl, paste0(compiled_dir, "/pace_rs_rel_ampl_across_years.tif"), overwrite = TRUE)

