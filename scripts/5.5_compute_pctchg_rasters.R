# Maps of processed GEDI DATA


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

# Now we'll calculate Relative Amplitude % change for a few of our variables ----------------------------------

# Set working directory to the parent folder
setwd(compiled_dir)

# Years to process
yearids <- 2019:2021

# Variables of interest
varids <- c("sif_par", "cci", "fesc", "pai_toc", "modis_lai", "pri_nar")

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
print(names(all_stack))

rel_ampl_list <- list()

# loop through years
for (var in varids) {
  #extract only the layers for a given variable
  v_layers <- all_stack[[grep(paste0("^", var, "$"), names(all_stack))]]
  
  #calculate per-pixel minimum & maximum ACROSS all layers (dates) of that variable
  v_min <- app(v_layers, fun = min, na.rm = TRUE)
  v_max <- app(v_layers, fun = max, na.rm = TRUE)
  v_rel_ampl <- (abs(v_max - v_min) / ((abs(v_max) + abs(v_min)) / 2)) * 100
  
  #name the output layer
  names(v_rel_ampl) <- paste0(var, "_rel_ampl")
  
  #store in list
  rel_ampl_list[[var]] <- v_rel_ampl
}

#combine into one
rs_rel_ampl <- rast(rel_ampl_list)

plot(rs_rel_ampl)

# save
writeRaster(rs_rel_ampl, paste0(compiled_dir, "/rs_rel_ampl.tif"), overwrite = TRUE)

################# ------------------------------------------------------
#Workshopping:
# Set working directory to the parent folder
setwd(compiled_dir)

# Years to process
yearids <- 2019:2021

# Variables of interest
varids <- c("sif_par", "cci", "fesc", "pai_toc", "modis_lai", "pri_nar")

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

rel_ampl_list <- list()

for (var in varids) {
  # Extract only the layers for a given variable
  v_layers <- all_stack[[grep(paste0("^", var, "$"), names(all_stack))]]
  
  # Split layers by year based on total layers per year
  year_layers <- list()
  start_idx <- 1
  for (year in yearids) {
    # Count layers for this year
    n_year <- length(list.files(file.path(compiled_dir, year), pattern = "\\.tif$", full.names = TRUE))
    end_idx <- start_idx + n_year - 1
    year_layers[[as.character(year)]] <- v_layers[[start_idx:end_idx]]
    start_idx <- end_idx + 1
  }
  
  # Find minimum number of layers across years for alignment
  min_layers <- min(sapply(year_layers, nlyr))
  
  # Compute mean per day across years
  doy_means <- lapply(1:min_layers, function(d) {
    # Select the d-th layer from each year (if it exists)
    layers_for_day <- lapply(year_layers, function(yl) yl[[d]])
    # Stack and compute mean
    app(rast(layers_for_day), fun = mean, na.rm = TRUE)
  })
  
  # Stack mean-of-day rasters
  doy_stack <- rast(doy_means)
  
  # Compute min, max, relative amplitude across DOY-mean stack
  v_min <- app(doy_stack, fun = min, na.rm = TRUE)
  v_max <- app(doy_stack, fun = max, na.rm = TRUE)
  v_rel_ampl <- (abs(v_max - v_min) / ((abs(v_max) + abs(v_min)) / 2)) * 100
  
  # Name and store
  names(v_rel_ampl) <- paste0(var, "_rel_ampl")
  rel_ampl_list[[var]] <- v_rel_ampl
}

# Combine all variables
rs_rel_ampl <- rast(rel_ampl_list)
plot(rs_rel_ampl)
writeRaster(rs_rel_ampl, paste0(compiled_dir, "/rs_rel_ampl_across_years.tif"), overwrite = TRUE)

#Let's take a quick look at the Southern deciduous region
# cropext <- ext(-70, -50, -21, -5)
# rs_rel_ampl_c <- crop(rs_rel_ampl, cropext)
# 
# plot(rs_rel_ampl_c$cci)
# 
# deciduous_poly <- terra::draw(x = "polygon", col = 'red', n = 30)
# 
# plot(rs_rel_ampl$sif_par)
# plot(deciduous_poly, add = T)
# 
# writeVector(deciduous_poly, paste0(compiled_dir, "/deciduous_poly.shp"), overwrite = T)

# Now let's produce a large mean raster of our key variables -----------------------------

for (year in yearids){
  
  rastpath <- paste0(compiled_dir, "/", year)
  
  rast_files <- mixedsort(sort(list.files(rastpath, pattern=glob2rx("complete_rast*.tif$"), recursive = TRUE, full.names = TRUE)))
  
  rastname <- unlist(lapply(rast_files, function(x) sub(".*complete_rast_(\\d+).tif", "rast\\1", x)))
  
  doy <- as.numeric(gsub(paste0("rast", year), "", rastname))
  
  rastlist <- lapply(seq_along(rast_files), function(i) {
    # Vectorize shapefiles and filter columns
    current_r <- rast(rast_files[[i]]) %>%
      select(c(doymin, georeg_agg, pai_toc, sif_par, modis_lai, nirv, pri_nar, cci))
    
    return(current_r)
  })
  
  #merge all dates into one mean for each variable. We end up with an annual mean raster
  yr_r <- do.call(mosaic, c(rastlist, fun = 'mean'))
  
  yr_ext <- ext(yr_r)
  
  yr_r$year <- year
  
  yr_r <- extend(yr_r, yr_ext)
  
  # Save the merged raster for the current year
  out_path <- paste0(compiled_dir, "/merged_mean_rast_", year, ".tif")
  writeRaster(yr_r, out_path, overwrite = TRUE)
  
  # Print a message to confirm the processing of each year
  cat("Merged raster for", year, "saved to", out_path, "\n")
}

# Process yearly data---------------------------------------

merge_files <- mixedsort(sort(list.files(compiled_dir, pattern=glob2rx("merged_mean_rast_*.tif$"), recursive = TRUE, full.names = TRUE)))

mergelist <- lapply(merge_files, rast)
merge_sprc <- sprc(mergelist) #maybe delete
allyr_r <- mosaic(merge_sprc) #maybe delete

#Process just the PAI sampling years
paiyears <- lapply(mergelist, function(r) {
  select(r, c("pai_toc", "year")) %>% 
    drop_na()
})

merge_paiyr <- sprc(paiyears)
paiyr_mos <- merge(merge_paiyr)
paiyr <- paiyr_mos$year

plot(paiyr)

writeRaster(paiyr, paste0(compiled_dir, "/gedi_yrly_rasterized.tif"), overwrite = TRUE)

#
# Let's process the PACE data in the same way ----------------------------------
# Variables of interest
pacevars <- c("ndvi", "cci", "cire", "car", "pri", "chlcar")
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
filtered_rasters <- lapply(pace_files, read_and_filter)

#Stack em up
pace_stack <- rast(filtered_rasters)

# Create a new list, and process each variable in the list
pace_rel_ampl_list <- list()

for (var in pacevars) {
  # Extract all layers for the variable across time
  v_layers <- pace_stack[[grep(paste0("^", var, "$"), names(pace_stack))]]
  
  # Compute min, max, and relative amplitude
  v_min <- app(v_layers, min, na.rm = TRUE)
  v_max <- app(v_layers, max, na.rm = TRUE)
  v_rel_ampl <- (abs(v_max - v_min) / ((abs(v_max) + abs(v_min)) / 2)) * 100
  
  # Set name and store
  names(v_rel_ampl) <- paste0(var, "_rel_ampl")
  pace_rel_ampl_list[[var]] <- v_rel_ampl
}

# Combine into one output raster
pace_rs_rel_ampl <- rast(pace_rel_ampl_list)

# save
writeRaster(pace_rs_rel_ampl, paste0(compiled_dir, "/pace_rs_rel_ampl.tif"), overwrite = TRUE)


################# ------------------------------------------------------
#Workshopping PACE:

pacevars <- c("ndvi", "cci", "cire", "car", "pri", "chlcar")
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
filtered_rasters <- lapply(pace_files, read_and_filter)

paceyears <- 2024:2025

#Stack em up
pace_stack <- rast(filtered_rasters)

# Create a new list, and process each variable in the list
pace_rel_ampl_list <- list()
# 
# for (var in pacevars) {
#   # Extract only the layers for a given variable
#   v_layers <- pace_stack[[grep(paste0("^", var, "$"), names(pace_stack))]]
# 
#   # Split layers by year based on total layers per year
#   year_layers <- list()
#   start_idx <- 1
#   for (year in paceyears) {
#     # Count layers for this year
#     n_year <- length(list.files(file.path(pacedir), pattern = "\\.tif$", full.names = TRUE))
#     end_idx <- start_idx + n_year - 1
#     year_layers[[as.character(year)]] <- v_layers[[start_idx:end_idx]]
#     start_idx <- end_idx + 1
#   }
# 
#   # Find minimum number of layers across years for alignment
#   min_layers <- min(sapply(year_layers, nlyr))
# 
#   # Compute mean per day across years
#   doy_means <- lapply(1:min_layers, function(d) {
#     # Select the d-th layer from each year (if it exists)
#     layers_for_day <- lapply(year_layers, function(yl) yl[[d]])
#     # Stack and compute mean
#     app(rast(layers_for_day), fun = mean, na.rm = TRUE)
#   })
# 
#   # Stack mean-of-day rasters
#   doy_stack <- rast(doy_means)
# 
#   # Compute min, max, relative amplitude across DOY-mean stack
#   v_min <- app(doy_stack, fun = min, na.rm = TRUE)
#   v_max <- app(doy_stack, fun = max, na.rm = TRUE)
#   v_rel_ampl <- (abs(v_max - v_min) / ((abs(v_max) + abs(v_min)) / 2)) * 100
# 
#   # Name and store
#   names(v_rel_ampl) <- paste0(var, "_rel_ampl")
#   pace_rel_ampl_list[[var]] <- v_rel_ampl
# }
# 
# # Combine all variables
# pace_rel_ampl <- rast(pace_rel_ampl_list)


n_files <- length(filtered_rasters)
if (n_files != length(pace_files)) stop("pace_files and filtered_rasters must align in order and length")

# extract file dates (assumes yyyymmdd appears in filename, e.g. pace_amz_4km_20240305)
file_dates <- as.Date(gsub(".*(\\d{8}).*", "\\1", basename(pace_files)), format = "%Y%m%d")
file_years <- as.integer(format(file_dates, "%Y"))

pace_rel_ampl_list <- list()

for (var in pacevars) {
  message("Processing variable: ", var)
  
  # --- Build a per-file list of the variable layer (robust to missing variables) ---
  file_idx_with_var <- which(sapply(filtered_rasters, function(r) var %in% names(r)))
  if (length(file_idx_with_var) == 0) {
    warning("Variable ", var, " not found in any files; skipping.")
    next
  }
  
  # Extract the single-layer SpatRaster for the variable from each file that has it
  var_layer_list <- lapply(file_idx_with_var, function(i) {
    r <- filtered_rasters[[i]]
    idx <- which(names(r) == var)
    # if multiple layers of same name in a file (unlikely), take the first
    r[[idx[1]]]
  })
  
  # Stack these per-file variable layers into a single SpatRaster (time series)
  v_layers <- rast(var_layer_list)   # length = number of files that contained `var`
  
  # years corresponding to these layers
  years_for_var <- file_years[file_idx_with_var]
  
  # --- Group indices by year (in chronological order because pace_files were sorted) ---
  idx_by_year <- split(seq_along(years_for_var), years_for_var)
  
  # Convert split indices into SpatRasters per year (each with length = number of periods that year)
  year_layers <- lapply(idx_by_year, function(idxs) {
    # idxs are indices into v_layers (and file_idx_with_var)
    v_layers[[idxs]]
  })
  
  # Ensure each year group is ordered by date (should be already because pace_files were sorted)
  # but we can check / sort by the underlying file_dates if desired:
  # (not needed if pace_files already sorted)
  
  # --- Align years by the minimum number of periods present in any year ---
  min_periods <- min(sapply(year_layers, nlyr))
  if (min_periods < 1) {
    warning("No periods found for variable ", var)
    next
  }
  
  # --- For each period index (1..min_periods), compute mean across years ---
  # result: a list of SpatRasters (one per period) where each is the mean across years
  period_means <- lapply(seq_len(min_periods), function(d) {
    # collect d-th layer from each year (each element of year_layers is a SpatRaster)
    layers_for_period <- lapply(year_layers, function(yr_rast) yr_rast[[d]])
    # stack them and compute mean
    app(rast(layers_for_period), mean, na.rm = TRUE)
  })
  
  # Stack the period-means (this is the seasonal series of means for this variable)
  doy_stack <- rast(period_means)
  
  # --- Compute min/max and relative amplitude across the period-mean stack ---
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




#Old, with percent change:

# #early dry
# rast145 <- rast(paste0(compiled_dir, "/", year, "/compiled_rast_", year, "145.tif"))%>%
#   select(c(doymin, zone, pai, pai_us, pai_toc, sif_par, modis_lai, nirv, pri_nar, cci))
# rast161 <- rast(paste0(compiled_dir, "/", year, "/compiled_rast_", year, "161.tif"))%>%
#   select(c(doymin, zone, pai, pai_us, pai_toc, sif_par, modis_lai, nirv, pri_nar, cci))
# rast177 <- rast(paste0(compiled_dir, "/", year, "/compiled_rast_", year, "177.tif"))%>%
#   select(c(doymin, zone, pai, pai_us, pai_toc, sif_par, modis_lai, nirv, pri_nar, cci))
# 
# earlydrylist <- list(rast145, rast161, rast177)
# 
# #late dry
# rast225 <- rast(paste0(compiled_dir, "/", year, "/compiled_rast_", year, "225.tif"))%>%
#   select(c(doymin, zone, pai, pai_us, pai_toc, sif_par, modis_lai, nirv, pri_nar, cci))
# rast241 <- rast(paste0(compiled_dir, "/", year, "/compiled_rast_", year, "241.tif"))%>%
#   select(c(doymin, zone, pai, pai_us, pai_toc, sif_par, modis_lai, nirv, pri_nar, cci))
# rast257 <- rast(paste0(compiled_dir, "/", year, "/compiled_rast_", year, "257.tif"))%>%
#   select(c(doymin, zone, pai, pai_us, pai_toc, sif_par, modis_lai, nirv, pri_nar, cci))
# 
# latedrylist <- list(rast225, rast241, rast257)
# 
# #early wet
# rast273 <- rast(paste0(compiled_dir, "/", year, "/compiled_rast_", year, "273.tif"))%>%
#   select(c(doymin, zone, pai, pai_us, pai_toc, sif_par, modis_lai, nirv, pri_nar, cci))
# rast289 <- rast(paste0(compiled_dir, "/", year, "/compiled_rast_", year, "289.tif"))%>%
#   select(c(doymin, zone, pai, pai_us, pai_toc, sif_par, modis_lai, nirv, pri_nar, cci))
# rast305 <- rast(paste0(compiled_dir, "/", year, "/compiled_rast_", year, "305.tif"))%>%
#   select(c(doymin, zone, pai, pai_us, pai_toc, sif_par, modis_lai, nirv, pri_nar, cci))
# 
# earlywetlist <- list(rast273, rast289, rast305)
# 
# #Take the mean of each sub-season
# mean_earlydry <- do.call(mosaic, c(earlydrylist, fun = 'mean'))
# mean_latedry <- do.call(mosaic, c(latedrylist, fun = 'mean'))
# mean_earlywet <- do.call(mosaic, c(earlywetlist, fun = 'mean'))
# 
# #add the year
# mean_earlydry$year <- year
# mean_latedry$year <- year
# mean_earlywet$year <- year
# 
# #Calculate the difference
# drydiff_r <- mean_latedry - mean_earlydry
# wetdrydiff_r <- mean_earlywet - mean_earlydry
# 
# #Calculate relative percent change
# dry_relampl_r <- abs(mean_latedry - mean_earlydry) / ((abs(mean_latedry) + abs(mean_earlydry)) / 2) * 100
# wetdry_relampl_r <- abs(mean_earlywet - mean_earlydry) / ((abs(mean_earlywet) + abs(mean_earlydry)) / 2) * 100

# diff_files<- mixedsort(sort(list.files(compiled_dir, pattern=glob2rx("dry_diff_rast_*.tif$"), recursive = TRUE, full.names = TRUE)))
# 
# wddiff_files<- mixedsort(sort(list.files(compiled_dir, pattern=glob2rx("wetdry_diff_rast_*.tif$"), recursive = TRUE, full.names = TRUE)))
# 
# relchng_files<- mixedsort(sort(list.files(compiled_dir, pattern=glob2rx("dry_relampl_rast_*.tif$"), recursive = TRUE, full.names = TRUE)))
# 
# wdrelchng_files<- mixedsort(sort(list.files(compiled_dir, pattern=glob2rx("wetdry_relampl_rast_*.tif$"), recursive = TRUE, full.names = TRUE)))


# #Now process just dry season differences
# difflist <- lapply(diff_files, rast)
# diff_sprc <- sprc(difflist)
# #allyr_r <- merge(merge_sprc)
# diff_r <- mosaic(diff_sprc)
# 
# #Now process wet and dry season differences
# wddifflist <- lapply(wddiff_files, rast)
# wddiff_sprc <- sprc(wddifflist)
# wddiff_r <- mosaic(wddiff_sprc)
# 
# #Now process just dry season pct chng
# relchnglist <- lapply(relchng_files, rast)
# relchng_sprc <- sprc(relchnglist)
# relchng_r <- mosaic(relchng_sprc)
# 
# #Now process wet and dry season pct chng
# wdrelchnglist <- lapply(wdrelchng_files, rast)
# wdrelchng_sprc <- sprc(wdrelchnglist)
# wdrelchng_r <- mosaic(wdrelchng_sprc)



