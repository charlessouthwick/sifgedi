
#Process MCD18C2_par_data

#Note raw units are in W/m^2
rm(list=ls())
gc()

library(terra)
library(parallel)

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"
boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"

yearid <- "2021"

pardir <- paste0(boxwd, "/MCD18C2_par_data", "/", yearid)
parraw <- paste0(pardir, "/raw")
pardaily <- paste0(pardir, "/daily_amz")
groupedpar <- paste0(pardir, "/par16day_amz")

amz_vect <- vect(paste0(wd, "/amz_shps/amz_biome.shp"))
newcrs <- "EPSG:4326"

amz_ext <- ext(-80.5, -43.3, -21, 9)

#parfile <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi/MCD18C2_par_data/2019/raw/MCD18C2.A2019013.062.2024065050808.hdf"

parfiles <- unlist(list.files(parraw, pattern="*hdf", full.names=T))

process_par <- function(parfile) {
  #for (parfile in seq_along(parfiles)) {
  
  par <- rast(parfile)
  
  parname <- basename(parfile)
  pardate <- sub(".*A(\\d{7}).*", "\\1", parname)
  
  par_c <- crop(par, amz_ext)
  
  par_c[par_c == -1] <- NA
  
  par_sum <- terra::app(par_c, fun = sum)
  
  names(par_sum) <- "par_daily"
  
  par_proj <- project(par_sum, "EPSG:4326")
  
  writeRaster(par_proj, paste0(pardaily, "/par_amz_daily_", pardate, ".tif"), overwrite = TRUE)
  
}

num_cores <- 10
# Use mclapply to run in parallel, or lapply to not
results <- mclapply(parfiles, process_par, mc.cores = num_cores)
#results <- lapply(parfiles, process_par)


#Now process into 16-day chunks ----------------------------------

# List all PAR files
all_rast <- list.files(pardaily, pattern = "par_amz_daily_\\d{7}\\.tif$", full.names = TRUE)

# Extract DOY from filename
get_doy <- function(filename) {
  doy_str <- str_extract(filename, "\\d{7}")  # e.g. 2021332
  as.integer(substr(doy_str, 5, 7))           # extract just the DOY (e.g. 332)
}

# Get DOY for each file
doy_vec <- sapply(all_rast, get_doy)

# Order files and DOYs together
ordered_idx <- order(doy_vec)
all_rast <- all_rast[ordered_idx]
doy_vec <- doy_vec[ordered_idx]

# Assign group based on 16-day bins (starts: 1, 17, 33, ...)
bin_starts <- seq(1, 366, by = 16)
bin_labels <- paste0("doy", bin_starts)

# Bin each DOY to the correct start DOY
bin_id <- cut(doy_vec, breaks = c(bin_starts, 366), right = FALSE, labels = bin_labels)

# Group files by bin
grouped_files <- split(all_rast, bin_id)

# For each group, compute mean raster
mean_rasters <- lapply(names(grouped_files), function(label) {
  file_list <- grouped_files[[label]]
  
  if (length(file_list) == 0) return(NULL)  # Skip empty sets
  
  rlist <- rast(file_list)
  rmean <- terra::app(rlist, fun = mean, na.rm = TRUE)
  names(rmean) <- label
  rmean
})

# Optionally remove the NULLs if needed:
nonull_rasters <- Filter(Negate(is.null), mean_rasters)

for (r in seq_along(nonull_rasters)) {
doydate <- names(nonull_rasters[[r]])
writeRaster(nonull_rasters[[r]], paste0(groupedpar, "/par_amz_16day_", yearid, "_", doydate, ".tif"), overwrite = T)
}


