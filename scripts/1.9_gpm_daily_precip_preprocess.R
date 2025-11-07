
#This script enables downloading and initial processing of IMERG precipitation data, available (with an account) from: https://arthurhouhttps.pps.eosdis.nasa.gov/gpmdata/

library(httr)
library(rvest)
library(lubridate)
library(parallel)
library(viridis)
library(terra)
library(tidyverse)

# Set download directory
# dl_dir <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi/gpm_precip_data/gpm_gis_zips"
# setwd(dl_dir)
# 
# accum_dir <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi/gpm_precip_data/gpm_accum_files"
# prec_dir <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi/gpm_precip_data/gpm_prec_amz"

#Desktop alternative
gpm_dir <- "/Users/charlessouthwick/Documents/PhD/sifgedi/gpm_precip_data"
dl_dir <- paste0(gpm_dir, "/gpm_gis_zips")
accum_dir <- paste0(gpm_dir, "/gpm_accum_files")
prec_dir <- paste0(gpm_dir, "/gpm_prec_amz")

setwd(dl_dir)

###
#Step 1. Download IMERG files from online repository ---------------------
###

# Set path to .netrc file
usr <- ifelse(Sys.getenv("USERPROFILE") != "", Sys.getenv("USERPROFILE"), Sys.getenv("HOME"))
netrc <- file.path(usr, '.netrc')

# Function to build URL for a given date
build_url <- function(date) {
  y <- format(date, "%Y")
  m <- format(date, "%m")
  d <- format(date, "%d")
  paste0("https://arthurhouhttps.pps.eosdis.nasa.gov/gpmdata/", y, "/", m, "/", d, "/gis/")
}

# Function to extract download links for '3B-DAY-GIS' files on a given date
get_day_links <- function(date) {
  url <- build_url(date)
  tryCatch({
    # Read the directory page
    page <- read_html(GET(url, config(netrc = TRUE, netrc_file = netrc)))
    # Extract links that start with "3B-DAY-GIS"
    links <- page %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      grep("^3B-DAY-GIS.", ., value = TRUE)
    
    # Return full URLs
    paste0(url, links)
  }, error = function(e) {
    NULL  # Skip if the page is not available
  })
}

# Function to download a single file
download_file <- function(file_url) {
  filename <- tail(strsplit(file_url, '/')[[1]], n = 1)
  response <- GET(file_url, write_disk(filename, overwrite = TRUE),
                  config(netrc = TRUE, netrc_file = netrc),
                  progress(), set_cookies("LC" = "cookies"))
  
  if (response$status_code == 200) {
    message(sprintf("✔ %s downloaded", filename))
  } else {
    warning(sprintf("✘ %s failed (status %s)", filename, response$status_code))
  }
}

# Generate all dates from 2019 to 2021
dates <- seq(ymd("2019-01-01"), ymd("2021-12-31"), by = "1 day")

# Get file links (use parallel here if desired)
cl <- makeCluster(8)
clusterExport(cl, c("build_url", "netrc"))
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(httr))
clusterEvalQ(cl, library(xml2))

all_links <- parLapply(cl, dates, get_day_links)
stopCluster(cl)

zip_links_list <- lapply(all_links, function(x) grep("\\.zip$", x, value = TRUE))

# Flatten and filter NULLs
file_urls <- unlist(zip_links_list)
file_urls <- file_urls[!is.na(file_urls)]

results <- lapply(file_urls, download_file)


###
# Step 2: Unzip, and select the files that end in: .total.accum. ------------------------------

zip_files <- list.files(dl_dir, pattern = "\\.zip$", full.names = TRUE)

# Loop through each zip file in your list of zip links
for (zip_file in zip_files) {
  
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  unzip(zip_file, exdir = temp_dir)
  unzipped_files <- list.files(temp_dir, full.names = TRUE)
  
  total_accum_files <- grep("\\.total\\.accum\\.(tif|tfw)$", unzipped_files, value = TRUE)
  
  # Move the selected .total.accum files to accum_dir
  for (file in total_accum_files) {
    
    target_path <- file.path(accum_dir, basename(file))
    
    file.rename(file, target_path)
  }
  
  unlink(temp_dir, recursive = TRUE)
}


###
# Step 3: Process data into real precipitation over the Amazon. ------------------
###

#Set new working directory
boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"

#Get shapefile and extent of interest
amz_v <- vect(paste0(boxwd, "/amz_shps/amz_biome.shp"))
amz_ext <-  c(-80.5, -43.3, -21, 9)

#read in the ecoregions
ecoreg <- vect(paste0(boxwd, "/amz_shps/amz_biome_subregions.shp"))
florzn <- vect(paste0(boxwd, "/amz_shps/flor_zn_new.shp"))
amz_geo <- vect(paste0(boxwd, "/amz_shps/amz_geocrop.shp"))
amz_geo_agg <- vect(paste0(boxwd, "/amz_shps/amz_geocrop_extended.shp"))

plot(amz_geo_agg, col = rainbow(length(amz_geo_agg)), main = "Aggregated Regions", border = "black")

# List .tif files
precfiles <- list.files(accum_dir, pattern = "\\.tif$", full.names = TRUE)

# Organize files into a data frame  -------------
prec_info <- data.frame(
  file = precfiles,
  stringsAsFactors = FALSE
)

# Extract year and date from filenames
prec_info$datestr <- str_extract(prec_info$file, "\\d{8}")
prec_info$date <- as.Date(prec_info$datestr, format = "%Y%m%d")
prec_info$year <- format(prec_info$date, "%Y")
prec_info$doy <- as.integer(format(prec_info$date, "%j"))

# Split by year
years <- unique(prec_info$year)

# Apply raster processing over years over 16-day chunks -------------------

for (yr in years) {
  message("Processing year: ", yr)
  df_year <- prec_info[prec_info$year == yr, ]
  
  # Define 16-day periods
  doy_starts <- seq(1, 353, by = 16)
  
  for (doy_start in doy_starts) {
    doy_end <- doy_start + 15
    
    files_period <- df_year$file[df_year$doy >= doy_start & df_year$doy <= doy_end]
    
    if (length(files_period) == 0) next  # Skip if no data
    
    # Read, crop, and scale rasters
    rasters <- lapply(files_period, function(f) {
      r <- rast(f)
      r <- crop(r, amz_ext)
      r <- r / 10  # Scale from tenths of mm to mm (See: https://gpm.nasa.gov/data/directory)
      return(r)
    })
    
    # Stack and take mean
    r_stack <- rast(rasters)
    r_sum <- sum(r_stack, na.rm = TRUE)
    r_sd <- stdev(r_stack, na.rm = TRUE)
    
    #rasterize
    ecoreg_grid <- rasterize(ecoreg, r_sum, field = "Subregion")
    florzn_grid <- rasterize(florzn, r_sum, field = "Zone")
    geo_grid <- rasterize(amz_geo, r_sum, field = "name")
    geo_agg_grid <- rasterize(amz_geo_agg, r_sum, field = "region")
    
    reg_grid <- c(ecoreg_grid, florzn_grid, geo_grid, geo_agg_grid)
    names(reg_grid) <- c("subregion", "zone", "georeg", "georeg_agg")
    
    #Combine layers
    r_out <- c(r_sum, r_sd)
    names(r_out) <- c("prec_16day_sum", "prec_sd")
    
    r_out_full <- c(r_out, reg_grid)
    
    # Inside your loop:
    outname <- sprintf("amz_prec_%s_%03d.tif", yr, doy_start)
    outpath <- file.path(prec_dir, outname)
    
    writeRaster(r_out_full, outpath, overwrite = TRUE)
    message("Saved: ", outpath)
  }
}