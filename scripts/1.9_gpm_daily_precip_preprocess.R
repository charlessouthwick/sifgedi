
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
dl_dir <- "/Users/charlessouthwick/Documents/PhD/sifgedi/gpm_precip_data/gpm_gis_zips"
setwd(dl_dir)

accum_dir <- "/Users/charlessouthwick/Documents/PhD/sifgedi/gpm_precip_data/gpm_accum_files"
prec_dir <- "/Users/charlessouthwick/Documents/PhD/sifgedi/gpm_precip_data/gpm_prec_amz"

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
wd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"

#Get shapefile and extent of interest
amz_v <- vect(paste0(wd, "/amz_shps/amz_biome.shp"))
amz_ext <-  c(-80.5, -43.3, -21, 9)

#read in the ecoregions
ecoreg <- vect(paste0(wd, "/amz_shps/amz_biome_subregions.shp"))
florzn <- vect(paste0(wd, "/amz_shps/flor_zn_new.shp"))
amz_geo <- vect(paste0(wd, "/amz_shps/amz_geocrop.shp"))
amz_geo_agg <- vect(paste0(wd, "/amz_shps/amz_geocrop_extended.shp"))

plot(amz_geo_agg, col = rainbow(length(amz_geo_agg)), main = "Aggregated Regions", border = "black")

#writeVector(amz_geo_agg, paste0(wd, "/amz_shps/amz_geo_agg.shp"))

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


###
#Step 4: visualize precipitation variability ---------------------------------------------
###
## For FIGURE S***


# wd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"
# 
# #Example grid to use
# precgrid <- rast(paste0(wd, "/gpm_precip_data/gpm_prec_amz/amz_prec_2019_001.tif"))
# plot(precgrid)
# 
# # List all processed files
# rfiles <- list.files(prec_dir, pattern = "^amz_prec_\\d{4}_\\d{3}\\.tif$", full.names = TRUE)
# 
# # Faceted by georegions -------------------------------
# # Extract year and DOY from filename
# rinfo_g <- data.frame(
#   file = rfiles,
#   year = str_extract(rfiles, "\\d{4}"),
#   doy = as.integer(str_extract(rfiles, "_\\d{3}(?=\\.tif$)") %>% str_remove("_"))
# )
# 
# # Initialize list to store results
# trend_data_g <- lapply(seq_len(nrow(rinfo_g)), function(i) {
#   r <- rast(rinfo_g$file[i])
#   r <- crop(r, amz_v)  # Crop to Amazon extent
#   
#   # Extract the relevant layers
#   prec <- r$prec_16day_sum
#   georeg <- r$georeg
#   
#   # Skip if zone is all NA
#   if (is.null(georeg)) return(NULL)
#   
#   # Compute zonal mean and SD
#   mean_df <- zonal(prec, georeg, fun = "mean", na.rm = TRUE)
#   sd_df <- zonal(prec, georeg, fun = "sd", na.rm = TRUE)
#   
#   df <- left_join(mean_df, sd_df, by = "georeg")
#   colnames(df) <- c("georeg", "mean_mm", "sd_mm")
#   df$year <- rinfo_g$year[i]
#   df$doy <- rinfo_g$doy[i]
#   df
# })
# 
# # Combine all into one dataframe
# trend_df_g <- bind_rows(trend_data_g)
# trend_df_g$year <- as.factor(trend_df_g$year)
# trend_df_g$georeg <- as.factor(trend_df_g$georeg)
# 
# # Plot with facets by zone
# ggplot(trend_df_g, aes(x = doy, y = mean_mm, color = year)) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = mean_mm - sd_mm, ymax = mean_mm + sd_mm), width = 2, alpha = 0.3) +
#   facet_wrap(~ georeg) +
#   scale_x_continuous(breaks = seq(0, 365, by = 32)) +
#   labs(
#     title = "16-day Mean Precipitation by Georegion",
#     x = "Day of Year",
#     y = "Precipitation (mm)",
#     color = "Year"
#   ) +
#   theme_minimal()
# 
# #Let's look at the mean line
# # Average across years
# trend_summ_g <- trend_df_g %>%
#   group_by(georeg, doy) %>%
#   summarise(
#     mean_mm = mean(mean_mm, na.rm = TRUE),
#     sd_mm = mean(sd_mm, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# # Plot mean trend faceted by zone
# ggplot(trend_summ_g, aes(x = doy, y = mean_mm)) +
#   geom_line(color = "steelblue") +
#   geom_point(size = 1, color = "steelblue") +
#   geom_errorbar(aes(ymin = mean_mm - sd_mm, ymax = mean_mm + sd_mm), width = 2, alpha = 0.1) +
#   facet_wrap(~ georeg) +
#   scale_x_continuous(breaks = seq(0, 365, by = 32)) +
#   labs(
#     title = "Mean 16-day Precipitation Trend by Georegion",
#     x = "Day of Year",
#     y = "Precipitation (mm)"
#   ) +
#   theme_classic()+
#   geom_hline(yintercept = 100, linetype = "dashed", color = "gray40", lwd = 0.7) +
#   geom_hline(yintercept = 75, linetype = "dashed", color = "red4", lwd = 0.7)+
#   geom_hline(yintercept = 50, linetype = "dashed", color = "cadetblue", lwd = 0.7)+
#   geom_vline(xintercept = 145, linetype = "dashed", color = "orange3", lwd = 0.3)+
#   geom_vline(xintercept = 273, linetype = "dashed", color = "orange3", lwd = 0.3)
# 
# 
# #Supplemental:
# #Plot whole Amazon trends: ------------------------------------------
# 
# # Extract year and DOY from filename
# rinfo <- data.frame(
#   file = rfiles,
#   year = str_extract(rfiles, "\\d{4}"),
#   doy = as.integer(str_extract(rfiles, "_\\d{3}(?=\\.tif$)") %>% str_remove("_"))
# )
# 
# # Initialize list to store results
# trend_data <- lapply(seq_len(nrow(rinfo)), function(i) {
#   r <- rast(rinfo$file[i])
#   r <- crop(r, amz_v) #Crop to Amazon extent
#   mean_prec <- global(r$prec_16day_sum, fun = mean, na.rm = TRUE)
#   sd_prec <- global(r$prec_16day_sum, fun = sd, na.rm = TRUE)
#   data.frame(
#     year = rinfo$year[i],
#     doy = rinfo$doy[i],
#     mean_mm = mean_prec[1, 1],
#     sd_mm = sd_prec[1, 1]
#   )
# })
# 
# # Combine into a dataframe
# trend_df <- bind_rows(trend_data)
# trend_df$year <- as.factor(trend_df$year)
# 
# #plot by region
# ggplot(trend_df, aes(x = doy, y = mean_mm, color = year)) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = mean_mm - sd_mm, ymax = mean_mm + sd_mm), width = 2, alpha = 0.3) +
#   scale_x_continuous(breaks = seq(0, 365, by = 32)) +
#   labs(
#     title = "16-day Mean Precipitation over Amazonia",
#     x = "Day of Year",
#     y = "Precipitation (mm)",
#     color = "Year"
#   ) +
#   theme_minimal()


#Older, for zones

#Plot faceted by zones -------------------------------------------
# 
# # Extract year and DOY from filename
# rinfo_z <- data.frame(
#   file = rfiles,
#   year = str_extract(rfiles, "\\d{4}"),
#   doy = as.integer(str_extract(rfiles, "_\\d{3}(?=\\.tif$)") %>% str_remove("_"))
# )
# 
# # Initialize list to store results
# trend_data_z <- lapply(seq_len(nrow(rinfo_z)), function(i) {
#   r <- rast(rinfo_z$file[i])
#   r <- crop(r, amz_v)  # Crop to Amazon extent
#   
#   # Extract the relevant layers
#   prec <- r$prec_16day_sum
#   zone <- r$zone
#   
#   # Skip if zone is all NA
#   if (is.null(zone)) return(NULL)
#   
#   # Compute zonal mean and SD
#   mean_df <- zonal(prec, zone, fun = "mean", na.rm = TRUE)
#   sd_df <- zonal(prec, zone, fun = "sd", na.rm = TRUE)
#   
#   df <- left_join(mean_df, sd_df, by = "zone")
#   colnames(df) <- c("zone", "mean_mm", "sd_mm")
#   df$year <- rinfo_z$year[i]
#   df$doy <- rinfo_z$doy[i]
#   df
# })
# 
# # Combine all into one dataframe
# trend_df_z <- bind_rows(trend_data_z)
# trend_df_z$year <- as.factor(trend_df_z$year)
# trend_df_z$zone <- as.factor(trend_df_z$zone)
# 
# # Plot with facets by zone
# ggplot(trend_df_z, aes(x = doy, y = mean_mm, color = year)) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = mean_mm - sd_mm, ymax = mean_mm + sd_mm), width = 2, alpha = 0.3) +
#   facet_wrap(~ zone) +
#   scale_x_continuous(breaks = seq(0, 365, by = 32)) +
#   labs(
#     title = "16-day Mean Precipitation by Zone",
#     x = "Day of Year",
#     y = "Precipitation (mm)",
#     color = "Year"
#   ) +
#   theme_minimal()
# 
# # Average across years
# trend_summ_z <- trend_df_z %>%
#   group_by(zone, doy) %>%
#   summarise(
#     mean_mm = mean(mean_mm, na.rm = TRUE),
#     sd_mm = mean(sd_mm, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# # Plot mean trend faceted by zone
# ggplot(trend_summ_z, aes(x = doy, y = mean_mm)) +
#   geom_line(color = "steelblue") +
#   geom_point(size = 1, color = "steelblue") +
#   geom_errorbar(aes(ymin = mean_mm - sd_mm, ymax = mean_mm + sd_mm), width = 2, alpha = 0.1) +
#   facet_wrap(~ zone) +
#   scale_x_continuous(breaks = seq(0, 365, by = 32)) +
#   labs(
#     title = "16-day Mean Precipitation by Zone",
#     x = "Day of Year",
#     y = "Precipitation (mm)"
#   ) +
#   theme_classic()+
#   geom_hline(yintercept = 100, linetype = "dashed", color = "gray40", lwd = 0.7) +
#   geom_hline(yintercept = 75, linetype = "dashed", color = "red4", lwd = 0.7) +
#   geom_hline(yintercept = 50, linetype = "dashed", color = "cadetblue", lwd = 0.7)+
#   geom_vline(xintercept = 145, linetype = "dashed", color = "orange3", lwd = 0.3) +
#   geom_vline(xintercept = 273, linetype = "dashed", color = "orange3", lwd = 0.3)
