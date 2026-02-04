
#General script for processing OCO-3 SIF files, using a fixed extent
# Note a more inclusive qc_flag of 0 + 1 was used -- use with caution!

#NASA Files taken from: OCO-3 Level 2 bias-corrected solar-induced fluorescence and other select fields from the IMAP-DOAS algorithm aggregated as daily files, Retrospective processing V11r (OCO3_L2_Lite_SIF) at GES DISC

#OCO-2/OCO-3 Science Team, Vivienne Payne, Abhishek Chatterjee (2024), OCO-3 Level 2 bias-corrected solar-induced fluorescence and other select fields from the IMAP-DOAS algorithm aggregated as daily files, Retrospective processing V11r, Greenbelt, MD, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/HC776J71KV41

#ATBD: https://docserver.gesdisc.eosdis.nasa.gov/public/project/OCO/OCO_L2_ATBD.pdf

library(ncdf4)
library(tidyverse)
library(terra)

rm(list=ls())
gc()

amz_ext <- ext(-80.5, -43.3, -21, 9)

#Short term fix -----------------
# library(sf)
# epsg4326 <- st_crs(4326)$wkt
# oco3_grid <- rast(amz_ext, resolution = 0.01, crs = epsg4326)
#-----------------------------

oco3_grid <- rast(amz_ext, resolution = 0.01, crs = "epsg:4326")

datadir <- "/Users/charlessouthwick/Documents/PhD/sifgedi/oco3_data"
rawdir <- file.path(datadir, "raw_ncfiles")
#outdir <- file.path(filedir, "oco3_processed_vect")
rastdir <- file.path(datadir, "oco3_processed_rast")

oco3files <- list.files(rawdir, pattern = "oco3_LtSIF_", full.names = TRUE)

filedates <- as.Date(substr(basename(oco3files), 12, 17), format = "%y%m%d")

#chunk into 16-day periods
# oco3_df <- data.frame(file = oco3files, date = filedates) %>%
#   arrange(date) %>%
#   mutate(period_start = lubridate::floor_date(date, unit = "16 days"))
oco3_df <- data.frame(file = oco3files, date = filedates) %>%
  arrange(date) %>%
  mutate(
    year = lubridate::year(date),
    doy  = lubridate::yday(date),
    
    period_doy = ((doy - 1) %/% 16) * 16 + 1,
    period_start = as.Date(period_doy - 1, origin = paste0(year, "-01-01"))
  )

# Loop through each 16-day period
for (i in unique(oco3_df$period_start)) {
  cat("Processing period starting", format(as.Date(i), "%Y%m%d"), "\n")
  
  period_files <- oco3_df$file[oco3_df$period_start == i]
  
  period_list <- list()
  counter <- 0
  
  for (j in period_files) {
    nc <- suppressWarnings(nc_open(j))
    #Read coordinates
    lon <- suppressWarnings(ncvar_get(nc, "Longitude"))
    lat <- suppressWarnings(ncvar_get(nc, "Latitude"))
    
    #Read SIF variables
    dsif740  <- suppressWarnings(ncvar_get(nc, "Daily_SIF_740nm"))
    dsif757  <- suppressWarnings(ncvar_get(nc, "Daily_SIF_757nm"))
    dsif771  <- suppressWarnings(ncvar_get(nc, "Daily_SIF_771nm"))
    sif740  <- suppressWarnings(ncvar_get(nc, "SIF_740nm"))
    sza <- suppressWarnings(ncvar_get(nc, "SZA"))
    vza <- suppressWarnings(ncvar_get(nc, "VZA"))
    cflag <- suppressWarnings(ncvar_get(nc, "Cloud/cloud_flag_abp"))
    qc_flag   <- suppressWarnings(ncvar_get(nc, "Quality_Flag"))
    
    nc_close(nc)
    
    #Create data frame, filter and coerce to spatvector
    df <- data.frame(longitude = lon,
                     latitude = lat,
                     dsif740 = dsif740,
                     dsif757 = dsif757,
                     dsif771 = dsif771,
                     sif740 = sif740,
                     vza = vza * 180 / pi, #quick convert to degrees for ease
                     sza = sza,
                     cflag = cflag,
                     qc_flag = qc_flag)
    
    #We'll grab good and best soundings
    df_filt <- subset(df,
                      qc_flag %in% c(0, 1) &
                        cflag < 2 &
                        is.finite(dsif740) &
                        sza < 60 &
                        vza < 60
                        )
    
    df_amz <- subset(df_filt, latitude > amz_ext[3] & latitude < amz_ext[4] & longitude > amz_ext[1] & longitude < amz_ext[2])
    
    if (nrow(df_amz) > 0) {
      counter <- counter + 1
      period_list[[counter]] <- df_amz
    }
  }
  
  # Combine all good observations in the 16-day window
  if (length(period_list) > 0) {
    sif_df <- do.call(rbind, period_list)
    sif_v <- vect(sif_df, geom = c("longitude", "latitude"))
    
    dsif740_r <- rasterize(sif_v, oco3_grid, field = "dsif740", fun = mean, na.rm = TRUE)
    dsif757_r <- rasterize(sif_v, oco3_grid, field = "dsif757", fun = mean, na.rm = TRUE)
    dsif771_r <- rasterize(sif_v, oco3_grid, field = "dsif771", fun = mean, na.rm = TRUE)
    sif740_r <- rasterize(sif_v, oco3_grid, field = "sif740", fun = mean, na.rm = TRUE)
    
    sif_stack <- c(dsif740_r, dsif757_r, dsif771_r, sif740_r)
    names(sif_stack) <- c("dsif740", "dsif757", "dsif771", "sif740")
    
    
    datename <- format(as.Date(i), "%Y%m%d")
    
    outfile_r <- file.path(rastdir, paste0("oco3_sif_", datename, "_16day.tif"))
    
    writeRaster(sif_stack, outfile_r, overwrite = TRUE)
    
    # Save individual 16-day vector
    #outfile <- file.path(outdir, paste0("oco3_sif_", datename, "_16day_vectorized.shp"))
    
    #writeVector(sif_v, outfile, overwrite = TRUE)
    cat("✅ Saved:", outfile_r, "\n")
    
  }
}


# clean up and summarize -------------------------------------

#extract dataset
boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"
final_dir <- paste0(boxwd, "/complete_data")

amz_vect <- vect(paste0(boxwd, "/amz_shps/amz_geo_agg_extended.shp"))

#Georegions
geo_grid <- rasterize(amz_vect, oco3_grid, field = "region")
names(geo_grid) <- "georeg"
geo_amz <- crop(geo_grid, amz_vect)

# Landcover mask
lc_tree <- rast(paste0(wd, "/mcd12c1_landcover/mcd12c1_2019_lc_masked_amz.tif"))
lc_tree <- project(lc_tree, geo_grid)
lc_tree <- crop(lc_tree, amz_vect)

#List files
rastfiles <- list.files(rastdir, pattern = "oco3_sif", full.names = T)

sifproclist <- list()
counter <- 0

#process files with lc mask and georegions
for (i in seq_along(rastfiles)){
  
  rfile <- rastfiles[i]
  
  sifdate <- as.numeric(gsub("_16day.tif", "", gsub("oco3_sif_", "", basename(rfile))))
  
  date_obj <- ymd(sifdate)
  yearid <- year(date_obj)
  doy <- yday(date_obj)
  
  sif_r <- rast(rfile)
  
  sif_r <- sif_r[[c("dsif740", "sif740")]]
  
  sif_c <- crop(sif_r, amz_vect)
  
  sif_lcmsk <- mask(sif_c, lc_tree)
  
  sif_geo <- c(sif_lcmsk, geo_amz)
  
  sif_df <- terra::as.data.frame(sif_geo, xy = TRUE, na.rm = TRUE)
  
  #Safety valve for empty dataframes
  if (nrow(sif_df) == 0) next
  
  sif_df$year     <- yearid
  sif_df$doy   <- doy
  sif_df$truedate <- date_obj
  
  counter <- counter + 1
  sifproclist[[counter]] <- sif_df
  
}

sif_df_c <- do.call(rbind, sifproclist)

write.csv(sif_df_c, paste0(final_dir, "/oco3_df_complete.csv"), row.names = FALSE)



########
## Summarize -------------------------------------

s_err <- function(x) sd(x, na.rm = T)/sqrt(sum(!is.na(x)))


df2 <- sif_df_c %>% 
  #filter(chlcar < 7 & chlcar > 0) %>%  #There are a few outlier points due to denominator 'explosion'
  mutate(year = factor(year, levels = c('2019', '2020', '2021')),
         georeg = factor(georeg, levels = c('NWA', 'NOA', 'CA', 'Southern')))


vars_noyr <- c("dsif740", "sif740", "doy")  # includes doy
vars_yr <- c("dsif740", "sif740") #does not include doy

summarize_oco <- function(data, group_vars, vars_to_summarize, n_sif1 = "dsif740") {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(across(all_of(vars_to_summarize), 
                     list(mean = \(x) mean(x, na.rm = T), 
                          se = \(x) s_err(x)), 
                     .names = "{.fn}_{.col}"),
              ndsif = sum(!is.na(.data[[n_sif1]])),
              .groups = "drop")
}


# No year groupings
df_summ <- summarize_oco(df2, c("truedate"), vars_noyr)%>% 
  mutate(region = 'all') %>%
  rename(doy = mean_doy) %>%
  select(-se_doy) %>% 
  select(region, everything())

df_georeg_summ <- df2 %>% 
  filter(!is.na(georeg)) %>%
  summarize_oco(., c("georeg", "truedate"), vars_noyr) %>% 
  rename(doy = mean_doy) %>% 
  select(-se_doy)

#Year groupings
df_yr_summ <- summarize_oco(df2, "doy", vars_yr) %>% 
  mutate(region = 'all') %>%
  select(region, everything())

df_yr_georeg_summ <- df2 %>% 
  filter(!is.na(georeg)) %>% 
  summarize_oco(., c("georeg", "doy"), vars_yr)

# Write complete cases grouped datasets to csvs
write.csv(df_summ, paste0(final_dir, "/oco3_summ.csv"), row.names = FALSE)
write.csv(df_georeg_summ, paste0(final_dir, "/oco3_georeg_summ.csv"), row.names = FALSE)

write.csv(df_yr_summ, paste0(final_dir, "/oco3_yr_summ.csv"), row.names = FALSE)
write.csv(df_yr_georeg_summ, paste0(final_dir, "/oco3_yr_georeg_summ.csv"), row.names = FALSE)


# 
# # ----------- Now we rasterize ------------- #
# 
# rastdir <- file.path(filedir, "oco3_processed_rast")
# 
# oco3_grid <- rast(amz_ext, resolution = 0.01, crs = "EPSG:4326")
# 
# vectfiles <- list.files(outdir, pattern = "^oco3_sif_.*_16day_vectorized\\.shp$", full.names = TRUE)
# 
# for (i in seq_along(vectfiles)){
#   
#   file_name <- vectfiles[[i]]
#   
#   r_name <- substr(basename(file_name), 1, 23)
#   
#   sif_v <- vect(file_name)
#   
#   sif740_r <- rasterize(sif_v, oco3_grid, field = "sif740", fun = mean, na.rm = TRUE)
#   sif757_r <- rasterize(sif_v, oco3_grid, field = "sif757", fun = mean, na.rm = TRUE)
#   sif771_r <- rasterize(sif_v, oco3_grid, field = "sif771", fun = mean, na.rm = TRUE)
#   
#   sif_stack <- c(sif740_r, sif757_r, sif771_r)
#   names(sif_stack) <- c("sif740", "sif757", "sif771")
#   
#   outfile_r <- file.path(rastdir, paste0(r_name, ".tif"))
#   
#   writeRaster(sif_stack, outfile_r, overwrite = TRUE)
# }
# 
# 
# plot(sif_stack)
