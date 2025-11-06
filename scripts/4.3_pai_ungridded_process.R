
#Code to process and summarize GEDI PAI 'point' data (ungridded). This preps some files needed for the 'pai_ungridded_figs' R script.

rm(list=ls())
gc()

library(tidyverse)
library(terra)
library(gtools)
library(tidyterra)
library(rstatix)
library(parallel)

#This code takes ~45 GB of memory

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"
boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"
complete_dir <- paste0(boxwd, "/complete_data")

#load a SIF file for use as a grid --------------------

yearid <- "2019"
yearid <- c(2019, 2020, 2021)

#read in the ecoregions
florzn <- vect(paste0(boxwd, "/amz_shps/flor_zn_new.shp"))
amz_geo <- vect(paste0(boxwd, "/amz_shps/amz_geocrop.shp"))
georeg_agg_v <- vect(paste0(boxwd, "/amz_shps/amz_geo_agg_extended.shp"))

sifgrid <- rast(nrow = 600, ncol = 744, resolution = 0.05, extent = c(-80.5, -43.3, -21, 9), crs = "EPSG:4326")

florzn_grid <- rasterize(florzn, sifgrid, field = "Zone")
#geo_grid <- rasterize(amz_geo_agg, sifgrid, field = "name")
georeg_agg_r <- rasterize(georeg_agg_v, sifgrid, field = "region")


##GEDI files --------------------------------------------
all_yrs_data <- list()

for (year in yearid){
  
  gedipath <- paste0(wd, "/gedi_amz_2B_", year, "/gedi_shp_processed_20241030")
  gedigridpath <- paste0(wd, "/gedi_amz_2B_", year, "/gedi_gridded")
  
  gedi_shp_files <- mixedsort(sort(list.files(gedipath, pattern=glob2rx("amz_gedivect_doy*.shp*"), recursive = T, full.names=T)))
  
  gediname <-  unlist(strsplit(unlist(lapply(strsplit(gedi_shp_files, '/'), function(x) x[9])), ".shp"))
  
  gedishortname <- paste0("gedi", unlist(substring((lapply(strsplit(gediname, '_'), function(x) x[[3]])), 4, 6)))
  
  doy <- as.numeric(gsub("gedi", "", gedishortname))
  
  start_time <-  Sys.time()
  
  # Loop through shapefiles, apply operations, and calculate the total valid returns
  gedivectlist <- lapply(seq_along(gedi_shp_files), function(i) {
    # Vectorize shapefiles and filter columns
    current_vect <- vect(gedi_shp_files[[i]]) %>%
      select(c(pai, cover, doy, pai_us, pai_toc, modis_tree)) %>%
      filter(modis_tree >= 75) %>% 
      select(-modis_tree)
    
    current_vect$doymin <- NULL
    current_vect$doy <- NULL
    
    # Calculate the minimum day of the year for a layer (doy) and add it to the current object
    current_vect$doymin <- doy[i]
    
    # Return the filtered object
    return(current_vect)
  })
  
  # Print the times for each file
  end_time <- Sys.time()
  
  total_time <- difftime(end_time, start_time, units = "mins")
  cat(length(gedi_shp_files), "files processed in ", total_time, "minutes\n")
  
  
  for (i in seq_along(gedivectlist)){
    
    paivect <- gedivectlist[[i]]
    
    zone <- terra::extract(florzn_grid$Zone, paivect, ID = F)
    georeg_agg_pt <- terra::extract(georeg_agg_r$region, paivect, ID = F)
    
    paivect$zone <- zone
    paivect$georeg_agg <- georeg_agg_pt
    
    gedivectlist[[i]] <- paivect
  }
  
  
  # Assign names
  names(gedivectlist) <- gedishortname
  
  # Create an empty list to store the data frames
  df_list <- list()
  
  # Loop through the list of SpatVectors in gedivectlist
  for (i in seq_along(gedivectlist)) {
    # Convert the current SpatVector to a data frame
    df <- terra::as.data.frame(gedivectlist[[i]])
    
    # Add the data frame to the list
    df_list[[i]] <- df
  }
  
  # Combine all data frames into one data frame
  pai_df <- do.call(rbind, df_list)
  
  pai_df$year <- year
  
  # Add year data to the list
  all_yrs_data[[as.character(year)]] <- pai_df
  
}

# Combine all years into a single data frame
final_pai_df <- do.call(rbind, all_yrs_data)

# Print total PAI returns before regridding
cat("****", nrow(final_pai_df), "total PAI returns used before regridding\n")

#final_pai_df <- final_pai_df %>% na.omit(.)

rownames(final_pai_df) <- NULL

write.csv(final_pai_df, paste0(complete_dir, "/pai_all_years_fulldf.csv"), row.names = FALSE)


##
# Grouped Summaries of all PAI data ---------------------------------------------------
##

final_pai_df <- read.csv(paste0(complete_dir, "/pai_all_years_fulldf.csv"))

final_pai_df$doymin <- as.factor(final_pai_df$doymin)

# Create a function for standard error
s_err <- function(x) sd(x)/sqrt(length(x))

# pai_geo_summ <- final_pai_df %>% 
#   group_by(georeg_agg, doymin) %>%
#   summarize(mean_pai = mean(pai),
#             median_pai = median(pai),
#             sd_pai = sd(pai),
#             se_pai = s_err(pai),
#             mean_tocpai = mean(pai_toc),
#             median_tocpai = median(pai_toc),
#             sd_tocpai = sd(pai_toc),
#             se_tocpai = s_err(pai_toc),
#             mean_uspai = mean(pai_us),
#             median_uspai = median(pai_us),
#             sd_uspai = sd(pai_us),
#             se_uspai = s_err(pai_us),
#             n_used = length(pai_us))


pai_geo_summ <- final_pai_df %>% 
  group_by(georeg_agg, doymin) %>%
  summarize(mean_pai = mean(pai),
            median_pai = median(pai),
            sd_pai = sd(pai),
            se_pai = s_err(pai),
            mean_tocpai = mean(pai_toc),
            median_tocpai = median(pai_toc),
            sd_tocpai = sd(pai_toc),
            se_tocpai = s_err(pai_toc),
            mean_uspai = mean(pai_us),
            median_uspai = median(pai_us),
            sd_uspai = sd(pai_us),
            se_uspai = s_err(pai_us),
            n_used = length(pai_us))

pai_geo_yr_summ <- final_pai_df %>% 
  group_by(year, georeg_agg, doymin) %>%
  summarize(mean_pai = mean(pai),
            median_pai = median(pai),
            sd_pai = sd(pai),
            se_pai = s_err(pai),
            mean_tocpai = mean(pai_toc),
            median_tocpai = median(pai_toc),
            sd_tocpai = sd(pai_toc),
            se_tocpai = s_err(pai_toc),
            mean_uspai = mean(pai_us),
            median_uspai = median(pai_us),
            sd_uspai = sd(pai_us),
            se_uspai = s_err(pai_us),
            n_used = length(pai_us))


# pai_geo_yr_summ <- final_pai_df %>% 
#   group_by(year, georeg_agg, doymin) %>%
#   summarize(mean_pai = mean(pai),
#             median_pai = median(pai),
#             sd_pai = sd(pai),
#             se_pai = s_err(pai),
#             mean_tocpai = mean(pai_toc),
#             median_tocpai = median(pai_toc),
#             sd_tocpai = sd(pai_toc),
#             se_tocpai = s_err(pai_toc),
#             mean_uspai = mean(pai_us),
#             median_uspai = median(pai_us),
#             sd_uspai = sd(pai_us),
#             se_uspai = s_err(pai_us),
#             n_used = length(pai_us))


write.csv(pai_geo_summ, paste0(complete_dir, "/pai_ungridded_geo_monthly.csv"), row.names = FALSE)
#write.csv(pai_zone_summ, paste0(complete_dir, "/pai_zone_monthly.csv"), row.names = FALSE)
write.csv(pai_geo_yr_summ, paste0(complete_dir, "/pai_ungridded_geo_yr_monthly.csv"), row.names = FALSE)
#write.csv(pai_zone_yr_summ, paste0(complete_dir, "/pai_zone_yr_monthly.csv"), row.names = FALSE)

