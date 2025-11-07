

#Use IMERG data to dynamically establish dry seasons
library(terra)
library(tidyverse)
library(viridis)

# Load Data and vectors -------------------------------------------------
boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"
wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"

prec_dir <- paste0(wd, "/gpm_precip_data/gpm_prec_amz")
compiled_dir <- paste0(boxwd, "/compiled_rasters")
complete_dir <- pastte0(boxwd, "/complete_data")

amz_v <- vect(paste0(boxwd, "/amz_shps/amz_biome.shp"))
georeg_agg <- vect(paste0(boxwd, "/amz_shps/amz_geo_agg_extended.shp"))
#amz_geo <- vect(paste0(boxwd, "/amz_shps/amz_geocrop.shp"))

plot(georeg_agg, col = rainbow(length(georeg_agg)), main = "Aggregated Regions", border = "black")

# List all processed files
rfiles <- list.files(prec_dir, pattern = "^amz_prec_\\d{4}_\\d{3}\\.tif$", full.names = TRUE)

#####
#Step 1. We want a few maps: average 16-day precipitation (mm/16day), and number of 16-day periods with precipitation less than 50 mm. --------------------------

# Create a data frame
rinfo_r <- data.frame(
  file = rfiles,
  year = str_extract(rfiles, "\\d{4}"),
  doy = str_extract(rfiles, "_\\d{3}(?=\\.tif$)") %>% str_remove("_")
)

# Group by DOY
doys <- unique(rinfo_r$doy)

# Output list for per-DOY means
mean_per_doy <- list()

for (doy in doys) {
  # Files matching this DOY
  files_doy <- rinfo_r %>% filter(doy == !!doy) %>% pull(file)
  
  # Load and stack
  r_list <- lapply(files_doy, function(f) crop(rast(f)$prec_16day_sum, amz_v, mask = T))
  r_stack <- rast(r_list)
  
  # Mean across years for this DOY
  r_mean <- mean(r_stack, na.rm = TRUE)
  
  mean_per_doy[[doy]] <- r_mean
}

# Stack all mean DOY rasters (across years)
# Each layer represents the 3-year mean 16-day precipitation for the whole Amz
r_all_mean_doys <- rast(mean_per_doy)

# Compute mean and standard deviation for each layer
globmeans <- global(r_all_mean_doys, fun = "mean", na.rm = TRUE)[,1]
globsds <- global(r_all_mean_doys, fun = "sd", na.rm = TRUE)[,1]

# Extract layer names to use as DOY identifiers
globdoys <- names(r_all_mean_doys)

# Combine into a dataframe
amz_glob_precip <- data.frame(
  doy = as.numeric(globdoys),
  mean_mm = globmeans,
  sd_mm = globsds
)

# Find the DOYs where mean_mm is less than 75
dry_periods <- amz_glob_precip$doy[amz_glob_precip$mean_mm < 75]

# Define globdry_start and globdry_end
globdry_start <- min(dry_periods)
globdry_end <- max(dry_periods)
globdry_duration <- globdry_end - globdry_start
globdry_end_window <- globdry_end + 15

# Wettest period
wettest_row <- which.max(amz_glob_precip$mean_mm)
globwet_peak <- amz_glob_precip$doy[wettest_row]

# Handle edge cases for neighbors
globwet_start <- amz_glob_precip$doy[max(wettest_row - 1, 1)]
globwet_end <- amz_glob_precip$doy[min(wettest_row + 1, nrow(amz_glob_precip))]
globwet_end_window <- globwet_end + 15

# Early wet season
globearlywet_start <- globdry_end + 16
globearlywet_end <- globdry_end + 32
globearlywet_end_window <- globearlywet_end + 15

# Combine into a named vector or data.frame
glob_seasonality <- data.frame(
  region = 'all',
  method = "days_under_75mm",
  globwet_start,
  globwet_peak,
  globwet_end,
  globwet_end_window,
  globdry_start,
  globdry_end,
  globdry_end_window,
  globdry_duration,
  globearlywet_start,
  globearlywet_end,
  globearlywet_end_window
)

drycol <- "grey80"
earlywetcol <- "grey45"
peakwetcol <- "grey10"

# Create a data frame for the seasonal rectangles
season_rects <- data.frame(
  xmin = c(globdry_start, globearlywet_start, globwet_start),
  xmax = c(globdry_end_window, globearlywet_end_window, globwet_end_window),
  season = c("DRY", "DWT", "PW"),
  fill = c(drycol, earlywetcol, peakwetcol)
)
# Create the plot with geom_rect layers
ggplot(amz_glob_precip, aes(x = doy, y = mean_mm)) +
  # Add season rectangles first (under the line)
  geom_rect(
    data = season_rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  scale_fill_identity() +  # use the fill colors as-is
  # Precipitation data
  geom_line(color = "steelblue") +
  geom_point(size = 1, color = "steelblue") +
  geom_errorbar(aes(ymin = mean_mm - sd_mm, ymax = mean_mm + sd_mm), width = 2, alpha = 0.1) +
  # Axes and labels
  scale_x_continuous(breaks = seq(0, 365, by = 32)) +
  labs(
    title = "Mean 16-day basin-wide 'Global' Precipitation",
    x = "Day of Year",
    y = "Precipitation (mm)"
  ) +
  # Style and reference lines
  theme_classic() +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray40", lwd = 0.7) +
  geom_hline(yintercept = 75, linetype = "dashed", color = "red4", lwd = 0.7) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "cadetblue", lwd = 0.7) +
  geom_vline(xintercept = 192, linetype = "dashed", color = "orange3", lwd = 0.2) +
  geom_vline(xintercept = 273, linetype = "dashed", color = "orange3", lwd = 0.2)

##
#Mapping ---------------------------------------------------

# Create a map of mean annual precipitation
mean_prec_crop <- mean(r_all_mean_doys, na.rm = TRUE)  # mean 16-day precip per year
# Find the number of days below 50mm precipitation
count_low_crop <- app(r_all_mean_doys, fun = function(x) sum(x < 50))  # periods < 500 mm

# Get ordered DOY names (for layer names and reference)
doy_names <- names(r_all_mean_doys)

# Create a DOY vector (same length as number of layers)
doy_values <- as.integer(doys)  # make sure DOYs are numeric and ordered

# Apply function to get first DOY with mean precipitation < 50 mm
first_low_doy <- app(r_all_mean_doys, fun = function(x) {
  below_thresh <- which(x < 50)
  if (length(below_thresh) == 0) {
    return(NA)  # no periods < 50 mm
  } else {
    return(doy_values[below_thresh[1]])
  }
})

last_low_doy <- app(r_all_mean_doys, fun = function(x) {
  below_thresh <- which(x < 50)
  if (length(below_thresh) == 0) {
    return(NA)  # no periods < 50 mm
  } else {
    return(doy_values[below_thresh[length(below_thresh)]])
  }
})

# Total precipitation:
years <- unique(rinfo_r$year)

annual_precip_list <- list()

# Loop over each year to compute total annual precipitation
for (yr in years) {
  # Files for this year
  files_year <- rinfo_r %>% filter(year == !!yr) %>% pull(file)
  
  # Load and crop rasters
  r_list <- lapply(files_year, function(f) crop(rast(f)$prec_16day_sum, amz_v, mask = TRUE))
  
  # Stack all 16-day rasters and sum to get annual total
  r_stack <- rast(r_list)
  r_annual_total <- sum(r_stack, na.rm = TRUE)
  
  annual_precip_list[[yr]] <- r_annual_total
}

# Stack and compute mean across years
r_annual_total_stack <- rast(annual_precip_list)
mean_annual_precip <- mean(r_annual_total_stack, na.rm = TRUE)

# Set up side-by-side layout
par(mfrow = c(2, 2))

plot(mean_prec_crop, col = viridis(100, direction = -1), main = "Mean 16-day Precipitation (mm)")
plot(georeg_agg, add = T, lwd = 2, border = "black")

plot(count_low_crop, col = viridis(100, option = "magma"), main = "# of 16-day Periods with Precip < 50 mm")
plot(georeg_agg, add = T, lwd = 2, border = "red")

plot(mean_annual_precip, col = viridis(100, option = "mako", direction = -1), main = "Mean total annual rainfall (mm)")
plot(georeg_agg, add = T, lwd = 2, border = "black")

plot(first_low_doy, col = viridis(100, option = "plasma"), main = "DOY of First days < 50 mm")
plot(georeg_agg, add = T, lwd = 2, border = "red")

par(mfrow = c(1,1))

#We'll save these for use in actual mapping for figures, script called 'amz_maps.R'
writeRaster(mean_prec_crop, paste0(compiled_dir, "/amz_mean_16day_precip.tif"), overwrite = TRUE)
writeRaster(count_low_crop, paste0(compiled_dir, "/amz_under50mmcountdoy_precip.tif"), overwrite = TRUE)
writeRaster(mean_annual_precip, paste0(compiled_dir, "/amz_mean_annual_precip.tif"), overwrite = TRUE)
writeRaster(first_low_doy, paste0(compiled_dir, "/amz_firstdoyunder50mm_precip.tif"), overwrite = TRUE)

#####
# Use georegional plots to guide selection of Dynamic seasons--------------------------------------------------------

# Create new dataframe: extract year and DOY from filename
rinfo_g2 <- data.frame(
  file = rfiles,
  year = str_extract(rfiles, "\\d{4}"),
  doy = as.integer(str_extract(rfiles, "_\\d{3}(?=\\.tif$)") %>% str_remove("_"))
)

# Initialize list to store results
trend_data_g2 <- lapply(seq_len(nrow(rinfo_g2)), function(i) {
  r <- rast(rinfo_g2$file[i])
  r <- crop(r, amz_v)  # Crop to Amazon extent
  
  # Extract the relevant layers
  prec <- r$prec_16day_sum
  georeg_agg <- r$georeg_agg
  
  # Skip if zone is all NA
  if (is.null(georeg_agg)) return(NULL)
  
  # Compute zonal mean and SD
  mean_df <- zonal(prec, georeg_agg, fun = "mean", na.rm = TRUE)
  sd_df <- zonal(prec, georeg_agg, fun = "sd", na.rm = TRUE)
  
  df <- left_join(mean_df, sd_df, by = "georeg_agg")
  colnames(df) <- c("georeg_agg", "mean_mm", "sd_mm")
  df$year <- rinfo_g2$year[i]
  df$doy <- rinfo_g2$doy[i]
  df
})

# Combine all into one dataframe
trend_df_g2 <- bind_rows(trend_data_g2)
trend_df_g2$year <- as.factor(trend_df_g2$year)
trend_df_g2$georeg <- as.factor(trend_df_g2$georeg)

#Let's look at the mean line
# Average across years
trend_summ_g2 <- trend_df_g2 %>%
  group_by(georeg_agg, doy) %>%
  summarise(
    mean_mm = mean(mean_mm, na.rm = TRUE),
    sd_mm = mean(sd_mm, na.rm = TRUE),
    .groups = "drop"
  )

# Plot with facets by zone
ggplot(trend_df_g2, aes(x = doy, y = mean_mm, color = year)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_mm - sd_mm, ymax = mean_mm + sd_mm), width = 2, alpha = 0.3) +
  facet_wrap(~ georeg_agg) +
  scale_x_continuous(breaks = seq(0, 365, by = 32)) +
  labs(
    title = "16-day Mean Precipitation by Georegion",
    x = "Day of Year",
    y = "Precipitation (mm)",
    color = "Year"
  ) +
  theme_minimal()

# Plot mean trend faceted by georegion
ggplot(trend_summ_g2, aes(x = doy, y = mean_mm)) +
  geom_line(color = "steelblue") +
  geom_point(size = 1, color = "steelblue") +
  geom_errorbar(aes(ymin = mean_mm - sd_mm, ymax = mean_mm + sd_mm), width = 2, alpha = 0.1) +
  facet_wrap(~ georeg_agg) +
  scale_x_continuous(breaks = seq(0, 365, by = 32)) +
  labs(
    title = "Mean 16-day Precipitation Trend by Georegion",
    x = "Day of Year",
    y = "Precipitation (mm)"
  ) +
  theme_classic()+
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray40", lwd = 0.7) +
  geom_hline(yintercept = 75, linetype = "dashed", color = "red4", lwd = 0.7)+
  geom_hline(yintercept = 50, linetype = "dashed", color = "cadetblue", lwd = 0.7)+
  geom_vline(xintercept = 145, linetype = "dashed", color = "orange3", lwd = 0.2)+
  geom_vline(xintercept = 273, linetype = "dashed", color = "orange3", lwd = 0.2)

#Now use these plots to guide a conditional establishment of dry seasonality
manual_dry_season <- function(df_region) {
  region <- unique(df_region$georeg_agg)
  
  if (region == "Southern") {
    low_50 <- df_region %>% filter(mean_mm < 50)
    if (nrow(low_50) >= 2) {
      start <- min(low_50$doy)
      end <- max(low_50$doy)
      return(tibble(
        georeg_agg = region,
        dry_start = start,
        dry_end = end,
        dry_duration = end - start,
        method = "days_under_50mm"
      ))
    } else {
      return(tibble(
        georeg_agg = region,
        dry_start = NA,
        dry_end = NA,
        dry_duration = NA,
        method = "no_dry_season_found"
      ))
    }
    
  } else if (region == "CA") {
    low_75 <- df_region %>% filter(mean_mm < 75)
    if (nrow(low_75) >= 2) {
      start <- min(low_75$doy)
      end <- max(low_75$doy)
      return(tibble(
        georeg_agg = region,
        dry_start = start,
        dry_end = end,
        dry_duration = end - start,
        method = "days_under_75mm"
      ))
    } else {
      return(tibble(
        georeg_agg = region,
        dry_start = NA,
        dry_end = NA,
        dry_duration = NA,
        method = "no_dry_season_found"
      ))
    }
    
  } else if (region == "NWA") {
    start <- 193
    end <- 225
    return(tibble(
      georeg_agg = region,
      dry_start = start,
      dry_end = end,
      dry_duration = end - start,
      method = "manual_assign"
    ))
    
  } else if (region == "NOA") {
    start <- 17
    end <- 49
    return(tibble(
      georeg_agg = region,
      dry_start = start,
      dry_end = end,
      dry_duration = end - start,
      method = "manual_assign"
    ))
    
  } else {
    return(tibble(
      georeg_agg = region,
      dry_start = NA,
      dry_end = NA,
      dry_duration = NA,
      method = "region_not_specified"
    ))
  }
}

# Apply to each region
dry_season_manual <- trend_summ_g2 %>%
  group_by(georeg_agg) %>%
  group_split() %>%
  purrr::map_dfr(manual_dry_season)

dry_season_manual <- dry_season_manual %>%
  mutate(
    dry_end_window = dry_end + 15,
    earlywet_start = dry_end + 16,
    earlywet_end = dry_end + 47,
    earlywet_end_window = earlywet_end + 15
  )

# Now add peak wet season windows per georegion
wet_peaks <- trend_summ_g2 %>%
  group_by(georeg_agg) %>%
  filter(mean_mm == max(mean_mm, na.rm = TRUE)) %>%
  slice(1) %>%  # In case of ties, pick the first
  ungroup() %>%
  select(georeg_agg, wet_peak_doy = doy) %>%
  mutate(
    wet_start = wet_peak_doy - 16,
    wet_end = wet_peak_doy + 16,
    wet_end_window = wet_end + 15
  )

# Join with dry_season_manual
dry_season_manual <- left_join(dry_season_manual, wet_peaks, by = "georeg_agg")

trend_summ_g2_plot <- left_join(trend_summ_g2, dry_season_manual, by = "georeg_agg")

trend_summ_g2_plot$georeg_agg <- factor(trend_summ_g2_plot$georeg_agg,
                                        levels = c("NWA", "NOA", "CA", "Southern"))

#Save 'global' seasonalities
write.csv(glob_seasonality, paste0(complete_dir, "/global_precip_seasonality.csv"), row.names = FALSE)

# Save the summary and the custom dry season
write.csv(dry_season_manual, paste0(complete_dir, "/dynamic_precip_seasonality.csv"), row.names = FALSE)

write.csv(trend_summ_g2_plot, paste0(complete_dir, "/precipitation_zonal_summary.csv"), row.names = FALSE)


#THIS HAS BEEN MOVED TO A LATER SCRIPT!

#Create supplemental plot -----------------------------------
# figdir <- paste0(boxwd, "/figures")
# 
# drycol <- "grey80"
# earlywetcol <- "grey45"
# peakwetcol <- "grey10"
# 
# # Create a data frame for the seasonal rectangles
# season_rects <- data.frame(
#   xmin = c(globdry_start, globearlywet_start, globwet_start),
#   xmax = c(globdry_end_window, globearlywet_end_window, globwet_end_window),
#   season = c("DRY", "DWT", "PW"),
#   fill = c(drycol, earlywetcol, peakwetcol)
# )
# 
# #seasonality <- dry_season_manual
# seasonality <- read.csv(paste0(boxwd, "/complete_data/dynamic_precip_seasonality.csv"))
# seasonality$georeg_agg <- factor(seasonality$georeg_agg, levels = c("NWA", "NOA", "CA", "Southern"))
# 
# trend_summ_g2_plot <- read.csv(paste0(boxwd, "/complete_data/precipitation_zonal_summary.csv"))
# trend_summ_g2_plot$georeg_agg <- factor(trend_summ_g2_plot$georeg_agg, levels = c("NWA", "NOA", "CA", "Southern"))
# 
# georeg_labels <- c(
#   "NWA" = "Northwest",
#   "NOA" = "Northern",
#   "CA"  = "Central",
#   "Southern" = "Southern"
# )
# 
# 
# custom_annotate <- function(y_text_pos = NULL) {
#   list(
#     # Dry season shading
#     geom_rect(aes(xmin = dry_start, xmax = dry_end_window, ymin = -Inf, ymax = Inf),
#               fill = drycol, alpha = 0.2, inherit.aes = FALSE, data = seasonality),
#     
#     # Early wet season shading
#     geom_rect(aes(xmin = earlywet_start, xmax = earlywet_end_window, ymin = -Inf, ymax = Inf),
#               fill = earlywetcol, alpha = 0.2, inherit.aes = FALSE, data = seasonality),
#     
#     # Wet peak season shading
#     geom_rect(aes(xmin = wet_start, xmax = wet_end_window, ymin = -Inf, ymax = Inf),
#               fill = peakwetcol, alpha = 0.2, inherit.aes = FALSE, data = seasonality),
#     
#     # Optional labels (if you provide y_text_pos)
#     if (!is.null(y_text_pos)) {
#       list(
#         geom_text(data = seasonality, aes(x = dry_start, y = y_text_pos, label = "DRY"), 
#                   inherit.aes = FALSE, hjust = -0.1, size = 3),
#         geom_text(data = seasonality, aes(x = earlywet_start, y = y_text_pos, label = "DWT"), 
#                   inherit.aes = FALSE, hjust = -0.1, size = 3),
#         geom_text(data = seasonality, aes(x = wet_start, y = y_text_pos, label = "PW"), 
#                   inherit.aes = FALSE, hjust = -0.1, size = 3)
#       )
#     } else {
#       NULL
#     }
#   )
# }
# 
# # Create plot
# annual_precip_ts <- ggplot(trend_summ_g2_plot, aes(x = doy, y = mean_mm)) +
#   # Precip line and points
#   geom_line(color = "steelblue", linewidth = 1.1) +
#   geom_point(size = 1.3, color = "steelblue") +
#   
#   # Error bars
#   geom_errorbar(aes(ymin = mean_mm - sd_mm, ymax = mean_mm + sd_mm), 
#                 width = 2.3, alpha = 0.4, color = "steelblue") +
#   
#   # Facet by region
#   facet_wrap(~ georeg_agg, labeller = as_labeller(georeg_labels)) +
#   
#   # Axes and labels
#   scale_x_continuous(breaks = seq(0, 365, by = 32)) +
#   labs(
#     x = "Day of Year",
#     y = "Precipitation (mm)"
#   ) +
#   
#   # Horizontal lines
#   geom_hline(yintercept = 100, linetype = "dashed", color = "gray40", lwd = 0.8) +
#   geom_hline(yintercept = 75, linetype = "dashed", color = "red4", lwd = 0.8) +
#   geom_hline(yintercept = 50, linetype = "dashed", color = "cadetblue", lwd = 0.8) +
# 
#   # Theme
#   theme_classic()+
#   custom_annotate(-20)
# annual_precip_ts
# 
# #ggsave(paste0(figdir, "/annual_precip_georeg_supp.png"), annual_precip_ts, dpi = 300, width = 10, height = 8)
# ggsave(paste0(figdir, "/annual_precip_georeg_supp.tiff"), annual_precip_ts, units = 'in', device = 'tiff', dpi = 600, width = 10, height = 8, compression = 'lzw')
# 

