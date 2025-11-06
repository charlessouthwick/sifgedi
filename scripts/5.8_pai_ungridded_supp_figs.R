# GEDI Ungridded

library(tidyverse)
library(viridis)


#Setup --------------------------------------------------------

#Set up directories
#wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"
boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"
complete_dir <- paste0(boxwd, "/complete_data")
figdir <- paste0(boxwd, "/figures")


seasonality <- read.csv(paste0(complete_dir, "/dynamic_precip_seasonality.csv"))


#Load the gridded data; restructure for merging ----------------------------
gedi_yr_georeg_summ <- read.csv(paste0(complete_dir, "/gedi_yr_georeg_naincl_summ.csv")) %>% 
  select(doymin, georeg_agg, mean_pai, se_pai, mean_pai_toc, se_pai_toc) %>% 
  rename(mean_paigrid = mean_pai,
         se_paigrid = se_pai,
         mean_pai_tocgrid = mean_pai_toc,
         se_pai_tocgrid = se_pai_toc)

# Load the ungridded 'point' data; restructure for merging -------------------------
pai_geo_summ <- read.csv(paste0(complete_dir, "/pai_ungridded_geo_monthly.csv"))
pai_geo_yr_summ <- read.csv(paste0(complete_dir, "/pai_ungridded_geo_yr_monthly.csv"))

seasonality$georeg_agg <- factor(seasonality$georeg_agg, levels = c("NWA", "NOA", "CA", "Southern"))

pai_geo_summ <- pai_geo_summ %>% filter(!is.na(georeg_agg))
pai_geo_yr_summ <- pai_geo_yr_summ %>% filter(!is.na(georeg_agg))

pai_geo_summ <- pai_geo_summ %>% left_join(seasonality, by = "georeg_agg")
pai_geo_yr_summ <- pai_geo_yr_summ %>% left_join(seasonality, by = "georeg_agg")

#Merge ungridded and gridded data
pai_merge <- left_join(pai_geo_summ, gedi_yr_georeg_summ)

pai_merge <- pai_merge %>%
  filter(georeg_agg %in% c("NWA", "NOA", "CA", "Southern")) %>%
  mutate(georeg_agg = factor(georeg_agg, levels = c("NWA", "NOA", "CA", "Southern"))) %>%
  droplevels()

levels(pai_merge$georeg_agg)

#Visualize -----------------------------------------------------

sif_col <- "#d6b71d"
toc_col <- "#BC5090"
us_col <- sif_col
pai_col <- "#6A4C93"

toc_col2 <- "#66d8c3"
pai_col2 <- "#c08ad1"

# Colors for yearly data 
yr_vir_pal <- viridis(n = 3, option = "C", end = 0.8)
col2019 <- yr_vir_pal[1]
col2020 <- yr_vir_pal[2]
col2021 <- yr_vir_pal[3]

color_vals <- c("2019" = col2019, "2020" = col2020, "2021" = col2021)

#Color palette for peak wet, dry, and early wet
drycol <- "grey80"
earlywetcol <- "grey45"
peakwetcol <- "grey10"

# Function to add custom annotations
custom_annotate <- function(y_text_pos = NULL) {
  list(
    # Dry season shading
    geom_rect(aes(xmin = dry_start, xmax = dry_end_window, ymin = -Inf, ymax = Inf),
              fill = drycol, alpha = 0.2, inherit.aes = FALSE, data = seasonality),
    
    # Early wet season shading
    geom_rect(aes(xmin = earlywet_start, xmax = earlywet_end_window, ymin = -Inf, ymax = Inf),
              fill = earlywetcol, alpha = 0.2, inherit.aes = FALSE, data = seasonality),
    
    # Wet peak season shading
    geom_rect(aes(xmin = wet_start, xmax = wet_end_window, ymin = -Inf, ymax = Inf),
              fill = peakwetcol, alpha = 0.2, inherit.aes = FALSE, data = seasonality),
    
    # Optional labels (if you provide y_text_pos)
    if (!is.null(y_text_pos)) {
      list(
        geom_text(data = seasonality, aes(x = dry_start, y = y_text_pos, label = "DRY"), 
                  inherit.aes = FALSE, hjust = -0.1, size = 3),
        geom_text(data = seasonality, aes(x = earlywet_start, y = y_text_pos, label = "DWT"), 
                  inherit.aes = FALSE, hjust = -0.1, size = 3),
        geom_text(data = seasonality, aes(x = wet_start, y = y_text_pos, label = "PW"), 
                  inherit.aes = FALSE, hjust = -0.1, size = 3)
      )
    } else {
      NULL
    }
  )
}

# Combined plot; Fig S2 ------------------------------------------
georeg_labels <- c(
  "NWA" = "Northwest",
  "NOA" = "Northern",
  "CA"  = "Central",
  "Southern" = "Southern"
)

merge_geo_ts <- ggplot(data = pai_merge) +
  # PAI layers
  geom_point(aes(x = doymin, y = median_pai, color = "'Point' PAI"), size = 2.3) +
  geom_errorbar(aes(x = doymin, ymin = median_pai - se_pai, ymax = median_pai + se_pai, color = "'Point' PAI"), 
                linewidth = 0.3, alpha = 0.9) +
  geom_line(aes(x = doymin, y = median_pai, color = "'Point' PAI"), linewidth = 0.7) +
  geom_smooth(aes(x = doymin, y = median_pai, color = "'Point' PAI"), linewidth = 0.4, fill = pai_col, alpha = 0.3) +
  
  # TOC PAI layers
  geom_point(aes(x = doymin, y = median_tocpai, color = "'Point' TOC PAI"), size = 2.3) +
  geom_errorbar(aes(x = doymin, ymin = median_tocpai - se_tocpai, ymax = median_tocpai + se_tocpai, color = "TOC PAI"), 
                linewidth = 0.3, alpha = 0.9) +
  geom_line(aes(x = doymin, y = median_tocpai, color = "'Point' TOC PAI"), linewidth = 0.7) +
  geom_smooth(aes(x = doymin, y = median_tocpai, color = "'Point' TOC PAI"), linewidth = 0.4, fill = toc_col, alpha = 0.3) +
  
  # Gridded PAI layers
  geom_point(aes(x = doymin, y = mean_paigrid, color = "Gridded PAI"), size = 2.3) +
  geom_errorbar(aes(x = doymin, ymin = mean_paigrid - se_paigrid, ymax = mean_paigrid + se_paigrid, color = "Gridded PAI"), 
                linewidth = 0.3, alpha = 0.9) +
  geom_line(aes(x = doymin, y = mean_paigrid, color = "Gridded PAI"), linewidth = 0.7) +
  geom_smooth(aes(x = doymin, y = mean_paigrid, color = "Gridded PAI"), linewidth = 0.4, fill = pai_col2, alpha = 0.3) +
  
  # Gridded TOC PAI layers
  geom_point(aes(x = doymin, y = mean_pai_tocgrid, color = "Gridded TOC PAI"), size = 2.3) +
  geom_errorbar(aes(x = doymin, ymin = mean_pai_tocgrid - se_pai_tocgrid, ymax = mean_pai_tocgrid + se_pai_tocgrid, color = "Gridded TOC PAI"), 
                linewidth = 0.3, alpha = 0.9) +
  geom_line(aes(x = doymin, y = mean_pai_tocgrid, color = "Gridded TOC PAI"), linewidth = 0.7) +
  geom_smooth(aes(x = doymin, y = mean_pai_tocgrid, color = "Gridded TOC PAI"), linewidth = 0.4, fill = toc_col2, alpha = 0.3) +
  
  # Axis labels and theme
  labs(x = "Day of Year", 
       y = expression("GEDI PAI Product (m"^2~"/m"^2*")"), 
       color = "Variable") +  # Adding the legend title
  theme_classic(base_family = "serif") + 
  theme(axis.title.y = element_text(size = 18)) + 
  ylim(0.7, 5) + 
  
  facet_wrap(~georeg_agg, scales = 'fixed', labeller = as_labeller(georeg_labels)) +
  
  # Manual color scale for the legend
  scale_color_manual(values = c(
    "Gridded PAI" = pai_col2,
    "'Point' PAI" = pai_col,
    "Gridded TOC PAI" = toc_col2,
    "'Point' TOC PAI" = toc_col
  ), 
  # Reorder the legend
  breaks = c("Gridded PAI", "'Point' PAI", "Gridded TOC PAI", "'Point' TOC PAI")) +
  
  # Remove fill from the legend (only use line colors)
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  
  custom_annotate(y_text_pos = 0.7)

# Display the combined plot
merge_geo_ts

#ggsave(paste0(figdir, "/compare_pai_grid_ungrid.png"), merge_geo_ts, dpi = 300, width = 9, height = 7)
ggsave(paste0(figdir, "/compare_pai_grid_ungrid.tiff"), merge_geo_ts, device = 'tiff', units = 'in', dpi = 600, width = 9, height = 7, compression = 'lzw')


cor(pai_merge$mean_pai_tocgrid, pai_merge$median_tocpai)
cor(pai_merge$mean_paigrid, pai_merge$median_pai)

mean(pai_merge$mean_paigrid) - mean(pai_merge$median_pai)

sd(pai_merge$median_pai)
