
#Charles Southwick
#Create supplemental plot of ecoregional precipitation

library(tidyverse)

boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"
figdir <- paste0(boxwd, "/figures")

drycol <- "grey80"
earlywetcol <- "grey45"
peakwetcol <- "grey10"

seasonality <- read.csv(paste0(boxwd, "/complete_data/dynamic_precip_seasonality.csv"))
seasonality$georeg_agg <- factor(seasonality$georeg_agg, levels = c("NWA", "NOA", "CA", "Southern"))

trend_summ_g2_plot <- read.csv(paste0(boxwd, "/complete_data/precipitation_zonal_summary.csv"))
trend_summ_g2_plot$georeg_agg <- factor(trend_summ_g2_plot$georeg_agg, levels = c("NWA", "NOA", "CA", "Southern"))

georeg_labels <- c(
  "NWA" = "Northwest",
  "NOA" = "Northern",
  "CA"  = "Central",
  "Southern" = "Southern"
)

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

# Create plot
annual_precip_ts <- ggplot(trend_summ_g2_plot, aes(x = doy, y = mean_mm)) +
  # Precip line and points
  geom_line(color = "steelblue", linewidth = 1.1) +
  geom_point(size = 1.3, color = "steelblue") +
  
  # Error bars
  geom_errorbar(aes(ymin = mean_mm - sd_mm, ymax = mean_mm + sd_mm), 
                width = 2.3, alpha = 0.4, color = "steelblue") +
  
  # Facet by region
  facet_wrap(~ georeg_agg, labeller = as_labeller(georeg_labels)) +
  
  # Axes and labels
  scale_x_continuous(breaks = seq(0, 365, by = 32)) +
  labs(
    x = "Day of Year",
    y = "Precipitation (mm)"
  ) +
  
  # Horizontal lines
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray40", lwd = 0.8) +
  geom_hline(yintercept = 75, linetype = "dashed", color = "red4", lwd = 0.8) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "cadetblue", lwd = 0.8) +
  
  # Theme
  theme_classic()+
  custom_annotate(-20)
annual_precip_ts

#ggsave(paste0(figdir, "/annual_precip_georeg_supp.png"), annual_precip_ts, dpi = 300, width = 10, height = 8)
ggsave(paste0(figdir, "/annual_precip_georeg_supp.tiff"), annual_precip_ts, units = 'in', device = 'tiff', dpi = 600, width = 10, height = 8, compression = 'lzw')


