
#This repository contains code to process the grouped summary data for plotting time-series figures.

# 


rm(list=ls())

library(tidyverse)
library(viridis)
library(patchwork)

#Setup --------------------------------------------------------

#Set up directories
#wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"
boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"
#daterun_folder <- "/may17_fullyrs_complete"

complete_dir <- paste0(boxwd, "/complete_data")
figdir <- paste0(boxwd, "/figures")

seasonality <- read.csv(paste0(complete_dir, "/dynamic_precip_seasonality.csv"))
glob_season <- read.csv(paste0(complete_dir, "/global_precip_seasonality.csv"))

#Choose variables for analysis: NA-excluded, or NA-inclusive data:

#Set use either "naincl" or "naexcl"

#datatype <- "naexcl"
datatype <- "naincl"


# Read in datasets dynamically
assign("gedi_yr_summ", read.csv(paste0(complete_dir, "/gedi_yr_", datatype, "_summ.csv")))
assign("gedi_yr_zone_summ", read.csv(paste0(complete_dir, "/gedi_yr_zone_", datatype, "_summ.csv")))
assign("gedi_yr_georeg_summ", read.csv(paste0(complete_dir, "/gedi_yr_georeg_", datatype, "_summ.csv")))

assign("gedi_summ", read.csv(paste0(complete_dir, "/gedi_", datatype, "_summ.csv")))
assign("gedi_zone_summ", read.csv(paste0(complete_dir, "/gedi_zone_", datatype, "_summ.csv")))
assign("gedi_georeg_summ", read.csv(paste0(complete_dir, "/gedi_georeg_", datatype, "_summ.csv")))


gedi_zone_summ$year <- as.factor(gedi_zone_summ$year)
gedi_georeg_summ$year <- as.factor(gedi_georeg_summ$year)
gedi_summ$year <- as.factor(gedi_summ$year)

#Join the seasonality data
gedi_georeg_summ <- gedi_georeg_summ %>% left_join(seasonality, by = "georeg_agg")
gedi_yr_georeg_summ <- gedi_yr_georeg_summ %>% left_join(seasonality, by = "georeg_agg")
gedi_yr_summ <- gedi_yr_summ %>% left_join(glob_season, by = c("zone" = "region"))

#NEED TO DO THE FOLLOWING STEP FOR THE OTHER DATA ----

gedi_georeg_summ <- gedi_georeg_summ %>%
  mutate(
    sub_szn = case_when(
      doymin >= wet_start &
        doymin <= wet_end_window ~ "peakwet",
      doymin >= dry_start & 
        doymin <= dry_end_window ~ "dry",
      doymin >= earlywet_start & 
        doymin <= earlywet_end_window ~ "earlywet",
      TRUE ~ 'other'
    ) 
  ) %>% 
  select(georeg_agg, truedate, sub_szn, everything())

gedi_yr_georeg_summ <- gedi_yr_georeg_summ %>%
  mutate(
    sub_szn = case_when(
      doymin >= wet_start &
        doymin <= wet_end_window ~ "peakwet",
      doymin >= dry_start & 
        doymin <= dry_end_window ~ "dry",
      doymin >= earlywet_start & 
        doymin <= earlywet_end_window ~ "earlywet",
      TRUE ~ 'other'
    ) 
  ) %>% 
  select(georeg_agg, doymin, sub_szn, everything())

#create 'seasonal' groupings based on seasonality data:
gedi_yr_summ <- gedi_yr_summ %>%
  mutate(
    sub_szn = case_when(
      doymin >= globwet_start &
        doymin <= globwet_end_window ~ "peakwet",
      doymin >= globdry_start & 
        doymin <= globdry_end_window ~ "dry",
      doymin >= globearlywet_start & 
        doymin <= globearlywet_end_window ~ "earlywet",
      TRUE ~ 'other'
    )
  )

#Set levels
gedi_georeg_summ$georeg_agg <- factor(gedi_georeg_summ$georeg_agg,
                                      levels = c("NWA", "NOA", "CA", "Southern"))

seasonality$georeg_agg <- factor(seasonality$georeg_agg,
                                 levels = levels(gedi_georeg_summ$georeg_agg))


#Constants and plot setup -----------------------------

# Colors for variables
prec_col <- "#3b528b"
sif_col <- "#d6b71d"
sif_col2 <- "#E69F00"
#phif_col <- "#A6761D"
phif_col <- "#d6b71d"
mod_col <- "#D95F02"
toc_col <- "#BC5090"
us_col <- sif_col
pai_col <- "#6A4C93"
pri_col <- "#7570B3"
fesc_col <- "#66A61E"
cci_col <- "#1C9099"

# Colors for yearly data 
yr_vir_pal <- viridis(n = 3, option = "C", end = 0.8)
col2019 <- yr_vir_pal[1]
col2020 <- yr_vir_pal[2]
col2021 <- yr_vir_pal[3]

color_vals <- c("2019" = col2019, "2020" = col2020, "2021" = col2021)

#Color palette for peak wet, dry, and early wet
drycol <- "goldenrod1"
earlywetcol <- "purple"
peakwetcol <- "cadetblue"

drycol <- "grey80"
earlywetcol <- "grey45"
peakwetcol <- "grey10"

georeg_labels <- c(
  "NWA" = "Northwest",
  "NOA" = "Northern",
  "CA"  = "Central",
  "Southern" = "Southern"
)

# Data frame for 'global' Amazon color scheme
glob_szn_rects <- data.frame(
  xmin = c(glob_season$globdry_start, glob_season$globearlywet_start, glob_season$globwet_start),
  xmax = c(glob_season$globdry_end_window, glob_season$globearlywet_end_window, glob_season$globwet_end_window),
  season = c("DRY", "DWT", "PW"),
  fill = c(drycol, earlywetcol, peakwetcol)
)

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

##
# Figure 1***: Plots of all years as separate colors ----------------------------------------
##

#Function to create these plots
create_yr_plot <- function(data, x_var, y_var, y_label, se_var, group_var, color_var, color_vals, y_limits = NULL, facet_var = NULL) {
  # Base plot setup
  plot <- ggplot(data, aes(x = .data[[x_var]],
                           y = .data[[y_var]],
                           group = .data[[group_var]],
                           color = .data[[color_var]])) +
    geom_line()+
    geom_point() +
    geom_errorbar(aes(ymin = .data[[y_var]] - .data[[se_var]], 
                      ymax = .data[[y_var]] + .data[[se_var]]), 
                  linewidth = 0.3, alpha = 0.9) +
    labs(x = "Day of Year", y = y_label, color = "Year") +
    theme_classic() +
    scale_color_manual(values = color_vals) +
  
  # Add y-limits if given the chance
  (if (!is.null(y_limits)) scale_y_continuous(limits = y_limits) else NULL)
  
  #Add facet_wrap if facet_var is provided
  if (!is.null(facet_var)) {
    plot <- plot + facet_wrap(vars(!!sym(facet_var)), nrow = 1, labeller = as_labeller(georeg_labels))
  }
  
  return(plot)
}

#Define helper function to calculate absolute symmetric relative difference (%)
get_rel_ampl <- function(data, var) {
  vals <- data[[var]]
  vals <- vals[!is.na(vals)]
  
  if (length(vals) == 0) return(NA)
  
  min_val <- min(vals)
  max_val <- max(vals)
  
  # Avoid division by zero or exploding % from near-zero
  if (abs(min_val) + abs(max_val) == 0) return(0)
  
  rel_ampl <- round(abs(max_val - min_val) / mean(abs(c(min_val, max_val))) * 100, 1)
  return(rel_ampl)
}

chngvars <- c("mean_iprec", "mean_sif_par", "mean_sif743", "mean_sif743_cor", "mean_sifc_par", "mean_pai_toc", "mean_pai", "mean_modis_lai", "mean_phif", "mean_nirv", "mean_fesc", "mean_fpar", "mean_cci", "mean_pri_nar")

# Grouped computation
rel_df_grouped <- gedi_yr_georeg_summ %>%
  group_by(georeg_agg) %>%
  group_map(~ {
    tibble(
      georeg_agg = .y$georeg_agg,
      variable = chngvars,
      rel_ampl = map_dbl(chngvars, function(v) get_rel_ampl(.x, v))
    )
  }) %>%
  bind_rows()

rel_df_grouped <- rel_df_grouped %>%
  mutate(georeg_agg = factor(georeg_agg, levels = c("NWA", "NOA", "CA", "Southern")))

#Now create a custom annotation function
add_rel_ampl_annotation <- function(plot, rel_df, target_var, 
                                    label_prefix = "Δ = ", 
                                    hjust = 1.1, vjust = 1.5, 
                                    size = 3.5) {
  # Filter annotation data for the selected variable
  annot_df <- rel_df %>%
    filter(variable == target_var) %>%
    mutate(
      label = paste0(label_prefix, rel_ampl, "%"),
      x = Inf,
      y = Inf
    )
  
  # Add to plot
  plot + 
    geom_text(
      data = annot_df,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = hjust,
      vjust = vjust,
      size = size
    )
}

levels(gedi_georeg_summ$georeg_agg)
str(gedi_georeg_summ$georeg_agg)
#plots
# Precip
plot_prec_geo <- create_yr_plot(
  data = gedi_georeg_summ, 
  x_var = "doymin", 
  y_var = "mean_iprec", 
  y_label = "16-day precip (mm)", 
  se_var = "se_iprec", 
  group_var = "year", 
  color_var = "year", 
  color_vals = color_vals,
  facet_var = "georeg_agg"
)+
  custom_annotate(y_text_pos = 0)

plot_prec_geo <- add_rel_ampl_annotation(plot_prec_geo, rel_df_grouped, "mean_iprec")
plot_prec_geo

# SIF/PAR plot
plot_sif_par_geo <- create_yr_plot(gedi_georeg_summ, 
                                 x_var = "doymin", 
                                 y_var = "mean_sif_par", 
                                 y_label = expression("SIF/PAR ("*sr^{-1}*"·"*nm^{-1}*")"), 
                                 se_var = "se_sif_par", 
                                 group_var = "year", 
                                 color_var = "year", 
                                 color_vals = color_vals, 
                                 facet_var = "georeg_agg") + 
  custom_annotate(0.0000016)

plot_sif_par_geo <- add_rel_ampl_annotation(plot_sif_par_geo, rel_df_grouped, "mean_sif_par")
plot_sif_par_geo

#Corrected SIF/PAR
plot_sifc_par_geo <- create_yr_plot(gedi_georeg_summ, 
                                   x_var = "doymin", 
                                   y_var = "mean_sifc_par", 
                                   y_label = expression("SIF/PAR (cos(SZA)) ("*sr^{-1}*"·"*nm^{-1}*")"), 
                                   se_var = "se_sifc_par", 
                                   group_var = "year", 
                                   color_var = "year", 
                                   color_vals = color_vals, 
                                   facet_var = "georeg_agg") + 
  custom_annotate(0.0000016)

plot_sifc_par_geo <- add_rel_ampl_annotation(plot_sifc_par_geo, rel_df_grouped, "mean_sifc_par")
plot_sifc_par_geo

# PhiF plot
plot_phif_geo <- create_yr_plot(gedi_georeg_summ, 
                                   x_var = "doymin", 
                                   y_var = "mean_phif", 
                                   y_label = "PhiF", 
                                   se_var = "se_phif", 
                                   group_var = "year", 
                                   color_var = "year", 
                                   color_vals = color_vals, 
                                   facet_var = "georeg_agg") + 
  custom_annotate(0.0000065)

plot_phif_geo <- add_rel_ampl_annotation(plot_phif_geo, rel_df_grouped, "mean_phif")
plot_phif_geo

# Canopy PAI plot
plot_pai_toc_geo <- create_yr_plot(gedi_georeg_summ, 
                                 x_var = "doymin", 
                                 y_var = "mean_pai_toc", 
                                 y_label = expression("Canopy PAI ("*m^{2}*"/"*m^{2}*")"), 
                                 se_var = "se_pai_toc", 
                                 group_var = "year", 
                                 color_var = "year", 
                                 color_vals = color_vals,
                                 facet_var = "georeg_agg")+ 
  custom_annotate(0.72)

plot_pai_toc_geo <- add_rel_ampl_annotation(plot_pai_toc_geo, rel_df_grouped, "mean_pai_toc")
plot_pai_toc_geo

# NIRv plot
plot_fesc_geo <- create_yr_plot(gedi_georeg_summ, 
                              x_var = "doymin", 
                              y_var = "mean_fesc", 
                              y_label = "fesc", 
                              se_var = "se_nirv", 
                              group_var = "year", 
                              color_var = "year", 
                              color_vals = color_vals, 
                              facet_var = "georeg_agg")+ 
  custom_annotate(0.273)

plot_fesc_geo <- add_rel_ampl_annotation(plot_fesc_geo, rel_df_grouped, "mean_fesc")
plot_fesc_geo

#MODIS LAI plot
plot_modlai_geo <- create_yr_plot(gedi_georeg_summ, 
                               x_var = "doymin", 
                               y_var = "mean_modis_lai", 
                               y_label = expression("MODIS LAI ("*m^{2}*"/"*m^{2}*")"), 
                               se_var = "se_modis_lai", 
                               group_var = "year", 
                               color_var = "year", 
                               color_vals = color_vals, 
                               facet_var = "georeg_agg")+
  custom_annotate(4.7)

plot_modlai_geo <- add_rel_ampl_annotation(plot_modlai_geo, rel_df_grouped, "mean_modis_lai")
plot_modlai_geo

#CCI plot
plot_cci_geo <- create_yr_plot(gedi_georeg_summ, 
                               x_var = "doymin", 
                               y_var = "mean_cci", 
                               y_label = "CCI", 
                               se_var = "se_cci", 
                               group_var = "year", 
                               color_var = "year", 
                               color_vals = color_vals, 
                               facet_var = "georeg_agg")+
  custom_annotate(0.07)

plot_cci_geo <- add_rel_ampl_annotation(plot_cci_geo, rel_df_grouped, "mean_cci")
plot_cci_geo


#PRI (narrow) plot
plot_prinar_geo <- create_yr_plot(gedi_georeg_summ, 
                              x_var = "doymin", 
                              y_var = "mean_pri_nar", 
                              y_label = "PRI (narrow)", 
                              se_var = "se_pri_nar", 
                              group_var = "year", 
                              color_var = "year", 
                              color_vals = color_vals, 
                              facet_var = "georeg_agg")+
  custom_annotate(-0.0938)

plot_prinar_geo <- add_rel_ampl_annotation(plot_prinar_geo, rel_df_grouped, "mean_pri_nar")
plot_prinar_geo

#NIRv TROPO
plot_nirv_tropo_refl_geo <- create_yr_plot(gedi_georeg_summ, 
                                  x_var = "doymin", 
                                  y_var = "mean_nirv_tropo_refl", 
                                  y_label = "NIRv TROPO", 
                                  se_var = "se_nirv_tropo_refl", 
                                  group_var = "year", 
                                  color_var = "year", 
                                  color_vals = color_vals, 
                                  facet_var = "georeg_agg")+
  custom_annotate(0.09)

plot_nirv_tropo_refl_geo <- add_rel_ampl_annotation(plot_nirv_tropo_refl_geo, rel_df_grouped, "mean_nirv_refl_tropo")
plot_nirv_tropo_refl_geo

plot_nirv_tropo_rad_geo <- create_yr_plot(gedi_georeg_summ, 
                                      x_var = "doymin", 
                                      y_var = "mean_nirv_tropo_rad", 
                                      y_label = "Using TROPO. NIR Radiance", 
                                      se_var = "se_nirv_tropo_rad", 
                                      group_var = "year", 
                                      color_var = "year", 
                                      color_vals = color_vals, 
                                      facet_var = "georeg_agg")+
  custom_annotate(40)

plot_nirv_tropo_rad_geo <- add_rel_ampl_annotation(plot_nirv_tropo_rad_geo, rel_df_grouped, "mean_nirv_radtr")
plot_nirv_tropo_rad_geo

plot_phif_tropo_refl_geo <- create_yr_plot(gedi_georeg_summ, 
                                      x_var = "doymin", 
                                      y_var = "mean_phif_tropo_refl", 
                                      y_label = "TROPOSIF PhiF", 
                                      se_var = "se_phif_tropo_refl", 
                                      group_var = "year", 
                                      color_var = "year", 
                                      color_vals = color_vals, 
                                      facet_var = "georeg_agg")+
  custom_annotate(0.000013)

plot_phif_tropo_refl_geo <- add_rel_ampl_annotation(plot_phif_tropo_refl_geo, rel_df_grouped, "mean_phif_tropo")
plot_phif_tropo_refl_geo

# Modify each plot to work for the overall plot structure
plot_prec_geo <- plot_prec_geo + 
  theme(axis.title.x = element_blank())

plot_sif_par_geo <- plot_sif_par_geo + 
  theme(strip.text = element_blank(),
        axis.title.x = element_blank())

plot_phif_geo <- plot_phif_geo + 
  theme(strip.text = element_blank(),
        axis.title.x = element_blank())

plot_cci_geo <- plot_cci_geo + 
  theme(strip.text = element_blank(),
        axis.title.x = element_blank())

plot_fesc_geo <- plot_fesc_geo + 
  theme(strip.text = element_blank(),
        axis.title.x = element_blank())

plot_modlai_geo <- plot_modlai_geo + 
  theme(strip.text = element_blank(),
        axis.title.x = element_blank())

plot_pai_toc_geo <- plot_pai_toc_geo + 
  theme(strip.text = element_blank())

# plot_prinar_geo <- plot_prinar_geo + 
#   theme(strip.text = element_blank(),
#         axis.title.x = element_blank())


# Combine plots into one figure
georeg_plot <- plot_prec_geo /
  plot_sif_par_geo /
  #plot_phif_geo /
  plot_cci_geo /
  plot_fesc_geo /
  plot_modlai_geo /
  plot_pai_toc_geo +
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')',
                  tag_sep = ' ') &
  theme(legend.position = "bottom")

georeg_plot

#save plot
#ggsave(paste0(figdir, "/multiyear_georeg_abs_trends.png"), georeg_plot, dpi = 300, width = 14, height = 11)
ggsave(paste0(figdir, "/multiyear_georeg_abs_trends.tiff"), device = 'tiff', georeg_plot, dpi = 600, width = 14, height = 11, compression = 'lzw')


# Fig 2. Plots based on % Change ----------------------------------------------------
# Filtering data by georeg_agg
yr_geo_CA <- gedi_yr_georeg_summ %>% filter(georeg_agg == "CA")
yr_geo_NOA <- gedi_yr_georeg_summ %>% filter(georeg_agg == "NOA")
yr_geo_NWA <- gedi_yr_georeg_summ %>% filter(georeg_agg == "NWA")
yr_geo_Southern <- gedi_yr_georeg_summ %>% filter(georeg_agg == "Southern")
yr_all <- gedi_yr_summ


# Extract baseline values at start of dry season
baseCA_pai <- yr_geo_CA %>% filter(georeg_agg == "CA", doymin == dry_start) %>% pull(mean_pai)
baseCA_sif_par <- yr_geo_CA %>% filter(georeg_agg == "CA", doymin == dry_start) %>% pull(mean_sif_par)
baseCA_sif <- yr_geo_CA %>% filter(georeg_agg == "CA", doymin == dry_start) %>% pull(mean_sif743_cor)
baseCA_tocpai <- yr_geo_CA %>% filter(georeg_agg == "CA", doymin == dry_start) %>% pull(mean_pai_toc)
baseCA_uspai <- yr_geo_CA %>% filter(georeg_agg == "CA", doymin == dry_start) %>% pull(mean_pai_us)
baseCA_fesc <- yr_geo_CA %>% filter(georeg_agg == "CA", doymin == dry_start) %>% pull(mean_fesc)
baseCA_phif <- yr_geo_CA %>% filter(georeg_agg == "CA", doymin == dry_start) %>% pull(mean_phif)
baseCA_iprec <- yr_geo_CA %>% filter(georeg_agg == "CA", doymin == dry_start) %>% pull(mean_iprec)
baseCA_nirv <- yr_geo_CA %>% filter(georeg_agg == "CA", doymin == dry_start) %>% pull(mean_nirv)
baseCA_modlai <- yr_geo_CA %>% filter(georeg_agg == "CA", doymin == dry_start) %>% pull(mean_modis_lai)
baseCA_sdvfp <- yr_geo_CA %>% filter(georeg_agg == "CA", doymin == dry_start) %>% pull(mean_sdvfp)
baseCA_prinar <- yr_geo_CA %>% filter(georeg_agg == "CA", doymin == dry_start) %>% pull(mean_pri_nar)
baseCA_cci <- yr_geo_CA %>% filter(georeg_agg == "CA", doymin == dry_start) %>% pull(mean_cci)

baseNOA_pai <- yr_geo_NOA %>% filter(georeg_agg == "NOA", doymin == dry_start) %>% pull(mean_pai)
baseNOA_sif_par <- yr_geo_NOA %>% filter(georeg_agg == "NOA", doymin == dry_start) %>% pull(mean_sif_par)
baseNOA_sif <- yr_geo_NOA %>% filter(georeg_agg == "NOA", doymin == dry_start) %>% pull(mean_sif743_cor)
baseNOA_tocpai <- yr_geo_NOA %>% filter(georeg_agg == "NOA", doymin == dry_start) %>% pull(mean_pai_toc)
baseNOA_uspai <- yr_geo_NOA %>% filter(georeg_agg == "NOA", doymin == dry_start) %>% pull(mean_pai_us)
baseNOA_fesc <- yr_geo_NOA %>% filter(georeg_agg == "NOA", doymin == dry_start) %>% pull(mean_fesc)
baseNOA_phif <- yr_geo_NOA %>% filter(georeg_agg == "NOA", doymin == dry_start) %>% pull(mean_phif)
baseNOA_nirv <- yr_geo_NOA %>% filter(georeg_agg == "NOA", doymin == dry_start) %>% pull(mean_nirv)
baseNOA_iprec <- yr_geo_NOA %>% filter(georeg_agg == "NOA", doymin == dry_start) %>% pull(mean_iprec)
baseNOA_modlai <- yr_geo_NOA %>% filter(georeg_agg == "NOA", doymin == dry_start) %>% pull(mean_modis_lai)
baseNOA_sdvfp <- yr_geo_NOA %>% filter(georeg_agg == "NOA", doymin == dry_start) %>% pull(mean_sdvfp)
baseNOA_prinar <- yr_geo_NOA %>% filter(georeg_agg == "NOA", doymin == dry_start) %>% pull(mean_pri_nar)
baseNOA_cci <- yr_geo_NOA %>% filter(georeg_agg == "NOA", doymin == dry_start) %>% pull(mean_cci)

baseNWA_pai <- yr_geo_NWA %>% filter(georeg_agg == "NWA", doymin == dry_start) %>% pull(mean_pai)
baseNWA_sif_par <- yr_geo_NWA %>% filter(georeg_agg == "NWA", doymin == dry_start) %>% pull(mean_sif_par)
baseNWA_sif <- yr_geo_NWA %>% filter(georeg_agg == "NWA", doymin == dry_start) %>% pull(mean_sif743_cor)
baseNWA_tocpai <- yr_geo_NWA %>% filter(georeg_agg == "NWA", doymin == dry_start) %>% pull(mean_pai_toc)
baseNWA_uspai <- yr_geo_NWA %>% filter(georeg_agg == "NWA", doymin == dry_start) %>% pull(mean_pai_us)
baseNWA_fesc <- yr_geo_NWA %>% filter(georeg_agg == "NWA", doymin == dry_start) %>% pull(mean_fesc)
baseNWA_phif <- yr_geo_NWA %>% filter(georeg_agg == "NWA", doymin == dry_start) %>% pull(mean_phif)
baseNWA_nirv <- yr_geo_NWA %>% filter(georeg_agg == "NWA", doymin == dry_start) %>% pull(mean_nirv)
baseNWA_iprec <- yr_geo_NWA %>% filter(georeg_agg == "NWA", doymin == dry_start) %>% pull(mean_iprec)
baseNWA_modlai <- yr_geo_NWA %>% filter(georeg_agg == "NWA", doymin == dry_start) %>% pull(mean_modis_lai)
baseNWA_sdvfp <- yr_geo_NWA %>% filter(georeg_agg == "NWA", doymin == dry_start) %>% pull(mean_sdvfp)
baseNWA_prinar <- yr_geo_NWA %>% filter(georeg_agg == "NWA", doymin == dry_start) %>% pull(mean_pri_nar)
baseNWA_cci <- yr_geo_NWA %>% filter(georeg_agg == "NWA", doymin == dry_start) %>% pull(mean_cci)

baseSouthern_pai <- yr_geo_Southern %>% filter(georeg_agg == "Southern", doymin == dry_start) %>% pull(mean_pai)
baseSouthern_sif_par <- yr_geo_Southern %>% filter(georeg_agg == "Southern", doymin == dry_start) %>% pull(mean_sif_par)
baseSouthern_sif <- yr_geo_Southern %>% filter(georeg_agg == "Southern", doymin == dry_start) %>% pull(mean_sif743_cor)
baseSouthern_tocpai <- yr_geo_Southern %>% filter(georeg_agg == "Southern", doymin == dry_start) %>% pull(mean_pai_toc)
baseSouthern_uspai <- yr_geo_Southern %>% filter(georeg_agg == "Southern", doymin == dry_start) %>% pull(mean_pai_us)
baseSouthern_fesc <- yr_geo_Southern %>% filter(georeg_agg == "Southern", doymin == dry_start) %>% pull(mean_fesc)
baseSouthern_phif <- yr_geo_Southern %>% filter(georeg_agg == "Southern", doymin == dry_start) %>% pull(mean_phif)
baseSouthern_iprec <- yr_geo_Southern %>% filter(georeg_agg == "Southern", doymin == dry_start) %>% pull(mean_iprec)
baseSouthern_nirv <- yr_geo_Southern %>% filter(georeg_agg == "Southern", doymin == dry_start) %>% pull(mean_nirv)
baseSouthern_modlai <- yr_geo_Southern %>% filter(georeg_agg == "Southern", doymin == dry_start) %>% pull(mean_modis_lai)
baseSouthern_sdvfp <- yr_geo_Southern %>% filter(georeg_agg == "Southern", doymin == dry_start) %>% pull(mean_sdvfp)
baseSouthern_prinar <- yr_geo_Southern %>% filter(georeg_agg == "Southern", doymin == dry_start) %>% pull(mean_pri_nar)
baseSouthern_cci <- yr_geo_Southern %>% filter(georeg_agg == "Southern", doymin == dry_start) %>% pull(mean_cci)

base_pai <- yr_all %>% filter(doymin == globwet_start) %>% pull(mean_pai)
base_sif_par <- yr_all %>% filter(doymin == globwet_start) %>% pull(mean_sif_par)
base_sif <- yr_all %>% filter(doymin == globwet_start) %>% pull(mean_sif743_cor)
base_tocpai <- yr_all %>% filter(doymin == globwet_start) %>% pull(mean_pai_toc)
base_uspai <- yr_all %>% filter(doymin == globwet_start) %>% pull(mean_pai_us)
base_fesc <- yr_all %>% filter(doymin == globwet_start) %>% pull(mean_fesc)
base_phif <- yr_all %>% filter(doymin == globwet_start) %>% pull(mean_phif)
base_iprec <- yr_all %>% filter(doymin == globwet_start) %>% pull(mean_iprec)
base_nirv <- yr_all %>% filter(doymin == globwet_start) %>% pull(mean_nirv)
base_modlai <- yr_all %>% filter(doymin == globwet_start) %>% pull(mean_modis_lai)
base_sdvfp <- yr_all %>% filter(doymin == globwet_start) %>% pull(mean_sdvfp)
base_prinar <- yr_all %>% filter(doymin == globwet_start) %>% pull(mean_pri_nar)
base_cci <- yr_all %>% filter(doymin == globwet_start) %>% pull(mean_cci)

#Using symmetric percent change:
# Symmetric percent change helper function
spc <- function(new, base) {
  100 * (new - base) / ((new + base) / 2)
}

#absolute case
spc_abs <- function(new, base) {
  100 * abs((new - base)) / ((abs(new) + abs(base)) / 2)
}

# CA region
yr_geo_CA <- yr_geo_CA %>%
  mutate(
    mean_pai_pct_chg = spc(mean_pai, baseCA_pai),
    mean_tocpai_pct_chg = spc(mean_pai_toc, baseCA_tocpai),
    mean_uspai_pct_chg = spc(mean_pai_us, baseCA_uspai),
    mean_sif_par_pct_chg = spc(mean_sif_par, baseCA_sif_par),
    mean_sif_pct_chg = spc(mean_sif743_cor, baseCA_sif),
    mean_fesc_pct_chg = spc(mean_fesc, baseCA_fesc),
    mean_phif_pct_chg = spc(mean_phif, baseCA_phif),
    mean_nirv_pct_chg = spc(mean_nirv, baseCA_nirv),
    mean_iprec_pct_chg = spc(mean_iprec, baseCA_iprec),
    mean_modlai_pct_chg = spc(mean_modis_lai, baseCA_modlai),
    mean_sdvfp_pct_chg = spc(mean_sdvfp, baseCA_sdvfp),
    mean_prinar_pct_chg = spc(mean_pri_nar, baseCA_prinar), #using absolute function
    mean_cci_pct_chg = spc(mean_cci, baseCA_cci)
  )

# NOA region
yr_geo_NOA <- yr_geo_NOA %>%
  mutate(
    mean_pai_pct_chg = spc(mean_pai, baseNOA_pai),
    mean_tocpai_pct_chg = spc(mean_pai_toc, baseNOA_tocpai),
    mean_uspai_pct_chg = spc(mean_pai_us, baseNOA_uspai),
    mean_sif_par_pct_chg = spc(mean_sif_par, baseNOA_sif_par),
    mean_sif_pct_chg = spc(mean_sif743_cor, baseNOA_sif),
    mean_fesc_pct_chg = spc(mean_fesc, baseNOA_fesc),
    mean_phif_pct_chg = spc(mean_phif, baseNOA_phif),
    mean_nirv_pct_chg = spc(mean_nirv, baseNOA_nirv),
    mean_iprec_pct_chg = spc(mean_iprec, baseNOA_iprec),
    mean_modlai_pct_chg = spc(mean_modis_lai, baseNOA_modlai),
    mean_sdvfp_pct_chg = spc(mean_sdvfp, baseNOA_sdvfp),
    mean_prinar_pct_chg = spc(mean_pri_nar, baseNOA_prinar), #using absolute function
    mean_cci_pct_chg = spc(mean_cci, baseNOA_cci)
  )

# NWA region
yr_geo_NWA <- yr_geo_NWA %>%
  mutate(
    mean_pai_pct_chg = spc(mean_pai, baseNWA_pai),
    mean_tocpai_pct_chg = spc(mean_pai_toc, baseNWA_tocpai),
    mean_uspai_pct_chg = spc(mean_pai_us, baseNWA_uspai),
    mean_sif_par_pct_chg = spc(mean_sif_par, baseNWA_sif_par),
    mean_sif_pct_chg = spc(mean_sif743_cor, baseNWA_sif),
    mean_fesc_pct_chg = spc(mean_fesc, baseNWA_fesc),
    mean_phif_pct_chg = spc(mean_phif, baseNWA_phif),
    mean_nirv_pct_chg = spc(mean_nirv, baseNWA_nirv),
    mean_iprec_pct_chg = spc(mean_iprec, baseNWA_iprec),
    mean_modlai_pct_chg = spc(mean_modis_lai, baseNWA_modlai),
    mean_sdvfp_pct_chg = spc(mean_sdvfp, baseNWA_sdvfp),
    mean_prinar_pct_chg = spc(mean_pri_nar, baseNWA_prinar), #using absolute function
    mean_cci_pct_chg = spc(mean_cci, baseNWA_cci)
  )

# Southern region
yr_geo_Southern <- yr_geo_Southern %>%
  mutate(
    mean_pai_pct_chg = spc(mean_pai, baseSouthern_pai),
    mean_tocpai_pct_chg = spc(mean_pai_toc, baseSouthern_tocpai),
    mean_uspai_pct_chg = spc(mean_pai_us, baseSouthern_uspai),
    mean_sif_par_pct_chg = spc(mean_sif_par, baseSouthern_sif_par),
    mean_sif_pct_chg = spc(mean_sif743_cor, baseSouthern_sif),
    mean_fesc_pct_chg = spc(mean_fesc, baseSouthern_fesc),
    mean_phif_pct_chg = spc(mean_phif, baseSouthern_phif),
    mean_nirv_pct_chg = spc(mean_nirv, baseSouthern_nirv),
    mean_iprec_pct_chg = spc(mean_iprec, baseSouthern_iprec),
    mean_modlai_pct_chg = spc(mean_modis_lai, baseSouthern_modlai),
    mean_sdvfp_pct_chg = spc(mean_sdvfp, baseSouthern_sdvfp),
    mean_prinar_pct_chg = spc(mean_pri_nar, baseSouthern_prinar), #using absolute function
    mean_cci_pct_chg = spc(mean_cci, baseSouthern_cci)
  )

# All regions
yr_all <- yr_all %>%
  mutate(
    mean_pai_pct_chg = spc(mean_pai, base_pai),
    mean_tocpai_pct_chg = spc(mean_pai_toc, base_tocpai),
    mean_uspai_pct_chg = spc(mean_pai_us, base_uspai),
    mean_sif_par_pct_chg = spc(mean_sif_par, base_sif_par),
    mean_sif_pct_chg = spc(mean_sif743_cor, base_sif),
    mean_fesc_pct_chg = spc(mean_fesc, base_fesc),
    mean_phif_pct_chg = spc(mean_phif, base_phif),
    mean_iprec_pct_chg = spc(mean_iprec, base_iprec),
    mean_nirv_pct_chg = spc(mean_nirv, base_nirv),
    mean_modlai_pct_chg = spc(mean_modis_lai, base_modlai),
    mean_sdvfp_pct_chg = spc(mean_sdvfp, base_sdvfp),
    mean_prinar_pct_chg = spc(mean_pri_nar, base_prinar), #using absolute function
    mean_cci_pct_chg = spc(mean_cci, base_cci)
  )


#
# Figure ***: Percent Change time-series ---------------------------

#New custom_annotate:
custom_annotate2 <- function(region, y_text_pos = NULL) {
  region_season <- seasonality[seasonality$georeg_agg == region, ]
  
  annotations <- list(
    # Dry season shading
    annotate("rect",
             xmin = region_season$dry_start,
             xmax = region_season$dry_end_window,
             ymin = -Inf, ymax = Inf,
             fill = drycol, alpha = 0.2),
    
    # Early wet season shading
    annotate("rect",
             xmin = region_season$earlywet_start,
             xmax = region_season$earlywet_end_window,
             ymin = -Inf, ymax = Inf,
             fill = earlywetcol, alpha = 0.2),
    
    # Wet peak season shading
    annotate("rect",
             xmin = region_season$wet_start,
             xmax = region_season$wet_end_window,
             ymin = -Inf, ymax = Inf,
             fill = peakwetcol, alpha = 0.2),
    
    #Dry season start
    geom_vline(xintercept = region_season$dry_start, linetype = "dotted", color = "red", linewidth = 0.8)
   
  )
  
  # Optional text labels
  if (!is.null(y_text_pos)) {
    annotations <- c(annotations, list(
      annotate("text", x = region_season$dry_start, y = y_text_pos, label = "DRY", hjust = -0.1, size = 3),
      annotate("text", x = region_season$earlywet_start, y = y_text_pos, label = "DWT", hjust = -0.1, size = 3),
      annotate("text", x = region_season$wet_start, y = y_text_pos, label = "PW", hjust = -0.1, size = 3)
    ))
  }
  
  return(annotations)
}


#% Change: CCI, SIF/PAR, PRI, TOC PAI ----------------------------------

make_sif_cci_pai_plot <- function(data, doy_col, sif_mean_col, sif_se_col, sif_base, phif_mean_col, phif_se_col, phif_base,tocpai_mean_col, tocpai_se_col, tocpai_base, fesc_mean_col, fesc_se_col, fesc_base, mod_mean_col, mod_se_col, mod_base, cci_mean_col, cci_se_col, cci_base, sif_color, phif_color, toc_color, cci_color, fesc_color, mod_color, zone_label, ylim_range = c(-55, 63), line_alpha = 0.4, loess_alpha = 0.2) {
  
  p <- ggplot(data, aes(x = as.numeric(as.character(!!sym(doy_col))))) +
    
    # SIF Layer
    geom_point(aes(y = !!sym(sif_mean_col), color = "SIF/PAR"), size = 2.3, alpha = line_alpha) +
    geom_errorbar(aes(
      ymin = !!sym(sif_mean_col) - 100 * !!sym(sif_se_col) / !!sym(sif_base), 
      ymax = !!sym(sif_mean_col) + 100 * !!sym(sif_se_col) / !!sym(sif_base), 
      color = "SIF/PAR"), 
      linewidth = 0.3, alpha = line_alpha) +
    geom_line(aes(y = !!sym(sif_mean_col), color = "SIF/PAR"), alpha = line_alpha) +
    #geom_smooth(aes(y = !!sym(sif_mean_col)), method = "loess", alpha = loess_alpha, color = sif_color, fill = sif_color) +
    geom_smooth(aes(y = !!sym(sif_mean_col)), method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2, color = sif_color, fill = sif_color)+
    
    # --- PhiF layer (only if not NA) ---
    if (!is.na(phif_mean_col)) {
      p <- p +
        geom_point(aes(y = !!sym(phif_mean_col), color = "PhiF"), size = 2.3, alpha = line_alpha) +
        geom_errorbar(aes(
          ymin = !!sym(phif_mean_col) - 100 * !!sym(phif_se_col) / !!sym(phif_base), 
          ymax = !!sym(phif_mean_col) + 100 * !!sym(phif_se_col) / !!sym(phif_base), 
          color = "PhiF"), linewidth = 0.3, alpha = line_alpha) +
        geom_line(aes(y = !!sym(phif_mean_col), color = "PhiF"), alpha = line_alpha) +
        geom_smooth(aes(y = !!sym(phif_mean_col)), method = "gam", se = TRUE,
                    alpha = 0.3, linewidth = 1.2, color = phif_color, fill = phif_color)
    }
    
    # PhiF Layer
    # geom_point(aes(y = !!sym(phif_mean_col), color = "PhiF"), size = 2.3, alpha = line_alpha) +
    # geom_errorbar(aes(
    #   ymin = !!sym(phif_mean_col) - 100 * !!sym(phif_se_col) / !!sym(phif_base), 
    #   ymax = !!sym(phif_mean_col) + 100 * !!sym(phif_se_col) / !!sym(phif_base), 
    #   color = "PhiF"), 
    #   linewidth = 0.3, alpha = line_alpha) +
    # geom_line(aes(y = !!sym(phif_mean_col), color = "PhiF"), alpha = line_alpha) +
    # #geom_smooth(aes(y = !!sym(phif_mean_col)), method = "loess", alpha = loess_alpha, color = phif_color, fill = phif_color) +
    # geom_smooth(aes(y = !!sym(phif_mean_col)), method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2, color = phif_color, fill = phif_color)+
    
    #TOC PAI Layer
  p <- p +
      
    geom_point(aes(y = !!sym(tocpai_mean_col), color = "TOC PAI"), size = 2.3, alpha = line_alpha) +
    geom_errorbar(aes(
      ymin = !!sym(tocpai_mean_col) - 100 * !!sym(tocpai_se_col) / !!sym(tocpai_base), 
      ymax = !!sym(tocpai_mean_col) + 100 * !!sym(tocpai_se_col) / !!sym(tocpai_base), 
      color = "TOC PAI"), 
      linewidth = 0.3, alpha = line_alpha) +
    geom_line(aes(y = !!sym(tocpai_mean_col), color = "TOC PAI"), alpha = line_alpha) +
    #geom_smooth(aes(y = !!sym(tocpai_mean_col)), method = "loess", alpha = loess_alpha, color = toc_color, fill = toc_color) +
    geom_smooth(aes(y = !!sym(tocpai_mean_col)), method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2, color = toc_color, fill = toc_color)+
    
    # MOD LAI Layer
    geom_point(aes(y = !!sym(mod_mean_col), color = "MODIS LAI"), size = 2.3, alpha = line_alpha) +
    geom_errorbar(aes(
      ymin = !!sym(mod_mean_col) - 100 * !!sym(mod_se_col) / !!sym(mod_base), 
      ymax = !!sym(mod_mean_col) + 100 * !!sym(mod_se_col) / !!sym(mod_base), 
      color = "MODIS LAI"), 
      linewidth = 0.3, alpha = line_alpha) +
    geom_line(aes(y = !!sym(mod_mean_col), color = "MODIS LAI"), alpha = line_alpha) +
    #geom_smooth(aes(y = !!sym(mod_mean_col)), method = "loess", alpha = loess_alpha, color = mod_color, fill = mod_color) +
    geom_smooth(aes(y = !!sym(mod_mean_col)), method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2, color = mod_color, fill = mod_color)+
    
    # fesc Layer
    geom_point(aes(y = !!sym(fesc_mean_col), color = "fesc"), size = 2.3, alpha = line_alpha) +
    geom_errorbar(aes(
      ymin = !!sym(fesc_mean_col) - 100 * !!sym(fesc_se_col) / !!sym(fesc_base),
      ymax = !!sym(fesc_mean_col) + 100 * !!sym(fesc_se_col) / !!sym(fesc_base),
      color = "fesc"),
      linewidth = 0.3, alpha = line_alpha) +
    geom_line(aes(y = !!sym(fesc_mean_col), color = "fesc"), alpha = line_alpha) +
    #geom_smooth(aes(y = !!sym(fesc_mean_col)), method = "loess", alpha =loess_alpha, color = fesc_color, fill = fesc_color) +
    geom_smooth(aes(y = !!sym(fesc_mean_col)), method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2, color = fesc_color, fill = fesc_color)+
    
    #CCI Layer
    geom_point(aes(y = !!sym(cci_mean_col), color = "CCI"), size = 2.3, alpha = line_alpha) +
    geom_errorbar(aes(
      ymin = !!sym(cci_mean_col) - 100 * !!sym(cci_se_col) / !!sym(cci_base), 
      ymax = !!sym(cci_mean_col) + 100 * !!sym(cci_se_col) / !!sym(cci_base), 
      color = "TOC PAI"), 
      linewidth = 0.3, alpha = line_alpha) +
    geom_line(aes(y = !!sym(cci_mean_col), color = "CCI"), alpha = line_alpha) +
   # geom_smooth(aes(y = !!sym(cci_mean_col)), method = "loess", alpha = loess_alpha, color = cci_color, fill = cci_color) +
    geom_smooth(aes(y = !!sym(cci_mean_col)), method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2, color = cci_color, fill = cci_color)+
    
    # Additional layers and aesthetics
    ylim(ylim_range) +
    labs(y = "% Change relative to dry period onset", color = "RS variable") +
    theme_minimal() +
    labs(x = "Day of Year") +
    annotate(geom = "text", x = 10, y = ylim_range[2] - 10, hjust = 0, label = zone_label, color = "black", size = rel(7), family = "serif") +
    # Manual color scale
    scale_color_manual(
      values = setNames(c(sif_color, toc_color, fesc_color, mod_color, cci_color, phif_color), c("SIF/PAR", "TOC PAI", "fesc", "MODIS LAI", "CCI", "PhiF")),
      labels = c(
        "SIF/PAR" = "SIF/PAR",
        "TOC PAI" = expression(paste("PAI"[TOC])),
        "fesc" = expression(paste("f"[esc])),
        "PhiF" = expression(paste(Phi, "F")),
        "CCI" = "CCI",
        "MODIS LAI" = "MODIS LAI"
      )
    )
  return(p)
}


# CCI vs SIF/PAR plots
sif_cci_pai_CA <- make_sif_cci_pai_plot(
  data = yr_geo_CA,
  doy_col = "doymin",
  sif_mean_col = "mean_sif_par_pct_chg",
  sif_se_col = "se_sif_par",
  sif_base = "baseCA_sif_par",
  #phif_mean_col = "mean_phif_pct_chg",
  phif_mean_col = NA,  # turn off PhiF
  phif_se_col = "se_phif",
  phif_base = "baseCA_phif",
  tocpai_mean_col = "mean_tocpai_pct_chg",
  tocpai_se_col = "se_pai_toc",
  tocpai_base = "baseCA_tocpai",
  mod_mean_col = "mean_modlai_pct_chg",
  mod_se_col = "se_modis_lai",
  mod_base = "baseCA_modlai",
  cci_mean_col = "mean_cci_pct_chg",
  cci_se_col = "se_cci",
  cci_base = "baseCA_cci",
  fesc_mean_col = "mean_fesc_pct_chg",
  fesc_se_col = "se_fesc",
  fesc_base = "baseCA_fesc",
  sif_color = sif_col2,
  phif_color = phif_col,
  toc_color = toc_col,
  mod_color = mod_col,
  fesc_color = fesc_col,
  cci_color = cci_col,
  zone_label = "Central Amz. (seasonal)"
) + custom_annotate2(region = "CA", y_text_pos = -55)
sif_cci_pai_CA

sif_cci_pai_NOA <- make_sif_cci_pai_plot(
  data = yr_geo_NOA,
  doy_col = "doymin",
  sif_mean_col = "mean_sif_par_pct_chg",
  sif_se_col = "se_sif_par",
  sif_base = "baseNOA_sif_par",
  #phif_mean_col = "mean_phif_pct_chg",
  phif_mean_col = NA,  # turn off PhiF
  phif_se_col = "se_phif",
  phif_base = "baseCA_phif",
  tocpai_mean_col = "mean_tocpai_pct_chg",
  tocpai_se_col = "se_pai_toc",
  tocpai_base = "baseNOA_tocpai",
  cci_mean_col = "mean_cci_pct_chg",
  mod_mean_col = "mean_modlai_pct_chg",
  mod_se_col = "se_modis_lai",
  mod_base = "baseNOA_modlai",
  cci_se_col = "se_cci",
  cci_base = "baseNOA_cci",
  fesc_mean_col = "mean_fesc_pct_chg",
  fesc_se_col = "se_fesc",
  fesc_base = "baseNOA_fesc",
  sif_color = sif_col2,
  phif_color = phif_col,
  toc_color = toc_col,
  mod_color = mod_col,
  fesc_color = fesc_col,
  cci_color = cci_col,
  zone_label = "Northern Amz. (bimodal)"
) + custom_annotate2(region = "NOA", y_text_pos = -55)
sif_cci_pai_NOA

sif_cci_pai_NWA <- make_sif_cci_pai_plot(
  data = yr_geo_NWA,
  doy_col = "doymin",
  sif_mean_col = "mean_sif_par_pct_chg",
  sif_se_col = "se_sif_par",
  sif_base = "baseNWA_sif_par",
  #phif_mean_col = "mean_phif_pct_chg",
  phif_mean_col = NA,  # turn off PhiF
  phif_se_col = "se_phif",
  phif_base = "baseCA_phif",
  tocpai_mean_col = "mean_tocpai_pct_chg",
  tocpai_se_col = "se_pai_toc",
  tocpai_base = "baseNWA_tocpai",
  mod_mean_col = "mean_modlai_pct_chg",
  mod_se_col = "se_modis_lai",
  mod_base = "baseNWA_modlai",
  cci_mean_col = "mean_cci_pct_chg",
  cci_se_col = "se_cci",
  cci_base = "baseNWA_cci",
  fesc_mean_col = "mean_fesc_pct_chg",
  fesc_se_col = "se_fesc",
  fesc_base = "baseNWA_fesc",
  sif_color = sif_col2,
  phif_color = phif_col,
  toc_color = toc_col,
  mod_color = mod_col,
  fesc_color = fesc_col,
  cci_color = cci_col,
  zone_label = "Northwest Amz. (ever-wet)"
) + custom_annotate2(region = "NWA", y_text_pos = -55)
sif_cci_pai_NWA

sif_cci_pai_Southern <- make_sif_cci_pai_plot(
  data = yr_geo_Southern,
  doy_col = "doymin",
  sif_mean_col = "mean_sif_par_pct_chg",
  sif_se_col = "se_sif_par",
  sif_base = "baseSouthern_sif_par",
  #phif_mean_col = "mean_phif_pct_chg",
  phif_mean_col = NA,  # turn off PhiF
  phif_se_col = "se_phif",
  phif_base = "baseCA_phif",
  tocpai_mean_col = "mean_tocpai_pct_chg",
  tocpai_se_col = "se_pai_toc",
  tocpai_base = "baseSouthern_tocpai",
  mod_mean_col = "mean_modlai_pct_chg",
  mod_se_col = "se_modis_lai",
  mod_base = "baseSouthern_modlai",
  cci_mean_col = "mean_cci_pct_chg",
  cci_se_col = "se_cci",
  cci_base = "baseSouthern_cci",
  fesc_mean_col = "mean_fesc_pct_chg",
  fesc_se_col = "se_fesc",
  fesc_base = "baseSouthern_fesc",
  sif_color = sif_col2,
  phif_color = phif_col,
  toc_color = toc_col,
  mod_color = mod_col,
  fesc_color = fesc_col,
  cci_color = cci_col,
  zone_label = "Southern Amz. (strongly seasonal)"
) + custom_annotate2(region = "Southern", y_text_pos = -55)
sif_cci_pai_Southern


# Create plot grid
sif_cci_pai_NWA <- sif_cci_pai_NWA + theme(legend.position = "none")
sif_cci_pai_CA <- sif_cci_pai_CA + theme(legend.position = "none")

sif_cci_paigrid <- (sif_cci_pai_NWA + sif_cci_pai_NOA) / (sif_cci_pai_CA + sif_cci_pai_Southern) +plot_annotation(tag_levels = 'a',
                                                                                                                  tag_prefix = '(',
                                                                                                                  tag_suffix = ')',
                                                                                                                  tag_sep = ' ')
sif_cci_paigrid

# CCI decreases during or following dry season
# Less chlorophyll relative to carotenoids --> stress acclimation
# Wet season: more chlorophyll relative to carotenoids

#save the plot
#ggsave(paste0(figdir, "/perc_chg_georeg_cci_gam.png"), sif_cci_paigrid, dpi = 300, width = 13, height = 8)
ggsave(paste0(figdir, "/perc_chg_georeg_cci_gam.tiff"), device = 'tiff', sif_cci_paigrid, dpi = 600, width = 13, height = 8, compression = 'lzw')


##
#Figure S1  SIF-related metrics as time series ------------------------------------
##
plot_time_series <- function(data, y_var, se_var, y_label, color = sif_col2, season_rects = glob_szn_rects) {
  ggplot(data = data, aes(x = doymin, y = .data[[y_var]])) +
    geom_point(stat = "identity", color = color, size = 2.3) +
    geom_errorbar(aes(
      ymin = .data[[y_var]] - .data[[se_var]],
      ymax = .data[[y_var]] + .data[[se_var]]
    ), linewidth = 0.3, colour = color, alpha = 0.4) +
    geom_line(color = color, alpha = 0.4) +
    geom_smooth(method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2,
                color = color, fill = color) +
    labs(x = "Day of Year", y = y_label) +
    theme_classic(base_family = "serif") +
    theme(axis.title.y = element_text(size = 12)) +
    
    # Seasonal shading
    geom_rect(data = season_rects,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = season_rects$fill,
              inherit.aes = FALSE,
              alpha = 0.2) +
    
    # Season labels
    geom_text(data = season_rects,
              aes(x = (xmin + xmax) / 2, y = -Inf, label = season),
              inherit.aes = FALSE,
              vjust = -0.5,
              size = 3)
}

sifsimp_ts         <- plot_time_series(gedi_yr_summ, "mean_sif743", "se_sif743",
                                   expression("SIF simple ("*mW*"·"*m^{-2}*"·"*sr^{-1}*"·"*nm^{-1}*")"))

sif_ts         <- plot_time_series(gedi_yr_summ, "mean_sif743_cor", "se_sif743_cor",
                                   expression("SIF corr ("*mW*"·"*m^{-2}*"·"*sr^{-1}*"·"*nm^{-1}*")"))

sifc_ts         <- plot_time_series(gedi_yr_summ, "mean_sifcor_csza", "se_sifcor_csza",
                                   expression("SIF corr cosSZA ("*mW*"·"*m^{-2}*"·"*sr^{-1}*"·"*nm^{-1}*")"))

sifpar_ts      <- plot_time_series(gedi_yr_summ, "mean_sif_par", "se_sif_par",
                                   expression("SIF/PAR ("*sr^{-1}*"·"*nm^{-1}*")"))

sifc_par_ts      <- plot_time_series(gedi_yr_summ, "mean_sifc_par", "se_sifc_par",
                                   expression("SIF/PAR (cos(SZA)) ("*sr^{-1}*"·"*nm^{-1}*")"))

sifapar_ts     <- plot_time_series(gedi_yr_summ, "mean_sif_apar", "se_sif_apar",
                                   expression("SIF/APAR ("*sr^{-1}*"·"*nm^{-1}*")"))

(sifsimp_ts + sif_ts + sifc_ts) / (sifpar_ts + sifc_par_ts + sifapar_ts)

nirvp_ts       <- plot_time_series(gedi_yr_summ, "mean_nirvp", "se_nirvp",
                                   expression("NIRvP ("*mW*"·"*m^{-2}*")"))

nirv_ts        <- plot_time_series(gedi_yr_summ, "mean_nirv", "se_nirv", "NIRv")

phif_ts        <- plot_time_series(gedi_yr_summ, "mean_phif", "se_phif", expression(Phi*"F; MODIS; Refl."))

phif_tropo_refl_ts    <- plot_time_series(gedi_yr_summ, "mean_phif_tropo_refl", "se_phif_tropo_refl", expression(Phi*"F; TROPO; Refl."))

phif_tropo_rad_ts   <- plot_time_series(gedi_yr_summ, "mean_phif_tropo_rad", "se_phif_tropo_rad",
                                        expression(Phi*"F; TROPO; Rad."))

sifreltrop_ts  <- plot_time_series(gedi_yr_summ, "mean_sif_rel_tropo", "se_sif_rel_tropo", "SIF/NIR; TROPO; Rad.")

sifreltropc_ts  <- plot_time_series(gedi_yr_summ, "mean_sifc_rel_tropoc", "se_sifc_rel_tropoc", "SIF/NIR cos(SZA); TROPO; Rad.")


nirvp_tropo_refl_ts <- plot_time_series(gedi_yr_summ, "mean_nirvp_tropo_refl", "se_nirvp_tropo_refl",
                                        expression("NIRvP; TROPO; Refl."))

nirv_tropo_rad_ts <- plot_time_series(gedi_yr_summ, "mean_nirv_tropo_rad", "se_nirv_tropo_rad", "NIRv; TROPO; Rad.")

nirv_tropoc_rad_ts <- plot_time_series(gedi_yr_summ, "mean_nirv_tropoc_rad", "se_nirv_tropoc_rad", "NIRv (cos(SZA)); TROPO; Rad.")

fesc_tropo_rad_ts <- plot_time_series(gedi_yr_summ, "mean_fesc_tropo_rad", "se_fesc_tropo_rad", expression(F[esc]~"; TROPO; Rad."))

# Combine
sifderiv <- (sif_ts | nirv_tropo_rad_ts) / (sifpar_ts | sifreltrop_ts) / (sifapar_ts | phif_tropo_rad_ts) / (phif_ts | phif_tropo_refl_ts)+
  plot_annotation(tag_levels = 'a',  # auto-label panels a–h
                  tag_prefix = '(',
                  tag_suffix = ')',
                  tag_sep = ' ')      # e.g. "(a)" "(b)"

sifderiv

#save plot
#ggsave(paste0(figdir, "/sif_derivs_supp.png"), sifderiv, units='in', dpi = 300, width=11, height=8)
ggsave(paste0(figdir, "/sif_derivs_supp.tiff"), sifderiv, units='in', device = 'tiff', dpi = 600, width=11, height=8, compression = 'lzw')


# Figure S5: US and TOC PAI -------------------------------
# Reshape data to long format
pai_long <- gedi_yr_georeg_summ %>%
  select(doymin, georeg_agg,
         mean_pai_us, se_pai_us,
         mean_pai_toc, se_pai_toc) %>%
  pivot_longer(cols = starts_with("mean_pai"),
               names_to = "pai_type",
               names_prefix = "mean_",
               values_to = "pai_value") %>%
  mutate(se = ifelse(pai_type == "pai_us", se_pai_us, se_pai_toc),
         pai_type = factor(pai_type, levels = c("pai_us", "pai_toc"),
                           labels = c("Understory PAI", "Canopy PAI")),
         georeg_agg = factor(georeg_agg, levels = c("NWA", "NOA", "CA", "Southern")))

georeg_labels <- c(
  "NWA" = "Northwest",
  "NOA" = "Northern",
  "CA"  = "Central",
  "Southern" = "Southern"
)

pai_can_ts <- ggplot(data = pai_long, aes(x = doymin, y = pai_value, color = pai_type, fill = pai_type)) +
  geom_point(size = 2.3, color = 'black') +
  geom_errorbar(aes(ymin = pai_value - se, ymax = pai_value + se),
                linewidth = 0.3, alpha = 0.9, color = 'black') +
  geom_line(color = 'black') +
  stat_smooth(linewidth = 0.4, alpha = 0.4) +
  labs(x = "Day of Year",
       y = expression("PAI ("*m^{2}*"/"*m^{2}*")"),
       color = NULL,
       fill = NULL) +
  scale_color_manual(values = c("Understory PAI" = us_col,
                                "Canopy PAI" = toc_col)) +
  scale_fill_manual(values = c("Understory PAI" = us_col,
                               "Canopy PAI" = toc_col)) +
  facet_wrap(~georeg_agg, labeller = as_labeller(georeg_labels)) +
  theme_classic(base_family = "serif") +
  theme(
    axis.title.y = element_text(size = 14),
    legend.position = "right",
    legend.text = element_text(size = 10)
  ) +
  ylim(1, 3) +
  custom_annotate(y_text_pos = 1)

pai_can_ts

#ggsave(paste0(figdir, "/georeg_toc_vs_us_figS5.png"), pai_can_ts, dpi = 300, width = 10, height = 6)
ggsave(paste0(figdir, "/georeg_toc_vs_us_figS5.tiff"), device = 'tiff', units = 'in', pai_can_ts, dpi = 600, width = 10, height = 6, compression = 'lzw')


# SIF/PAR + Structure plots (Not in manuscript! Just for visualization)--------------------------------------------

make_sif_pai_plot <- function(data, doy_col, sif_mean_col, sif_se_col, sif_base, tocpai_mean_col, tocpai_se_col, tocpai_base, fesc_mean_col, fesc_se_col, fesc_base, mod_mean_col, mod_se_col, mod_base, sif_color, toc_color, fesc_color, mod_color, zone_label, ylim_range = c(-30, 63), line_alpha = 0.4, loess_alpha = 0.2) {
  ggplot(data, aes(x = as.numeric(as.character(!!sym(doy_col))))) +
    
    # SIF Layer
    geom_point(aes(y = !!sym(sif_mean_col), color = "SIF/PAR"), size = 2.3, alpha = line_alpha) +
    geom_errorbar(aes(
      ymin = !!sym(sif_mean_col) - 100 * !!sym(sif_se_col) / !!sym(sif_base), 
      ymax = !!sym(sif_mean_col) + 100 * !!sym(sif_se_col) / !!sym(sif_base), 
      color = "SIF/PAR"), 
      linewidth = 0.3, alpha = line_alpha) +
    geom_line(aes(y = !!sym(sif_mean_col), color = "SIF/PAR"), alpha = line_alpha) +
    #geom_smooth(aes(y = !!sym(sif_mean_col)), method = "loess", alpha = loess_alpha, color = sif_color, fill = sif_color) +
    geom_smooth(aes(y = !!sym(sif_mean_col)), method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2, color = sif_color, fill = sif_color)+
    
    #TOC PAI Layer
    geom_point(aes(y = !!sym(tocpai_mean_col), color = "TOC PAI"), size = 2.3, alpha = line_alpha) +
    geom_errorbar(aes(
      ymin = !!sym(tocpai_mean_col) - 100 * !!sym(tocpai_se_col) / !!sym(tocpai_base), 
      ymax = !!sym(tocpai_mean_col) + 100 * !!sym(tocpai_se_col) / !!sym(tocpai_base), 
      color = "TOC PAI"), 
      linewidth = 0.3, alpha = line_alpha) +
    geom_line(aes(y = !!sym(tocpai_mean_col), color = "TOC PAI"), alpha = line_alpha) +
    #geom_smooth(aes(y = !!sym(tocpai_mean_col)), method = "loess", alpha = loess_alpha, color = toc_color, fill = toc_color) +
    geom_smooth(aes(y = !!sym(tocpai_mean_col)), method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2, color = toc_color, fill = toc_color)+
    
    # MOD LAI Layer
    geom_point(aes(y = !!sym(mod_mean_col), color = "MODIS LAI"), size = 2.3, alpha = line_alpha) +
    geom_errorbar(aes(
      ymin = !!sym(mod_mean_col) - 100 * !!sym(mod_se_col) / !!sym(mod_base), 
      ymax = !!sym(mod_mean_col) + 100 * !!sym(mod_se_col) / !!sym(mod_base), 
      color = "MODIS LAI"), 
      linewidth = 0.3, alpha = line_alpha) +
    geom_line(aes(y = !!sym(mod_mean_col), color = "MODIS LAI"), alpha = line_alpha) +
    #geom_smooth(aes(y = !!sym(mod_mean_col)), method = "loess", alpha = loess_alpha, color = mod_color, fill = mod_color) +
    geom_smooth(aes(y = !!sym(mod_mean_col)), method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2, color = mod_color, fill = mod_color)+
    
    # fesc Layer
    geom_point(aes(y = !!sym(fesc_mean_col), color = "fesc"), size = 2.3, alpha = line_alpha) +
    geom_errorbar(aes(
      ymin = !!sym(fesc_mean_col) - 100 * !!sym(fesc_se_col) / !!sym(fesc_base),
      ymax = !!sym(fesc_mean_col) + 100 * !!sym(fesc_se_col) / !!sym(fesc_base),
      color = "fesc"),
      linewidth = 0.3, alpha = line_alpha) +
    geom_line(aes(y = !!sym(fesc_mean_col), color = "fesc"), alpha = line_alpha) +
    #geom_smooth(aes(y = !!sym(fesc_mean_col)), method = "loess", alpha =loess_alpha, color = fesc_color, fill = fesc_color) +
    geom_smooth(aes(y = !!sym(fesc_mean_col)), method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2, color = fesc_color, fill = fesc_color)+
    
    # Additional layers and aesthetics
    ylim(ylim_range) +
    labs(y = "% Change relative to dry period onset", color = "RS variable") +
    theme_minimal() +
    labs(x = "Day of Year") +
    annotate(geom = "text", x = 10, y = ylim_range[2] - 10, hjust = 0, label = zone_label, color = "black", size = rel(7), family = "serif") +
    # Manual color scale
    scale_color_manual(
      values = setNames(c(sif_color, toc_color, fesc_color, mod_color), c("SIF/PAR", "TOC PAI", "fesc", "MODIS LAI")),
      labels = c(
        "SIF/PAR" = "SIF/PAR",
        "TOC PAI" = expression(paste("PAI"[TOC])),
        "fesc" = expression(paste("f"[esc])),
        "CCI" = "CCI",
        "MODIS LAI" = "MODIS LAI"
      )
    )
}


# CCI vs SIF/PAR plots
sif_pai_CA <- make_sif_pai_plot(
  data = yr_geo_CA,
  doy_col = "doymin",
  sif_mean_col = "mean_sif_par_pct_chg",
  sif_se_col = "se_sif_par",
  sif_base = "baseCA_sif_par",
  tocpai_mean_col = "mean_tocpai_pct_chg",
  tocpai_se_col = "se_pai_toc",
  tocpai_base = "baseCA_tocpai",
  mod_mean_col = "mean_modlai_pct_chg",
  mod_se_col = "se_modis_lai",
  mod_base = "baseCA_modlai",
  fesc_mean_col = "mean_fesc_pct_chg",
  fesc_se_col = "se_fesc",
  fesc_base = "baseCA_fesc",
  sif_color = sif_col2,
  toc_color = toc_col,
  mod_color = mod_col,
  fesc_color = fesc_col,
  zone_label = "Cent. Amz. (seasonal)"
) + custom_annotate2(region = "CA")
sif_pai_CA

sif_pai_NOA <- make_sif_pai_plot(
  data = yr_geo_NOA,
  doy_col = "doymin",
  sif_mean_col = "mean_sif_par_pct_chg",
  sif_se_col = "se_sif_par",
  sif_base = "baseNOA_sif_par",
  tocpai_mean_col = "mean_tocpai_pct_chg",
  tocpai_se_col = "se_pai_toc",
  tocpai_base = "baseNOA_tocpai",
  mod_mean_col = "mean_modlai_pct_chg",
  mod_se_col = "se_modis_lai",
  mod_base = "baseNOA_modlai",
  fesc_mean_col = "mean_fesc_pct_chg",
  fesc_se_col = "se_fesc",
  fesc_base = "baseNOA_fesc",
  sif_color = sif_col2,
  toc_color = toc_col,
  mod_color = mod_col,
  fesc_color = fesc_col,
  zone_label = "N. Amz. (bimodal)"
) + custom_annotate2(region = "NOA")
sif_pai_NOA

sif_pai_NWA <- make_sif_pai_plot(
  data = yr_geo_NWA,
  doy_col = "doymin",
  sif_mean_col = "mean_sif_par_pct_chg",
  sif_se_col = "se_sif_par",
  sif_base = "baseNWA_sif_par",
  tocpai_mean_col = "mean_tocpai_pct_chg",
  tocpai_se_col = "se_pai_toc",
  tocpai_base = "baseNWA_tocpai",
  mod_mean_col = "mean_modlai_pct_chg",
  mod_se_col = "se_modis_lai",
  mod_base = "baseNWA_modlai",
  fesc_mean_col = "mean_fesc_pct_chg",
  fesc_se_col = "se_fesc",
  fesc_base = "baseNWA_fesc",
  sif_color = sif_col2,
  toc_color = toc_col,
  mod_color = mod_col,
  fesc_color = fesc_col,
  zone_label = "NW. Amz. (ever-wet)"
) + custom_annotate2(region = "NWA")
sif_pai_NWA

sif_pai_Southern <- make_sif_pai_plot(
  data = yr_geo_Southern,
  doy_col = "doymin",
  sif_mean_col = "mean_sif_par_pct_chg",
  sif_se_col = "se_sif_par",
  sif_base = "baseSouthern_sif_par",
  tocpai_mean_col = "mean_tocpai_pct_chg",
  tocpai_se_col = "se_pai_toc",
  tocpai_base = "baseSouthern_tocpai",
  mod_mean_col = "mean_modlai_pct_chg",
  mod_se_col = "se_modis_lai",
  mod_base = "baseSouthern_modlai",
  fesc_mean_col = "mean_fesc_pct_chg",
  fesc_se_col = "se_fesc",
  fesc_base = "baseSouthern_fesc",
  sif_color = sif_col2,
  toc_color = toc_col,
  mod_color = mod_col,
  fesc_color = fesc_col,
  zone_label = "S. Amz. (strongly seasonal)"
) + custom_annotate2(region = "Southern")
sif_pai_Southern


# Create plot grid
sif_pai_NWA <- sif_pai_NWA + theme(legend.position = "none")
sif_pai_CA <- sif_pai_CA + theme(legend.position = "none")

sif_paigrid <- (sif_pai_NWA + sif_pai_NOA) / (sif_pai_CA + sif_pai_Southern)
sif_paigrid

#save the plot
ggsave(paste0(figdir, "/perc_chg_georeg_nocci_gam.png"), sif_paigrid, dpi = 300, width = 13, height = 8)




#Correlogram - Not in Manuscript! Just for visualization ----------------------------------------
# Select just the SIF-derived numeric variables
sif_vars <- gedi_yr_summ %>%
  dplyr::select(
    mean_sif743_cor,
    mean_sif_par,
    mean_sif_apar,
    mean_phif,
    mean_phif_tropo_refl,
    mean_phif_tropo_rad,
    mean_nirvp,
    mean_nirvp_tropo_refl,
    mean_nirvp_tropo_rad,
    mean_sif_rel_tropo
  )

var_labels <- c(
  expression("SIF (Rad.)"),
  expression("SIF/PAR (Rad.)"),
  expression("SIF/APAR (Rad.)"),
  expression(Phi*"F (MODIS, Refl.)"),
  expression(Phi*"F (TROPO, Refl.)"),
  expression(Phi*"F (TROPO, Rad.)"),
  expression("NIRvP (MODIS, Refl.)"),
  expression("NIRvP (TROPO, Refl.)"),
  expression("NIRvP (TROPO, Rad.)"),
  expression("SIF/NIR (TROPO, Rad.)")
)


# Compute correlation matrix
mask <- complete.cases(sif_vars)
cor_mat <- cor(sif_vars[mask, ], method = "pearson")
diag(cor_mat) 

library(ggcorrplot)
sif_corrplot <- ggcorrplot(
  cor_mat,
  hc.order = FALSE,
  type = "lower",
  lab = TRUE,
  lab_size = 3,
  show.diag = TRUE,
  colors = c("blue", "white", "red"),
  outline.color = 'gray50'
)+
  scale_x_discrete(labels = var_labels) +
  scale_y_discrete(labels = var_labels) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
sif_corrplot

ggsave(paste0(figdir, "/sif_derivs_corrplot_supp.png"), sif_corrplot, units='in', dpi = 300, width=8, height=8)


##
##


##





##OLD Figure S****:  Annual trends across whole 'global' Amazon ------------------------------------
##

# Plot for all LAI-type data

# Prepare the data in long format with variable names and corresponding standard errors
pai_summ_long <- gedi_summ %>%
  pivot_longer(cols = c(mean_pai_toc, mean_pai, mean_modis_lai),
               names_to = "variable",
               values_to = "value") %>%
  mutate(se = case_when(
    variable == "mean_pai_toc" ~ se_pai_toc,
    variable == "mean_pai" ~ se_pai,
    variable == "mean_modis_lai" ~ se_modis_lai
  ),
  variable_label = case_when(
    variable == "mean_pai_toc" ~ "Canopy PAI",
    variable == "mean_pai" ~ "PAI",
    variable == "mean_modis_lai" ~ "MODIS LAI"
  ))


fescnirv_summ_long <- gedi_summ %>%
  pivot_longer(cols = c(mean_fesc, mean_nirv),
               names_to = "variable",
               values_to = "value") %>%
  mutate(se = case_when(
    variable == "mean_fesc" ~ se_fesc,
    variable == "mean_nirv" ~ se_nirv
  ),
  variable_label = case_when(
    variable == "mean_fesc" ~ "Fesc",
    variable == "mean_nirv" ~ "NIRv"
  ))

# Find the max point of each line for annotation placement
# Pre-defined annotation positions
annotation_positions <- data.frame(
  x = c(50, 50, 50),      # Example x positions (center of DOY range)
  y = c(5.9, 3.5, 2.1),      # Positions above the max values of each line
  label = c("MODIS LAI", "Total PAI", "Canopy PAI")
)

annotation_positions2 <- data.frame(
  x = c(0, 0),      # Example x positions (center of DOY range)
  y = c(0.34, 0.25),      # Positions above the max values of each line
  label = c("Fesc", "NIRv")
)

#Define helper function to calculate absolute symmetric relative difference (%)
get_rel_ampl <- function(data, var) {
  vals <- data[[var]]
  vals <- vals[!is.na(vals)]
  
  if (length(vals) == 0) return(NA)
  
  min_val <- min(vals)
  max_val <- max(vals)
  
  # Avoid division by zero or exploding % from near-zero
  if (abs(min_val) + abs(max_val) == 0) return(0)
  
  rel_ampl <- round(abs(max_val - min_val) / mean(abs(c(min_val, max_val))) * 100, 1)
  return(rel_ampl)
}

chngvars <- c("mean_sif_par", "mean_sif743_cor", "mean_pai_toc", "mean_pai", "mean_modis_lai", "mean_phif", "mean_nirv", "mean_fesc", "mean_fpar", "mean_cci", "mean_pri_nar")

# Apply both functions in one go
rel_list <- lapply(chngvars, function(v) {
  list(
    rel_ampl = get_rel_ampl(gedi_yr_summ, v)
  )
})
names(rel_list) <- chngvars
rel_df <- do.call(rbind, lapply(rel_list, as.data.frame))
rel_df

# Create the combined plot with annotations
plot_comb_pai <- ggplot(pai_summ_long, 
                        aes(x = doymin,
                            y = value,
                            group = interaction(year, variable),
                            color = year)) +
  geom_line(linewidth = 0.4, alpha = 0.9) +
  geom_point() +
  geom_errorbar(aes(ymin = value - se, ymax = value + se), 
                linewidth = 0.3, alpha = 0.9) +
  labs(x = "Day of Year", 
       y = expression("LAI Product (" * m^2/m^2 * ")"), 
       color = "Year") +
  theme_classic() +
  scale_color_manual(values = color_vals) +
  scale_y_continuous(limits = c(3.7, 6.05), labels = scales::label_number(accuracy = 0.01))+
  annotate("text", x = 0, y = 6, 
           label = annotation_positions$label[1], hjust = 0, color = "black", size = 4) +
  annotate("text", x = 0, y = 4.45, 
           label = annotation_positions$label[2], hjust = 0, color = "black", size = 4) +
  annotate("text", x = 70,
           y = 4.45, label = paste0("ΔRel: ", rel_df['mean_pai', 'rel_ampl'], "%"), hjust = 0, size = 4)+
  annotate("text", x = 70,
           y = 6, label = paste0("ΔRel: ", rel_df['mean_modis_lai', 'rel_ampl'], "%"), hjust = 0, size = 4)+
  geom_rect(
    data = glob_szn_rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  scale_fill_identity()

plot_comb_pai

plot_tocpai <- create_yr_plot(gedi_summ, 
                              x_var = "doymin", 
                              y_var = "mean_pai_toc", 
                              y_label = "TOC PAI", 
                              se_var = "se_pai_toc", 
                              group_var = "year", 
                              color_var = "year", 
                              color_vals = color_vals,
                              y_limits = c(1, 1.8))+
  geom_smooth(method = 'gam', se = FALSE, lwd = 0.6)+
  labs(x = "Day of Year", 
       y = expression("Canopy PAI (" * m^2/m^2 * ")"), 
       color = "Year") +
  annotate("text", x = min(gedi_summ$doymin),
           y = 1*1.02, label = paste0("ΔRel: ", rel_df['mean_pai_toc', 'rel_ampl'], "%"), hjust = 0, size = 4)+
  geom_rect(
    data = glob_szn_rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  scale_fill_identity()

plot_tocpai

# GeomLine# Create a combined plot
plot_comb_fescnirv <- ggplot(fescnirv_summ_long,
                             aes(x = doymin,
                                 y = value,
                                 group = interaction(year, variable),
                                 color = year)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
                linewidth = 0.3, alpha = 0.9) +
  labs(x = "Day of Year",
       y = expression("Fesc and NIRv"),
       color = "Year") +
  theme_classic() +
  scale_color_manual(values = color_vals) +
  scale_y_continuous(limits = c(0.23, 0.37), labels = scales::label_number(accuracy = 0.01))+
  annotate("text", x = 0, hjust = 0, y = annotation_positions2$y[1],
           label = annotation_positions2$label[1], color = "black", size = 4) +
  annotate("text", x = 0, hjust = 0, y = annotation_positions2$y[2],
           label = annotation_positions2$label[2], color = "black", size = 4) +
  annotate("text", x = 45,
           y = 0.25, label = paste0("ΔRel: ", rel_df['mean_nirv', 'rel_ampl'], "%"), hjust = 0, size = 4)+
  annotate("text", x = 45,
           y = 0.34, label = paste0("ΔRel: ", rel_df['mean_fesc', 'rel_ampl'], "%"), hjust = 0, size = 4)+
  geom_rect(
    data = glob_szn_rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  scale_fill_identity()

plot_comb_fescnirv

# SIF/PAR plot
plot_sif_par <- create_yr_plot(gedi_summ, 
                               x_var = "doymin", 
                               y_var = "mean_sif_par", 
                               y_label = expression("SIF/PAR ("*sr^{-1}*"·"*nm^{-1}*")"), 
                               se_var = "se_sif_par", 
                               group_var = "year", 
                               color_var = "year", 
                               color_vals = color_vals,
                               y_limits = c(0.0000018, 0.0000039))+
  geom_smooth(method = 'gam', se = FALSE, lwd = 0.6)+
  annotate("text", x = min(gedi_summ$doymin, na.rm = TRUE),
           y = 0.0000018*1.03, label = paste0("ΔRel: ", rel_df['mean_sif_par', 'rel_ampl'], "%"), hjust = 0, size = 4)+
  geom_rect(
    data = glob_szn_rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  scale_fill_identity()

plot_sif_par

# SIF plot
plot_sif <- create_yr_plot(gedi_summ, 
                           x_var = "doymin", 
                           y_var = "mean_sif743_cor", 
                           y_label = expression("SIF ("*mW*"·"*m^{-2}*"·"*sr^{-1}*"·"*nm^{-1}*")"), 
                           se_var = "se_sif743_cor", 
                           group_var = "year", 
                           color_var = "year", 
                           color_vals = color_vals)+
  geom_smooth(method = 'gam', se = FALSE, lwd = 0.6)+
  annotate("text", x = min(gedi_summ$doymin, na.rm = TRUE),
           y = 0.39*1.02, label = paste0("ΔRel: ", rel_df['mean_sif743_cor', 'rel_ampl'], "%"), hjust = 0, size = 4)+
  geom_rect(
    data = glob_szn_rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  scale_fill_identity()

plot_sif

# PhiF plot
plot_phif <- create_yr_plot(gedi_summ, 
                            x_var = "doymin", 
                            y_var = "mean_phif", 
                            y_label = "Phi F", 
                            se_var = "se_phif", 
                            group_var = "year", 
                            color_var = "year", 
                            color_vals = color_vals)+
  annotate("text", x = min(gedi_summ$doymin, na.rm = TRUE),
           y = 0.000008*1.01, label = paste0("ΔRel: ", rel_df['mean_nirv', 'rel_ampl'], "%"), hjust = 0, size = 4)+
  geom_rect(
    data = glob_szn_rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  scale_fill_identity()

plot_phif

# PRI narrowband
plot_prinar <- create_yr_plot(gedi_summ, 
                              x_var = "doymin", 
                              y_var = "mean_pri_nar", 
                              y_label = "PRI (narrow)", 
                              se_var = "se_pri_nar", 
                              group_var = "year", 
                              color_var = "year", 
                              color_vals = color_vals)+
  annotate("text", x = min(gedi_summ$doymin, na.rm = TRUE),
           y = -0.0893, label = paste0("ΔRel: ", rel_df['mean_nirv', 'rel_ampl'], "%"), hjust = 0, size = 4)+
  geom_rect(
    data = glob_szn_rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  scale_fill_identity()

plot_prinar

# CCI plot
plot_cci <- create_yr_plot(gedi_summ, 
                           x_var = "doymin", 
                           y_var = "mean_cci", 
                           y_label = "CCI", 
                           se_var = "se_cci", 
                           group_var = "year", 
                           color_var = "year", 
                           color_vals = color_vals)+
  annotate("text", x = min(gedi_summ$doymin, na.rm = TRUE),
           y = min(gedi_summ$mean_cci)*1.03, label = paste0("ΔRel: ", rel_df['mean_cci', 'rel_ampl'], "%"), hjust = 0, size = 4)+
  geom_rect(
    data = glob_szn_rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  scale_fill_identity()

plot_cci

# Combine the plots into a grid with a shared legend below
yearly_trend_plot <- (
  (plot_sif + theme(legend.position = "none")) |
    (plot_comb_fescnirv + theme(legend.position = "none"))
) /
  (
    (plot_sif_par + theme(legend.position = "none")) |
      (plot_comb_pai + theme(legend.position = "none"))
  ) /
  (
    (plot_phif + theme(legend.position = "none")) |
      (plot_tocpai + theme(legend.position = "none"))
  )/
  (
    (plot_cci + theme(legend.position = "none")) |
      (plot_prinar + theme(legend.position = "bottom"))
  )+
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

yearly_trend_plot

# Save plot
ggsave(paste0(figdir, "/multiyear_abs_glob_trends.png"), yearly_trend_plot, dpi = 300, width = 11, height = 8)


## END SCRIPT ##
