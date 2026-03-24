
#Visualize PACE data
#This code ultimately produces Fig 5


library(tidyverse)
library(viridis)
library(patchwork)

rm(list=ls())

wd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"
final_dir <- paste0(wd, "/pace_vi_data")
figdir <- paste0(wd, "/figures")
complete_dir <- paste0(wd, "/complete_data")

seasonality <- read.csv(paste0(wd, "/complete_data/dynamic_precip_seasonality.csv"))
glob_season <- read.csv(paste0(wd, "/complete_data/global_precip_seasonality.csv"))

df_summ <- read.csv(paste0(complete_dir, ("/pace_summ.csv")))
df_georeg_summ <- read.csv(paste0(complete_dir, ("/pace_georeg_summ.csv")))
df_yr_summ <- read.csv(paste0(complete_dir, ("/pace_yr_summ.csv")))
df_yr_georeg_summ <- read.csv(paste0(complete_dir, ("/pace_yr_georeg_summ.csv")))

gedi_yr_georeg_summ <- read.csv(paste0(complete_dir, "/gedi_yr_georeg_naincl_summ.csv"))
gedi_yr_georeg_summ <- gedi_yr_georeg_summ %>% left_join(seasonality, by = "georeg_agg")

seasonality <- seasonality %>% rename(georeg = georeg_agg)

#Join the seasonality data
df_yr_georeg_summ <- df_yr_georeg_summ %>% left_join(seasonality, by = join_by("georeg" == "georeg"))
df_yr_summ <- df_yr_summ %>% left_join(glob_season, by = join_by("region" == "region"))

df_yr_georeg_summ$georeg <- factor(df_yr_georeg_summ$georeg,
                                      levels = c("NWA", "NOA", "CA", "Southern"))
seasonality$georeg <- factor(seasonality$georeg,
                                 levels = levels(df_yr_georeg_summ$georeg))

gedi_yr_georeg_summ <- gedi_yr_georeg_summ %>% rename(georeg = georeg_agg)

df_yr_georeg_summ <- df_yr_georeg_summ %>%
  mutate(
    sub_szn = case_when(
      doy >= wet_start &
        doy <= wet_end_window ~ "peakwet",
      doy >= dry_start & 
        doy <= dry_end_window ~ "dry",
      doy >= earlywet_start & 
        doy <= earlywet_end_window ~ "earlywet",
      TRUE ~ 'other'
    ) 
  ) %>% 
  select(georeg, doy, sub_szn, everything())

#create 'seasonal' groupings based on seasonality data:
df_yr_summ <- df_yr_summ %>%
  mutate(
    sub_szn = case_when(
      doy >= globwet_start &
        doy <= globwet_end_window ~ "peakwet",
      doy >= globdry_start & 
        doy <= globdry_end_window ~ "dry",
      doy >= globearlywet_start & 
        doy <= globearlywet_end_window ~ "earlywet",
      TRUE ~ 'other'
    )
  )

#For adding SIF/PAR to plots
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
  select(georeg, doymin, sub_szn, everything())

gedi_yr_georeg_summ$georeg_agg <- factor(gedi_yr_georeg_summ$georeg,
                                      levels = c("NWA", "NOA", "CA", "Southern"))


chlcar_col <- "#66A61E"#"#BC5090"
car_col <- "#BC5090" #"#D95F02" "#EE3377"
pri_col <- "#7570B3"
cire_col <- "#D95F02"#"#66A61E"
cci_col <- "#33BBEE" #"#1C9099"
nirv_col <- "#44AA99"
sif_col <- "#E69F00"#"#d6b71d"
phif_col <-"#B07D1A"


#Color palette for peak wet, dry, and early wet
drycol <- "goldenrod1"
earlywetcol <- "purple"
peakwetcol <- "cadetblue"

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


#Function to create these plots
create_yr_plot <- function(data, x_var, y_var, y_label, se_var, color_vals, y_limits = NULL, facet_var = NULL) {
  # Base plot setup
  plot <- ggplot(data, aes(x = .data[[x_var]],
                           y = .data[[y_var]])) +
    geom_line(alpha = 0.5, linewidth = 0.6, color = color_vals) +
    #geom_point() +
    #geom_errorbar(aes(ymin = .data[[y_var]] - .data[[se_var]], 
    #                  ymax = .data[[y_var]] + .data[[se_var]]), 
    #              linewidth = 0.3, alpha = 0.9) +
    geom_smooth(method = "gam", se = TRUE, alpha = 0.2, linewidth = 1, color = color_vals, fill = color_vals) +
    
    
    labs(x = "Day of Year", y = y_label) +
    theme_classic() +
    theme(
      axis.title = element_text(face = "plain")  # bold both x and y axis labels
    )+
    scale_color_manual(values = color_vals) +
    
    # Add y-limits if given the chance
    (if (!is.null(y_limits)) scale_y_continuous(limits = y_limits) else NULL)
  
  # Add facet_wrap if facet_var is provided
  if (!is.null(facet_var)) {
    plot <- plot + facet_wrap(vars(!!sym(facet_var)), nrow = 1)
  }
  
  return(plot)
}

# this dataset has a different factor order than the PACE dataset. We'll reorder.
gedi_yr_georeg_summ <- gedi_yr_georeg_summ %>%
  mutate(georeg = factor(georeg, levels = c("NWA", "NOA", "CA", "Southern")))

georeg_labels <- c(
  "NWA" = "Northwest",
  "NOA" = "Northern",
  "CA"  = "Central",
  "Southern" = "Southern"
)

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

chngvars <- c("mean_pri", "mean_cci", "mean_car", "mean_cire", "mean_ndvi", "mean_chlcar")

# Grouped computation
rel_pace_df_grouped <- df_yr_georeg_summ %>%
  group_by(georeg) %>%
  group_map(~ {
    tibble(
      georeg = .y$georeg,
      variable = chngvars,
      rel_ampl = map_dbl(chngvars, function(v) get_rel_ampl(.x, v))
    )
  }) %>%
  bind_rows()

rel_pace_df_grouped$georeg <- factor(rel_pace_df_grouped$georeg,
                                         levels = c("NWA", "NOA", "CA", "Southern"))

# Grouped computation for SIF and MODIS CCI
sifvars <- c("mean_sif_par", "mean_sif_parm", "mean_modcci", "mean_pai", "mean_pai_toc", "mean_modis_lai", "mean_fesc", "mean_fesc_tropo_rad", "mean_phifm_tropo_refl")
rel_sif_df_grouped <- gedi_yr_georeg_summ %>%
  rename(mean_modcci = mean_cci) %>% 
  group_by(georeg) %>%
  group_map(~ {
    tibble(
      georeg = .y$georeg,
      variable = sifvars,
      rel_ampl = map_dbl(sifvars, function(v) get_rel_ampl(.x, v))
    )
  }) %>%
  bind_rows()

rel_sif_df_grouped <- rel_sif_df_grouped %>%
  mutate(georeg = factor(georeg, levels = c("NWA", "NOA", "CA", "Southern")))

#Summarize all
all_rel_df <- bind_rows(rel_pace_df_grouped, rel_sif_df_grouped)

rel_df_summary <- all_rel_df %>%
  group_by(variable) %>%
  summarise(mean_pct_chg = mean(rel_ampl, na.rm = TRUE)) %>%
  ungroup()



pacemin_doy_by_var <- df_yr_georeg_summ %>%
  group_by(georeg) %>%
  summarise(
    across(
      all_of(chngvars),
      ~ doy[which.min(.)],
      .names = "doymin_{.col}"
    ),
    .groups = "drop"
  )

sifmin_doy_by_var <- gedi_yr_georeg_summ %>%
  rename(mean_modcci = mean_cci) %>% 
  group_by(georeg) %>%
  summarise(
    across(
      all_of(sifvars),
      ~ doymin[which.min(.)],
      .names = "doymin_{.col}"
    ),
    .groups = "drop"
  )

all_min_df <- full_join(pacemin_doy_by_var, sifmin_doy_by_var)

all_min_avg <- all_min_df %>% 
bind_rows(
  summarise(., across(where(is.numeric), mean, na.rm = TRUE), georeg = "mean") %>%
    select(georeg, everything())
)




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

plot_pri_geo <- create_yr_plot(df_yr_georeg_summ, 
                                   x_var = "doy", 
                                   y_var = "mean_pri", 
                                   y_label = "PRI", 
                                   se_var = "se_pri", 
                                   color_vals = pri_col, 
                                   facet_var = "georeg") + 
  custom_annotate(-0.055)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

# plot_pri_geo <- add_rel_ampl_annotation(plot_pri_geo, rel_pace_df_grouped, "mean_pri")
plot_pri_geo

plot_cci_geo <- create_yr_plot(df_yr_georeg_summ, 
                               x_var = "doy", 
                               y_var = "mean_cci", 
                               y_label = "CCI", 
                               se_var = "se_cci", 
                               color_vals = cci_col, 
                               facet_var = "georeg") + 
  custom_annotate(0.083)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

plot_cci_geo <- add_rel_ampl_annotation(plot_cci_geo, rel_pace_df_grouped, "mean_cci")
plot_cci_geo

plot_ndvi_geo <- create_yr_plot(df_yr_georeg_summ, 
                                  x_var = "doy", 
                                  y_var = "mean_ndvi", 
                                  y_label = "NDVI", 
                                  se_var = "se_ndvi", 
                                  color_vals = cire_col, 
                                  facet_var = "georeg") + 
  custom_annotate(0.62)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

plot_ndvi_geo <- add_rel_ampl_annotation(plot_ndvi_geo, rel_pace_df_grouped, "mean_ndvi")
plot_ndvi_geo

plot_cire_geo <- create_yr_plot(df_yr_georeg_summ, 
                               x_var = "doy", 
                               y_var = "mean_cire", 
                               y_label = expression("CI"[re]), 
                               se_var = "se_cire", 
                               color_vals = cire_col, 
                               facet_var = "georeg") + 
  custom_annotate(1.6)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

plot_cire_geo <- add_rel_ampl_annotation(plot_cire_geo, rel_pace_df_grouped, "mean_cire")
plot_cire_geo

plot_car_geo <- create_yr_plot(df_yr_georeg_summ, 
                                  x_var = "doy", 
                                  y_var = "mean_car", 
                                  y_label = "Car", 
                                  se_var = "se_car", 
                                  color_vals = car_col, 
                                  facet_var = "georeg") + 
  custom_annotate(0.8)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

plot_car_geo <- add_rel_ampl_annotation(plot_car_geo, rel_pace_df_grouped, "mean_car")
plot_car_geo

plot_chlcar_geo <- create_yr_plot(df_yr_georeg_summ, 
                               x_var = "doy", 
                               y_var = "mean_chlcar", 
                               y_label = "Chl:Car", 
                               se_var = "se_chlcar", 
                               color_vals = chlcar_col, 
                               facet_var = "georeg") + 
  custom_annotate(0.2)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

plot_chlcar_geo <- add_rel_ampl_annotation(plot_chlcar_geo, rel_pace_df_grouped, "mean_chlcar")
plot_chlcar_geo

# SIF/PAR plot
plot_sifpar_geo <- create_yr_plot(gedi_yr_georeg_summ, 
                                  x_var = "doymin", 
                                  y_var = "mean_sif_par", 
                                  y_label = expression("SIF/PAR ("*sr^{-1}*"·"*nm^{-1}*")"), 
                                  se_var = "se_sif_par", 
                                  color_vals = sif_col, 
                                  facet_var = "georeg") + 
  custom_annotate(0.0000017)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

plot_sifpar_geo <- add_rel_ampl_annotation(plot_sifpar_geo, rel_sif_df_grouped, "mean_sif_par")
plot_sifpar_geo


plot_sifparm_geo <- create_yr_plot(gedi_yr_georeg_summ, 
                                  x_var = "doymin", 
                                  y_var = "mean_sif_parm", 
                                  y_label = expression("SIF/PAR MOD ("*sr^{-1}*"·"*nm^{-1}*")"), 
                                  se_var = "se_sif_parm", 
                                  color_vals = sif_col, 
                                  facet_var = "georeg") + 
  custom_annotate(4.6e-07)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

plot_sifparm_geo <- add_rel_ampl_annotation(plot_sifparm_geo, rel_sif_df_grouped, "mean_sif_parm")
plot_sifparm_geo

plot_phifmtroporefl_geo <- create_yr_plot(gedi_yr_georeg_summ, 
                                   x_var = "doymin", 
                                   y_var = "mean_phifm_tropo_refl", 
                                   y_label = expression(Phi*"F;" ~TROPO[Refl]), 
                                   se_var = "se_phifm_tropo_rad", 
                                   color_vals = phif_col, 
                                   facet_var = "georeg") + 
  custom_annotate(3e-06)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

plot_phifmtroporefl_geo <- add_rel_ampl_annotation(plot_phifmtroporefl_geo, rel_sif_df_grouped, "mean_phifm_tropo_refl")
plot_phifmtroporefl_geo

plot_modiscci_geo <- create_yr_plot(gedi_yr_georeg_summ, 
                                  x_var = "doymin", 
                                  y_var = "mean_cci", 
                                  y_label = "MODIS CCI", 
                                  se_var = "se_cci", 
                                  color_vals = cci_col, 
                                  facet_var = "georeg") + 
  custom_annotate(0.13)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

plot_modiscci_geo <- add_rel_ampl_annotation(plot_modiscci_geo, rel_sif_df_grouped, "mean_cci")
plot_modiscci_geo


phifplot <- plot_phifmtroporefl_geo + theme(axis.title.x = element_blank())
cireplot <- plot_cire_geo + theme(axis.title.x = element_blank(),
                                  strip.text = element_blank())
cciplot <- plot_cci_geo + theme(axis.title.x = element_blank(),
                                strip.text = element_blank())
modcciplot <- plot_modiscci_geo + theme(axis.title.x = element_blank(),
                                  strip.text = element_blank())
# ndviplot <- plot_ndvi_geo + theme(axis.title.x = element_blank(),
#                                       strip.text = element_blank())
carplot <- plot_car_geo + theme(axis.title.x = element_blank(),
                                strip.text = element_blank())
chlcarplot <- plot_chlcar_geo + theme(axis.title.x = element_blank(),
                                      strip.text = element_blank())
priplot <- plot_pri_geo + theme(strip.text = element_blank())
sifparplot <- plot_sifpar_geo + theme(strip.text = element_blank())

georeg_plot <- phifplot / cireplot / carplot / cciplot / chlcarplot / priplot +
  plot_layout(guides = "collect")+
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')',
                  tag_sep = ' ')
georeg_plot

#ggsave(paste0(figdir, "/PACE_georeg_trends.png"), georeg_plot, dpi = 300, width = 11, height = 8)
ggsave(paste0(figdir, "/PACE_georeg_trends_mar26.tiff"), georeg_plot, device = 'tiff', units = 'in', dpi = 600, width = 11, height = 8, compression = 'lzw')

