
#Visualize PACE data


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


chlcar_col <- "#BC5090"
car_col <- "#D95F02"
pri_col <- "#7570B3"
cire_col <- "#66A61E"
cci_col <- "#1C9099"
sif_col <- "#d6b71d"
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
    #geom_line() +
    #geom_point() +
    #geom_errorbar(aes(ymin = .data[[y_var]] - .data[[se_var]], 
    #                  ymax = .data[[y_var]] + .data[[se_var]]), 
    #              linewidth = 0.3, alpha = 0.9) +
    geom_smooth(method = "gam", se = TRUE, alpha = 0.3, linewidth = 1, color = color_vals, fill = color_vals) +
    
    
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
sifvars <- c("mean_sif_par", "mean_sif_parm", "mean_modcci", "mean_pai", "mean_pai_toc", "mean_modis_lai", "mean_fesc", "mean_fesc_tropo_rad", "mean_phifm_tropo_rad")
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

plot_pri_geo <- add_rel_ampl_annotation(plot_pri_geo, rel_pace_df_grouped, "mean_pri")
plot_pri_geo

plot_cci_geo <- create_yr_plot(df_yr_georeg_summ, 
                               x_var = "doy", 
                               y_var = "mean_cci", 
                               y_label = "PACE CCI", 
                               se_var = "se_cci", 
                               color_vals = cci_col, 
                               facet_var = "georeg") + 
  custom_annotate(0.08)+
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

plot_phifmtroporad_geo <- create_yr_plot(gedi_yr_georeg_summ, 
                                   x_var = "doymin", 
                                   y_var = "mean_phifm_tropo_rad", 
                                   y_label = expression(Phi*"F;" ~MOD[PAR]~TROPO[Rad]), 
                                   se_var = "se_phifm_tropo_rad", 
                                   color_vals = phif_col, 
                                   facet_var = "georeg") + 
  custom_annotate(0.95e-08)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

plot_phifmtroporad_geo <- add_rel_ampl_annotation(plot_phifmtroporad_geo, rel_sif_df_grouped, "mean_phifm_tropo_rad")
plot_phifmtroporad_geo

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


phifplot <- plot_phifmtroporad_geo + theme(axis.title.x = element_blank())
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

georeg_plot <- phifplot / cireplot / carplot / chlcarplot / priplot +
  plot_layout(guides = "collect")+
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')',
                  tag_sep = ' ')
georeg_plot

#ggsave(paste0(figdir, "/PACE_georeg_trends.png"), georeg_plot, dpi = 300, width = 11, height = 8)
ggsave(paste0(figdir, "/PACE_georeg_trends.tiff"), georeg_plot, device = 'tiff', units = 'in', dpi = 600, width = 11, height = 8, compression = 'lzw')



#Possible supplement: A similar percent change plot -------------------
#Note that these should be interpreted with caution since PACE data come from different years than TROPOMI data.

gedi_tojoin <- gedi_yr_georeg_summ %>% 
  rename(mean_ccimod = mean_cci,
         se_ccimod = se_cci)

pace_tojoin <- df_yr_georeg_summ %>% 
  rename(mean_ccipace = mean_cci,
         se_ccipace = se_cci) %>% 
  select(-c(dry_start:wet_end_window))

all_j <- left_join(gedi_tojoin, pace_tojoin, by = c("doymin" = "doy", "georeg", "sub_szn"))


spc <- function(new, base) {
  100 * (new - base) / ((new + base) / 2)
}

spc_vars <- c(
  "mean_phifm_tropo_rad",
  "mean_cire",
  "mean_chlcar",
  "mean_ccipace",
  "mean_ccimod",
  "mean_pri",
  "mean_car",
  "mean_nirv",
  "mean_fesc",
  "mean_fesc_tropo_rad",
  "mean_sif_parm"
)

baseline_tbl <- all_j %>%
  filter(doymin == dry_start) %>%
  select(georeg_agg, all_of(spc_vars)) %>%
  rename_with(~ paste0(.x, "_base"), -georeg_agg)

all_j_spc <- all_j %>%
  left_join(baseline_tbl, by = "georeg_agg") %>%
  mutate(
    across(
      all_of(spc_vars),
      ~ spc(.x, get(paste0(cur_column(), "_base"))),
      .names = "{.col}_pct_chg"
    )
  ) %>%
  select(-ends_with("_base"))


plot_long <- all_j_spc %>%
  select(
    doymin,
    georeg_agg,
    mean_phifm_tropo_rad_pct_chg,
    mean_cire_pct_chg,
    mean_ccipace_pct_chg
  ) %>%
  pivot_longer(
    cols = -c(doymin, georeg_agg),
    names_to = "variable",
    values_to = "pct_chg"
  ) %>%
  mutate(
    variable = recode(
      variable,
      mean_phifm_tropo_rad_pct_chg = "PhiF (TROPOMI)",
      mean_cire_pct_chg = "CIre (PACE)",
      mean_ccipace_pct_chg = "CCI (PACE)"
    )
  )

ggplot(plot_long, aes(x = doymin, y = pct_chg, color = variable)) +
  geom_hline(yintercept = 0, linewidth = 0.5, alpha = 0.6) +
  
  # raw time series (low alpha)
  geom_line(alpha = 0.25, linewidth = 0.6) +
  
  # GAM smoother
  geom_smooth(
    aes(fill = variable, color = variable),
    method = "gam",
    formula = y ~ s(x, k = 10),
    linewidth = 1.1,
    se = TRUE,
    alpha = 0.3
  ) +
  ylim(-60,60)+
  
  facet_wrap(~ georeg_agg, scales = "free_x") +
  scale_color_manual(
    values = c("CIre (PACE)"= cire_col,
      "CCI (PACE)" = cci_col,
      "PhiF (TROPOMI)" = phif_col
    )
  ) +
  scale_fill_manual(
    values = c(
      "CIre (PACE)"           = cire_col,
      "CCI (PACE)"            = cci_col,
      "PhiF (TROPOMI)" = phif_col
    )
  ) +
  guides(fill = "none")+
  labs(
    x = "Date",
    y = "SPC (% change from dry-season onset)",
    color = "Variable"
  ) +
  theme_classic()





# 
# # Filtering data by georeg_agg
# yr_geo_CA <- df_yr_georeg_summ %>% filter(georeg == "CA")
# yr_geo_NOA <- df_yr_georeg_summ %>% filter(georeg == "NOA")
# yr_geo_NWA <- df_yr_georeg_summ %>% filter(georeg == "NWA")
# yr_geo_Southern <- df_yr_georeg_summ %>% filter(georeg == "Southern")
# 
# 
# sif_geo_CA <- gedi_yr_georeg_summ %>% filter(georeg == "CA")
# sif_geo_NOA <- gedi_yr_georeg_summ %>% filter(georeg == "NOA")
# sif_geo_NWA <- gedi_yr_georeg_summ %>% filter(georeg == "NWA")
# sif_geo_Southern <- gedi_yr_georeg_summ %>% filter(georeg == "Southern")
# 
# 
# # Extract baseline values at start of dry season
# baseCA_cci <- yr_geo_CA %>% filter(georeg == "CA", doy == dry_start) %>% pull(mean_cci)
# baseCA_cire <- yr_geo_CA %>% filter(georeg == "CA", doy == dry_start) %>% pull(mean_cire)
# baseCA_car <- yr_geo_CA %>% filter(georeg == "CA", doy == dry_start) %>% pull(mean_car)
# baseCA_chlcar <- yr_geo_CA %>% filter(georeg == "CA", doy == dry_start) %>% pull(mean_chlcar)
# baseCA_sif_par <- sif_geo_CA %>% filter(georeg == "CA", doymin == dry_start) %>% pull(mean_sif_par)
# 
# baseNOA_cci <- yr_geo_NOA %>% filter(georeg == "NOA", doy == dry_start) %>% pull(mean_cci)
# baseNOA_cire <- yr_geo_NOA %>% filter(georeg == "NOA", doy == dry_start) %>% pull(mean_cire)
# baseNOA_car <- yr_geo_NOA %>% filter(georeg == "NOA", doy == dry_start) %>% pull(mean_car)
# baseNOA_chlcar <- yr_geo_NOA %>% filter(georeg == "NOA", doy == dry_start) %>% pull(mean_chlcar)
# baseNOA_sif_par <- sif_geo_NOA %>% filter(georeg == "NOA", doymin == dry_start) %>% pull(mean_sif_par)
# 
# baseNWA_cci <- yr_geo_NWA %>% filter(georeg == "NWA", doy == dry_start) %>% pull(mean_cci)
# baseNWA_cire <- yr_geo_NWA %>% filter(georeg == "NWA", doy == dry_start) %>% pull(mean_cire)
# baseNWA_car <- yr_geo_NWA %>% filter(georeg == "NWA", doy == dry_start) %>% pull(mean_car)
# baseNWA_chlcar <- yr_geo_NWA %>% filter(georeg == "NWA", doy == dry_start) %>% pull(mean_chlcar)
# baseNWA_sif_par <- sif_geo_NWA %>% filter(georeg == "NWA", doymin == dry_start) %>% pull(mean_sif_par)
# 
# baseSouthern_cci <- yr_geo_Southern %>% filter(georeg == "Southern", doy == dry_start) %>% pull(mean_cci)
# baseSouthern_cire <- yr_geo_Southern %>% filter(georeg == "Southern", doy == dry_start) %>% pull(mean_cire)
# baseSouthern_car <- yr_geo_Southern %>% filter(georeg == "Southern", doy == dry_start) %>% pull(mean_car)
# baseSouthern_chlcar <- yr_geo_Southern %>% filter(georeg == "Southern", doy == dry_start) %>% pull(mean_chlcar)
# baseSouthern_sif_par <- sif_geo_Southern %>% filter(georeg == "Southern", doymin == dry_start) %>% pull(mean_sif_par)
# 
# #Using symmetric percent change:
# # Symmetric percent change helper function
# spc <- function(new, base) {
#   100 * (new - base) / ((new + base) / 2)
# }
# 
# #absolute case
# spc_abs <- function(new, base) {
#   100 * abs((new - base)) / ((abs(new) + abs(base)) / 2)
# }
# 
# # CA region
# yr_geo_CA <- yr_geo_CA %>%
#   mutate(
#     mean_cci_pct_chg = spc(mean_cci, baseCA_cci),
#     mean_cire_pct_chg = spc(mean_cire, baseCA_cire),
#     mean_car_pct_chg = spc(mean_car, baseCA_car),
#     mean_chlcar_pct_chg = spc(mean_chlcar, baseCA_chlcar)
#   )
# sif_geo_CA <- sif_geo_CA %>% 
#   mutate(mean_sif_par_pct_chg = spc(mean_sif_par, baseCA_sif_par))
# 
# # NOA region
# yr_geo_NOA <- yr_geo_NOA %>%
#   mutate(
#     mean_cci_pct_chg = spc(mean_cci, baseNOA_cci),
#     mean_cire_pct_chg = spc(mean_cire, baseNOA_cire),
#     mean_car_pct_chg = spc(mean_car, baseNOA_car),
#     mean_chlcar_pct_chg = spc(mean_chlcar, baseNOA_chlcar)
#   )
# sif_geo_NOA <- sif_geo_NOA %>% 
#   mutate(mean_sif_par_pct_chg = spc(mean_sif_par, baseNOA_sif_par))
# 
# # NWA region
# yr_geo_NWA <- yr_geo_NWA %>%
#   mutate(
#     mean_cci_pct_chg = spc(mean_cci, baseNWA_cci),
#     mean_cire_pct_chg = spc(mean_cire, baseNWA_cire),
#     mean_car_pct_chg = spc(mean_car, baseNWA_car),
#     mean_chlcar_pct_chg = spc(mean_chlcar, baseNWA_chlcar)
#   )
# sif_geo_NWA <- sif_geo_NWA %>% 
#   mutate(mean_sif_par_pct_chg = spc(mean_sif_par, baseNWA_sif_par))
# 
# # Southern region
# yr_geo_Southern <- yr_geo_Southern %>%
#   mutate(
#     mean_cci_pct_chg = spc(mean_cci, baseSouthern_cci),
#     mean_cire_pct_chg = spc(mean_cire, baseSouthern_cire),
#     mean_car_pct_chg = spc(mean_car, baseSouthern_car),
#     mean_chlcar_pct_chg = spc(mean_chlcar, baseSouthern_chlcar)
#   )
# sif_geo_Southern <- sif_geo_Southern %>% 
#   mutate(mean_sif_par_pct_chg = spc(mean_sif_par, baseSouthern_sif_par))
# 
# 
# # Figure ***: Percent Change time-series ---------------------------
# 
# #New custom_annotate:
# custom_annotate2 <- function(region, y_text_pos = NULL) {
#   region_season <- seasonality[seasonality$georeg == region, ]
#   
#   annotations <- list(
#     # Dry season shading
#     annotate("rect",
#              xmin = region_season$dry_start,
#              xmax = region_season$dry_end_window,
#              ymin = -Inf, ymax = Inf,
#              fill = drycol, alpha = 0.2),
#     
#     # Early wet season shading
#     annotate("rect",
#              xmin = region_season$earlywet_start,
#              xmax = region_season$earlywet_end_window,
#              ymin = -Inf, ymax = Inf,
#              fill = earlywetcol, alpha = 0.2),
#     
#     # Wet peak season shading
#     annotate("rect",
#              xmin = region_season$wet_start,
#              xmax = region_season$wet_end_window,
#              ymin = -Inf, ymax = Inf,
#              fill = peakwetcol, alpha = 0.2),
#     
#     #Dry season start
#     geom_vline(xintercept = region_season$dry_start, linetype = "dotted", color = "red", linewidth = 0.8)
#     
#   )
#   
#   # Optional text labels
#   if (!is.null(y_text_pos)) {
#     annotations <- c(annotations, list(
#       annotate("text", x = region_season$dry_start, y = y_text_pos, label = "DRY", hjust = -0.1, size = 3),
#       annotate("text", x = region_season$earlywet_start, y = y_text_pos, label = "DWT", hjust = -0.1, size = 3),
#       annotate("text", x = region_season$wet_start, y = y_text_pos, label = "PW", hjust = -0.1, size = 3)
#     ))
#   }
#   
#   return(annotations)
# }
# 
# 

# 
# make_pace_plot <- function(
#     data, data2,
#     doy_col, doy_col2,
#     sifpar_mean_col, sifpar_se_col, sifpar_base,
#     cci_mean_col, cci_se_col, cci_base,
#     cire_mean_col, cire_se_col, cire_base,
#     car_mean_col, car_se_col, car_base,
#     chlcar_mean_col, chlcar_se_col, chlcar_base,
#     sifpar_color, cci_color, cire_color, car_color, chlcar_color,
#     zone_label,
#     ylim_range = c(-100, 100), line_alpha = 0.4, loess_alpha = 0.2
# ) {
#   
#   ggplot() +
#     
#     # --- SIF/PAR layer (data2) ---
#     geom_point(data = data2,
#                aes(x = as.numeric(!!sym(doy_col2)),
#                    y = !!sym(sifpar_mean_col),
#                    color = "SIF/PAR"),
#                size = 2.3, alpha = line_alpha) +
#     geom_errorbar(data = data2,
#                   aes(x = as.numeric(!!sym(doy_col2)),
#                       ymin = !!sym(sifpar_mean_col) - 100 * !!sym(sifpar_se_col) / !!sym(sifpar_base),
#                       ymax = !!sym(sifpar_mean_col) + 100 * !!sym(sifpar_se_col) / !!sym(sifpar_base),
#                       color = "SIF/PAR"),
#                   linewidth = 0.3, alpha = line_alpha) +
#     geom_line(data = data2,
#               aes(x = as.numeric(!!sym(doy_col2)),
#                   y = !!sym(sifpar_mean_col), color = "SIF/PAR"),
#               alpha = line_alpha) +
#     geom_smooth(data = data2,
#                 aes(x = as.numeric(!!sym(doy_col2)),
#                     y = !!sym(sifpar_mean_col)),
#                 method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2,
#                 color = sifpar_color, fill = sifpar_color) +
#     
#     # --- CCI layer (data) ---
#     geom_point(data = data,
#                aes(x = as.numeric(!!sym(doy_col)),
#                    y = !!sym(cci_mean_col), color = "CCI"),
#                size = 2.3, alpha = line_alpha) +
#     geom_errorbar(data = data,
#                   aes(x = as.numeric(!!sym(doy_col)),
#                       ymin = !!sym(cci_mean_col) - 100 * !!sym(cci_se_col) / !!sym(cci_base),
#                       ymax = !!sym(cci_mean_col) + 100 * !!sym(cci_se_col) / !!sym(cci_base),
#                       color = "CCI"),
#                   linewidth = 0.3, alpha = line_alpha) +
#     geom_line(data = data,
#               aes(x = as.numeric(!!sym(doy_col)),
#                   y = !!sym(cci_mean_col), color = "CCI"),
#               alpha = line_alpha) +
#     geom_smooth(data = data,
#                 aes(x = as.numeric(!!sym(doy_col)),
#                     y = !!sym(cci_mean_col)),
#                 method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2,
#                 color = cci_color, fill = cci_color) +
#     
#     # --- CIre layer ---
#     geom_point(data = data,
#                aes(x = as.numeric(!!sym(doy_col)),
#                    y = !!sym(cire_mean_col), color = "CIre"),
#                size = 2.3, alpha = line_alpha) +
#     geom_errorbar(data = data,
#                   aes(x = as.numeric(!!sym(doy_col)),
#                       ymin = !!sym(cire_mean_col) - 100 * !!sym(cire_se_col) / !!sym(cire_base),
#                       ymax = !!sym(cire_mean_col) + 100 * !!sym(cire_se_col) / !!sym(cire_base),
#                       color = "CIre"),
#                   linewidth = 0.3, alpha = line_alpha) +
#     geom_line(data = data,
#               aes(x = as.numeric(!!sym(doy_col)),
#                   y = !!sym(cire_mean_col), color = "CIre"),
#               alpha = line_alpha) +
#     geom_smooth(data = data,
#                 aes(x = as.numeric(!!sym(doy_col)),
#                     y = !!sym(cire_mean_col)),
#                 method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2,
#                 color = cire_color, fill = cire_color) +
#     
#     # --- Car layer ---
#     geom_point(data = data,
#                aes(x = as.numeric(!!sym(doy_col)),
#                    y = !!sym(car_mean_col), color = "Car"),
#                size = 2.3, alpha = line_alpha) +
#     geom_errorbar(data = data,
#                   aes(x = as.numeric(!!sym(doy_col)),
#                       ymin = !!sym(car_mean_col) - 100 * !!sym(car_se_col) / !!sym(car_base),
#                       ymax = !!sym(car_mean_col) + 100 * !!sym(car_se_col) / !!sym(car_base),
#                       color = "Car"),
#                   linewidth = 0.3, alpha = line_alpha) +
#     geom_line(data = data,
#               aes(x = as.numeric(!!sym(doy_col)),
#                   y = !!sym(car_mean_col), color = "Car"),
#               alpha = line_alpha) +
#     geom_smooth(data = data,
#                 aes(x = as.numeric(!!sym(doy_col)),
#                     y = !!sym(car_mean_col)),
#                 method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2,
#                 color = car_color, fill = car_color) +
#     
#     # --- ChlCar layer ---
#     geom_point(data = data,
#                aes(x = as.numeric(!!sym(doy_col)),
#                    y = !!sym(chlcar_mean_col), color = "ChlCar"),
#                size = 2.3, alpha = line_alpha) +
#     geom_errorbar(data = data,
#                   aes(x = as.numeric(!!sym(doy_col)),
#                       ymin = !!sym(chlcar_mean_col) - 100 * !!sym(chlcar_se_col) / !!sym(chlcar_base),
#                       ymax = !!sym(chlcar_mean_col) + 100 * !!sym(chlcar_se_col) / !!sym(chlcar_base),
#                       color = "ChlCar"),
#                   linewidth = 0.3, alpha = line_alpha) +
#     geom_line(data = data,
#               aes(x = as.numeric(!!sym(doy_col)),
#                   y = !!sym(chlcar_mean_col), color = "ChlCar"),
#               alpha = line_alpha) +
#     geom_smooth(data = data,
#                 aes(x = as.numeric(!!sym(doy_col)),
#                     y = !!sym(chlcar_mean_col)),
#                 method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2,
#                 color = chlcar_color, fill = chlcar_color) +
#     
#     # --- Layout ---
#     ylim(ylim_range) +
#     labs(x = "Day of Year",
#          y = "% Change relative to dry period onset",
#          color = "RS variable") +
#     theme_minimal() +
#     annotate(geom = "text",
#              x = 10, y = ylim_range[2] - 10,
#              hjust = 0, label = zone_label,
#              color = "black", size = rel(7), family = "serif") +
#     
#     # --- Manual color scale ---
#     scale_color_manual(
#       values = setNames(c(sifpar_color, cci_color, cire_color, car_color, chlcar_color),
#                         c("SIF/PAR", "CCI", "CIre", "Car", "ChlCar")),
#       labels = c("SIF/PAR" = "SIF/PAR",
#                  "CCI" = "CCI",
#                  "CIre" = expression(paste("CI"[re])),
#                  "Car" = "Car",
#                  "ChlCar" = "Chl:Car")
#     )
# }
# 
# 
# # CCI vs SIF/PAR plots
# sif_pace_CA <- make_pace_plot(
#   data = yr_geo_CA,
#   data2 = sif_geo_CA,
#   doy_col = "doy",
#   doy_col2 = "doymin",
#   sifpar_mean_col = "mean_sif_par_pct_chg",
#   sifpar_se_col = "se_sif_par",
#   sifpar_base = "baseCA_sif_par",
#   cci_mean_col = "mean_cci_pct_chg",  # turn off PhiF
#   cci_se_col = "se_cci",
#   cci_base = "baseCA_cci",
#   cire_mean_col = "mean_cire_pct_chg",
#   cire_se_col = "se_cire",
#   cire_base = "baseCA_cire",
#   car_mean_col = "mean_car_pct_chg",
#   car_se_col = "se_car",
#   car_base = "baseCA_car",
#   chlcar_mean_col = "mean_chlcar_pct_chg",
#   chlcar_se_col = "se_chlcar",
#   chlcar_base = "baseCA_chlcar",
#   sifpar_color = sif_col,
#   cci_color = cci_col,
#   cire_color = cire_col,
#   car_color = car_col,
#   chlcar_color = chlcar_col,
#   zone_label = "Central Amz. (seasonal)"
# ) + custom_annotate2(region = "CA", y_text_pos = -100)
# sif_pace_CA
# 
# 
# # CCI vs SIF/PAR plots
# sif_pace_NWA <- make_pace_plot(
#   data = yr_geo_NWA,
#   data2 = sif_geo_NWA,
#   doy_col = "doy",
#   doy_col2 = "doymin",
#   sifpar_mean_col = "mean_sif_par_pct_chg",
#   sifpar_se_col = "se_sif_par",
#   sifpar_base = "baseNWA_sif_par",
#   cci_mean_col = "mean_cci_pct_chg",  # turn off PhiF
#   cci_se_col = "se_cci",
#   cci_base = "baseNWA_cci",
#   cire_mean_col = "mean_cire_pct_chg",
#   cire_se_col = "se_cire",
#   cire_base = "baseNWA_cire",
#   car_mean_col = "mean_car_pct_chg",
#   car_se_col = "se_car",
#   car_base = "baseNWA_car",
#   chlcar_mean_col = "mean_chlcar_pct_chg",
#   chlcar_se_col = "se_chlcar",
#   chlcar_base = "baseNWA_chlcar",
#   sifpar_color = sif_col,
#   cci_color = cci_col,
#   cire_color = cire_col,
#   car_color = car_col,
#   chlcar_color = chlcar_col,
#   zone_label = "Northwest Amz. (a-seasonal)"
# ) + custom_annotate2(region = "NWA", y_text_pos = -100)
# sif_pace_NWA
# 
# # CCI vs SIF/PAR plots
# sif_pace_NOA <- make_pace_plot(
#   data = yr_geo_NOA,
#   data2 = sif_geo_NOA,
#   doy_col = "doy",
#   doy_col2 = "doymin",
#   sifpar_mean_col = "mean_sif_par_pct_chg",
#   sifpar_se_col = "se_sif_par",
#   sifpar_base = "baseNOA_sif_par",
#   cci_mean_col = "mean_cci_pct_chg",  # turn off PhiF
#   cci_se_col = "se_cci",
#   cci_base = "baseNOA_cci",
#   cire_mean_col = "mean_cire_pct_chg",
#   cire_se_col = "se_cire",
#   cire_base = "baseNOA_cire",
#   car_mean_col = "mean_car_pct_chg",
#   car_se_col = "se_car",
#   car_base = "baseNOA_car",
#   chlcar_mean_col = "mean_chlcar_pct_chg",
#   chlcar_se_col = "se_chlcar",
#   chlcar_base = "baseNOA_chlcar",
#   sifpar_color = sif_col,
#   cci_color = cci_col,
#   cire_color = cire_col,
#   car_color = car_col,
#   chlcar_color = chlcar_col,
#   zone_label = "Northern Amz. (seasonal)"
# ) + custom_annotate2(region = "NOA", y_text_pos = -100)
# sif_pace_NOA
# 
# # CCI vs SIF/PAR plots
# sif_pace_Southern <- make_pace_plot(
#   data = yr_geo_Southern,
#   data2 = sif_geo_Southern,
#   doy_col = "doy",
#   doy_col2 = "doymin",
#   sifpar_mean_col = "mean_sif_par_pct_chg",
#   sifpar_se_col = "se_sif_par",
#   sifpar_base = "baseSouthern_sif_par",
#   cci_mean_col = "mean_cci_pct_chg",  # turn off PhiF
#   cci_se_col = "se_cci",
#   cci_base = "baseSouthern_cci",
#   cire_mean_col = "mean_cire_pct_chg",
#   cire_se_col = "se_cire",
#   cire_base = "baseSouthern_cire",
#   car_mean_col = "mean_car_pct_chg",
#   car_se_col = "se_car",
#   car_base = "baseSouthern_car",
#   chlcar_mean_col = "mean_chlcar_pct_chg",
#   chlcar_se_col = "se_chlcar",
#   chlcar_base = "baseCA_chlcar",
#   sifpar_color = sif_col,
#   cci_color = cci_col,
#   cire_color = cire_col,
#   car_color = car_col,
#   chlcar_color = chlcar_col,
#   zone_label = "Southern Amz. (strongly seasonal)"
# ) + custom_annotate2(region = "Southern", y_text_pos = -100)
# sif_pace_Southern


make_pace_plot <- function(
    data, data2,
    doy_col, doy_col2,
    sifpar_mean_col, sifpar_se_col, sifpar_base,
    #cci_mean_col, cci_se_col, cci_base,
    cire_mean_col, cire_se_col, cire_base,
    car_mean_col, car_se_col, car_base,
    #chlcar_mean_col, chlcar_se_col, chlcar_base,
    sifpar_color,
    #cci_color,
    cire_color,
    car_color,
    #chlcar_color,
    zone_label,
    ylim_range = c(-110, 100), line_alpha = 0.4, loess_alpha = 0.2
) {
  
  ggplot() +
    
    # --- SIF/PAR layer (data2) ---
    geom_point(data = data2,
               aes(x = as.numeric(!!sym(doy_col2)),
                   y = !!sym(sifpar_mean_col),
                   color = "SIF/PAR"),
               size = 2.3, alpha = line_alpha) +
    geom_errorbar(data = data2,
                  aes(x = as.numeric(!!sym(doy_col2)),
                      ymin = !!sym(sifpar_mean_col) - 100 * !!sym(sifpar_se_col) / !!sym(sifpar_base),
                      ymax = !!sym(sifpar_mean_col) + 100 * !!sym(sifpar_se_col) / !!sym(sifpar_base),
                      color = "SIF/PAR"),
                  linewidth = 0.3, alpha = line_alpha) +
    geom_line(data = data2,
              aes(x = as.numeric(!!sym(doy_col2)),
                  y = !!sym(sifpar_mean_col), color = "SIF/PAR"),
              alpha = line_alpha) +
    geom_smooth(data = data2,
                aes(x = as.numeric(!!sym(doy_col2)),
                    y = !!sym(sifpar_mean_col)),
                method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2,
                color = sifpar_color, fill = sifpar_color) +
    
    # --- CCI layer (data) ---
    # geom_point(data = data,
    #            aes(x = as.numeric(!!sym(doy_col)),
    #                y = !!sym(cci_mean_col), color = "CCI"),
    #            size = 2.3, alpha = line_alpha) +
    # geom_errorbar(data = data,
    #               aes(x = as.numeric(!!sym(doy_col)),
    #                   ymin = !!sym(cci_mean_col) - 100 * !!sym(cci_se_col) / !!sym(cci_base),
    #                   ymax = !!sym(cci_mean_col) + 100 * !!sym(cci_se_col) / !!sym(cci_base),
    #                   color = "CCI"),
    #               linewidth = 0.3, alpha = line_alpha) +
    # geom_line(data = data,
    #           aes(x = as.numeric(!!sym(doy_col)),
    #               y = !!sym(cci_mean_col), color = "CCI"),
    #           alpha = line_alpha) +
    # geom_smooth(data = data,
    #             aes(x = as.numeric(!!sym(doy_col)),
    #                 y = !!sym(cci_mean_col)),
    #             method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2,
    #             color = cci_color, fill = cci_color) +
    
    # --- CIre layer ---
    geom_point(data = data,
               aes(x = as.numeric(!!sym(doy_col)),
                   y = !!sym(cire_mean_col), color = "CIre"),
               size = 2.3, alpha = line_alpha) +
    geom_errorbar(data = data,
                  aes(x = as.numeric(!!sym(doy_col)),
                      ymin = !!sym(cire_mean_col) - 100 * !!sym(cire_se_col) / !!sym(cire_base),
                      ymax = !!sym(cire_mean_col) + 100 * !!sym(cire_se_col) / !!sym(cire_base),
                      color = "CIre"),
                  linewidth = 0.3, alpha = line_alpha) +
    geom_line(data = data,
              aes(x = as.numeric(!!sym(doy_col)),
                  y = !!sym(cire_mean_col), color = "CIre"),
              alpha = line_alpha) +
    geom_smooth(data = data,
                aes(x = as.numeric(!!sym(doy_col)),
                    y = !!sym(cire_mean_col)),
                method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2,
                color = cire_color, fill = cire_color) +
    
    # --- Car layer ---
    geom_point(data = data,
               aes(x = as.numeric(!!sym(doy_col)),
                   y = !!sym(car_mean_col), color = "Car"),
               size = 2.3, alpha = line_alpha) +
    geom_errorbar(data = data,
                  aes(x = as.numeric(!!sym(doy_col)),
                      ymin = !!sym(car_mean_col) - 100 * !!sym(car_se_col) / !!sym(car_base),
                      ymax = !!sym(car_mean_col) + 100 * !!sym(car_se_col) / !!sym(car_base),
                      color = "Car"),
                  linewidth = 0.3, alpha = line_alpha) +
    geom_line(data = data,
              aes(x = as.numeric(!!sym(doy_col)),
                  y = !!sym(car_mean_col), color = "Car"),
              alpha = line_alpha) +
    geom_smooth(data = data,
                aes(x = as.numeric(!!sym(doy_col)),
                    y = !!sym(car_mean_col)),
                method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2,
                color = car_color, fill = car_color) +
    
    # # --- ChlCar layer ---
    # geom_point(data = data,
    #            aes(x = as.numeric(!!sym(doy_col)),
    #                y = !!sym(chlcar_mean_col), color = "ChlCar"),
    #            size = 2.3, alpha = line_alpha) +
    # geom_errorbar(data = data,
    #               aes(x = as.numeric(!!sym(doy_col)),
    #                   ymin = !!sym(chlcar_mean_col) - 100 * !!sym(chlcar_se_col) / !!sym(chlcar_base),
    #                   ymax = !!sym(chlcar_mean_col) + 100 * !!sym(chlcar_se_col) / !!sym(chlcar_base),
    #                   color = "ChlCar"),
    #               linewidth = 0.3, alpha = line_alpha) +
    # geom_line(data = data,
    #           aes(x = as.numeric(!!sym(doy_col)),
    #               y = !!sym(chlcar_mean_col), color = "ChlCar"),
    #           alpha = line_alpha) +
    # geom_smooth(data = data,
    #             aes(x = as.numeric(!!sym(doy_col)),
    #                 y = !!sym(chlcar_mean_col)),
    #             method = "gam", se = TRUE, alpha = 0.3, linewidth = 1.2,
    #             color = chlcar_color, fill = chlcar_color) +
    # 
    # --- Layout ---
    ylim(ylim_range) +
    labs(x = "Day of Year",
         y = "% Change relative to dry period onset",
         color = "RS variable") +
    theme_minimal() +
    annotate(geom = "text",
             x = 10, y = ylim_range[2] - 10,
             hjust = 0, label = zone_label,
             color = "black", size = rel(7), family = "serif") +
    
    # --- Manual color scale ---
    scale_color_manual(
      values = setNames(c(sifpar_color, cire_color, car_color),
                        c("SIF/PAR", "CIre", "Car")),
      labels = c("SIF/PAR" = "SIF/PAR",
                 "CCI" = "CCI",
                 "CIre" = expression(paste("CI"[re])),
                 "Car" = "Car",
                 "ChlCar" = "Chl:Car")
    )
}


# CCI vs SIF/PAR plots
sif_pace_CA <- make_pace_plot(
  data = yr_geo_CA,
  data2 = sif_geo_CA,
  doy_col = "doy",
  doy_col2 = "doymin",
  sifpar_mean_col = "mean_sif_par_pct_chg",
  sifpar_se_col = "se_sif_par",
  sifpar_base = "baseCA_sif_par",
  #cci_mean_col = "mean_cci_pct_chg",  # turn off PhiF
  #cci_se_col = "se_cci",
  #cci_base = "baseCA_cci",
  cire_mean_col = "mean_cire_pct_chg",
  cire_se_col = "se_cire",
  cire_base = "baseCA_cire",
  car_mean_col = "mean_car_pct_chg",
  car_se_col = "se_car",
  car_base = "baseCA_car",
  #chlcar_mean_col = "mean_chlcar_pct_chg",
  #chlcar_se_col = "se_chlcar",
  #chlcar_base = "baseCA_chlcar",
  sifpar_color = sif_col,
  #cci_color = cci_col,
  cire_color = cire_col,
  car_color = car_col,
  #chlcar_color = chlcar_col,
  zone_label = "Central Amz. (seasonal)"
) + custom_annotate2(region = "CA", y_text_pos = -110)
sif_pace_CA


# CCI vs SIF/PAR plots
sif_pace_NWA <- make_pace_plot(
  data = yr_geo_NWA,
  data2 = sif_geo_NWA,
  doy_col = "doy",
  doy_col2 = "doymin",
  sifpar_mean_col = "mean_sif_par_pct_chg",
  sifpar_se_col = "se_sif_par",
  sifpar_base = "baseNWA_sif_par",
  #cci_mean_col = "mean_cci_pct_chg",  # turn off PhiF
  #cci_se_col = "se_cci",
  #cci_base = "baseNWA_cci",
  cire_mean_col = "mean_cire_pct_chg",
  cire_se_col = "se_cire",
  cire_base = "baseNWA_cire",
  car_mean_col = "mean_car_pct_chg",
  car_se_col = "se_car",
  car_base = "baseNWA_car",
  #chlcar_mean_col = "mean_chlcar_pct_chg",
  #chlcar_se_col = "se_chlcar",
  #chlcar_base = "baseNWA_chlcar",
  sifpar_color = sif_col,
  #cci_color = cci_col,
  cire_color = cire_col,
  car_color = car_col,
  #chlcar_color = chlcar_col,
  zone_label = "Northwest Amz. (a-seasonal)"
) + custom_annotate2(region = "NWA", y_text_pos = -110)
sif_pace_NWA

# CCI vs SIF/PAR plots
sif_pace_NOA <- make_pace_plot(
  data = yr_geo_NOA,
  data2 = sif_geo_NOA,
  doy_col = "doy",
  doy_col2 = "doymin",
  sifpar_mean_col = "mean_sif_par_pct_chg",
  sifpar_se_col = "se_sif_par",
  sifpar_base = "baseNOA_sif_par",
  #cci_mean_col = "mean_cci_pct_chg",  # turn off PhiF
  #cci_se_col = "se_cci",
  #cci_base = "baseNOA_cci",
  cire_mean_col = "mean_cire_pct_chg",
  cire_se_col = "se_cire",
  cire_base = "baseNOA_cire",
  car_mean_col = "mean_car_pct_chg",
  car_se_col = "se_car",
  car_base = "baseNOA_car",
  #chlcar_mean_col = "mean_chlcar_pct_chg",
  #chlcar_se_col = "se_chlcar",
  #chlcar_base = "baseNOA_chlcar",
  sifpar_color = sif_col,
  #cci_color = cci_col,
  cire_color = cire_col,
  car_color = car_col,
  #chlcar_color = chlcar_col,
  zone_label = "Northern Amz. (seasonal)"
) + custom_annotate2(region = "NOA", y_text_pos = -110)
sif_pace_NOA

# CCI vs SIF/PAR plots
sif_pace_Southern <- make_pace_plot(
  data = yr_geo_Southern,
  data2 = sif_geo_Southern,
  doy_col = "doy",
  doy_col2 = "doymin",
  sifpar_mean_col = "mean_sif_par_pct_chg",
  sifpar_se_col = "se_sif_par",
  sifpar_base = "baseSouthern_sif_par",
  #cci_mean_col = "mean_cci_pct_chg",  # turn off PhiF
  #cci_se_col = "se_cci",
  #cci_base = "baseSouthern_cci",
  cire_mean_col = "mean_cire_pct_chg",
  cire_se_col = "se_cire",
  cire_base = "baseSouthern_cire",
  car_mean_col = "mean_car_pct_chg",
  car_se_col = "se_car",
  car_base = "baseSouthern_car",
  #chlcar_mean_col = "mean_chlcar_pct_chg",
  #chlcar_se_col = "se_chlcar",
  #chlcar_base = "baseCA_chlcar",
  sifpar_color = sif_col,
  #cci_color = cci_col,
  cire_color = cire_col,
  car_color = car_col,
  #chlcar_color = chlcar_col,
  zone_label = "Southern Amz. (strongly seasonal)"
) + custom_annotate2(region = "Southern", y_text_pos = -110)
sif_pace_Southern

# Create plot grid
sif_pace_NWA <- sif_pace_NWA + theme(legend.position = "none")
sif_pace_CA <- sif_pace_CA + theme(legend.position = "none")

sif_pacegrid <- (sif_pace_NWA + sif_pace_NOA) / (sif_pace_CA + sif_pace_Southern)
sif_pacegrid


#save the plot
ggsave(paste0(figdir, "/perc_chg_pace_cci_gam.png"), sif_pacegrid, dpi = 300, width = 13, height = 8)
