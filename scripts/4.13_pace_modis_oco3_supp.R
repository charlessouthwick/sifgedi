
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

pace_summ <- read.csv(paste0(complete_dir, ("/pace_summ.csv")))
pace_georeg_summ <- read.csv(paste0(complete_dir, ("/pace_georeg_summ.csv")))
pace_yr_summ <- read.csv(paste0(complete_dir, ("/pace_yr_summ.csv")))
pace_yr_georeg_summ <- read.csv(paste0(complete_dir, ("/pace_yr_georeg_summ.csv")))

gedi_summ <- read.csv(paste0(complete_dir, "/gedi_naincl_summ.csv"))
gedi_georeg_summ <- read.csv(paste0(complete_dir, "/gedi_georeg_naincl_summ.csv"))
gedi_yr_summ <- read.csv(paste0(complete_dir, "/gedi_yr_naincl_summ.csv"))
gedi_yr_georeg_summ <- read.csv(paste0(complete_dir, "/gedi_yr_georeg_naincl_summ.csv"))

oco3orig_yr_summ <- read.csv(paste0(complete_dir, "/oco3_yr_summ.csv"))
oco3orig_yr_georeg_summ <- read.csv(paste0(complete_dir, "/oco3_yr_georeg_summ.csv"))
oco3orig_summ <- read.csv(paste0(complete_dir, "/oco3_summ.csv"))
oco3orig_georeg_summ <- read.csv(paste0(complete_dir, "/oco3_georeg_summ.csv"))

oco3new_yr_summ <- read.csv(paste0(complete_dir, "/oco3_yr_summ_2024thru2025.csv"))
oco3new_yr_georeg_summ <- read.csv(paste0(complete_dir, "/oco3_yr_georeg_summ_2024thru2025.csv"))
oco3new_summ <- read.csv(paste0(complete_dir, "/oco3_summ_2024thru2025.csv"))
oco3new_georeg_summ <- read.csv(paste0(complete_dir, "/oco3_georeg_summ_2024thru2025.csv"))

maiacnew_yr_summ <- read.csv(paste0(complete_dir, "/maiac_nirv_cci_yr_summ_2024thru2025.csv"))
maiacnew_yr_georeg_summ <- read.csv(paste0(complete_dir, "/maiac_nirv_cci_yr_georeg_summ_2024thru2025.csv"))
maiacnew_summ <- read.csv(paste0(complete_dir, "/maiac_nirv_cci_summ_2024thru2025.csv"))
maiacnew_georeg_summ <- read.csv(paste0(complete_dir, "/maiac_nirv_cci_georeg_summ_2024thru2025.csv"))

seasonality <- seasonality %>% rename(georeg = georeg_agg)

pace_summ <- pace_summ %>% mutate(truedate = as.character(ymd(truedate)))
pace_georeg_summ <- pace_georeg_summ %>% mutate(truedate = as.character(ymd(truedate)))

##
#8 DAY OR 16-DAY?????
##

pace_yr_georeg_summ <- pace_yr_georeg_summ %>% 
  left_join(seasonality, by = join_by("georeg" == "georeg")) %>% 
  left_join(., oco3new_yr_georeg_summ, by = join_by("georeg", "doy")) %>% 
  left_join(., maiacnew_yr_georeg_summ %>% rename(mean_ccimod = mean_cci,
                                                  se_ccimod = se_cci,
                                                  mean_ndvimod = mean_ndvi,
                                                  se_ndvimod = se_ndvi,
                                                  mean_nirvmod = mean_nirv,
                                                  se_nirvmod = se_nirv),
            by = join_by("georeg", "doy"))

pace_yr_summ <- pace_yr_summ %>% 
  left_join(glob_season, by = join_by("region" == "region")) %>% 
  left_join(., oco3new_yr_summ, by = join_by("region", "doy")) %>% 
  left_join(., maiacnew_yr_summ %>% rename(mean_ccimod = mean_cci,
                                                  se_ccimod = se_cci,
                                                  mean_ndvimod = mean_ndvi,
                                                  se_ndvimod = se_ndvi,
                                                  mean_nirvmod = mean_nirv,
                                                  se_nirvmod = se_nirv),
            by = join_by("region", "doy"))

pace_georeg_summ <- pace_georeg_summ %>% 
  left_join(seasonality, by = join_by("georeg" == "georeg")) %>% 
  left_join(., oco3new_georeg_summ, by = join_by("georeg", "truedate")) %>% 
  left_join(., maiacnew_georeg_summ %>% rename(mean_ccimod = mean_cci,
                                           se_ccimod = se_cci,
                                           mean_ndvimod = mean_ndvi,
                                           se_ndvimod = se_ndvi,
                                           mean_nirvmod = mean_nirv,
                                           se_nirvmod = se_nirv),
            by = join_by("georeg", "truedate"))

pace_summ <- pace_summ %>% 
  left_join(glob_season, by = join_by("region" == "region")) %>% 
  left_join(., oco3new_summ, by = join_by("region", "truedate")) %>% 
  left_join(., maiacnew_summ %>% rename(mean_ccimod = mean_cci,
                                               se_ccimod = se_cci,
                                               mean_ndvimod = mean_ndvi,
                                               se_ndvimod = se_ndvi,
                                               mean_nirvmod = mean_nirv,
                                               se_nirvmod = se_nirv),
            by = join_by("region", "truedate"))


gedi_yr_georeg_summ <- gedi_yr_georeg_summ %>% 
  left_join(seasonality, by = join_by("georeg_agg" == "georeg")) %>% 
  left_join(., oco3orig_yr_georeg_summ, by = c('georeg_agg' = 'georeg', 'doymin' = 'doy'))
  
gedi_yr_summ <- gedi_yr_summ %>% 
  left_join(glob_season, by = join_by("zone" == "region")) %>% 
  left_join(., oco3orig_yr_summ, by = c("zone" = "region", 'doymin' = 'doy'))

gedi_georeg_summ <- gedi_georeg_summ %>% 
  left_join(seasonality, by = join_by("georeg_agg" == "georeg")) %>% 
  left_join(., oco3orig_georeg_summ, by = c('georeg_agg' = 'georeg', 'truedate', 'doymin' = 'doy'))

gedi_summ <- gedi_summ %>% 
  left_join(glob_season, by = join_by("zone" == "region")) %>% 
  left_join(., oco3orig_summ, by = c("zone" = "region", 'truedate', 'doymin' = 'doy'))

# seasonality$georeg <- factor(seasonality$georeg,
#                              levels = levels(df_yr_georeg_summ$georeg))

gedi_yr_georeg_summ <- gedi_yr_georeg_summ %>% rename(georeg = georeg_agg)
gedi_yr_summ <- gedi_yr_summ %>% rename(region = zone)
gedi_georeg_summ <- gedi_georeg_summ %>% rename(georeg = georeg_agg)
gedi_summ <- gedi_summ %>% rename(region = zone)


pace_yr_georeg_summ <- pace_yr_georeg_summ %>%
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
pace_yr_summ <- pace_yr_summ %>%
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

pace_yr_georeg_summ$georeg <- factor(pace_yr_georeg_summ$georeg,
                                     levels = c("NWA", "NOA", "CA", "Southern"))
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
  
  # Drop rows where the x variable is NA before plotting
  data <- data %>% filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))
  
  # Base plot setup
  plot <- ggplot(data, aes(x = .data[[x_var]],
                           y = .data[[y_var]])) +
    geom_line(alpha = 0.8, linewidth = 0.6, color = color_vals) +
    geom_point(color = color_vals) +
    geom_errorbar(aes(ymin = .data[[y_var]] - .data[[se_var]],
                     ymax = .data[[y_var]] + .data[[se_var]]),
                 linewidth = 0.3, alpha = 0.9, color = color_vals) +
    #geom_smooth(method = "gam", se = TRUE, alpha = 0.2, linewidth = 1, color = color_vals, fill = color_vals) +
    
    
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

georeg_labels <- c(
  "NWA" = "Northwest",
  "NOA" = "Northern",
  "CA"  = "Central",
  "Southern" = "Southern"
)

# get_rel_ampl <- function(data, var) {
#   vals <- data[[var]]
#   vals <- vals[!is.na(vals)]
#   
#   if (length(vals) == 0) return(NA)
#   
#   min_val <- min(vals)
#   max_val <- max(vals)
#   
#   # Avoid division by zero or exploding % from near-zero
#   if (abs(min_val) + abs(max_val) == 0) return(0)
#   
#   rel_ampl <- round(abs(max_val - min_val) / mean(abs(c(min_val, max_val))) * 100, 1)
#   return(rel_ampl)
# }

# chngvars <- c("mean_pri", "mean_cci", "mean_ccimod", "mean_car", "mean_cire", "mean_ndvi", "mean_chlcar", "mean_nirvmod")
# 
# # Grouped computation
# rel_pace_df_grouped <- pace_yr_georeg_summ %>%
#   group_by(georeg) %>%
#   group_map(~ {
#     tibble(
#       georeg = .y$georeg,
#       variable = chngvars,
#       rel_ampl = map_dbl(chngvars, function(v) get_rel_ampl(.x, v))
#     )
#   }) %>%
#   bind_rows()
# 
# rel_pace_df_grouped$georeg <- factor(rel_pace_df_grouped$georeg,
#                                      levels = c("NWA", "NOA", "CA", "Southern"))
# 
# # Grouped computation for SIF and MODIS CCI
# sifvars <- c("mean_sif_par", "mean_sif_parm", "mean_modcci", "mean_pai", "mean_pai_toc", "mean_modis_lai", "mean_fesc", "mean_fesc_tropo_rad", "mean_phifm_tropo_refl")
# 
# rel_sif_df_grouped <- gedi_yr_georeg_summ %>%
#   rename(mean_modcci = mean_cci) %>% 
#   group_by(georeg) %>%
#   group_map(~ {
#     tibble(
#       georeg = .y$georeg,
#       variable = sifvars,
#       rel_ampl = map_dbl(sifvars, function(v) get_rel_ampl(.x, v))
#     )
#   }) %>%
#   bind_rows()
# 
# rel_sif_df_grouped <- rel_sif_df_grouped %>%
#   mutate(georeg = factor(georeg, levels = c("NWA", "NOA", "CA", "Southern")))
# 
# #Summarize all
# all_rel_df <- bind_rows(rel_pace_df_grouped, rel_sif_df_grouped)
# 
# rel_df_summary <- all_rel_df %>%
#   group_by(variable) %>%
#   summarise(mean_pct_chg = mean(rel_ampl, na.rm = TRUE)) %>%
#   ungroup()
# 
# 
# 
# pacemin_doy_by_var <- pace_yr_georeg_summ %>%
#   group_by(georeg) %>%
#   summarise(
#     across(
#       all_of(chngvars),
#       ~ doy[which.min(.)],
#       .names = "doymin_{.col}"
#     ),
#     .groups = "drop"
#   )
# 
# sifmin_doy_by_var <- gedi_yr_georeg_summ %>%
#   rename(mean_modcci = mean_cci) %>% 
#   group_by(georeg) %>%
#   summarise(
#     across(
#       all_of(sifvars),
#       ~ doymin[which.min(.)],
#       .names = "doymin_{.col}"
#     ),
#     .groups = "drop"
#   )
# 
# all_min_df <- full_join(pacemin_doy_by_var, sifmin_doy_by_var)
# 
# all_min_avg <- all_min_df %>% 
#   bind_rows(
#     summarise(., across(where(is.numeric), mean, na.rm = TRUE), georeg = "mean") %>%
#       select(georeg, everything())
#   )




#Now create a custom annotation function
# add_rel_ampl_annotation <- function(plot, rel_df, target_var, 
#                                     label_prefix = "Δ = ", 
#                                     hjust = 1.1, vjust = 1.5, 
#                                     size = 3.5) {
#   # Filter annotation data for the selected variable
#   annot_df <- rel_df %>%
#     filter(variable == target_var) %>%
#     mutate(
#       label = paste0(label_prefix, rel_ampl, "%"),
#       x = Inf,
#       y = Inf
#     )
#   
#   # Add to plot
#   plot + 
#     geom_text(
#       data = annot_df,
#       aes(x = x, y = y, label = label),
#       inherit.aes = FALSE,
#       hjust = hjust,
#       vjust = vjust,
#       size = size
#     )
# }

plot_ccimod1921_geo <- create_yr_plot(gedi_yr_georeg_summ, 
                                      x_var = "doymin", 
                                      y_var = "mean_cci", 
                                      y_label = "CCI MODIS ('19-'21)", 
                                      se_var = "se_cci", 
                                      color_vals = cci_col, 
                                      facet_var = "georeg") + 
  custom_annotate(0.05)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

#plot_ccimod1921_geo <- add_rel_ampl_annotation(plot_ccimod1921_geo, rel_sif_df_grouped, "mean_cci")
plot_ccimod1921_geo

plot_ccimod2425_geo <- create_yr_plot(pace_yr_georeg_summ, 
                               x_var = "doy", 
                               y_var = "mean_ccimod", 
                               y_label = "CCI MODIS ('24-'25)", 
                               se_var = "se_ccimod", 
                               color_vals = cci_col, 
                               facet_var = "georeg") + 
  custom_annotate(0.05)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

#plot_ccimod2425_geo <- add_rel_ampl_annotation(plot_ccimod2425_geo, rel_pace_df_grouped, "mean_ccimod")
plot_ccimod2425_geo

plot_ccipace_geo <- create_yr_plot(pace_yr_georeg_summ, 
                                   x_var = "doy", 
                                   y_var = "mean_cci", 
                                   y_label = "CCI PACE ('24-'25)", 
                                   se_var = "se_cci", 
                                   color_vals = cci_col, 
                                   facet_var = "georeg") + 
  custom_annotate(0.05)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

#plot_ccipace_geo <- add_rel_ampl_annotation(plot_ccipace_geo, rel_pace_df_grouped, "mean_cci")
plot_ccipace_geo

plot_nirvmod1921_geo <- create_yr_plot(gedi_yr_georeg_summ, 
                                       x_var = "doymin", 
                                       y_var = "mean_nirv", 
                                       y_label = "NIRv MODIS ('19-'21)", 
                                       se_var = "se_nirv", 
                                       color_vals = cire_col, 
                                       facet_var = "georeg") + 
  custom_annotate(0.23)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

#plot_nirvmod1921_geo <- add_rel_ampl_annotation(plot_nirvmod1921_geo, rel_sif_df_grouped, "mean_nirv")
plot_nirvmod1921_geo

plot_nirvmod2425_geo <- create_yr_plot(pace_yr_georeg_summ, 
                                      x_var = "doy", 
                                      y_var = "mean_nirvmod", 
                                      y_label = "NIRv MODIS ('24-'25)", 
                                      se_var = "se_nirvmod", 
                                      color_vals = cire_col, 
                                      facet_var = "georeg") + 
  custom_annotate(0.23)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

#plot_nirvmod2425_geo <- add_rel_ampl_annotation(plot_nirvmod2425_geo, rel_pace_df_grouped, "mean_ccimod")
plot_nirvmod2425_geo

# SIF/PAR plot
plot_oco1921_geo <- create_yr_plot(gedi_yr_georeg_summ, 
                                  x_var = "doymin", 
                                  y_var = "mean_dsif740", 
                                  y_label = expression("OCO-3 SIF ('19-'21)"), 
                                  se_var = "se_dsif740", 
                                  color_vals = sif_col, 
                                  facet_var = "georeg") + 
  custom_annotate(0.23)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

#plot_oco1921_geo <- add_rel_ampl_annotation(plot_oco1921_geo, rel_sif_df_grouped, "mean_dsif740")
plot_oco1921_geo

plot_oco2425_geo <- create_yr_plot(pace_yr_georeg_summ, 
                                      x_var = "doy", 
                                      y_var = "mean_dsif740", 
                                      y_label = expression("OCO-3 SIF ('24-'25)"), 
                                      se_var = "se_dsif740", 
                                      color_vals = sif_col, 
                                      facet_var = "georeg") + 
  custom_annotate(0.23)+
  facet_wrap(vars(georeg), nrow = 1, labeller = labeller(georeg = georeg_labels))

#plot_oco2425_geo <- add_rel_ampl_annotation(plot_oco2425_geo, rel_sif_df_grouped, "mean_dsif740")
plot_oco2425_geo


georeg_plot <- (plot_oco1921_geo + theme(axis.title.x = element_blank())) /
  (plot_oco2425_geo + theme(axis.title.x = element_blank(),
                           strip.text = element_blank())) /
  (plot_nirvmod1921_geo + theme(axis.title.x = element_blank(),
                                strip.text = element_blank())) /
  (plot_nirvmod2425_geo + theme(axis.title.x = element_blank(),
                                strip.text = element_blank())) /
  (plot_ccimod1921_geo + theme(axis.title.x = element_blank(),
                               strip.text = element_blank())) /
  (plot_ccimod2425_geo + theme(axis.title.x = element_blank(),
                               strip.text = element_blank())) /
  (plot_ccipace_geo + theme(strip.text = element_blank())) +
  plot_layout(guides = "collect")+
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')',
                  tag_sep = ' ')
georeg_plot


#ggsave(paste0(figdir, "/PACE_georeg_trends.png"), georeg_plot, dpi = 300, width = 11, height = 8)
ggsave(paste0(figdir, "/supp_CCI_NIRv_different_years_georeg_trends_v2jun23.tiff"), georeg_plot, device = 'tiff', units = 'in', dpi = 600, width = 11, height = 8, compression = 'lzw')


###
# Correlations between years -----------------------------
###

#reorganize to focus on comparison variables
gedi_yr_geo_sel <- gedi_yr_georeg_summ %>% 
  dplyr::select(georeg, doymin, mean_cci, mean_nirv, mean_dsif740) %>% 
  rename(doy = doymin,
         mean_cci1921 = mean_cci,
         mean_nirv1921 = mean_nirv,
         mean_dsif1921 = mean_dsif740)
pace_yr_geo_sel <- pace_yr_georeg_summ %>% 
  dplyr::select(georeg, doy, mean_ccimod, mean_nirvmod, mean_dsif740) %>% 
  rename(mean_cci2425 = mean_ccimod,
         mean_nirv2425 = mean_nirvmod,
         mean_dsif2425 = mean_dsif740)

multi_yr_georeg <- gedi_yr_geo_sel %>% 
  left_join(., pace_yr_geo_sel, by = join_by(doy, georeg))

# RMSE
rmse <- function(obs, pred) {
  complete <- complete.cases(obs, pred)
  sqrt(mean((obs[complete] - pred[complete])^2))
}

# Pearson r with 95% CI via Fisher z-transform
pearson_r_ci <- function(x, y) {
  complete <- complete.cases(x, y)
  x <- x[complete]
  y <- y[complete]
  n <- length(x)
  ct <- cor.test(x, y, method = "pearson")
  
  tibble(
    r       = ct$estimate[[1]],
    r_lower = ct$conf.int[1],
    r_upper = ct$conf.int[2]
  )
}

# Pivot to long format so we can iterate cleanly over variables
multi_long <- multi_yr_georeg %>%
  pivot_longer(
    cols = -c(georeg, doy),
    names_to  = c("variable", "period"),
    names_pattern = "mean_(.+)(1921|2425)"
  ) %>%
  pivot_wider(names_from = period, values_from = value)

# Compute r (with CI), RMSE, normalized RMSE, and SD per georeg x variable
cross_yr_stats <- multi_long %>%
  group_by(georeg, variable) %>%
  summarise(
    n           = sum(complete.cases(`1921`, `2425`)),
    sd_1921     = sd(`1921`, na.rm = TRUE),
    sd_2425     = sd(`2425`, na.rm = TRUE),
    rmse_val    = rmse(`1921`, `2425`),
    #normalized RMSE here is the RMSE relative to mean seasonal amplitude
    # e.g., is the disagreement small relative to the seasonal amplitue?
    nrmse       = rmse_val / mean(c(sd_1921, sd_2425), na.rm = TRUE),
    r_df        = list(pearson_r_ci(`1921`, `2425`)),
    .groups = "drop"
  ) %>%
  unnest(r_df)

#clean it up
cross_yr_stats_clean <- cross_yr_stats %>%
  mutate(
    variable = recode(variable,
                      "cci"     = "MODIS CCI",
                      "nirv"    = "MODIS NIRv",
                      "dsif"    = "OCO-3 SIF"
    ),
    georeg = factor(georeg, levels = c("NWA", "NOA", "CA", "Southern")),
    # Format r as "0.85 [0.71, 0.93]" for readable
    r_conf = sprintf("%.2f [%.2f, %.2f]", r, r_lower, r_upper)
  ) %>%
  arrange(variable, georeg) %>%
  select(variable, georeg, n, sd_1921, sd_2425, rmse_val, nrmse, r, r_lower, r_upper, r_fmt)

print(cross_yr_stats_clean)


