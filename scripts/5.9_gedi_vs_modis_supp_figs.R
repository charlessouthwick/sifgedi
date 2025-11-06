
# GEDI PAI vs MODIS LAI, at MODIS resolution (250 m)
rm(list=ls())

library(tidyverse)
library(viridis)
library(patchwork)

#Setup --------------------------------------------------------

#Set up directories
#wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"
boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"
complete_dir <- paste0(boxwd, "/complete_data")
figdir <- paste0(boxwd, "/figures")

seasonality <- read.csv(paste0(complete_dir, "/dynamic_precip_seasonality.csv"))
glob_season <- read.csv(paste0(complete_dir, "/global_precip_seasonality.csv"))

gedimod19 <- read.csv(paste0(complete_dir, "/gedi_fpar_combined_2019.csv"))
gedimod20 <- read.csv(paste0(complete_dir, "/gedi_fpar_combined_2020.csv"))
gedimod21 <- read.csv(paste0(complete_dir, "/gedi_fpar_combined_2021.csv"))

gedimod <- rbind(gedimod19, gedimod20, gedimod21)

gedimod <- gedimod %>% rename(georeg_agg = region)

seasonality<- seasonality %>%
  filter(georeg_agg %in% c("NWA", "NOA", "CA", "Southern")) %>%
  mutate(georeg_agg = factor(georeg_agg, levels = c("NWA", "NOA", "CA", "Southern"))) %>%
  droplevels()

gedimod <- gedimod %>% left_join(seasonality, by = "georeg_agg")

gedimod <- gedimod %>%
  filter(georeg_agg %in% c("NWA", "NOA", "CA", "Southern")) %>%
  mutate(georeg_agg = factor(georeg_agg, levels = c("NWA", "NOA", "CA", "Southern"))) %>%
  droplevels()

gedimod <- gedimod %>%
  rename('doymin' = 'doymin_median') %>% 
  mutate(doymin = case_when(
    doymin == 8 ~ 1,
    doymin == 50 ~ 49,
    doymin == 108 ~ 97,
    doymin == 115 ~ 113,
    doymin == 258 ~ 257,
    doymin == 274 ~ 273,
    doymin == 290 ~ 289,
    doymin == 323 ~ 321,
    doymin == 338 ~ 337,
    TRUE ~ doymin
  ))

#test this
#gedimod <- cbind(gedimod, glob_season)

#gedimod <- gedimod %>% left_join(seasonality, by = "georeg_agg")

# Set up constants ------------------------------

# Create a function for standard error
s_err <- function(x) sd(x)/sqrt(length(x))

# otherwetind1 <- 1
# earlydryind <- 145
# middryind <- 193
# latedryind <- 225
# earlywetind <- 273
# otherwetind2 <- 321

#Color palette for peak wet, dry, and early wet
drycol <- "grey80"
earlywetcol <- "grey45"
peakwetcol <- "grey10"

szn_cols <- c(drycol, earlywetcol, peakwetcol)
sub_szn_levels <- c('dry', 'earlywet', 'peakwet')

# Colors for variables
mod_col <- "#D95F02"
toc_col <- "#BC5090"
pai_col <- "#6A4C93"

color_vals <- c("Canopy PAI" = toc_col, "Total PAI" = pai_col, "MODIS LAI" = mod_col)

# For adding vertical lines to plots to demarcate seasons
vline_xvals <- c(drycol, earlywetcol, peakwetcol)

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
                  inherit.aes = FALSE, hjust = -0.1, size = 2),
        geom_text(data = seasonality, aes(x = earlywet_start, y = y_text_pos, label = "DWT"), 
                  inherit.aes = FALSE, hjust = -0.1, size = 2),
        geom_text(data = seasonality, aes(x = wet_start, y = y_text_pos, label = "PW"), 
                  inherit.aes = FALSE, hjust = -0.1, size = 2)
      )
    } else {
      NULL
    }
  )
}

#Annotates for variables
annotation_positions <- data.frame(
  x = c(70, 70, 70),      # Example x positions (center of DOY range)
  y = c(6.6, 4.7, 2.0),      # Positions above the max values of each line
  label = c("MODIS LAI", "PAI", "Canopy PAI")
)

georeg_labels <- c(
  "NWA" = "Northwest",
  "NOA" = "Northern",
  "CA"  = "Central",
  "Southern" = "Southern"
)


#Group
gedimod_summ <- gedimod %>% 
  group_by(georeg_agg, doymin) %>% 
  summarize(mean_pai = mean(pai_median),
            se_pai = s_err(pai_median),
            mean_pai_toc = mean(pai_toc_median),
            se_pai_toc = s_err(pai_toc_median),
            mean_modlai = mean(Lai_500m),
            se_modlai = s_err(Lai_500m),
            n_per_date = n(),
            .groups = "drop")  # Drop to ungroup, keeps georeg_agg)

#Reshape
gedimod_summ_long <- gedimod_summ %>%
  pivot_longer(cols = c(mean_pai_toc, mean_pai, mean_modlai),
               names_to = "variable",
               values_to = "value") %>%
  mutate(se = case_when(
    variable == "mean_pai_toc" ~ se_pai_toc,
    variable == "mean_pai" ~ se_pai,
    variable == "mean_modlai" ~ se_modlai
  ),
  variable_label = case_when(
    variable == "mean_pai_toc" ~ "Canopy PAI",
    variable == "mean_pai" ~ "Total PAI",
    variable == "mean_modlai" ~ "MODIS LAI"
  ))

unique(gedimod_summ_long$georeg_agg)
levels(gedimod_summ_long$georeg_agg)

# Create the combined plot with annotations
plot_comb_pai <- ggplot(gedimod_summ_long, 
                        aes(x = as.numeric(as.character(doymin)),
                            y = value,
                            color = variable_label,
                            group = variable_label)) +
  geom_line(linewidth = rel(1.2)) +
  geom_point(size = rel(2)) +
  geom_errorbar(aes(ymin = value - se, ymax = value + se), 
                linewidth = 0.3, alpha = 0.9) +
  labs(x = "Day of Year", 
       y = expression("LAI Product (" * m^2/m^2 * ")"), 
       color = "LAI product") +
  theme_classic() +
  theme(
    legend.key.size = unit(0.5, "lines"),  # 50% of default size
    legend.text = element_text(size = rel(0.5))  # 50% of base text size
  ) +
  scale_color_manual(values = color_vals) +
  scale_y_continuous(limits = c(0.7, 7), labels = scales::label_number(accuracy = 0.01))+
  annotate("text", x = annotation_positions$x[1], y = annotation_positions$y[1], 
           label = annotation_positions$label[1], color = "black", size = 3) +
  annotate("text", x = annotation_positions$x[2], y = annotation_positions$y[2], 
           label = annotation_positions$label[2], color = "black", size = 3) +
  annotate("text", x = annotation_positions$x[3], y = annotation_positions$y[3], 
           label = annotation_positions$label[3], color = "black", size = 3)+
  facet_wrap(~georeg_agg, labeller = as_labeller(georeg_labels))+
  custom_annotate(y_text_pos = 0.7)

plot_comb_pai

cor(gedimod_summ$mean_pai, gedimod_summ$mean_modlai)

# Heat map plot -----------------------------------

gedimod <- gedimod %>%
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
  )

gedimod <- gedimod %>%
  mutate(sub_szn = factor(sub_szn, levels = c('dry', 'earlywet', 'peakwet', 'other')))

gediheat <- gedimod %>%
  filter(sub_szn %in% c('dry', 'earlywet', 'peakwet')) %>%
  ggplot(aes(x = pai_median, y = Lai_500m)) +
  geom_bin2d(bins = 75) +  # Adjust bins for granularity
  scale_fill_viridis_c(option = "plasma", name = "Density", alpha = 0.8) +
  geom_smooth(method = "lm", linewidth = rel(2), color = "red")+
  theme_classic() +
  theme(
    legend.key.size = unit(0.5, "lines"),  # 50% of default size
    legend.text = element_text(size = rel(0.5))  # 50% of base text size
  ) +
  labs(x = expression("Gridded Median PAI (" * m^2/m^2 * ")"),
       y = expression("MODIS LAI (" * m^2/m^2 * ")")) +
  geom_hline(yintercept = 7, col = 'black', lwd = 1.3, linetype = 'dashed')+
  facet_wrap(~georeg_agg, labeller = as_labeller(georeg_labels))
gediheat

# Boxplots ---------------------------

# Define a function to create boxplots
create_violin <- function(data, y_var, yformal, colors, y_limits = NULL, sub_szn_levels = c('dry', 'earlywet', 'peakwet')) {
  data %>%
    filter(sub_szn %in% sub_szn_levels) %>%
    mutate(sub_szn = factor(sub_szn, levels = sub_szn_levels)) %>%
    ggplot(aes(x = sub_szn, y = .data[[y_var]], fill = sub_szn)) +
    geom_violin(alpha = 0.8) +
    labs(y = yformal, x = NULL) +
    theme_minimal() +
    theme(
      legend.key.size = unit(0.5, "lines"),  # 50% of default size
      legend.text = element_text(size = rel(0.5))  # 50% of base text size
    ) +
    scale_x_discrete(labels = c("DRY", "DWT", "PW")) +
    scale_fill_manual(values = colors,
                      labels = c("DRY", "DWT", "PW"),
                      name = "Season")+
    (if (!is.null(y_limits)) scale_y_continuous(limits = y_limits) else NULL)
}

# Ensure levels in data match the expected levels
plotgedi <- gedimod %>%
  mutate(sub_szn = factor(sub_szn, levels = sub_szn_levels))

# Create individual plots
plot1 <- create_violin(plotgedi, "pai_median", expression("Overall PAI (" * m^2/m^2 * ")"), szn_cols, y_limits = c(0, 7))
plot2 <- create_violin(plotgedi, "pai_toc_median", expression("Canopy PAI (" * m^2/m^2 * ")"), szn_cols, y_limits = c(0, 7))
plot3 <- create_violin(plotgedi, "Lai_500m", expression("MODIS LAI (" * m^2/m^2 * ")"), szn_cols, y_limits = c(0, 7))


plot1
plot2
plot3

# Combine plots -------------------------------


combined_plot <- (
  ((plot1 | plot2 | plot3) + plot_layout(guides = "collect")) /
    ((gediheat) | (plot_comb_pai))
) +
  plot_layout(heights = c(1, 2.5)) +
  plot_annotation(tag_levels = list(c('(a)', '', '', '(b)', '(c)')))
combined_plot

#ggsave(paste0(figdir, "/compare_gedi_vs_modis.png"), combined_plot, dpi = 300, width = 10, height = 7)
ggsave(paste0(figdir, "/compare_gedi_vs_modis.tiff"), combined_plot, device = 'tiff', units = 'in', dpi = 600, width = 10, height = 7, compression = 'lzw')


