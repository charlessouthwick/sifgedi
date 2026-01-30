
#VZA testing script

#read in .tif files. Each is 3 layers. We want time series trends of each of those layers.

#stack em up

#Create three line plots with the same axis showing mean + SD trends through time. Will need to extract with terra global function.

#Want to do this once for global amazon, once for each subregion.
rm(list=ls())
gc()

library(tidyverse)
library(terra)
library(patchwork)
library(gtools)


wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"
boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"

vzatestdir <- paste0(wd, "/troposif_data/vza_testing")

amz_vect <- vect(paste0(wd, "/amz_shps/amz_biome.shp"))
amz_geo_agg <- vect(paste0(wd, "/amz_shps/amz_geo_agg_extended.shp"))
newcrs <- "EPSG:4326"

se_fun <- function(x, na.rm = TRUE) {
  x <- if (na.rm) x[!is.na(x)] else x
  sd(x) / sqrt(length(x))
}

vza_files <- mixedsort(list.files(vzatestdir, pattern = "troposif_vzatest_", full.names = T))


global_stats <- map_dfr(vza_files, function(f) {
  
  r <- rast(f)
  
  # extract date from filename
  date_id <- str_extract(basename(f), "\\d{4}_doy\\d{1,3}")
  
  g_mean <- global(r, mean, na.rm = TRUE)
  g_sd   <- global(r, sd,   na.rm = TRUE)
  g_se   <- global(r, se_fun)
  
  tibble(
    date = date_id,
    region = "whole_amz",
    vza  = names(r),
    mean = g_mean[[1]],
    sd   = g_sd[[1]],
    se   = g_se[[1]]
  )
})

region_stats <- map_dfr(vza_files, function(f) {
  
  r <- rast(f)
  
  date_id <- str_extract(basename(f), "\\d{4}_doy\\d{1,3}")
  
  # mean
  m <- extract(r, amz_geo_agg, fun = mean, na.rm = TRUE)
  s <- extract(r, amz_geo_agg, fun = sd,   na.rm = TRUE)
  e <- extract(r, amz_geo_agg, fun = se_fun)
  
  # Convert each to long format
  m_long <- m %>%
    as_tibble() %>%
    mutate(region = amz_geo_agg$region) %>%
    pivot_longer(
      cols = -c(ID, region),
      names_to = "vza",
      values_to = "mean"
    )
  
  s_long <- s %>%
    as_tibble() %>%
    pivot_longer(
      cols = -ID,
      names_to = "vza",
      values_to = "sd"
    )
  
  e_long <- e %>%
    as_tibble() %>%
    pivot_longer(
      cols = -ID,
      names_to = "vza",
      values_to = "se"
    )
  
  # Combine
  m_long %>%
    left_join(s_long, by = c("ID", "vza")) %>%
    left_join(e_long, by = c("ID", "vza")) %>%
    mutate(date = date_id) %>% 
    dplyr::select(-ID)
})


all_stats <- bind_rows(global_stats, region_stats)

all_stats <- all_stats %>%
  separate(date, into = c("year", "doy"), sep = "_doy") %>%
  mutate(date = as.Date(as.numeric(doy) - 1, origin = paste0(year, "-01-01")))


all_stats <- all_stats %>%
  mutate(
    vza = factor(
      vza,
      levels = c(
        "sif743_cor_vza25",
        "sif743_cor_vza35",
        "sif743_cor_vza45"
      ),
      labels = c(
        "VZA_25",
        "VZA_35",
        "VZA_45"
      )
    ),
    region = factor(
      region,
      levels = c("CA", "NOA", "NWA", "Southern", "whole_amz"),
      labels = c(
        "Central Amz",
        "Northern Amz",
        "Northwest Amz",
        "Southern Amz",
        "Whole Amz"
      )
    )
  )


ggplot(
  all_stats,
  aes(
    x = date,
    y = mean,
    color = vza,
    fill  = vza,
    group = vza
  )
) +
  geom_point(size = 2.3, alpha = 0.7) +
  geom_line(alpha = 0.3, linewidth = 0.6) +
  
  geom_smooth(
    method = "gam",
    se = TRUE,
    linewidth = 1.2,
    alpha = 0.2
  ) +
  
  scale_color_viridis_d(
    option = "inferno",
    breaks = c("VZA_25", "VZA_35", "VZA_45"),
    labels = list(
      expression(VZA[thresh] == 25*degree),
      expression(VZA[thresh] == 35*degree),
      expression(VZA[thresh] == 45*degree)
    ),
    direction = -1,
    begin = 0.8,
    end = 0.1,
    name = "VZA"
  ) +
  scale_fill_viridis_d(
    option = "inferno",
    breaks = c("VZA_25", "VZA_35", "VZA_45"),
    labels = list(
      expression(VZA[thresh] == 25*degree),
      expression(VZA[thresh] == 35*degree),
      expression(VZA[thresh] == 45*degree)
    ),
    direction = -1,
    begin = 0.8,
    end = 0.1,
    name = "VZA"
  ) +
  
  coord_cartesian(ylim = c(0.32, 0.75)) +
  
  facet_wrap(~ region) +
  
  labs(
    x = "Date",
    y = "Mean Daylength Corrected SIF"
  ) +
  
  theme_classic(base_family = "serif") +
  theme(
    axis.title.y = element_text(size = 12),
    legend.position = "right"
  )
