
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

figdir <- paste0(boxwd, "/figures")
vzatestdir <- paste0(wd, "/troposif_data/vza_testing")
cftestdir <- paste0(wd, "/troposif_data/cf_testing")

amz_vect <- vect(paste0(wd, "/amz_shps/amz_biome.shp"))
amz_geo_agg <- vect(paste0(wd, "/amz_shps/amz_geo_agg_extended.shp"))
newcrs <- "EPSG:4326"

se_fun <- function(x, na.rm = TRUE) {
  x <- if (na.rm) x[!is.na(x)] else x
  sd(x) / sqrt(length(x))
}

vza_files <- mixedsort(list.files(vzatestdir, pattern = "troposif_vzatest_", full.names = T))
cf_files <- mixedsort(list.files(cftestdir, pattern = "troposif_cftest_", full.names = T))

global_stats <- function(f) {
  
  r <- rast(f)
  
  date_id <- stringr::str_extract(basename(f), "\\d{4}_doy\\d{1,3}")
  
  g_mean <- global(r, mean, na.rm = TRUE)
  g_sd   <- global(r, sd,   na.rm = TRUE)
  g_se   <- global(r, se_fun)
  
  tibble(
    date   = date_id,
    region = "whole_amz",
    testvar    = names(r),
    mean   = g_mean[[1]],
    sd     = g_sd[[1]],
    se     = g_se[[1]]
  )
}

region_stats <- function(f, regions) {
  
  r <- rast(f)
  
  date_id <- stringr::str_extract(basename(f), "\\d{4}_doy\\d{1,3}")
  
  m <- extract(r, regions, fun = mean, na.rm = TRUE)
  s <- extract(r, regions, fun = sd,   na.rm = TRUE)
  e <- extract(r, regions, fun = se_fun)
  
  m_long <- m %>%
    as_tibble() %>%
    mutate(region = regions$region) %>%
    pivot_longer(cols = -c(ID, region), names_to = "testvar", values_to = "mean")
  
  s_long <- s %>%
    as_tibble() %>%
    pivot_longer(cols = -ID, names_to = "testvar",values_to = "sd")
  
  e_long <- e %>%
    as_tibble() %>%
    pivot_longer(cols = -ID, names_to = "testvar", values_to = "se")
  
  #join up
  m_long %>%
    left_join(s_long, by = c("ID", "testvar")) %>%
    left_join(e_long, by = c("ID", "testvar")) %>%
    mutate(date = date_id) %>%
    select(-ID)
}

# vza_g_stats  <- bind_rows(lapply(vza_files, global_stats)) %>% rename(vza = testvar)
# vza_reg_stats  <- bind_rows(lapply(vza_files, region_stats, regions = amz_geo_agg)) %>% rename(vza = testvar)

#Run in parallel
num_cores <- 8
#Start with VZA
vza_g_stats  <- bind_rows(
  parallel::mclapply(vza_files, global_stats, mc.cores = num_cores)) %>%
  rename(vza = testvar)
vza_reg_stats  <- bind_rows(
  parallel::mclapply(vza_files, region_stats, regions = amz_geo_agg, mc.cores = num_cores)) %>%
  rename(vza = testvar)

vza_stats <- bind_rows(vza_g_stats, vza_reg_stats)

#Now for cf
cf_g_stats  <- bind_rows(
  parallel::mclapply(cf_files, global_stats, mc.cores = num_cores)) %>%
  rename(cf = testvar)
cf_reg_stats  <- bind_rows(
  parallel::mclapply(cf_files, region_stats, regions = amz_geo_agg, mc.cores = num_cores)) %>%
  rename(cf = testvar)

cf_stats <- bind_rows(cf_g_stats, cf_reg_stats)



#Supplemental plot for VZA -----------------------------------------

vza_stats <- vza_stats %>%
  separate(date, into = c("year", "doy"), sep = "_doy") %>%
  mutate(date = as.Date(as.numeric(doy) - 1, origin = paste0(year, "-01-01")))


vza_stats <- vza_stats %>%
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

vza_wide <- vza_stats %>%
  select(region, date, vza, mean) %>%
  tidyr::pivot_wider(names_from = vza, values_from = mean)

vza_compare <- vza_wide %>%
  group_by(region) %>%
  summarise(
    bias_35v25 = mean(VZA_35 - VZA_25, na.rm = TRUE),
    rmse_35v25 = sqrt(mean((VZA_35 - VZA_25)^2, na.rm = TRUE)),
    r_35v25 = cor(VZA_35, VZA_25, use = "complete.obs"),
    
    bias_45v25 = mean(VZA_45 - VZA_25, na.rm = TRUE),
    rmse_45v25 = sqrt(mean((VZA_45 - VZA_25)^2, na.rm = TRUE)),
    r_45v25 = cor(VZA_45, VZA_25, use = "complete.obs"),
    
    bias_45v35 = mean(VZA_45 - VZA_35, na.rm = TRUE),
    rmse_45v35 = sqrt(mean((VZA_45 - VZA_35)^2, na.rm = TRUE)),
    r_45v35 = cor(VZA_45, VZA_35, use = "complete.obs")
    
  )

vza_annot <- vza_compare %>%
  mutate(
    label = paste0(
      "VZA 35 vs. 25:  Δ = ", sprintf("%.3f", bias_35v25),
      ",  RMSE = ", sprintf("%.3f", rmse_35v25), ",  corr = ", sprintf("%.3f", r_35v25), "\n",
      "VZA 45 vs. 25:  Δ = ", sprintf("%.3f", bias_45v25),
      ",  RMSE = ", sprintf("%.3f", rmse_45v25), ",  corr = ", sprintf("%.3f", r_45v25), "\n"
    )
  )

vzap <- vza_stats %>% 
  ggplot(., aes(x = date, y = mean, color = vza, fill  = vza, group = vza)) +
  geom_point(size = 2.3, alpha = 0.7) +
  geom_line(alpha = 0.3, linewidth = 0.6) +
  geom_smooth(
    method = "gam", se = TRUE, linewidth = 1.2, alpha = 0.2
  ) +
  scale_color_viridis_d(
    option = "inferno", direction = -1, begin = 0.8, end = 0.1, name = "VZA",
    breaks = c("VZA_25", "VZA_35", "VZA_45"),
    labels = list(
      expression(VZA[thresh] == 25*degree),
      expression(VZA[thresh] == 35*degree),
      expression(VZA[thresh] == 45*degree))
  ) +
  scale_fill_viridis_d(
    option = "inferno", direction = -1, begin = 0.8, end = 0.1, name = "VZA",
    breaks = c("VZA_25", "VZA_35", "VZA_45"),
    labels = list(
      expression(VZA[thresh] == 25*degree),
      expression(VZA[thresh] == 35*degree),
      expression(VZA[thresh] == 45*degree))
  ) +
  coord_cartesian(ylim = c(0.32, 0.75)) +
  facet_wrap(~ region) +
  labs(
    x = "Date", y = "Mean Daylength-corrected SIF"
  ) +
  
  theme_classic(base_family = "serif") +
  theme(
    axis.title.y = element_text(size = 12),
    legend.position = "right")+
  geom_text(
    data = vza_annot,
    aes(x = as.Date("2019-01-15"), y = 0.74, label = label),
    inherit.aes = FALSE, hjust = 0, vjust = 1, size = 3.2, family = "serif"
  )

vzap

#Supplemental Plot for Cloud Fraction -------------------------------------

cf_stats <- cf_stats %>%
  separate(date, into = c("year", "doy"), sep = "_doy") %>%
  mutate(date = as.Date(as.numeric(doy) - 1, origin = paste0(year, "-01-01")))

cf_stats <- cf_stats %>%
  mutate(
    cf = factor(
      cf,
      levels = c(
        "sif743_cor_cf01",
        "sif743_cor_cf02",
        "sif743_cor_cf03"
      ),
      labels = c(
        "CF_0.1",
        "CF_0.2",
        "CF_0.3"
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

cf_wide <- cf_stats %>%
  select(region, date, cf, mean) %>%
  tidyr::pivot_wider(names_from = cf, values_from = mean)

cf_compare <- cf_wide %>%
  group_by(region) %>%
  summarise(
    bias_02v01 = mean(CF_0.2 - CF_0.1, na.rm = TRUE),
    rmse_02v01 = sqrt(mean((CF_0.2 - CF_0.1)^2, na.rm = TRUE)),
    r_02v01 = cor(CF_0.2, CF_0.1, use = "complete.obs"),
    
    bias_03v01 = mean(CF_0.3 - CF_0.1, na.rm = TRUE),
    rmse_03v01 = sqrt(mean((CF_0.3 - CF_0.1)^2, na.rm = TRUE)),
    r_03v01 = cor(CF_0.3, CF_0.1, use = "complete.obs"),
    
    bias_03v02 = mean(CF_0.3 - CF_0.2, na.rm = TRUE),
    rmse_03v02 = sqrt(mean((CF_0.3 - CF_0.2)^2, na.rm = TRUE)),
    r_03v02 = cor(CF_0.3, CF_0.2, use = "complete.obs")
  )

cf_annot <- cf_compare %>%
  mutate(
    label = paste0(
      "CF 0.2 vs. 0.1:  Δ = ", sprintf("%.3f", bias_02v01),
      ",  RMSE = ", sprintf("%.3f", rmse_02v01), ",  corr  = ", sprintf("%.3f", r_02v01), "\n",
      "CF 0.3 vs. 0.1:  Δ = ", sprintf("%.3f", bias_03v01),
      ",  RMSE = ", sprintf("%.3f", rmse_03v01), ",  corr  = ", sprintf("%.3f", r_03v01), "\n"
    )
  )

cfp <- cf_stats %>% 
  ggplot(., aes(x = date, y = mean, color = cf, fill  = cf, group = cf)) +
  geom_point(size = 2.3, alpha = 0.7) +
  geom_line(alpha = 0.3, linewidth = 0.6) +
  geom_smooth(method = "gam", se = TRUE, linewidth = 1.2,alpha = 0.2) +
  scale_color_viridis_d(
    option = "plasma", direction = -1, begin = 0.8, end = 0.1, name = "CF",
    breaks = c("CF_0.1", "CF_0.2", "CF_0.3"),
    labels = list(
      expression(CF[thresh] == 0.1),
      expression(CF[thresh] == 0.2),
      expression(CF[thresh] == 0.3)
    )) +
  scale_fill_viridis_d(
    option = "plasma", direction = -1, begin = 0.8, end = 0.1, name = "CF",
    breaks = c("CF_0.1", "CF_0.2", "CF_0.3"),
    labels = list(
      expression(CF[thresh] == 0.1),
      expression(CF[thresh] == 0.2),
      expression(CF[thresh] == 0.3)
    )) +
  coord_cartesian(ylim = c(0.32, 0.75)) +
  facet_wrap(~ region) +
  labs(
    x = "Date",
    y = "Mean Daylength-corrected SIF"
  ) +
  theme_classic(base_family = "serif") +
  theme(axis.title.y = element_text(size = 12), legend.position = "right"
  ) +
  geom_text(
    data = cf_annot,
    aes(x = as.Date("2019-01-15"), y = 0.74, label = label),
    inherit.aes = FALSE, hjust = 0, vjust = 1, size = 3.2, family = "serif"
  )

cfp
  
#Save plots
ggsave(paste0(figdir, "/vza_supp_georeg_trends.tiff"), device = 'tiff', vzap, dpi = 600, width = 13, height = 9, compression = 'lzw')
ggsave(paste0(figdir, "/cf_supp_georeg_trends.tiff"), device = 'tiff', cfp, dpi = 600, width = 13, height = 9, compression = 'lzw')

