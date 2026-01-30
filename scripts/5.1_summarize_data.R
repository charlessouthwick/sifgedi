
rm(list=ls())
gc()

library(tidyverse)
library(RColorBrewer)
library(terra)
library(viridisLite)
library(viridis)
library(patchwork) #For arranging figures
library(lme4)
library(performance)

#wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"
boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"
#daterun_folder <- "/may17_fullyrs_complete"

# Setup --------------------------------------------------------
### Create a function for standard error
s_err <- function(x) sd(x, na.rm = T)/sqrt(sum(!is.na(x)))

complete_dir <- paste0(boxwd, "/complete_data")
figdir <- paste0(boxwd, "/figures")

#seasonality <- read.csv(paste0(complete_dir, "/dynamic_precip_seasonality.csv"))

gedi_2019_df <- read.csv(paste0(complete_dir, "/gedi_df_complete_2019.csv"))
gedi_2020_df <- read.csv(paste0(complete_dir, "/gedi_df_complete_2020.csv"))
gedi_2021_df <- read.csv(paste0(complete_dir, "/gedi_df_complete_2021.csv"))

#These are VERY large files and take a while to read in!
gedi_2019_naincl_df <- read.csv(paste0(complete_dir, "/gedi_df_complete_naincl_2019.csv"))
gedi_2020_naincl_df <- read.csv(paste0(complete_dir, "/gedi_df_complete_naincl_2020.csv"))
gedi_2021_naincl_df <- read.csv(paste0(complete_dir, "/gedi_df_complete_naincl_2021.csv"))

gedi_2019_df$year <- "2019"
gedi_2020_df$year <- "2020"
gedi_2021_df$year <- "2021"

gedi_2019_naincl_df$year <- "2019"
gedi_2020_naincl_df$year <- "2020"
gedi_2021_naincl_df$year <- "2021"

gedi_full_df <- rbind(gedi_2019_df, gedi_2020_df, gedi_2021_df)
gedi_full_naincl_df <- rbind(gedi_2019_naincl_df, gedi_2020_naincl_df, gedi_2021_naincl_df)


#rm(gedi_2019_naincl_df, gedi_2020_naincl_df, gedi_2021_naincl_df)

gedi_proc <- gedi_full_df %>%
  mutate(
    # Convert doymin to numeric
    doymin = as.numeric(doymin),
    
    # Convert 'samp_year' and 'doymin' to a proper date format
    truedate = as.Date(doymin - 1, origin = paste0(year, "-01-01")),
    
    # Create 'monthday' in "Aug-15" format
    monthday = format(truedate, "%b-%d"),
    
    truedate = format(truedate, "%Y-%m-%d"),
    
    vpsat = 6.11 * 10^((7.5 - tavg)/(237.3 + tavg)),
    vpd = vpsat - vapr
  )

gedi_naincl_proc <- gedi_full_naincl_df %>%
  mutate(
    # Convert doymin to numeric
    doymin = as.numeric(doymin),
    
    # Convert 'samp_year' and 'doymin' to a proper date format
    truedate = as.Date(doymin - 1, origin = paste0(year, "-01-01")),
    
    # Create 'monthday' in "Aug-15" format
    monthday = format(truedate, "%b-%d"),
    
    truedate = format(truedate, "%Y-%m-%d"),
    
    vpsat = 6.11 * 10^((7.5 - tavg)/(237.3 + tavg)),
    vpd = vpsat - vapr
  )


#Select variables
gedi_df <- gedi_proc %>%
  select(c(doymin, monthday, truedate, year, x, y, pai, pai_mean, pai_us, pai_toc, meanpavd, sdvfp, subregion, zone, georeg, georeg_agg, iprec, iprec_sd, prec, cwd, vpd, ndvi, nirv, nirvp, pri_nar, cci, apar, fpar, modis_lai, sif743, sif743_cor, sifs_par, sif_par, sifc_par, sif_apar, sif_csza, sifcor_csza, phif, ndvi_tropo, nirv_tropo_refl, nirv_tropo_rad, nirvp_tropo_rad, fesc, fesc_tropo_rad, sif_rel_tropo, phif_tropo_rad, sif_fesc_mod, sif_fesc_tr, number_of_hits))

gedi_naincl_df <- gedi_naincl_proc %>%
  select(c(doymin, monthday, truedate, year, x, y, pai, pai_mean, pai_us, pai_toc, meanpavd, sdvfp, subregion, zone, georeg, georeg_agg, iprec, iprec_sd, prec, cwd, vpd, ndvi, nirv, nirvp, pri_nar, cci, apar, fpar, modis_lai, sif743, sif743_cor, sifs_par, sif_par, sifc_par, sif_apar, sif_csza, sifcor_csza, phif, ndvi_tropo, nirv_tropo_refl, nirv_tropo_rad, nirvp_tropo_rad, fesc, fesc_tropo_rad, sif_rel_tropo, phif_tropo_rad, sif_fesc_mod, sif_fesc_tr, number_of_hits))


colSums(is.na(gedi_df))

hist(gedi_naincl_df$phif)
hist(gedi_naincl_df$phif_tropo_rad)

#Select filtering -- need tobe intentional about this!
gedi_df2 <- gedi_df %>% 
  na.omit() %>%  #Clean up the data frame for complete cases
  #filter(doymin < 353) %>% #Filter out the day at 353; this date has very little data and isn't a full 16-day period
  filter(phif > 0 & phif < 7e3) %>%  #There are a few outlier points
  filter(phif_tropo_rad > 0 & phif_tropo_rad < 6e-07) %>%  #There are a few outlier points
  filter(sif743_cor >= 0) %>% 
  filter(!is.infinite(sif_rel_tropo)) %>% 
  mutate(year = factor(year, levels = c('2019', '2020', '2021')),
         zone = factor(zone, levels = c('I', 'II', 'III', 'IV')),
         georeg_agg = factor(georeg_agg, levels = c('NWA', 'NOA', 'CA', 'Southern')))


gedi_naincl_df2 <- gedi_naincl_df %>% 
  #filter(doymin < 353) %>% #Filter out the day at 353
  filter(phif > 0 & phif < 7e3) %>%  #There are a few outlier points
  filter(phif_tropo_rad > 0 & phif_tropo_rad < 6e-07) %>%  #There are a few outlier points
  filter(sif743_cor >= 0) %>% 
  filter(!is.infinite(sif_rel_tropo)) %>% 
  mutate(year = factor(year, levels = c('2019', '2020', '2021')),
         zone = factor(zone, levels = c('I', 'II', 'III', 'IV')),
         georeg_agg = factor(georeg_agg, levels = c('NWA', 'NOA', 'CA', 'Southern')))

hist(gedi_naincl_df2$phif)
hist(gedi_naincl_df2$phif_tropo_rad)
hist(gedi_naincl_df2$sif_par)

cat("Processing results in", nrow(gedi_naincl_df2), "pixels use for summarizing\n")

# Filter to include any number of returns > 100
gedi_200n <- gedi_df2 %>% filter(number_of_hits > 200)
gedi_300n <- gedi_df2 %>% filter(number_of_hits > 300)
gedi_400n <- gedi_df2 %>% filter(number_of_hits > 400)
gedi_500n <- gedi_df2 %>% filter(number_of_hits > 500)

#Choose which dataset to analyze
gedithreshsel <- 300
gedi_sel <- paste0("gedi_", gedithreshsel, "n")
gedi_szn <- get(gedi_sel) #dataset used for statistics!

#The other one:
gediotherthresh <- 400
gedi_notsel <- paste0("gedi_", gediotherthresh, "n")
gedi_other <- get(gedi_notsel)

# How many matched points?
cat("Processing results in", nrow(gedi_szn), "matched GEDI pixels\n")

#Filter out PAI values for which there are fewer than 300 positive hits in a pixel
gedi_naincl_szn <- gedi_naincl_df2 %>%
  mutate(filter_flag = ifelse(!is.na(pai_toc) & number_of_hits < 300, FALSE, TRUE)) %>% 
  filter(filter_flag) %>% 
  select(-filter_flag)%>% 
  filter(!is.na(zone))

cat("Processing results in", nrow(gedi_naincl_szn), "matched SIF/PAR pixels\n")

# Define the list of variables to summarize
vars_noyr <- c("pai", "pai_toc", "pai_us", "modis_lai", "meanpavd", "sdvfp", "nirv", "nirvp", "fpar", "apar", "sif743", "sif743_cor", "sifcor_csza", "sif_par", "sifs_par", "sif_apar", "sifc_par", "phif", "fesc", "pri_nar", "cci", "sif_rel_tropo", "ndvi_tropo", "nirv_tropo_refl", "nirv_tropo_rad", "nirvp_tropo_rad", "phif_tropo_rad", "fesc_tropo_rad", "sif_fesc_mod", "sif_fesc_tr", "iprec", "cwd", "vpd", "doymin")  # includes doymin

vars_yr <- c("pai", "pai_toc", "pai_us", "modis_lai", "meanpavd", "sdvfp", "nirv", "nirvp", "fpar", "apar", "sif743", "sif743_cor", "sifcor_csza", "sif_par", "sifs_par", "sif_apar", "sifc_par", "phif", "fesc", "pri_nar", "cci", "sif_rel_tropo", "ndvi_tropo", "nirv_tropo_refl", "nirv_tropo_rad", "nirvp_tropo_rad", "phif_tropo_rad", "fesc_tropo_rad", "sif_fesc_mod", "sif_fesc_tr", "iprec", "cwd", "vpd") #does not include doymin

# Function to generate summaries with selectable variable sets
summarize_gedi <- function(data, group_vars, vars_to_summarize) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(across(all_of(vars_to_summarize), 
                     list(mean = \(x) mean(x, na.rm = T), 
                          se = \(x) s_err(x)), 
                     .names = "{.fn}_{.col}"),
              n_per_date = n(),
              .groups = "drop")
}

# # Function to mutate seasons
# mutate_szn <- function(data, earlydryind, earlywetind, otherwetind1, otherwetind2, middryind, latedryind) {
#   data %>%
#     mutate(
#       season = case_when(
#         doymin < earlydryind | doymin >= earlywetind ~ "Wet",
#         doymin >= earlydryind & doymin < earlywetind ~ "Dry"
#       ),
#       sub_szn = case_when(
#         doymin >= otherwetind1 & doymin < earlydryind | doymin >= otherwetind2 ~ "other_wet",
#         doymin >= earlydryind & doymin < middryind ~ "early_dry",
#         doymin >= middryind & doymin < latedryind ~ "mid_dry",
#         doymin >= latedryind & doymin < earlywetind ~ "late_dry",
#         doymin >= earlywetind & doymin < otherwetind2 ~ "early_wet"
#       )
#     )
# }


# Complete-cases dataset ------------------
# No year groupings
gedi_summ <- summarize_gedi(gedi_szn, c("truedate"), vars_noyr)%>% 
  mutate(zone = 'all') %>%
  rename(doymin = mean_doymin) %>%
  select(-se_doymin) %>% 
  select(zone, everything())

gedi_zone_summ <- summarize_gedi(gedi_szn, c("zone", "truedate"), vars_noyr) %>% 
  rename(doymin = mean_doymin) %>% 
  select(-se_doymin)

gedi_georeg_summ <- gedi_szn %>% 
  filter(!is.na(georeg_agg)) %>%
  summarize_gedi(., c("georeg_agg", "truedate"), vars_noyr) %>% 
  rename(doymin = mean_doymin) %>% 
  select(-se_doymin)

#Year groupings
gedi_yr_summ <- summarize_gedi(gedi_szn, "doymin", vars_yr) %>% 
  mutate(zone = 'all') %>%
  select(zone, everything())

gedi_yr_zone_summ <- summarize_gedi(gedi_szn, c("zone", "doymin"), vars_yr)

gedi_yr_georeg_summ <- gedi_szn %>% 
  filter(!is.na(georeg_agg)) %>% 
  summarize_gedi(., c("georeg_agg", "doymin"), vars_yr)

# Coerce 'truedate' to a date, then convert to a year
gedi_summ$truedate <- as.Date(gedi_summ$truedate)
gedi_zone_summ$truedate <- as.Date(gedi_zone_summ$truedate)
gedi_georeg_summ$truedate <- as.Date(gedi_georeg_summ$truedate)

gedi_summ$year <- as.character(year(gedi_summ$truedate))
gedi_zone_summ$year <- as.character(year(gedi_zone_summ$truedate))
gedi_georeg_summ$year <- as.character(year(gedi_georeg_summ$truedate))

#Now for the NA-inclusive dataset ------------------------

# No year groupings
gedi_naincl_summ <- gedi_naincl_szn %>% 
  summarize_gedi(., c("truedate"), vars_noyr)%>% 
  mutate(zone = 'all') %>%
  rename(doymin = mean_doymin) %>%
  select(-se_doymin) %>% 
  select(zone, everything())

gedi_zone_naincl_summ <- gedi_naincl_szn %>% 
  summarize_gedi(., c("zone", "truedate"), vars_noyr) %>% 
  rename(doymin = mean_doymin) %>% 
  select(-se_doymin)

gedi_georeg_naincl_summ <- gedi_naincl_szn %>% 
  filter(!is.na(georeg_agg)) %>% 
  summarize_gedi(., c("georeg_agg", "truedate"), vars_noyr) %>% 
  rename(doymin = mean_doymin) %>% 
  select(-se_doymin)

#Year groupings
gedi_yr_naincl_summ <- gedi_naincl_szn %>% 
  summarize_gedi(., "doymin", vars_yr) %>% 
  mutate(zone = 'all') %>%
  select(zone, everything())

gedi_yr_zone_naincl_summ <- gedi_naincl_szn %>% 
  summarize_gedi(., c("zone", "doymin"), vars_yr)

gedi_yr_georeg_naincl_summ <- gedi_naincl_szn %>% 
  filter(!is.na(georeg_agg)) %>% 
  summarize_gedi(., c("georeg_agg", "doymin"), vars_yr)

# Coerce 'truedate' to a date, then convert to a year
gedi_naincl_summ$truedate <- as.Date(gedi_naincl_summ$truedate)
gedi_zone_naincl_summ$truedate <- as.Date(gedi_zone_naincl_summ$truedate)
gedi_georeg_naincl_summ$truedate <- as.Date(gedi_georeg_naincl_summ$truedate)

gedi_naincl_summ$year <- as.character(year(gedi_naincl_summ$truedate))
gedi_zone_naincl_summ$year <- as.character(year(gedi_zone_naincl_summ$truedate))
gedi_georeg_naincl_summ$year <- as.character(year(gedi_georeg_naincl_summ$truedate))


# Write overall datasets
write.csv(gedi_szn, paste0(complete_dir, "/gedi_szn_", gedithreshsel, "n.csv"), row.names = FALSE)
write.csv(gedi_other, paste0(complete_dir, "/gedi_szn_", gediotherthresh, "n.csv"), row.names = FALSE)

# Write complete cases grouped datasets to csvs
write.csv(gedi_yr_summ, paste0(complete_dir, "/gedi_yr_summ.csv"), row.names = FALSE)
write.csv(gedi_yr_zone_summ, paste0(complete_dir, "/gedi_yr_zone_summ.csv"), row.names = FALSE)
write.csv(gedi_yr_georeg_summ, paste0(complete_dir, "/gedi_yr_georeg_summ.csv"), row.names = FALSE)

write.csv(gedi_summ, paste0(complete_dir, "/gedi_summ.csv"), row.names = FALSE)
write.csv(gedi_zone_summ, paste0(complete_dir, "/gedi_zone_summ.csv"), row.names = FALSE)
write.csv(gedi_georeg_summ, paste0(complete_dir, "/gedi_georeg_summ.csv"), row.names = FALSE)

# Write NA-inclusive grouped datasets to csvs
write.csv(gedi_yr_naincl_summ, paste0(complete_dir, "/gedi_yr_naincl_summ.csv"), row.names = FALSE)
write.csv(gedi_yr_zone_naincl_summ, paste0(complete_dir, "/gedi_yr_zone_naincl_summ.csv"), row.names = FALSE)
write.csv(gedi_yr_georeg_naincl_summ, paste0(complete_dir, "/gedi_yr_georeg_naincl_summ.csv"), row.names = FALSE)

write.csv(gedi_naincl_summ, paste0(complete_dir, "/gedi_naincl_summ.csv"), row.names = FALSE)
write.csv(gedi_zone_naincl_summ, paste0(complete_dir, "/gedi_zone_naincl_summ.csv"), row.names = FALSE)
write.csv(gedi_georeg_naincl_summ, paste0(complete_dir, "/gedi_georeg_naincl_summ.csv"), row.names = FALSE)




