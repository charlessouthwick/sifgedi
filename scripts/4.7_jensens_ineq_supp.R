
library(tidyverse)
library(terra)
library(parallel)

rm(list= ls())

boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"
wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"

yearid <- "2019"

#Thresholds for filtering
cf_thresh <- 0.2
sza_thresh <- 45 #Eliminates some high outliers at solstices
vza_thresh <- 35 #Retains ~ 45-60% of swath


tropopath <- paste0(wd, "/troposif_data")
ncpath <- paste0(wd, "/troposif_data/", yearid)
parent_dir_rast <- paste0(ncpath, "/complete_rast_", yearid)
outdir <- paste0(tropopath, "/sifpar_daily_testing")
final_dir <- paste0(boxwd, "/complete_data")

# List all date folders within ncpath and filter by name pattern
date_pattern <- "^\\d{4}\\.\\d{2}\\.\\d{2}$"
date_folders <- list.dirs(ncpath, full.names = TRUE, recursive = FALSE)
date_folders <- date_folders[grep(date_pattern, basename(date_folders))]

#List PAR files
pardir <- paste0(wd, "/MCD18C2_par_data", "/", yearid)
parraw <- paste0(pardir, "/raw")
pardaily <- paste0(pardir, "/daily_amz")

#This assumes that 'point' SIF data are dispersed evenly
#New Grid Structure
sifgrid <- rast(nrow = 600, ncol = 744, resolution = 0.05, extent = c(-80.5, -43.3, -21, 9), crs = "EPSG:4326")
amz_shp <- vect(paste0(wd, "/amz_shps/amz_biome.shp"))

num_cores <- 12 # ~4 minutes on Albert Lab Mac with no memory issues

# List all PAR files
allparfiles <- list.files(pardaily, pattern = "par_amz_daily_\\d{7}\\.tif$", full.names = TRUE)
allsiffiles <- unlist(lapply(date_folders, list.files, full.names = T))

# ---- PAR files ----
par_tbl <- tibble(
  par_file = allparfiles
) %>%
  mutate(
    doy = as.integer(str_extract(par_file, "\\d{7}") |> substr(5, 7)),
    year = as.integer(str_extract(par_file, "\\d{7}") |> substr(1, 4))
  )

# ---- SIF files ----
sif_tbl <- tibble(
  sif_file = allsiffiles
) %>%
  mutate(
    date = as.Date(
      basename(gsub(".csv", "", gsub("amz_TROPOSIF_L2B_", "", sif_file)))
    ),
    doy = yday(date),
    year = year(date)
  )

sifpar_tbl <- sif_tbl %>%
  inner_join(par_tbl, by = c("year", "doy")) %>%
  arrange(date)

process_sifpar_day <- function(i, sifpar_tbl) {
  
  sif_file <- sifpar_tbl$sif_file[i]
  par_file <- sifpar_tbl$par_file[i]
  filedoy  <- paste0("doy", sifpar_tbl$doy[i])
  
  df <- read.csv(sif_file)
  
  v <- vect(df, geom = c("lon", "lat"), crs = "EPSG:4326")
  
  v2 <- v[v$cf < cf_thresh & v$sza < sza_thresh & v$vza < vza_thresh, ]
  v2 <- v2[, !names(v2) %in% c("cf", "sza", "vza", "doy")]
  
  if (nrow(v2) == 0) {
    return(list(
      doy = filedoy,
      status = "no_valid_obs"
    ))
  }
  
  r_mean <- rasterize(v2, sifgrid, field = names(v2), fun = mean)
  r_n   <- rasterize(v2, sifgrid, field = "sif743", fun = length)
  
  names(r_mean) <- names(v2)
  names(r_n) <- "nsifobs"
  
  r_out <- c(r_mean, r_n)
  r_c <- crop(r_out, amz_shp, mask = TRUE)
  
  par_r <- rast(par_file)
  par_c <- crop(par_r, amz_shp, mask = TRUE)
  
  sif_par_c <- c(r_c, par_c)
  
  sif_par_c$sifpar <- sif_par_c$sif743_cor / (sif_par_c$par_daily * 1000)
  sif_par_c$sifspar <- sif_par_c$sif743 / (sif_par_c$par_daily * 1000)
  
  out_file <- file.path(
    outdir,
    paste0("amz_daily_sifpar_", yearid, "_", filedoy, ".tif")
  )
  
  writeRaster(sif_par_c, out_file, overwrite = TRUE)
  
  list(
    doy = filedoy,
    status = "ok",
    file = out_file
  )
  
}

results <- mclapply(
  X = seq_len(nrow(sifpar_tbl)),
  FUN = process_sifpar_day,
  sifpar_tbl = sifpar_tbl,
  mc.cores = num_cores
)

results_df <- bind_rows(results)

# Now compile into 16-day periods -------------------------------------------

amz_geoagg <- vect(paste0(boxwd, "/amz_shps/amz_geo_agg_extended.shp"))

#Georegions
geo_grid <- rasterize(amz_geoagg, sifgrid, field = "region")
names(geo_grid) <- "georeg"
geo_amz <- crop(geo_grid, amz_geoagg)

# Landcover mask
lc_tree <- rast(paste0(wd, "/mcd12c1_landcover/mcd12c1_2019_lc_masked_amz.tif"))
lc_tree <- project(lc_tree, geo_grid)
lc_tree <- crop(lc_tree, amz_geoagg)

daily_files <- list.files(
  outdir,
  pattern = "^amz_daily_sifpar_\\d{4}_doy\\d{1,3}\\.tif$",
  full.names = TRUE
)

daily_tbl <- tibble(
  file = daily_files
) %>%
  mutate(
    doy = as.integer(gsub("doy", "", str_extract(basename(file), "doy\\d+"))),
    year = as.integer(str_extract(basename(file), "\\d{4}")),
  ) %>%
  arrange(year, doy)

daily_tbl <- daily_tbl %>%
  mutate(
    doymin = ((doy - 1) %/% 16) * 16 + 1
  )

period_dirs <- split(daily_tbl, list(daily_tbl$year, daily_tbl$doymin), drop = TRUE)
#sanity check:
lapply(period_dirs[1:3], function(x) unique(x[, c("year", "doymin")]))

mean_rasters <- function(df) {
  
  r <- rast(df$file)
  doymin <- unique(df$doymin)
  yearid <- unique(df$year)
  
  # number of variables per day
  nl_day <- nlyr(rast(df$file[1]))
  
  # grouping index: same index for same variable across days
  index <- rep(seq_len(nl_day), times = nrow(df))
  
  r_mean <- tapp(
    r,
    index = index,
    fun = mean,
    na.rm = TRUE
  )
  
  # restore original variable names
  names(r_mean) <- names(rast(df$file[1]))
  
  sif_lcmsk <- mask(r_mean, lc_tree)
  
  sif_geo <- c(sif_lcmsk, geo_amz)
  
  sif_df <- terra::as.data.frame(sif_geo, xy = TRUE, na.rm = TRUE)
  
  truedate <- as.Date(doymin - 1, origin = paste0(yearid, "-01-01"))
  
  sif_df$doymin <- doymin
  sif_df$year <- yearid
  sif_df$truedate <- truedate
  
  sif_df
  
}

num_cores <- 12
sif_16day_df <- bind_rows(
  mclapply(
    period_dirs,
    mean_rasters,
    mc.cores = num_cores
  )
)



hist(sif_16day_df$sif743)
hist(sif_16day_df$sifpar)

sif_df_c <- sif_16day_df %>% 
  filter(sifpar > -1e-10 & sifpar < 5.5e-06)

hist(sif_df_c$sifpar)
hist(sif_df_c$sif743)
min(sif_df_c$sifpar)
max(sif_df_c$sifpar)


s_err <- function(x) sd(x, na.rm = T)/sqrt(sum(!is.na(x)))


df2 <- sif_df_c %>% 
  #filter(chlcar < 7 & chlcar > 0) %>%  #There are a few outlier points due to denominator 'explosion'
  mutate(year = factor(year, levels = c('2019', '2020', '2021')),
         georeg = factor(georeg, levels = c('NWA', 'NOA', 'CA', 'Southern'))) %>% 
  rename(sif743j = sif743,
         sif743_corj = sif743_cor,
         sifparj = sifpar,
         sifsparj = sifspar)


vars_noyr <- c("sif743_corj", "sif743j", "sifparj", "sifsparj", "doymin")  # includes doy
vars_yr <- c("sif743_corj", "sif743j", "sifparj", "sifsparj") #does not include doy

summarize_sif <- function(data, group_vars, vars_to_summarize, n_sif1 = "sif743j") {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(across(all_of(vars_to_summarize), 
                     list(mean = \(x) mean(x, na.rm = T), 
                          se = \(x) s_err(x)), 
                     .names = "{.fn}_{.col}"),
              ndsif = sum(!is.na(.data[[n_sif1]])),
              .groups = "drop")
}


# No year groupings
df_summ <- summarize_sif(df2, c("truedate"), vars_noyr)%>% 
  mutate(region = 'all') %>%
  rename(doymin = mean_doymin) %>%
  select(-se_doymin) %>% 
  select(region, everything())

df_georeg_summ <- df2 %>% 
  filter(!is.na(georeg)) %>%
  summarize_sif(., c("georeg", "truedate"), vars_noyr) %>% 
  rename(doymin = mean_doymin) %>% 
  select(-se_doymin)

#Year groupings
df_yr_summ <- summarize_sif(df2, "doymin", vars_yr) %>% 
  mutate(region = 'all') %>%
  select(region, everything())

df_yr_georeg_summ <- df2 %>% 
  filter(!is.na(georeg)) %>% 
  summarize_sif(., c("georeg", "doymin"), vars_yr)

# Write complete cases grouped datasets to csvs
write.csv(df_summ, paste0(final_dir, "/sifjentest_summ.csv"), row.names = FALSE)
write.csv(df_georeg_summ, paste0(final_dir, "/sifjentest_georeg_summ.csv"), row.names = FALSE)

write.csv(df_yr_summ, paste0(final_dir, "/sifjentest_yr_summ.csv"), row.names = FALSE)
write.csv(df_yr_georeg_summ, paste0(final_dir, "/sifjentest_yr_georeg_summ.csv"), row.names = FALSE)





