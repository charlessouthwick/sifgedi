## Join MCD19A1 CMGO + CMGL 16-day composites, compute CCI, apply landcover and
## georegion layers, and write tidy + summarized CSVs for the supplemental
## satellite analysis (companion to OCO-3 script 4.9).
##
## CCI = (b11 - b1) / (b11 + b1), where
##   b11 (~531 nm) comes from CMGO and
##   b1  (~645 nm, red) comes from CMGL.
## Computed here from COMPOSITED reflectance (ratio-of-means), after each product
## was independently QA-masked and 16-day-composited in 4.10 / 4.11.
##
## Note this appraoch mirrors the back half of 4.9 (OCO-3): mask to landcover-tree pixels, attach
## georegion, melt to a long data frame with year/doy/truedate, then summarize
## into four grouped CSVs (overall-by-date, georegion-by-date, overall-by-doy,
## georegion-by-doy). This is slightly different from the main body analysis, which used fixed DOYs instead of true dates. This will allow us to link PACE with OCO-3 and MAIAC CCI.

rm(list = ls())
gc()

library(tidyverse)
library(terra)
library(lubridate)

# ---- Paths -------------------------------------------------------------------
wd     <- "/Users/charlessouthwick/Documents/PhD/sifgedi"
boxwd  <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"
final_dir <- paste0(boxwd, "/complete_data")

cmgo_compiled <- paste0(wd, "/mcd19a1cmgo_data_2024thru2025/compiled")
cmgl_compiled <- paste0(wd, "/mcd19a1cmgl_data_2024thru2025/compiled")

# ---- Reference grid + extent (native 0.05 deg MAIAC CMG) ---------------------
# We do NOT resample to the 0.01 deg OCO-3 grid; per request everything stays on
# the native ~0.05 deg MAIAC grid. We take the CMGO composite grid as the
# canonical target and align CMGL / ancillary layers to it.
amz_vect <- vect(paste0(boxwd, "/amz_shps/amz_geo_agg_extended.shp"))

# Use one CMGO composite to define the target grid geometry.
cmgo_files <- list.files(cmgo_compiled, pattern = "cmgo_refl_.*_16day\\.tif$",
                         full.names = TRUE)
cmgl_files <- list.files(cmgl_compiled, pattern = "cmgl_refl_.*_16day\\.tif$",
                         full.names = TRUE)

stopifnot(length(cmgo_files) > 0, length(cmgl_files) > 0)

ref_grid <- rast(cmgo_files[1])[[1]]   # geometry only

# ---- Ancillary layers, aligned to the native MAIAC grid ----------------------
# Georegions
geo_grid <- rasterize(amz_vect, ref_grid, field = "region")
names(geo_grid) <- "georeg"
geo_amz <- crop(geo_grid, amz_vect)

# Landcover tree mask (projected/resampled onto the MAIAC grid)
lc_tree <- rast(paste0(wd, "/mcd12c1_landcover/mcd12c1_2019_lc_masked_amz.tif"))
lc_tree <- project(lc_tree, ref_grid)
lc_tree <- crop(lc_tree, amz_vect)

# ---- Match CMGO and CMGL composites by period date ---------------------------
date_from_name <- function(paths, prefix) {
  as.character(
    str_match(basename(paths), paste0(prefix, "_refl_(\\d{8})_16day"))[, 2]
  )
}

cmgo_tbl <- tibble(cmgo_file = cmgo_files,
                   datename  = date_from_name(cmgo_files, "cmgo"))
cmgl_tbl <- tibble(cmgl_file = cmgl_files,
                   datename  = date_from_name(cmgl_files, "cmgl"))

# Inner join: keep only periods present in BOTH products (CCI needs both bands).
pair_tbl <- inner_join(cmgo_tbl, cmgl_tbl, by = "datename") %>%
  arrange(datename)

calc_cci <- function(band11, band1) { (band11 - band1) / (band11 + band1) }

# ---- Per-period join, CCI, mask, melt to long --------------------------------
ccilist <- list()
counter <- 0

for (i in seq_len(nrow(pair_tbl))) {
  datename <- pair_tbl$datename[i]
  
  cmgo_r <- rast(pair_tbl$cmgo_file[i])   # refl_b11, refl_b12
  cmgl_r <- rast(pair_tbl$cmgl_file[i])   # refl_b1, refl_b2, ndvi, nirv
  
  # Guard against a one-cell offset between the two CMG products before the
  # ratio. Native resolution is preserved; this only snaps geometry to ref_grid.
  # cmgo_b11 <- resample(cmgo_r$refl_b11, ref_grid, method = "near")
  # cmgl_b1  <- resample(cmgl_r$refl_b1,  ref_grid, method = "near")
  # ndvi_r   <- resample(cmgl_r$ndvi,     ref_grid, method = "near")
  # nirv_r   <- resample(cmgl_r$nirv,     ref_grid, method = "near")
  
  cmg_all <- c(cmgl_r, cmgo_r)
  
  cci <- lapp(c(cmg_all$refl_b11, cmg_all$refl_b1), fun = calc_cci)
  names(cci) <- "cci"
  
  veg_stack <- c(cmg_all, cci)
  
  veg_crop  <- crop(veg_stack, amz_vect, mask = T)
  veg_lcmsk <- mask(veg_crop, lc_tree)
  veg_geo   <- c(veg_lcmsk, geo_amz)
  
  veg_df <- terra::as.data.frame(veg_geo, xy = TRUE, na.rm = TRUE)
  if (nrow(veg_df) == 0) next
  
  date_obj <- ymd(datename)
  veg_df$year    <- year(date_obj)
  veg_df$doy     <- yday(date_obj)
  veg_df$truedate <- date_obj
  
  counter <- counter + 1
  ccilist[[counter]] <- veg_df
  cat("Processed period:", datename, "(", nrow(veg_df), "pixels )\n")
}

cci_df_c <- do.call(rbind, ccilist)

write.csv(cci_df_c,
          paste0(final_dir, "/maiac_nirv_cci_df_complete_2024thru2025.csv"),
          row.names = FALSE)

# ---- Summaries (mirror 4.9 structure) ----------------------------------------
s_err <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

df2 <- cci_df_c %>%
  mutate(year   = factor(year, levels = c("2024", "2025")),
         georeg = factor(georeg, levels = c("NWA", "NOA", "CA", "Southern")))

vars_noyr <- c("cci", "ndvi", "nirv", "doy")  # includes doy
vars_yr   <- c("cci", "ndvi", "nirv")         # excludes doy

summarize_maiac <- function(data, group_vars, vars_to_summarize, n_var = "cci") {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(across(all_of(vars_to_summarize),
                     list(mean = \(x) mean(x, na.rm = TRUE),
                          se   = \(x) s_err(x)),
                     .names = "{.fn}_{.col}"),
              ncci = sum(!is.na(.data[[n_var]])),
              .groups = "drop")
}

# No-year groupings (by true 16-day date)
df_summ <- summarize_maiac(df2, c("truedate"), vars_noyr) %>%
  mutate(region = "all") %>%
  rename(doy = mean_doy) %>%
  select(-se_doy) %>%
  select(region, everything())

df_georeg_summ <- df2 %>%
  filter(!is.na(georeg)) %>%
  summarize_maiac(c("georeg", "truedate"), vars_noyr) %>%
  rename(doy = mean_doy) %>%
  select(-se_doy)

# Year-pooled groupings (by DOY)
df_yr_summ <- summarize_maiac(df2, "doy", vars_yr) %>%
  mutate(region = "all") %>%
  select(region, everything())

df_yr_georeg_summ <- df2 %>%
  filter(!is.na(georeg)) %>%
  summarize_maiac(c("georeg", "doy"), vars_yr)

write.csv(df_summ,
          paste0(final_dir, "/maiac_nirv_cci_summ_2024thru2025.csv"), row.names = FALSE)
write.csv(df_georeg_summ,
          paste0(final_dir, "/maiac_nirv_cci_georeg_summ_2024thru2025.csv"), row.names = FALSE)
write.csv(df_yr_summ,
          paste0(final_dir, "/maiac_nirv_cci_yr_summ_2024thru2025.csv"), row.names = FALSE)
write.csv(df_yr_georeg_summ,
          paste0(final_dir, "/maiac_nirv_cci_yr_georeg_summ_2024thru2025.csv"), row.names = FALSE)
