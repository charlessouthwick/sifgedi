
#Charles Southwick


library(tidyverse)


wd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"

amz_vect <- vect(paste0(wd, "/amz_shps/amz_geo_agg_extended.shp"))

csv_dir <- paste0(wd, "/pace_vi_data/clean_csvs")
final_dir <- paste0(wd, "/complete_data")

pacecsvs <- unlist(list.files(csv_dir, pattern="*csv", full.names=T))

pacelist <- list()

for (i in seq_along(pacecsvs)) {
 
  pacefile <- basename(pacecsvs[i])
  pacedate <- sub("pace_amz_df_(\\d{8})\\.csv", "\\1", pacefile)
  
  # Extract year
  paceyear <- substr(pacedate, 1, 4)
  
  # Convert to Date and get day of year
  date_obj <- as.Date(pacedate, format = "%Y%m%d")
  pacedoy <- as.integer(strftime(date_obj, format = "%j"))
  
  pace_df <- read.csv(pacecsvs[i])
  
  pace_df$year <- paceyear
  pace_df$doy <- pacedoy
  pace_df$truedate <- pacedate
  
  pacelist[[i]] <- pace_df
   
}

pace_fulldf <- do.call(rbind, pacelist)

write.csv(pace_fulldf, paste0(final_dir, "/pace_df_complete.csv"), row.names = FALSE)


########
## Summarize -------------------------------------

s_err <- function(x) sd(x, na.rm = T)/sqrt(sum(!is.na(x)))


df2 <- pace_fulldf %>% 
  na.omit() %>%
  filter(chlcar < 7 & chlcar > 0) %>%  #There are a few outlier points due to denominator 'explosion'
  filter(car < 15) %>% 
  filter(cire < 6) %>% 
  mutate(year = factor(year),
         georeg = factor(georeg, levels = c('NWA', 'NOA', 'CA', 'Southern')))


vars_noyr <- c("ndvi", "cci", "pri", "cire", "car", "chlcar", "doy")  # includes doy
vars_yr <- c("ndvi", "cci", "pri", "cire", "car", "chlcar") #does not include doymin

summarize_pace<- function(data, group_vars, vars_to_summarize) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(across(all_of(vars_to_summarize), 
                     list(mean = \(x) mean(x, na.rm = T), 
                          se = \(x) s_err(x)), 
                     .names = "{.fn}_{.col}"),
              n_per_date = n(),
              .groups = "drop")
}

# Complete-cases dataset ------------------
# No year groupings
df_summ <- summarize_pace(df2, c("truedate"), vars_noyr)%>% 
  mutate(region = 'all') %>%
  rename(doy = mean_doy) %>%
  select(-se_doy) %>% 
  select(region, everything())

df_georeg_summ <- df2 %>% 
  filter(!is.na(georeg)) %>%
  summarize_pace(., c("georeg", "truedate"), vars_noyr) %>% 
  rename(doymin = mean_doy) %>% 
  select(-se_doy)

#Year groupings
df_yr_summ <- summarize_pace(df2, "doy", vars_yr) %>% 
  mutate(region = 'all') %>%
  select(region, everything())

df_yr_georeg_summ <- df2 %>% 
  filter(!is.na(georeg)) %>% 
  summarize_pace(., c("georeg", "doy"), vars_yr)

# Write complete cases grouped datasets to csvs
write.csv(df_summ, paste0(final_dir, "/pace_summ.csv"), row.names = FALSE)
write.csv(df_georeg_summ, paste0(final_dir, "/pace_georeg_summ.csv"), row.names = FALSE)

write.csv(df_yr_summ, paste0(final_dir, "/pace_yr_summ.csv"), row.names = FALSE)
write.csv(df_yr_georeg_summ, paste0(final_dir, "/pace_yr_georeg_summ.csv"), row.names = FALSE)


