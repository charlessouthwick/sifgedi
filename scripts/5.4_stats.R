##New stat analysis

rm(list=ls())

library(tidyverse)#
library(GGally)#
library(usdm)
library(performance) #
library(viridis) #
library(rstatix) #
library(AICcmodavg) #
library(lme4) # # package used to run mixed models
library(nlme)
library(MuMIn) #
library(ggh4x)# #for at_panel function
library(broom.mixed) # for tidy model stats
library(kableExtra) # for latex style table
library(partR2) # for partial R2 calculation.
library(spdep)
library(sf)
library(mgcv)
library(gstat)
library(gratia) #for drawing plots
library(gstat) #for semivariogram functionality
library(ggcorrplot)

#Quick Processing --------------------------------------------------------

#Set up directories
#wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"
boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"
#daterun_folder <- "/may17_fullyrs_complete"

complete_dir <- paste0(boxwd, "/complete_data")
figdir <- paste0(boxwd, "/figures")

seasonality <- read.csv(paste0(complete_dir, "/dynamic_precip_seasonality.csv"))

#Alternative
#complete_dir <- paste0(boxwd, "/complete_data", daterun_folder)

sznlvls <- c('dry', 'earlywet', 'peakwet', 'other')
subsznlvls <- c('dry', 'earlywet', 'peakwet')
georeglvls <- c("Southern", "CA", "NWA", "NOA")

# Read in datasets ----- DO YOU WANT TO USE 400 or 300????
gedi_szn <- read.csv(paste0(complete_dir, "/gedi_szn_300n.csv"))

gedi_szn <- gedi_szn %>% left_join(seasonality, by = "georeg_agg")

gedi_szn <- gedi_szn %>% 
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

sum(is.na(gedi_szn))


gedi_szn <- gedi_szn %>%
  #na.omit() %>%               # NEED TO INCLUDE THIS IF NOT INCLUDED IN EARLIER DATASET!!
  mutate(year = factor(year),
         sub_szn = factor(sub_szn, levels = sznlvls),
         doymin = factor(as.character(doymin)),
         zone = factor(zone),
         georeg_agg = factor(georeg_agg, levels = georeglvls)
         ) %>% 
 filter(fpar > 0.4) #one outlier point here

gedi_3szn <- gedi_szn %>% filter(sub_szn %in% subsznlvls)

#Plotting setup
# Colors for yearly data 
yr_vir_pal <- viridis(n = 3, option = "C", end = 0.8)
col2019 <- yr_vir_pal[1]
col2020 <- yr_vir_pal[2]
col2021 <- yr_vir_pal[3]

color_vals <- c("2019" = col2019, "2020" = col2020, "2021" = col2021)

#Color palette for peak wet, dry, and early wet
drycol <- "goldenrod1"
earlywetcol <- "purple"
peakwetcol <- "cadetblue"

drycol <- "grey80"
earlywetcol <- "grey45"
peakwetcol <- "grey10"

szn_cols <- c(drycol, earlywetcol, peakwetcol)
# Custom facet labels
zn_labs <- c(
  "Southern" = "S. Amz. (Strongly seasonal)",
  "CA" = "C. Amz. (seasonal)",
  "NWA" = "N.W. Amz. (ever-wet)",
  "NOA" = "N. Amz. (bimodal)"
)


#Exlore correlation among predictors --------------------------------------------------------

varsel <- c("pai", "pai_toc", "modis_lai", "fpar", "fesc", "nirv", "sif_par", "sif_parm", "phif", "sif743_cor", "pri_nar", "cci", "fesc_tropo_rad", "sif_rel_tropo", "phif_tropo_rad")

my_fn <- function(data, mapping, method="lm", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, ...)
  p
}

corr_select <- gedi_3szn %>% dplyr::select(all_of(varsel)) #remove categorical
#ggpairs(data = corr_select, lower=list(continuous = my_fn))
#Some autocorrelation observed among predictor variables
#Some relationships are not strongly linear

key_vars <- gedi_szn %>%
  dplyr::select(
    pai,
    pai_toc,
    modis_lai,
    fesc,
    sif_par,
    sif_parm,
    cci,
    phif,
    phif_tropo_rad
  )
cor_mat <- cor(key_vars, method = "pearson")

sif_corrplot <- ggcorrplot(
  cor_mat,
  hc.order = FALSE,
  type = "lower",
  lab = TRUE,
  lab_size = 3,
  show.diag = TRUE,
  digits = 3,
  colors = c("blue", "white", "red")
)
sif_corrplot

#Let's check a 0.7 threshold for multicollinearity
gedi_3szn %>% 
  dplyr::select(all_of(varsel)) %>% 
  na.omit() %>% 
  cor() %>%
  as.data.frame() %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "correlation") %>%
  filter(correlation >= 0.7 & correlation < 1)
#There is some multicollinearity between:
# fesc & nirv
# pai & pai_toc
# all of the various SIF derivatives

#Let's look at a subset
gedi_3szn %>% 
  dplyr::select(c(pai_toc, modis_lai, fesc, nirv, sif_par, sif_parm, fpar, pri_nar, cci)) %>% 
  na.omit() %>% 
  usdm::vif(.)
#Let's make sure not to compare VIFs greater than 10.


#Outcome variable appropriateness --------------------------------------------

#let's take a look at outcome variable
hist(gedi_3szn$sif_par) #looks pretty normal
hist(gedi_3szn$sif_parm)

#Some summary stats
gedi_3szn %>%
  group_by(sub_szn) %>%
  get_summary_stats(c("pai", "pai_toc", "modis_lai", "fpar", "fesc", "nirv", "sif_par", "sif_parm", "phif", "sif743_cor", "pri_nar", "cci"), type = "five_number") %>% 
  print(n = 48)

gedi_3szn %>%
  group_by(georeg_agg) %>%
  get_summary_stats(c("pai", "pai_toc", "modis_lai", "fpar", "fesc", "nirv", "sif_par", "sif_parm", "phif", "sif743_cor", "pri_nar", "cci"), type = "five_number") %>% 
  print(n = 48)

gedi_3szn %>%
  get_summary_stats(c("pai", "pai_toc", "modis_lai", "fpar", "fesc","nirv", "sif_par", "sif_parm", "phif", "sif743_cor", "pri_nar", "cci"), type = "five_number")

#boxplot trends --------------------------------------------------------

gedi_3szn %>% ggplot(data = ., aes(x = sub_szn, y = pai_toc))+
  geom_boxplot()+
  facet_wrap(~georeg_agg)
gedi_3szn %>% ggplot(data = ., aes(x = sub_szn, y = sdvfp))+
  geom_boxplot()+
  facet_wrap(~georeg_agg)
gedi_3szn %>% ggplot(data = ., aes(x = sub_szn, y = nirv))+
  geom_boxplot()+
  facet_wrap(~georeg_agg)
gedi_3szn %>% ggplot(data = ., aes(x = sub_szn, y = phif))+
  geom_boxplot()+
  facet_wrap(~georeg_agg)
gedi_3szn %>% ggplot(data = ., aes(x = sub_szn, y = sif_par))+
  geom_boxplot()+
  facet_wrap(~georeg_agg)
gedi_3szn %>% ggplot(data = ., aes(x = sub_szn, y = sif_parm))+
  geom_boxplot()+
  facet_wrap(~georeg_agg)
gedi_3szn %>% ggplot(data = ., aes(x = sub_szn, y = fpar))+
  geom_boxplot()+
  facet_wrap(~georeg_agg)
gedi_3szn %>% ggplot(data = ., aes(x = sub_szn, y = modis_lai))+
  geom_boxplot()+
  facet_wrap(~georeg_agg)
gedi_3szn %>% ggplot(data = ., aes(x = sub_szn, y = fesc))+
  geom_boxplot()+
  facet_wrap(~georeg_agg)
gedi_3szn %>% ggplot(data = ., aes(x = sub_szn, y = pri_nar))+
  geom_boxplot()+
  facet_wrap(~georeg_agg)

#Some linear trends ------------------------
gedi_3szn %>% ggplot(data = ., aes(x = cci, y = phif, color = sub_szn))+
  geom_point(alpha = 0.2)+
  facet_wrap(~georeg_agg)
gedi_3szn %>% ggplot(data = ., aes(x = cci, y = sif_par, color = sub_szn))+
  geom_point(alpha = 0.2)+
  facet_wrap(~georeg_agg)
gedi_3szn %>% ggplot(data = ., aes(x = modis_lai, y = fesc, color = sub_szn))+
  geom_point(alpha = 0.2)+
  facet_wrap(~georeg_agg)
gedi_3szn %>% ggplot(data = ., aes(x = nirv, y = fesc, color = sub_szn))+
  geom_point(alpha = 0.2)+
  facet_wrap(~georeg_agg)


plotgedi <- gedi_3szn


#Boxplots -----------------------------

# Define a function to run the hierarchical model and extract ICC and R²
get_model_stats <- function(data, y_var) {
  # Fit the model (double-nested random effects)
  mod <- lmer(as.formula(paste(y_var, "~ sub_szn + (1|year)")), data = data, REML = FALSE)
  
  # Extract the ICC, Marginal and Conditional R²
  icc_value <- performance::icc(mod)$ICC_adjusted
  marginal_r2 <- performance::r2(mod)$R2_marginal
  conditional_r2 <- performance::r2(mod)$R2_conditional
  
  return(list(icc_adj = icc_value, marg_r2 = marginal_r2, cond_r2 = conditional_r2))
}

# Define y position relative to axis limits
y_pos <- function(y_limits, y_var) {
  if (is.null(y_limits)) {
    return(NA)  # Handle cases where y_limits is NULL
  }
  
  factor <- ifelse(y_var %in% c("pai_toc", "fesc", "nirv"), 0.8, 0.20)  
  return(y_limits[1] + factor * (y_limits[2] - y_limits[1]))  
}

# Define a function to create boxplots
create_violin <- function(data, y_var, yformal, colors, y_limits = NULL, sub_szn_lvls = subsznlvls) {
  
  # # Run the model and get the statistics
  # model_stats <- get_model_stats(data, y_var)
  # 
  # # Extract the values
  # icc_adj <- round(model_stats$icc_adj, 3)
  # marg_r2 <- round(model_stats$marg_r2, 3)
  # cond_r2 <- round(model_stats$cond_r2, 3)
  
  # Define the text to be displayed on the plot
  # stats_text <- paste("ICC: ", icc_adj, "\n", 
  #                     "Marg. R²: ", marg_r2, "\n", 
  #                     "Cond. R²: ", cond_r2)
  
  plot <- data %>%
    filter(sub_szn %in% subsznlvls) %>%
    mutate(sub_szn = factor(sub_szn, levels = subsznlvls)) %>%
    ggplot(aes(x = sub_szn, y = .data[[y_var]], fill = sub_szn)) +
    geom_violin(alpha = 0.8) +
    labs(y = yformal, x = NULL) +
    theme_minimal() +
    scale_x_discrete(labels = c("Dry", "Wet up", "Peak Wek")) +
    scale_fill_manual(values = colors,
                      labels = c("Dry", "Wet up", "Peak Wet"),
                      name = "Season")+
    (if (!is.null(y_limits)) scale_y_continuous(limits = y_limits) else NULL)+
    annotate("text", x = 2.2, y = y_pos(y_limits, y_var), label = stats_text, size = 2.5, hjust = 0, vjust = 1)+
    theme(
      axis.text.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(face = "bold") 
    )+
    facet_wrap(~georeg_agg)
  
  return(plot)
}

# 
# # Create individual plots
# plot1 <- create_violin(plotgedi, "sif_par", "SIF/PAR", szn_cols, y_limits = c(-0.003, 0.0075))
# plot2 <- create_violin(plotgedi, "sif743_cor", "SIF (daylength corr.)", szn_cols, y_limits = c(-0.3, 1.25))
# plot3 <- create_violin(plotgedi, "phif", "PhiF", szn_cols, y_limits = c(0, 4.5))
# plot4 <- create_violin(plotgedi, "pai_toc", "Canopy PAI", szn_cols, y_limits = c(0, 7))
# plot5 <- create_violin(plotgedi, "pai", "Total PAI", szn_cols, y_limits = c(0, 7))
# plot6 <- create_violin(plotgedi, "modis_lai", "MODIS LAI", szn_cols, y_limits = c(0, 7))
# plot7 <- create_violin(plotgedi, "fesc", "Fesc", szn_cols, y_limits = c(0.15, 0.95))
# plot8 <- create_violin(plotgedi, "nirv", "NIRv", szn_cols, y_limits = c(0.15, 0.95))
# plot9 <- create_violin(plotgedi, "fpar", "fPAR", szn_cols, y_limits = c(0.15, 0.95))
# 
# plot10 <- create_violin(plotgedi, "sif_nirvp", "SIF/NIRvP", szn_cols, y_limits = c(-1, 5))
# 
# plot11 <- create_violin(plotgedi, "pri_nar", "PRI narrow", szn_cols, y_limits = c(-0.15, 0.1))
# plot12 <- create_violin(plotgedi, "cci", "CCI", szn_cols, y_limits = c(-0.25,0.5))
# 
# # Combine the plots into a grid with a shared legend below
# combined_plot <- (
#   (plot1 + theme(legend.position = "none")) |
#     (plot2 + theme(legend.position = "none")) |
#     (plot3 + theme(legend.position = "none"))
# ) /
#   (
#     (plot4 + theme(legend.position = "none")) |
#       (plot5 + theme(legend.position = "none")) |
#       (plot6 + theme(legend.position = "none"))
#   ) /
#   (
#     (plot7 + theme(legend.position = "none")) |
#       (plot8 + theme(legend.position = "none")) |
#       (plot9 + theme(legend.position = "bottom")) # This plot will provide the legend
#   ) +
#   plot_layout(guides = "collect") & 
#   theme(legend.position = "bottom")
# 
# combined_plot
# 
# 
# 

#Modeling ----------------------------------------------------

#ANOVA for dry and wet season time period ------------------

# We only want to look at satellite indicators independent of SIF/PAR. This means we cannot use PhiF, SIF743_cor, phif_tropo, sifrel_tropo, sifrel to predict SIF.

# We will test the importance of structural variables (PAI_TOC, PAI, MODIS_LAI) on SIF.
# We will do this for sub_szn and georeg_agg as fixed effects.

# For contextualizing our violin plots, we will also do simple ANOVAs for sub_szn, with year and zone as random effects.




#MODELING WITH SEPARATE GEOREGIONS ----------------------------------------
#Let's look at a few regions separately (not as a fixed effect)

##########
#TEMPORARY!!!!!!
gedi_3szn <- gedi_3szn %>% select(-sif_par) %>% rename(sif_par = sif_parm)
####


ca <- gedi_3szn %>% dplyr::filter(georeg_agg == "CA")
soa <- gedi_3szn %>% dplyr::filter(georeg_agg == "Southern")
noa <- gedi_3szn %>% dplyr::filter(georeg_agg == "NOA")
nwa <- gedi_3szn %>% dplyr::filter(georeg_agg == "NWA")


# Summary of findings below: ----------------------
#Although year generally explains a small portion of the variance in each region, there is a significant difference between the null and random effect model, AND we have good reason to suspect a random effect. So, Year is included as a random effect.

#These results indicate that including an interaction effect of PAI_TOC barely plays a role in terms of explanatory power, although the relationships are consistently significant. Taken together suggests that PAI_TOC does not greatly modify the effect of fesc + cci on SIF/PAR. It is a 'tweak', not a driver.

#The radiative-transfer-based MODIS LAI always explains more of the variability in fesc than does TOC PAI

#NIRv explains SIF/PAR better than CCI in all regions, but still not well.

## Let us consider year as a random effect -------------------------
# 
# yr_modca <- lmer(sif_par ~ sub_szn + (1|year), data = ca) ##random effect
# null_modca <- lm(sif_par ~ sub_szn, data = ca) ##no random effect
# anova(yr_modca, null_modca, test = "LRT") # Compare models using Likelihood Ratio Test
# 
# yr_modsoa <- lmer(sif_par ~ sub_szn + (1|year), data = soa) ##random effect
# null_modsoa <- lm(sif_par ~ sub_szn, data = soa) ##no random effect
# anova(yr_modsoa, null_modsoa, test = "LRT")
# 
# yr_modnoa <- lmer(sif_par ~ sub_szn + (1|year), data = noa) ##random effect
# null_modnoa <- lm(sif_par ~ sub_szn, data = noa) ##no random effect
# anova(yr_modnoa, null_modnoa, test = "LRT")
# 
# yr_modnwa <- lmer(sif_par ~ sub_szn + (1|year), data = nwa) ##random effect
# null_modnwa <- lm(sif_par ~ sub_szn, data = nwa) ##no random effect
# anova(yr_modnwa, null_modnwa, test = "LRT")
# 
# icc(yr_modca)
# icc(yr_modsoa)
# icc(yr_modnoa)
# icc(yr_modnwa)

#Evidence for RE --> we'll use one!


#Now let's look at SIF/PAR predictors, CA first ---------------------------------------------

#We would expect an interaction between sub_szn and a predictor RS variable, so we will use one.

baseline_model <- lmer(sif_par ~ sub_szn + (1|year), data = ca, REML = FALSE)

mod_list <- list()
mod_list[['Baseline']] <- baseline_model
mod_list[['Fesc']] <- fescmod <- lmer(sif_par ~ sub_szn * fesc + (1|year), data = ca, REML = FALSE)
mod_list[['MODIS LAI']] <- modlaimod <- lmer(sif_par ~ sub_szn * modis_lai + (1|year), data = ca, REML = FALSE)

# Type III ANOVA for each model
anova_res <- lapply(mod_list, function(model) car::Anova(model, type = "III"))
anova_summ <- lapply(anova_res, anova_summary)
anova_res
anova_summ

# AIC Model Selection
aic_res <- aictab(mod_list, modnames = names(mod_list))
print(aic_res)

#Strong support for the Fesc model

# Best model selection
best_mod <- mod_list[[which.min(sapply(mod_list, AIC))]]

# Testing the more physiological variables ------------------------

mod_list2 <- list()
mod_list2[['Baseline']] <- baseline_model
mod_list2[['PRI']] <- primod <- lmer(sif_par ~ sub_szn * pri_nar + (1|year), data = ca, REML = FALSE)
mod_list2[['CCI']] <- ccimod <- lmer(sif_par ~ sub_szn * cci + (1|year), data = ca, REML = FALSE)

# Type III ANOVA for each model
anova_res2 <- lapply(mod_list2, function(model) car::Anova(model, type = "III"))
anova_summ2 <- lapply(anova_res2, anova_summary)
anova_res2
anova_summ2

# AIC Model Selection
aic_res2 <- aictab(mod_list2, modnames = names(mod_list2))
print(aic_res2)

#CCI is the most highly supported

# Best model selection
best_mod2 <- mod_list2[[which.min(sapply(mod_list2, AIC))]]
print(summary(best_mod2))


# Testing structural predictors: PAI, PAI_TOC, PAI_US

mod_list3 <- list()
mod_list3[['Baseline']] <- baseline_model
mod_list3[['PAI']] <- paimod <- lmer(sif_par ~ sub_szn * pai + (1|year), data = ca, REML = FALSE)
mod_list3[['PAI TOC']] <- paitocmod <- lmer(sif_par ~ sub_szn * pai_toc + (1|year), data = ca, REML = FALSE)
mod_list3[['PAI US']] <- paiusmod <- lmer(sif_par ~ sub_szn * pai_us + (1|year), data = ca, REML = FALSE)

# Type III ANOVA for each model
anova_res3 <- lapply(mod_list3, function(model) car::Anova(model, type = "III"))
anova_summ3 <- lapply(anova_res3, anova_summary)
anova_res3
anova_summ3

# AIC Model Selection
aic_res3 <- aictab(mod_list3, modnames = names(mod_list3))
print(aic_res3)

#PAI_TOC is most highly supported, though not strongly

# Best model selection
best_mod3 <- mod_list3[[which.min(sapply(mod_list3, AIC))]]


#COMPARE THE CCI MODEL AND Fesc model ----------------

# Fit models
fesc_model <- lmer(sif_par ~ sub_szn * fesc + (1|year), data = ca, REML = FALSE)
cci_model <- lmer(sif_par ~ sub_szn * cci + (1|year), data = ca, REML = FALSE)
fesc_cci_model <- lmer(sif_par ~ sub_szn * (fesc + cci) + (1|year), data = ca, REML = FALSE)

# Compare models again
aic_res4 <- aictab(list(baseline_model, fesc_model, cci_model, fesc_cci_model), modnames = c('baseline', 'fesc', 'cci', 'additive'))
print(aic_res4)

# Adding CCI improves the fit
r2_nakagawa(fesc_model) #23.6%
r2_nakagawa(cci_model) #23%
r2_nakagawa(fesc_cci_model) #24%

#fesc_cci_model is best


#COMPARE THE BEST MODEL AND BEST REFL MODEL + LIDAR ----------------
# "Does adding GEDI data improve the fesc model?"

# Fit models
fesc_model <- lmer(sif_par ~ sub_szn * (cci + fesc) + (1|year), data = ca, REML = FALSE)
fesc_add_model <- lmer(sif_par ~ sub_szn * (cci + fesc + pai_toc) + (1|year), data = ca, REML = FALSE)

# Compare models again
aic_res5 <- aictab(list(fesc_model, fesc_add_model), modnames = c('fesc', 'fesc+paitoc'))
print(aic_res5)

car::vif(fesc_add_model) #The model does not appear highly collinear

#Adding PAI_TOC improves the model AICc.

# Compute marginal and conditional R² values
r2_fesc <- r2_nakagawa(fesc_model)
r2_fesc_add <- r2_nakagawa(fesc_add_model)
print(r2_fesc)
print(r2_fesc_add)

cat("Including lidar adds",
    (round(as.numeric(r2_fesc_add[1]) - as.numeric(r2_fesc[1]), 4)*100),
    "% to the conditional Rsq and",
    (round(as.numeric(r2_fesc_add[2]) - as.numeric(r2_fesc[2]), 4)*100),
    "% to the marginal Rsq")

#Adding PAI_TOC barely makes a difference in terms of R2 (~0.4% difference in marginal & conditional R2)

icc(fesc_model)
icc(fesc_add_model)
#ICC is proportion of variance explained by year and georeg_agg.

#Unadjusted ICC is how much variability in outcome due to differences between groups without adjusting for fixed effects.
#Adjusted ICC is the proportion of variance that remains after accounting for fixed effects (i.e. how much variability attributed to random effects). 

#This shows no difference between ICC values in the different models.

anova(fesc_model, fesc_add_model, test = "Chisq")
#This test indicates that PAI_TOC significantly improves the model fit (but adds less than 1% explained variance.)


#We'll use the non-GEDI-additive model for our 'final' model here
final_ca <- lmer(sif_par ~ sub_szn * (cci + fesc) + (1|year), data = ca, REML = FALSE)

#Now test for temporal autocorrelation:
acf(resid(final_ca))

# Create a numeric time-like variable that is unique within year
ca <- ca %>%
  arrange(year, doymin) %>% # assuming doymin represents time
  group_by(year) %>%
  mutate(row_in_year = row_number()) %>%
  ungroup()

# Fit AR(1) model (lme package)
final_ca_ar1 <- lme(
  sif_par ~ sub_szn * (cci + fesc),
  random = ~1 | year,
  correlation = corAR1(form = ~ row_in_year | year),
  data = ca,
  method = "ML"
)

AIC(final_ca, final_ca_ar1)
AIC(final_ca) - AIC(final_ca_ar1)
#AR(1) correlation structure improves fit!

acf(resid(final_ca), main = "ACF: no AR(1)")
acf(resid(final_ca_ar1), main = "ACF: with AR(1)")

acf(residuals(final_ca_ar1, type = "normalized"))

#Some evidence for temporal autocorrelation, improved -- but not made perfect -- by AR(1) correlation structure

ca_bestmod_data <- ca %>%
  mutate(
    res = residuals(final_ca_ar1, type = "pearson"),
    fit = fitted(final_ca_ar1)
  )

#Now test for spatial autocorrelation -------------------------

# Convert to spatial object in planar CRS
ca_sf <- st_as_sf(ca_bestmod_data, coords = c("y", "x"), crs = 4326)
ca_sf_proj <- st_transform(ca_sf, crs = 3857)

# Create neighbor list & weights using distance-based neighbors (e.g., 500m)
coords <- st_coordinates(ca_sf_proj)
nb <- dnearneigh(coords, d1 = 0, d2 = 80000) # Try an 80km radius
table(card(nb) == 0) #Check number of regions with zero neighbors
lw <- nb2listw(nb, style = "W", zero.policy = TRUE) # Create spatial weights

# Run Moran’s I test
moran_test <- moran.test(ca_sf_proj$res, lw, zero.policy = TRUE)
print(moran_test)

#Evidence for spatial autocorrelation!!!

#We'll try and account for spatial autocorrelation using a GAMM model with a thin plate spline ---------------------------

# Extract projected coordinates into columns
coords_proj <- st_coordinates(ca_sf_proj)
ca$x_m <- coords_proj[, 1]
ca$y_m <- coords_proj[, 2]

sum(duplicated(ca[, c("x_m", "y_m")])) #nonzero means duplicated locations (repeat years)

# Add small jitter to break spatial ties
ca <- ca %>%
  mutate(
    x_jit = jitter(x_m, amount = 1),
    y_jit = jitter(y_m, amount = 1)
  )
sum(duplicated(ca[, c("x_jit", "y_jit")])) #should now be zero

#Now we'll test a a GAMM model accounting for BOTH spatial and temporal autocorrelation-------------
gamm_spatiotemp_ca <- gamm(
  sif_par ~ sub_szn * (cci + fesc) + 
    s(x_jit, y_jit, bs = "tp", k = 100), #thin plate spline
  correlation = corAR1(form = ~ row_in_year | year), #AR1 correlation structure
  random = list(year = ~1),
  data = ca,
  method = "ML"
)
ca$res_gamm <- residuals(gamm_spatiotemp_ca$gam)

ca_sf <- st_as_sf(ca, coords = c("x_jit", "y_jit"), crs = 3857)
ca_sp <- as_Spatial(ca_sf)

#New
vgm_emp <- variogram(res_gamm ~ 1, data = ca_sp)
plot(vgm_emp, main = "Semivariogram of GAM residuals")
vgm_model <- fit.variogram(vgm_emp, model = vgm("Exp"))
plot(vgm_emp, model = vgm_model, main = "Fitted Variogram (Exponential Model)")

#Original
vgm_orig <- variogram(res ~ 1, data = as_Spatial(ca_sf_proj))
plot(vgm_orig, main = "Semivariogram of GAM residuals")
vgm_model_orig <- fit.variogram(vgm_orig, model = vgm("Exp"))
plot(vgm_orig, model = vgm_model_orig, main = "Fitted Variogram (Exponential Model)")

#The variograms suggest we have slightly improved the autocorrelation

#Finally, to test nonlinearity in predictors, create a GAMM model with smoothers using corAR1 -----------------------------
gamm_s_spatiotemp_ca <- gamm(
  sif_par ~ sub_szn + 
    s(fesc, by = sub_szn) +
    s(cci, by = sub_szn) +
    s(x_jit, y_jit, bs = "tp", k = 100),
  correlation = corAR1(form = ~ row_in_year | year),
  random = list(year = ~1),
  data = ca,
  method = "ML"
)

#Compare all 'final' models:
aic_vals_ca <- data.frame(
  Model = c("lmer", "lmer + AR1", "GAMM Spat + AR1", "GAMM Spat + AR1 + s(var)"),
  n_obs = nrow(ca),
  log_lik = c(logLik(final_ca),
              logLik(final_ca_ar1),
              logLik(gamm_spatiotemp_ca$lme),
              logLik(gamm_s_spatiotemp_ca$lme)
  ),
  AIC = c(
    AIC(final_ca),
    AIC(final_ca_ar1),
    AIC(gamm_spatiotemp_ca$lme),
    AIC(gamm_s_spatiotemp_ca$lme)
  ),
  R2m = c(
    r.squaredGLMM(final_ca)[1],
    r.squaredGLMM(final_ca_ar1)[1],
    r.squaredGLMM(gamm_spatiotemp_ca$lme)[1],
    r.squaredGLMM(gamm_s_spatiotemp_ca$lme)[1]
  ),
  R2c = c(
    r.squaredGLMM(final_ca)[2],
    r.squaredGLMM(final_ca_ar1)[2],
    r.squaredGLMM(gamm_spatiotemp_ca$lme)[2],
    r.squaredGLMM(gamm_s_spatiotemp_ca$lme)[2]
  ),
  stringsAsFactors = FALSE
) %>% 
  arrange(AIC) %>%
  mutate(deltaAIC = AIC - min(AIC)) %>% 
  select(Model, AIC, deltaAIC, everything())

print(aic_vals_ca)

#The best model is the GAMM accounting for spatial and temporal autocorrelation!

best_lme_ca <- gamm_spatiotemp_ca$lme
best_gam_ca <- gamm_spatiotemp_ca$gam

#Extract residuals and fitted values
ca$res_best <- residuals(best_lme_ca, type = "normalized")
ca$fit_best <- fitted(best_lme_ca)

#Check heteroskedasticity
ggplot(ca, aes(x = fit_best, y = res_best, color = sub_szn)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Best Model)", x = "Fitted", y = "Residuals") +
  theme_minimal()

#QQ-plot
ggplot(ca, aes(x = qqnorm(ca$res_best, plot.it = FALSE)$x, y = qqnorm(res_best, plot.it = FALSE)$y)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +
  labs(title = "QQ Plot of Residuals",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

#Residual symmetry
ggplot(ca, aes(x = "all", y = res_best)) +
  geom_boxplot() +
  labs(title = "Residual Symmetry (Best Model)", x = "", y = "Residuals") +
  theme_minimal()

#Residuals vs fesc by season
ggplot(ca, aes(x = fesc, y = res_best, color = sub_szn, shape = sub_szn)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs fesc by Season", x = "fesc", y = "Residuals") +
  theme_minimal()

#Residuals vs cci by season
ggplot(ca, aes(x = cci, y = res_best, color = sub_szn, shape = sub_szn)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs cci by Season", x = "cci", y = "Residuals") +
  theme_minimal()

# ACF for temporal autocorrelation
acf(ca$res_best, main = "ACF of Residuals")

# One more look at semivariogram
ca_sp <- as_Spatial(st_as_sf(ca, coords = c("x_jit", "y_jit"), crs = 3857))
vgm_emp <- variogram(res_best ~ 1, ca_sp)
vgm_model <- fit.variogram(vgm_emp, model = vgm("Exp"))
plot(vgm_emp, main = "Semivariogram of Residuals (Best Model)")
plot(vgm_emp, model = vgm_model, main = "Fitted Variogram (with exp. model)")

#Only if the final model is a GAM:

# Estimated DF, p-value, etc.
summary(best_gam_ca)$s.table  # includes EDF, reference df, F, p-value

# Check basis dimension adequacy
k_check <- mgcv::k.check(best_gam_ca)
print(k_check) #possibly too few dimensions...

# Visualize smooth for spatial field
gratia::draw(best_gam_ca, select = 1)  # this assumes s(x_jit, y_jit) is the first smooth


#Now look at Southern Amz -------------------------------------------------

# Testing reflectance structural predictors: MODIS LAI and fesc -------------

baseline_model <- lmer(sif_par ~ sub_szn + (1|year), data = soa, REML = FALSE)

mod_list <- list()
mod_list[['Baseline']] <- baseline_model
mod_list[['Fesc']] <- fescmod <- lmer(sif_par ~ sub_szn * fesc + (1|year), data = soa, REML = FALSE)
mod_list[['MODIS LAI']] <- modlaimod <- lmer(sif_par ~ sub_szn * modis_lai + (1|year), data = soa, REML = FALSE)

# Type III ANOVA for each model
anova_res <- lapply(mod_list, function(model) car::Anova(model, type = "III"))
anova_summ <- lapply(anova_res, anova_summary)

# AIC Model Selection
aic_res <- aictab(mod_list, modnames = names(mod_list))
print(aic_res)

#Fesc is stronger here

# Best model selection
best_mod <- mod_list[[which.min(sapply(mod_list, AIC))]]


# Testing the more physiological variables ------------------------

mod_list2 <- list()
mod_list2[['Baseline']] <- baseline_model
mod_list2[['PRI']] <- primod <- lmer(sif_par ~ sub_szn * pri_nar + (1|year), data = soa, REML = FALSE)
mod_list2[['CCI']] <- ccimod <- lmer(sif_par ~ sub_szn * cci + (1|year), data = soa, REML = FALSE)

# Type III ANOVA for each model
anova_res2 <- lapply(mod_list2, function(model) car::Anova(model, type = "III"))
anova_summ2 <- lapply(anova_res2, anova_summary)

# AIC Model Selection
aic_res2 <- aictab(mod_list2, modnames = names(mod_list2))
print(aic_res2)

#CCI is the most highly supported

# Best model selection
best_mod2 <- mod_list2[[which.min(sapply(mod_list2, AIC))]]
print(summary(best_mod2))


# Testing 'the big three' predictors: PAI, PAI_TOC, SDVFP, meanpavd

mod_list3 <- list()
mod_list3[['Baseline']] <- baseline_model
mod_list3[['PAI']] <- paimod <- lmer(sif_par ~ sub_szn * pai + (1|year), data = soa, REML = FALSE)
mod_list3[['PAI TOC']] <- paitocmod <- lmer(sif_par ~ sub_szn * pai_toc + (1|year), data = soa, REML = FALSE)
mod_list3[['PAI US']] <- paiusmod <- lmer(sif_par ~ sub_szn * pai_us + (1|year), data = soa, REML = FALSE)

# Type III ANOVA for each model
anova_res3 <- lapply(mod_list3, function(model) car::Anova(model, type = "III"))
anova_summ3 <- lapply(anova_res3, anova_summary)

# AIC Model Selection
aic_res3 <- aictab(mod_list3, modnames = names(mod_list3))
print(aic_res3)

#PAI_TOC is most highly supported

# Best model selection
best_mod3 <- mod_list3[[which.min(sapply(mod_list3, AIC))]]
print(summary(best_mod3))


#COMPARE THE CCI MODEL AND Fesc model ----------------

# Fit models
fesc_model <- lmer(sif_par ~ sub_szn * fesc + (1|year), data = soa, REML = FALSE)
cci_model <- lmer(sif_par ~ sub_szn * cci + (1|year), data = soa, REML = FALSE)
fesc_cci_model <- lmer(sif_par ~ sub_szn * (fesc + cci) + (1|year), data = soa, REML = FALSE)

# Compare models again
aic_res4 <- aictab(list(baseline_model, fesc_model, cci_model, fesc_cci_model), modnames = c('baseline', 'fesc', 'cci', 'additive'))
print(aic_res4)

# Adding CCI improves the fit

#COMPARE THE BEST REFLECTANCE MODEL AND BEST LIDAR MODEL ----------------
# "Does adding PAI_TOC improve the fesc model?"

# Fit models
fesc_model <- lmer(sif_par ~ sub_szn * (cci + fesc) + (1|year), data = soa, REML = FALSE)
fesc_add_model <- lmer(sif_par ~ sub_szn * (cci + fesc + pai_toc) + (1|year), data = soa, REML = FALSE)

# Compare models again
aic_res5 <- aictab(list(fesc_model, fesc_add_model), modnames = c('fesc', 'fesc+paitoc'))
print(aic_res5)
#Adding PAI_TOC improves the model AICc.

car::vif(fesc_add_model) #The model does not appear highly collinear

# Compute marginal and conditional R² values
r2_fesc <- r2_nakagawa(fesc_model)
r2_fesc_add <- r2_nakagawa(fesc_add_model)
print(r2_fesc)
print(r2_fesc_add)

cat("Including lidar adds",
    (round(as.numeric(r2_fesc_add[1]) - as.numeric(r2_fesc[1]), 4)*100),
    "% to the conditional Rsq and",
    (round(as.numeric(r2_fesc_add[2]) - as.numeric(r2_fesc[2]), 4)*100),
    "% to the marginal Rsq")

icc(fesc_model)
icc(fesc_add_model)
#ICC is proportion of variance explained by year and georeg_agg.

anova(fesc_model, fesc_add_model, test = "Chisq")
#This test indicates that PAI_TOC significantly improves the model fit (p=0.00013)

# We'll use the additive model for Southern Amazon
final_soa <- lmer(sif_par ~ sub_szn * (cci + fesc + pai_toc) + (1|year), data = soa, REML = FALSE)

#Now test for temporal autocorrelation:

# Create a numeric time-like variable that is unique within year
soa <- soa %>%
  arrange(year, doymin) %>%
  group_by(year) %>%
  mutate(row_in_year = row_number()) %>%
  ungroup()

# Fit AR(1) model
final_soa_ar1 <- lme(
  sif_par ~ sub_szn * (cci + fesc + pai_toc),
  random = ~1 | year,
  correlation = corAR1(form = ~ row_in_year | year),
  data = soa,
  method = "ML"
)

AIC(final_soa, final_soa_ar1)

acf(resid(final_soa), main = "ACF: no AR(1)")
acf(resid(final_soa_ar1), main = "ACF: with AR(1)")

acf(residuals(final_soa_ar1, type = "normalized"))

#Some evidence for temporal autocorrelation, improved -- but not made perfect -- by AR(1) correlation structure

soa_bestmod_data <- soa %>%
  mutate(
    res = residuals(final_soa_ar1, type = "pearson"),
    fit = fitted(final_soa_ar1)
  )

#Now test for spatial autocorrelation -------------------------

# Convert to spatial object in planar CRS
soa_sf <- st_as_sf(soa_bestmod_data, coords = c("y", "x"), crs = 4326)
soa_sf_proj <- st_transform(soa_sf, crs = 3857)

# Create neighbor list & weights using distance-based neighbors (e.g., 500m)
coords <- st_coordinates(soa_sf_proj)
nb <- dnearneigh(coords, d1 = 0, d2 = 80000) # Try an 80km radius
table(card(nb) == 0) #Check number of regions with zero neighbors
lw <- nb2listw(nb, style = "W", zero.policy = TRUE) # Create spatial weights

# Run Moran’s I test
moran_test <- moran.test(soa_sf_proj$res, lw, zero.policy = TRUE)
print(moran_test)

#Evidence for spatial autocorrelation!!!

#We'll try and account for spatial autocorrelation using a GAMM model with a thin plate spline ---------------------------

# Extract projected coordinates into columns
coords_proj <- st_coordinates(soa_sf_proj)
soa$x_m <- coords_proj[, 1]
soa$y_m <- coords_proj[, 2]

sum(duplicated(soa[, c("x_m", "y_m")])) #nonzero means duplicated locations (repeat years)

# Add small jitter to break spatial ties
soa <- soa %>%
  mutate(
    x_jit = jitter(x_m, amount = 1),
    y_jit = jitter(y_m, amount = 1)
  )
sum(duplicated(soa[, c("x_jit", "y_jit")])) #should now be zero

#Now we'll test a a GAMM model accounting for BOTH spatial and temporal autocorrelation-------------
gamm_spatiotemp_soa <- gamm(
  sif_par ~ sub_szn * (cci + fesc + pai_toc) + 
    s(x_jit, y_jit, bs = "tp", k = 100), #thin plate spline
  correlation = corAR1(form = ~ row_in_year | year), #AR1 correlation structure
  random = list(year = ~1),
  data = soa,
  method = "ML"
)
soa$res_gamm <- residuals(gamm_spatiotemp_soa$gam)

soa_sf <- st_as_sf(soa, coords = c("x_jit", "y_jit"), crs = 3857)
soa_sp <- as_Spatial(soa_sf)

#New
vgm_emp <- variogram(res_gamm ~ 1, data = soa_sp)
plot(vgm_emp, main = "Semivariogram of GAM residuals")
vgm_model <- fit.variogram(vgm_emp, model = vgm("Exp"))
plot(vgm_emp, model = vgm_model, main = "Fitted Variogram (Exponential Model)")

#Original
vgm_orig <- variogram(res ~ 1, data = as_Spatial(soa_sf_proj))
plot(vgm_orig, main = "Semivariogram of GAM residuals")
vgm_model_orig <- fit.variogram(vgm_orig, model = vgm("Exp"))
plot(vgm_orig, model = vgm_model_orig, main = "Fitted Variogram (Exponential Model)")

#The variograms suggest we have slightly improved the autocorrelation

#Finally, to test nonlinearity in predictors, create a GAMM model with smoothers using corAR1 -----------------------------
gamm_s_spatiotemp_soa <- gamm(
  sif_par ~ sub_szn + 
    s(fesc, by = sub_szn) +
    s(cci, by = sub_szn) +
    s(pai_toc, by = sub_szn) +
    s(x_jit, y_jit, bs = "tp", k = 100),
  correlation = corAR1(form = ~ row_in_year | year),
  random = list(year = ~1),
  data = soa,
  method = "ML"
)

#Compare all 'final' models:
aic_vals_soa <- data.frame(
  Model = c("lmer", "lmer + AR1", "GAMM Spat + AR1", "GAMM Spat + AR1 + s(var)"),
  n_obs = nrow(soa),
  log_lik = c(logLik(final_soa),
              logLik(final_soa_ar1),
              logLik(gamm_spatiotemp_soa$lme),
              logLik(gamm_s_spatiotemp_soa$lme)
  ),
  AIC = c(
    AIC(final_soa),
    AIC(final_soa_ar1),
    AIC(gamm_spatiotemp_soa$lme),
    AIC(gamm_s_spatiotemp_soa$lme)
  ),
  R2m = c(
    r.squaredGLMM(final_soa)[1],
    r.squaredGLMM(final_soa_ar1)[1],
    r.squaredGLMM(gamm_spatiotemp_soa$lme)[1],
    r.squaredGLMM(gamm_s_spatiotemp_soa$lme)[1]
  ),
  R2c = c(
    r.squaredGLMM(final_soa)[2],
    r.squaredGLMM(final_soa_ar1)[2],
    r.squaredGLMM(gamm_spatiotemp_soa$lme)[2],
    r.squaredGLMM(gamm_s_spatiotemp_soa$lme)[2]
  ),
  stringsAsFactors = FALSE
) %>% 
  arrange(AIC) %>%
  mutate(deltaAIC = AIC - min(AIC)) %>% 
  select(Model, AIC, deltaAIC, everything())

print(aic_vals_soa)

#the GAMM with s(var) is better, with a deltaAIC of ~12. R2m and R2c are comparable. Probably not worth the added computation.

#The best model is the GAMM accounting for spatial and temporal autocorrelation!

best_lme_soa <- gamm_spatiotemp_soa$lme
best_gam_soa <- gamm_spatiotemp_soa$gam

#Extract residuals and fitted values
soa$res_best <- residuals(best_lme_soa, type = "normalized")
soa$fit_best <- fitted(best_lme_soa)

#Check heteroskedasticity
ggplot(soa, aes(x = fit_best, y = res_best, color = sub_szn)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Best Model)", x = "Fitted", y = "Residuals") +
  theme_minimal()

#QQ-plot
ggplot(soa, aes(x = qqnorm(soa$res_best, plot.it = FALSE)$x, y = qqnorm(res_best, plot.it = FALSE)$y)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +
  labs(title = "QQ Plot of Residuals",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

#Residual symmetry
ggplot(soa, aes(x = "all", y = res_best)) +
  geom_boxplot() +
  labs(title = "Residual Symmetry (Best Model)", x = "", y = "Residuals") +
  theme_minimal()

#Residuals vs fesc by season
ggplot(soa, aes(x = fesc, y = res_best, color = sub_szn, shape = sub_szn)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs fesc by Season", x = "fesc", y = "Residuals") +
  theme_minimal()

#Residuals vs cci by season
ggplot(soa, aes(x = cci, y = res_best, color = sub_szn, shape = sub_szn)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs cci by Season", x = "cci", y = "Residuals") +
  theme_minimal()

# ACF for temporal autocorrelation
acf(soa$res_best, main = "ACF of Residuals")

# One more look at semivariogram
soa_sp <- as_Spatial(st_as_sf(soa, coords = c("x_jit", "y_jit"), crs = 3857))
vgm_emp <- variogram(res_best ~ 1, soa_sp)
vgm_model <- fit.variogram(vgm_emp, model = vgm("Exp"))
plot(vgm_emp, main = "Semivariogram of Residuals (Best Model)")
plot(vgm_emp, model = vgm_model, main = "Fitted Variogram (with exp. model)")

#Only if the final model is a GAM:

# Estimated DF, p-value, etc.
summary(best_gam_soa)$s.table  # includes EDF, reference df, F, p-value

# Check basis dimension adequacy
k_check <- mgcv::k.check(best_gam_soa)
print(k_check) #possibly too few dimensions...

# Visualize smooth for spatial field
gratia::draw(best_gam_soa, select = 1)  # this assumes s(x_jit, y_jit) is the first smooth



#Now look at NW Amz -----------------------------------------------------------

# we'll include year as a random effect

# Testing 'the big three' reflectance structural predictors: MODIS LAI, NIRv, and fesc -------------

baseline_model <- lmer(sif_par ~ sub_szn + (1|year), data = nwa, REML = FALSE)

mod_list <- list()
mod_list[['Baseline']] <- baseline_model
mod_list[['Fesc']] <- fescmod <- lmer(sif_par ~ sub_szn * fesc + (1|year), data = nwa, REML = FALSE)
mod_list[['MODIS LAI']] <- modlaimod <- lmer(sif_par ~ sub_szn * modis_lai + (1|year), data = nwa, REML = FALSE)
# mod_list[['NIRv']] <- modlaimod <- lmer(sif_par ~ sub_szn * nirv + (1|year), data = nwa, REML = FALSE)

# Type III ANOVA for each model
anova_res <- lapply(mod_list, function(model) car::Anova(model, type = "III"))
anova_summ <- lapply(anova_res, anova_summary)

# AIC Model Selection
aic_res <- aictab(mod_list, modnames = names(mod_list))
print(aic_res)

#fesc is stronger here

# Best model selection
best_mod <- mod_list[[which.min(sapply(mod_list, AIC))]]


# Testing the more physiological variables ------------------------

mod_list2 <- list()
mod_list2[['Baseline']] <- baseline_model
mod_list2[['PRI']] <- primod <- lmer(sif_par ~ sub_szn * pri_nar + (1|year), data = nwa, REML = FALSE)
mod_list2[['CCI']] <- ccimod <- lmer(sif_par ~ sub_szn * cci + (1|year), data = nwa, REML = FALSE)

# Type III ANOVA for each model
anova_res2 <- lapply(mod_list2, function(model) car::Anova(model, type = "III"))
anova_summ2 <- lapply(anova_res2, anova_summary)

# AIC Model Selection
aic_res2 <- aictab(mod_list2, modnames = names(mod_list2))
print(aic_res2)

#CCI is the most highly supported

# Best model selection
best_mod2 <- mod_list2[[which.min(sapply(mod_list2, AIC))]]
print(summary(best_mod2))


# Testing 'the big three' predictors: PAI, PAI_TOC, SDVFP, meanpavd

mod_list3 <- list()
mod_list3[['Baseline']] <- baseline_model
mod_list3[['PAI']] <- paimod <- lmer(sif_par ~ sub_szn * pai + (1|year), data = nwa, REML = FALSE)
mod_list3[['PAI TOC']] <- paitocmod <- lmer(sif_par ~ sub_szn * pai_toc + (1|year), data = nwa, REML = FALSE)
mod_list3[['PAI US']] <- paiusmod <- lmer(sif_par ~ sub_szn * pai_us + (1|year), data = nwa, REML = FALSE)

# Type III ANOVA for each model
anova_res3 <- lapply(mod_list3, function(model) car::Anova(model, type = "III"))
anova_summ3 <- lapply(anova_res3, anova_summary)

# AIC Model Selection
aic_res3 <- aictab(mod_list3, modnames = names(mod_list3))
print(aic_res3)

#PAI_US is most highly supported, barely

# Best model selection
best_mod3 <- mod_list3[[which.min(sapply(mod_list3, AIC))]]
print(summary(best_mod3))


#COMPARE THE CCI MODEL AND Fesc model ----------------

# Fit models
fesc_model <- lmer(sif_par ~ sub_szn * fesc + (1|year), data = nwa, REML = FALSE)
cci_model <- lmer(sif_par ~ sub_szn * cci + (1|year), data = nwa, REML = FALSE)
fesc_cci_model <- lmer(sif_par ~ sub_szn * (fesc + cci) + (1|year), data = nwa, REML = FALSE)

# Compare models again
aic_res4 <- aictab(list(baseline_model, fesc_model, cci_model, fesc_cci_model), modnames = c('baseline', 'fesc', 'cci', 'additive'))
print(aic_res4)

# Adding CCI improves the fit, slightly

#COMPARE THE BEST REFLECTANCE MODEL AND BEST LIDAR MODEL ----------------
# "Does adding GEDI improve the fesc model?"

# Fit models
fesc_model <- lmer(sif_par ~ sub_szn * (cci + fesc) + (1|year), data = nwa, REML = FALSE)
fesc_add_model <- lmer(sif_par ~ sub_szn * (cci + fesc + pai_us) + (1|year), data = nwa, REML = FALSE)

# Compare models again
aic_res5 <- aictab(list(fesc_model, fesc_add_model), modnames = c('fesc', 'fesc+ustoc'))
print(aic_res5)


car::vif(fesc_add_model) #The model does not appear highly collinear

#Adding PAI_TOC improves the model AICc.

# Compute marginal and conditional R² values
r2_fesc <- r2_nakagawa(fesc_model)
r2_fesc_add <- r2_nakagawa(fesc_add_model)
print(r2_fesc)
print(r2_fesc_add)

cat("Including lidar adds",
    (round(as.numeric(r2_fesc_add[1]) - as.numeric(r2_fesc[1]), 4)*100),
    "% to the conditional Rsq and",
    (round(as.numeric(r2_fesc_add[2]) - as.numeric(r2_fesc[2]), 4)*100),
    "% to the marginal Rsq")

icc(fesc_model)
icc(fesc_add_model)

anova(fesc_model, fesc_add_model, test = "Chisq")
#This test indicates that PAI_TOC significantly improves the model fit

#adding PAI_US adds less than 1% explained variance, so we will not include it, even though the relationships are significant.

#We'll use the non-GEDI-additive model for our 'final' model here
final_nwa <- lmer(sif_par ~ sub_szn * (cci + fesc) + (1|year), data = nwa, REML = FALSE)

#Now test for temporal autocorrelation:

# Create a numeric time-like variable that is unique within year
nwa <- nwa %>%
  arrange(year, doymin) %>%
  group_by(year) %>%
  mutate(row_in_year = row_number()) %>%
  ungroup()

# Fit AR(1) model
final_nwa_ar1 <- lme(
  sif_par ~ sub_szn * (cci + fesc),
  random = ~1 | year,
  correlation = corAR1(form = ~ row_in_year | year),
  data = nwa,
  method = "ML"
)

AIC(final_nwa, final_nwa_ar1)
AIC(final_nwa) - AIC(final_nwa_ar1)

acf(resid(final_nwa), main = "ACF: no AR(1)")
acf(resid(final_nwa_ar1), main = "ACF: with AR(1)")

acf(residuals(final_nwa_ar1, type = "normalized"))

#Some evidence for temporal autocorrelation, improved -- but not made perfect -- by AR(1) correlation structure

nwa_bestmod_data <- nwa %>%
  mutate(
    res = residuals(final_nwa_ar1, type = "pearson"),
    fit = fitted(final_nwa_ar1)
  )

#Now test for spatial autocorrelation -------------------------

# Convert to spatial object in planar CRS
nwa_sf <- st_as_sf(nwa_bestmod_data, coords = c("y", "x"), crs = 4326)
nwa_sf_proj <- st_transform(nwa_sf, crs = 3857)

# Create neighbor list & weights using distance-based neighbors (e.g., 500m)
coords <- st_coordinates(nwa_sf_proj)
nb <- dnearneigh(coords, d1 = 0, d2 = 80000) # Try an 80km radius
table(card(nb) == 0) #Check number of regions with zero neighbors
lw <- nb2listw(nb, style = "W", zero.policy = TRUE) # Create spatial weights

# Run Moran’s I test
moran_test <- moran.test(nwa_sf_proj$res, lw, zero.policy = TRUE)
print(moran_test)

#Strong evidence for spatial autocorrelation!!!

#We'll try and account for spatial autocorrelation using a GAMM model with a thin plate spline ---------------------------

# Extract projected coordinates into columns
coords_proj <- st_coordinates(nwa_sf_proj)
nwa$x_m <- coords_proj[, 1]
nwa$y_m <- coords_proj[, 2]

sum(duplicated(nwa[, c("x_m", "y_m")])) #nonzero means duplicated locations (repeat years)

# Add small jitter to break spatial ties
nwa <- nwa %>%
  mutate(
    x_jit = jitter(x_m, amount = 1),
    y_jit = jitter(y_m, amount = 1)
  )
sum(duplicated(nwa[, c("x_jit", "y_jit")])) #should now be zero

#Now we'll test a a GAMM model accounting for BOTH spatial and temporal autocorrelation-------------
gamm_spatiotemp_nwa <- gamm(
  sif_par ~ sub_szn * (cci + fesc) + 
    s(x_jit, y_jit, bs = "tp", k = 100), #thin plate spline
  correlation = corAR1(form = ~ row_in_year | year), #AR1 correlation structure
  random = list(year = ~1),
  data = nwa,
  method = "ML"
)
nwa$res_gamm <- residuals(gamm_spatiotemp_nwa$gam)

nwa_sf <- st_as_sf(nwa, coords = c("x_jit", "y_jit"), crs = 3857)
nwa_sp <- as_Spatial(nwa_sf)

#New
vgm_emp <- variogram(res_gamm ~ 1, data = nwa_sp)
plot(vgm_emp, main = "Semivariogram of GAM residuals")
vgm_model <- fit.variogram(vgm_emp, model = vgm("Exp"))
plot(vgm_emp, model = vgm_model, main = "Fitted Variogram (Exponential Model)")

#Original
vgm_orig <- variogram(res ~ 1, data = as_Spatial(nwa_sf_proj))
plot(vgm_orig, main = "Semivariogram of GAM residuals")
vgm_model_orig <- fit.variogram(vgm_orig, model = vgm("Exp"))
plot(vgm_orig, model = vgm_model_orig, main = "Fitted Variogram (Exponential Model)")

#The variograms suggest we have slightly improved the autocorrelation

#Finally, to test nonlinearity in predictors, create a GAMM model with smoothers using corAR1 -----------------------------
gamm_s_spatiotemp_nwa <- gamm(
  sif_par ~ sub_szn + 
    s(fesc, by = sub_szn) +
    s(cci, by = sub_szn) +
    s(x_jit, y_jit, bs = "tp", k = 100),
  correlation = corAR1(form = ~ row_in_year | year),
  random = list(year = ~1),
  data = nwa,
  method = "ML"
)

#Compare all 'final' models:
aic_vals_nwa <- data.frame(
  Model = c("lmer", "lmer + AR1", "GAMM Spat + AR1", "GAMM Spat + AR1 + s(var)"),
  n_obs = nrow(nwa),
  log_lik = c(logLik(final_nwa),
              logLik(final_nwa_ar1),
              logLik(gamm_spatiotemp_nwa$lme),
              logLik(gamm_s_spatiotemp_nwa$lme)
  ),
  AIC = c(
    AIC(final_nwa),
    AIC(final_nwa_ar1),
    AIC(gamm_spatiotemp_nwa$lme),
    AIC(gamm_s_spatiotemp_nwa$lme)
  ),
  R2m = c(
    r.squaredGLMM(final_nwa)[1],
    r.squaredGLMM(final_nwa_ar1)[1],
    r.squaredGLMM(gamm_spatiotemp_nwa$lme)[1],
    r.squaredGLMM(gamm_s_spatiotemp_nwa$lme)[1]
  ),
  R2c = c(
    r.squaredGLMM(final_nwa)[2],
    r.squaredGLMM(final_nwa_ar1)[2],
    r.squaredGLMM(gamm_spatiotemp_nwa$lme)[2],
    r.squaredGLMM(gamm_s_spatiotemp_nwa$lme)[2]
  ),
  stringsAsFactors = FALSE
) %>% 
  arrange(AIC) %>%
  mutate(deltaAIC = AIC - min(AIC)) %>% 
  select(Model, AIC, deltaAIC, everything())

print(aic_vals_nwa)

#The best model is the GAMM accounting for spatial and temporal autocorrelation!

best_lme_nwa <- gamm_spatiotemp_nwa$lme
best_gam_nwa <- gamm_spatiotemp_nwa$gam

#Extract residuals and fitted values
nwa$res_best <- residuals(best_lme_nwa, type = "normalized")
nwa$fit_best <- fitted(best_lme_nwa)

#Check heteroskedasticity
ggplot(nwa, aes(x = fit_best, y = res_best, color = sub_szn)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Best Model)", x = "Fitted", y = "Residuals") +
  theme_minimal()

#QQ-plot
ggplot(nwa, aes(x = qqnorm(nwa$res_best, plot.it = FALSE)$x, y = qqnorm(res_best, plot.it = FALSE)$y)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +
  labs(title = "QQ Plot of Residuals",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

#Residual symmetry
ggplot(nwa, aes(x = "all", y = res_best)) +
  geom_boxplot() +
  labs(title = "Residual Symmetry (Best Model)", x = "", y = "Residuals") +
  theme_minimal()

#Residuals vs fesc by season
ggplot(nwa, aes(x = fesc, y = res_best, color = sub_szn, shape = sub_szn)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs fesc by Season", x = "fesc", y = "Residuals") +
  theme_minimal()

#Residuals vs cci by season
ggplot(nwa, aes(x = cci, y = res_best, color = sub_szn, shape = sub_szn)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs cci by Season", x = "cci", y = "Residuals") +
  theme_minimal()

# ACF for temporal autocorrelation
acf(nwa$res_best, main = "ACF of Residuals")

# One more look at semivariogram
nwa_sp <- as_Spatial(st_as_sf(nwa, coords = c("x_jit", "y_jit"), crs = 3857))
vgm_emp <- variogram(res_best ~ 1, nwa_sp)
vgm_model <- fit.variogram(vgm_emp, model = vgm("Exp"))
plot(vgm_emp, main = "Semivariogram of Residuals (Best Model)")
plot(vgm_emp, model = vgm_model, main = "Fitted Variogram (with exp. model)")

#still evidence for spatial autocorrelation in the result

#Only if the final model is a GAM:

# Estimated DF, p-value, etc.
summary(best_gam_nwa)$s.table  # includes EDF, reference df, F, p-value

# Check basis dimension adequacy
k_check <- mgcv::k.check(best_gam_nwa)
print(k_check) #possibly too few dimensions...

# Visualize smooth for spatial field
gratia::draw(best_gam_nwa, select = 1)  # this assumes s(x_jit, y_jit) is the first smooth


#Now look at N. Amz -----------------------------------------------------------

# Testing 'the big three' reflectance structural predictors: MODIS LAI, NIRv, and fesc -------------

baseline_model <- lmer(sif_par ~ sub_szn + (1|year), data = noa, REML = FALSE)

mod_list <- list()
mod_list[['Baseline']] <- baseline_model
mod_list[['Fesc']] <- fescmod <- lmer(sif_par ~ sub_szn * fesc + (1|year), data = noa, REML = FALSE)
mod_list[['MODIS LAI']] <- modlaimod <- lmer(sif_par ~ sub_szn * modis_lai + (1|year), data = noa, REML = FALSE)
# mod_list[['NIRv']] <- modlaimod <- lmer(sif_par ~ sub_szn * nirv + (1|year), data = noa, REML = FALSE)

# Type III ANOVA for each model
anova_res <- lapply(mod_list, function(model) car::Anova(model, type = "III"))
anova_summ <- lapply(anova_res, anova_summary)

# AIC Model Selection
aic_res <- aictab(mod_list, modnames = names(mod_list))
print(aic_res)

# Best model selection
best_mod <- mod_list[[which.min(sapply(mod_list, AIC))]]


# Testing the more physiological variables ------------------------

mod_list2 <- list()
mod_list2[['Baseline']] <- baseline_model
mod_list2[['PRI']] <- primod <- lmer(sif_par ~ sub_szn * pri_nar + (1|year), data = noa, REML = FALSE)
mod_list2[['CCI']] <- ccimod <- lmer(sif_par ~ sub_szn * cci + (1|year), data = noa, REML = FALSE)

# Type III ANOVA for each model
anova_res2 <- lapply(mod_list2, function(model) car::Anova(model, type = "III"))
anova_summ2 <- lapply(anova_res2, anova_summary)

# AIC Model Selection
aic_res2 <- aictab(mod_list2, modnames = names(mod_list2))
print(aic_res2)

#CCI is the most highly supported

# Best model selection
best_mod2 <- mod_list2[[which.min(sapply(mod_list2, AIC))]]
print(summary(best_mod2))


# Testing 'the big three' predictors: PAI, PAI_TOC, SDVFP, meanpavd

mod_list3 <- list()
mod_list3[['Baseline']] <- baseline_model
mod_list3[['PAI']] <- paimod <- lmer(sif_par ~ sub_szn * pai + (1|year), data = noa, REML = FALSE)
mod_list3[['PAI TOC']] <- paitocmod <- lmer(sif_par ~ sub_szn * pai_toc + (1|year), data = noa, REML = FALSE)
mod_list3[['PAI US']] <- paiusmod <- lmer(sif_par ~ sub_szn * pai_us + (1|year), data = noa, REML = FALSE)

# Type III ANOVA for each model
anova_res3 <- lapply(mod_list3, function(model) car::Anova(model, type = "III"))
anova_summ3 <- lapply(anova_res3, anova_summary)

# AIC Model Selection
aic_res3 <- aictab(mod_list3, modnames = names(mod_list3))
print(aic_res3)

#PAI_TOC is most highly supported.

# Best model selection
best_mod3 <- mod_list3[[which.min(sapply(mod_list3, AIC))]]
print(summary(best_mod3))


#COMPARE THE CCI MODEL AND Fesc model ----------------

# Fit models
fesc_model <- lmer(sif_par ~ sub_szn * fesc + (1|year), data = noa, REML = FALSE)
cci_model <- lmer(sif_par ~ sub_szn * cci + (1|year), data = noa, REML = FALSE)
fesc_cci_model <- lmer(sif_par ~ sub_szn * (fesc + cci) + (1|year), data = noa, REML = FALSE)

# Compare models again
aic_res4 <- aictab(list(baseline_model, fesc_model, cci_model, fesc_cci_model), modnames = c('baseline', 'fesc', 'cci', 'additive'))
print(aic_res4)

# Adding CCI improves the fit

#COMPARE THE BEST REFLECTANCE MODEL AND BEST LIDAR MODEL ----------------
# "Does adding PAI_TOC improve the fesc model?"

# Fit models
fesc_model <- lmer(sif_par ~ sub_szn * (cci + fesc) + (1|year), data = noa, REML = FALSE)
fesc_add_model <- lmer(sif_par ~ sub_szn * (cci + fesc + pai_toc) + (1|year), data = noa, REML = FALSE)

# Compare models again
aic_res5 <- aictab(list(fesc_model, fesc_add_model), modnames = c('fesc', 'fesc+paitoc'))
print(aic_res5)


car::vif(fesc_add_model) #The model does not appear highly collinear

# Compute marginal and conditional R² values
r2_fesc <- r2_nakagawa(fesc_model)
r2_fesc_add <- r2_nakagawa(fesc_add_model)
print(r2_fesc)
print(r2_fesc_add)

cat("Including lidar adds",
    (round(as.numeric(r2_fesc_add[1]) - as.numeric(r2_fesc[1]), 4)*100),
    "% to the conditional Rsq and",
    (round(as.numeric(r2_fesc_add[2]) - as.numeric(r2_fesc[2]), 4)*100),
    "% to the marginal Rsq")

icc(fesc_model)
icc(fesc_add_model)

anova(fesc_model, fesc_add_model, test = "Chisq")

# PAI_TOC actually makes a relatively strong difference here in the northern Amazon! It has been included in the final model.
final_noa <- lmer(sif_par ~ sub_szn * (cci + fesc + pai_toc) + (1|year), data = noa, REML = FALSE)


#Now test for temporal autocorrelation:

# Create a numeric time-like variable that is unique within year
noa <- noa %>%
  arrange(year, doymin) %>%
  group_by(year) %>%
  mutate(row_in_year = row_number()) %>%
  ungroup()

# Fit AR(1) model
final_noa_ar1 <- lme(
  sif_par ~ sub_szn * (cci + fesc + pai_toc),
  random = ~1 | year,
  correlation = corAR1(form = ~ row_in_year | year),
  data = noa,
  method = "ML"
)

AIC(final_noa, final_noa_ar1)

acf(resid(final_noa), main = "ACF: no AR(1)")
acf(resid(final_noa_ar1), main = "ACF: with AR(1)")

acf(residuals(final_noa_ar1, type = "normalized"))

#Some evidence for temporal autocorrelation, improved -- but not made perfect -- by AR(1) correlation structure

noa_bestmod_data <- noa %>%
  mutate(
    res = residuals(final_noa_ar1, type = "pearson"),
    fit = fitted(final_noa_ar1)
  )

#Now test for spatial autocorrelation -------------------------

# Convert to spatial object in planar CRS
noa_sf <- st_as_sf(noa_bestmod_data, coords = c("y", "x"), crs = 4326)
noa_sf_proj <- st_transform(noa_sf, crs = 3857)

# Create neighbor list & weights using distance-based neighbors (e.g., 500m)
coords <- st_coordinates(noa_sf_proj)
nb <- dnearneigh(coords, d1 = 0, d2 = 80000) # Try an 80km radius
table(card(nb) == 0) #Check number of regions with zero neighbors
lw <- nb2listw(nb, style = "W", zero.policy = TRUE) # Create spatial weights

# Run Moran’s I test
moran_test <- moran.test(noa_sf_proj$res, lw, zero.policy = TRUE)
print(moran_test)

#Evidence for spatial autocorrelation!!!

#We'll try and account for spatial autocorrelation using a GAMM model with a thin plate spline ---------------------------

# Extract projected coordinates into columns
coords_proj <- st_coordinates(noa_sf_proj)
noa$x_m <- coords_proj[, 1]
noa$y_m <- coords_proj[, 2]

sum(duplicated(noa[, c("x_m", "y_m")])) #nonzero means duplicated locations (repeat years)

# Add small jitter to break spatial ties
noa <- noa %>%
  mutate(
    x_jit = jitter(x_m, amount = 1),
    y_jit = jitter(y_m, amount = 1)
  )
sum(duplicated(noa[, c("x_jit", "y_jit")])) #should now be zero

#Now we'll test a a GAMM model accounting for BOTH spatial and temporal autocorrelation-------------
gamm_spatiotemp_noa <- gamm(
  sif_par ~ sub_szn * (cci + fesc + pai_toc) + 
    s(x_jit, y_jit, bs = "tp", k = 100), #thin plate spline
  correlation = corAR1(form = ~ row_in_year | year), #AR1 correlation structure
  random = list(year = ~1),
  data = noa,
  method = "ML"
)
noa$res_gamm <- residuals(gamm_spatiotemp_noa$gam)

noa_sf <- st_as_sf(noa, coords = c("x_jit", "y_jit"), crs = 3857)
noa_sp <- as_Spatial(noa_sf)

#New
vgm_emp <- variogram(res_gamm ~ 1, data = noa_sp)
plot(vgm_emp, main = "Semivariogram of GAM residuals")
vgm_model <- fit.variogram(vgm_emp, model = vgm("Exp"))
plot(vgm_emp, model = vgm_model, main = "Fitted Variogram (Exponential Model)")

#Original
vgm_orig <- variogram(res ~ 1, data = as_Spatial(noa_sf_proj))
plot(vgm_orig, main = "Semivariogram of LME residuals")
vgm_model_orig <- fit.variogram(vgm_orig, model = vgm("Exp"))
plot(vgm_orig, model = vgm_model_orig, main = "Fitted Variogram (Exponential Model)")

#The variograms suggest we have slightly improved the autocorrelation

#Finally, to test nonlinearity in predictors, create a GAMM model with smoothers using corAR1 -----------------------------
gamm_s_spatiotemp_noa <- gamm(
  sif_par ~ sub_szn + 
    s(fesc, by = sub_szn) +
    s(cci, by = sub_szn) +
    s(pai_toc, by = sub_szn) +
    s(x_jit, y_jit, bs = "tp", k = 100),
  correlation = corAR1(form = ~ row_in_year | year),
  random = list(year = ~1),
  data = noa,
  method = "ML"
)

#Compare all 'final' models:
aic_vals_noa <- data.frame(
  Model = c("lmer", "lmer + AR1", "GAMM Spat + AR1", "GAMM Spat + AR1 + s(var)"),
  n_obs = nrow(noa),
  log_lik = c(logLik(final_noa),
              logLik(final_noa_ar1),
              logLik(gamm_spatiotemp_noa$lme),
              logLik(gamm_s_spatiotemp_noa$lme)
  ),
  AIC = c(
    AIC(final_noa),
    AIC(final_noa_ar1),
    AIC(gamm_spatiotemp_noa$lme),
    AIC(gamm_s_spatiotemp_noa$lme)
  ),
  R2m = c(
    r.squaredGLMM(final_noa)[1],
    r.squaredGLMM(final_noa_ar1)[1],
    r.squaredGLMM(gamm_spatiotemp_noa$lme)[1],
    r.squaredGLMM(gamm_s_spatiotemp_noa$lme)[1]
  ),
  R2c = c(
    r.squaredGLMM(final_noa)[2],
    r.squaredGLMM(final_noa_ar1)[2],
    r.squaredGLMM(gamm_spatiotemp_noa$lme)[2],
    r.squaredGLMM(gamm_s_spatiotemp_noa$lme)[2]
  ),
  stringsAsFactors = FALSE
) %>% 
  arrange(AIC) %>%
  mutate(deltaAIC = AIC - min(AIC)) %>% 
  select(Model, AIC, deltaAIC, everything())

print(aic_vals_noa)

#The best model is the GAMM accounting for spatial and temporal autocorrelation!

best_lme_noa <- gamm_spatiotemp_noa$lme
best_gam_noa <- gamm_spatiotemp_noa$gam

#Extract residuals and fitted values
noa$res_best <- residuals(best_lme_noa, type = "normalized")
noa$fit_best <- fitted(best_lme_noa)

#Check heteroskedasticity
ggplot(noa, aes(x = fit_best, y = res_best, color = sub_szn)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Best Model)", x = "Fitted", y = "Residuals") +
  theme_minimal()

#QQ-plot
ggplot(noa, aes(x = qqnorm(noa$res_best, plot.it = FALSE)$x, y = qqnorm(res_best, plot.it = FALSE)$y)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +
  labs(title = "QQ Plot of Residuals",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

#Residual symmetry
ggplot(noa, aes(x = "all", y = res_best)) +
  geom_boxplot() +
  labs(title = "Residual Symmetry (Best Model)", x = "", y = "Residuals") +
  theme_minimal()

#Residuals vs fesc by season
ggplot(noa, aes(x = fesc, y = res_best, color = sub_szn, shape = sub_szn)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs fesc by Season", x = "fesc", y = "Residuals") +
  theme_minimal()

#Residuals vs cci by season
ggplot(noa, aes(x = cci, y = res_best, color = sub_szn, shape = sub_szn)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs cci by Season", x = "cci", y = "Residuals") +
  theme_minimal()

# ACF for temporal autocorrelation
acf(noa$res_best, main = "ACF of Residuals")

# One more look at semivariogram
noa_sp <- as_Spatial(st_as_sf(noa, coords = c("x_jit", "y_jit"), crs = 3857))
vgm_emp <- variogram(res_best ~ 1, noa_sp)
vgm_model <- fit.variogram(vgm_emp, model = vgm("Exp"))
plot(vgm_emp, main = "Semivariogram of Residuals (Best Model)")
plot(vgm_emp, model = vgm_model, main = "Fitted Variogram (with exp. model)")

#Only if the final model is a GAM:

# Estimated DF, p-value, etc.
summary(best_gam_noa)$s.table  # includes EDF, reference df, F, p-value

# Check basis dimension adequacy
k_check <- mgcv::k.check(best_gam_noa)
print(k_check) #possibly too few dimensions...

# Visualize smooth for spatial field
gratia::draw(best_gam_noa, select = 1)  # this assumes s(x_jit, y_jit) is the first smooth


#Overall summaries (for easy reference) -----------------------
print(aic_vals_ca)
print(aic_vals_soa)
print(aic_vals_noa)
print(aic_vals_nwa)

#compare these two southern amazon models and their r-squared values
r.squaredGLMM(gamm_s_spatiotemp_soa$lme)[1] - r.squaredGLMM(gamm_spatiotemp_soa$lme)[1]
r.squaredGLMM(gamm_s_spatiotemp_soa$lme)[2] - r.squaredGLMM(gamm_spatiotemp_soa$lme)[2]

summary(gamm_s_spatiotemp_soa$gam)$r.sq #0.318 
summary(gamm_spatiotemp_soa$gam)$r.sq #0.313 

#Note that only in the SOA does the  s(var) model outperform the base GAMM model, and does so with relatively low deltaAIC, and low impact on R2m (3%), R2c (2%), and R-adj.
# We'll use the same structure for all models

#Final model summaries:
final_model_summ <- data.frame(
  region = c('CA', 'SOA', 'NOA', 'NWA'),
  n_obs = c(
    nrow(ca),
    nrow(soa),
    nrow(noa),
    nrow(nwa)
  ),
  log_lik = c(
    round(logLik(gamm_spatiotemp_ca$lme), 0),
    round(logLik(gamm_spatiotemp_soa$lme), 0),
    round(logLik(gamm_spatiotemp_noa$lme), 0),
    round(logLik(gamm_spatiotemp_nwa$lme), 0)
  ),
  AIC = c(
    round(AIC(gamm_spatiotemp_ca$lme), 0),
    round(AIC(gamm_spatiotemp_soa$lme), 0),
    round(AIC(gamm_spatiotemp_noa$lme), 0),
    round(AIC(gamm_spatiotemp_nwa$lme), 0)
  ),
  R2m = c(
    round(r.squaredGLMM(gamm_spatiotemp_ca$lme)[1], 3),
    round(r.squaredGLMM(gamm_spatiotemp_soa$lme)[1], 3),
    round(r.squaredGLMM(gamm_spatiotemp_noa$lme)[1], 3),
    round(r.squaredGLMM(gamm_spatiotemp_nwa$lme)[1], 3)
  ),
  R2c = c(
    round(r.squaredGLMM(gamm_spatiotemp_ca$lme)[2], 3),
    round(r.squaredGLMM(gamm_spatiotemp_soa$lme)[2], 3),
    round(r.squaredGLMM(gamm_spatiotemp_noa$lme)[2], 3),
    round(r.squaredGLMM(gamm_spatiotemp_nwa$lme)[2], 3)
  ),
  R2adj_GAM = c(
    round(summary(gamm_spatiotemp_ca$gam)$r.sq, 3),
    round(summary(gamm_spatiotemp_soa$gam)$r.sq, 3),
    round(summary(gamm_spatiotemp_noa$gam)$r.sq, 3),
    round(summary(gamm_spatiotemp_nwa$gam)$r.sq, 3)
  ),
  EDF_GAM = c(
    round(sum(summary(gamm_spatiotemp_ca$gam)$edf), 2),
    round(sum(summary(gamm_spatiotemp_soa$gam)$edf), 2),
    round(sum(summary(gamm_spatiotemp_noa$gam)$edf), 2),
    round(sum(summary(gamm_spatiotemp_nwa$gam)$edf), 2)
  ),
  formula = c(
    as.character(gamm_spatiotemp_ca$gam$formula)[3],
    as.character(gamm_spatiotemp_soa$gam$formula)[3],
    as.character(gamm_spatiotemp_noa$gam$formula)[3],
    as.character(gamm_spatiotemp_nwa$gam$formula)[3]
    )
)
print(final_model_summ)



#Next, let's look at whether CCI or NIRv predict Phif ------------------------
phifmodlistca <- list()
phifmodlistca[["CCI"]] <- cciphifca <- lmer(phif ~ sub_szn + cci + (1|year), data = ca, REML = FALSE)
phifmodlistca[["NIRv"]] <- nirvphifca <- lmer(phif ~ sub_szn + nirv + (1|year), data = ca, REML = FALSE)

phifmodlistsoa <- list()
phifmodlistsoa[["CCI"]] <- cciphifsoa <- lmer(phif ~ sub_szn + cci + (1|year), data = soa, REML = FALSE)
phifmodlistsoa[["NIRv"]] <- nirvphifsoa <- lmer(phif ~ sub_szn + nirv + (1|year), data = soa, REML = FALSE)

phifmodlistnoa <- list()
phifmodlistnoa[["CCI"]] <- cciphifnoa <- lmer(phif ~ sub_szn + cci + (1|year), data = noa, REML = FALSE)
phifmodlistnoa[["NIRv"]] <- nirvphifnoa <- lmer(phif ~ sub_szn + nirv + (1|year), data = noa, REML = FALSE)

phifmodlistnwa <- list()
phifmodlistnwa[["CCI"]] <- cciphifnwa <- lmer(phif ~ sub_szn + cci + (1|year), data = nwa, REML = FALSE)
phifmodlistnwa[["NIRv"]] <- nirvphifnwa <- lmer(phif ~ sub_szn + nirv + (1|year), data = nwa, REML = FALSE)

aic_res_phifca <- aictab(phifmodlistca, modnames = names(phifmodlistca))
aic_res_phifsoa <- aictab(phifmodlistsoa, modnames = names(phifmodlistsoa))
aic_res_phifnoa <- aictab(phifmodlistnoa, modnames = names(phifmodlistnoa))
aic_res_phifnwa <- aictab(phifmodlistnwa, modnames = names(phifmodlistnwa))
print(aic_res_phifca)
print(aic_res_phifsoa)
print(aic_res_phifnoa)
print(aic_res_phifnwa)

#CCI is a better predictor of phif, strongly so in the SOA

r2_nakagawa(cciphifsoa)
r2_nakagawa(nirvphifsoa)
r2_nakagawa(cciphifca)
r2_nakagawa(nirvphifca)
r2_nakagawa(cciphifnoa)
r2_nakagawa(nirvphifnoa)
r2_nakagawa(cciphifnwa)
r2_nakagawa(nirvphifnwa)

#but note that the explained variance is still pretty low

