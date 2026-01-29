rm(list=ls())
gc()

library(terra)
library(gtools) #For mixedsort function
library(tidyverse)
library(tidyterra)


# Read in all datasets ------------------------------------------------------------------

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"
boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"

# Dynamically change for each year of processing!
yearid <- "2021"

amz_vect <- vect(paste0(wd, "/amz_shps/amz_biome.shp"))
newcrs <- "EPSG:4326"

# Landcover mask
lc_tree <- rast(paste0(wd, "/mcd12c1_landcover/mcd12c1_2019_lc_masked_amz.tif"))

# Read in a sample SIF Grid
sifpath <- paste0(wd, "/troposif_data/", yearid, "/complete_rast_", yearid)
sifgrid <- rast(nrow = 600, ncol = 744, resolution = 0.05, extent = c(-80.5, -43.3, -21, 9), crs = "EPSG:4326")

# Read in the ecoregions
ecoreg <- vect(paste0(wd, "/amz_shps/amz_biome_subregions.shp"))
florzn <- vect(paste0(wd, "/amz_shps/flor_zn_new.shp"))
amz_geo <- vect(paste0(wd, "/amz_shps/amz_geocrop.shp"))
amz_geo_agg <- vect(paste0(wd, "/amz_shps/amz_geo_agg_extended.shp"))

# Rasterize ecoregions/zones
ecoreg_grid <- rasterize(ecoreg, sifgrid, field = "Subregion")
florzn_grid <- rasterize(florzn, sifgrid, field = "Zone")
geo_grid <- rasterize(amz_geo, sifgrid, field = "name")
geo_agg <- rasterize(amz_geo_agg, sifgrid, field = "region")

reg_grid <- c(ecoreg_grid, florzn_grid, geo_grid, geo_agg)
names(reg_grid) <- c("subregion", "zone", "georeg", "georeg_agg")

# Read in the SIF files -------------------------------------------------------------

siffiles <- mixedsort(list.files(sifpath, pattern=".tif", recursive = TRUE, full.names=TRUE))
# Extract DOY from filenames
sifnumber <- as.numeric(sub(".*_rast_doy(\\d{1,3})\\.tif$", "\\1", siffiles))

# Extract 'doy' values from filenames in 'siffiles' and filter by 'gedinumber'
siffiles_short <- siffiles[sapply(siffiles, function(file) {
  # Use regular expression to extract the DOY from each file name
  doy <- sub(".*_rast_doy(\\d{1,3})\\.tif$", "\\1", file)
  # Check if extracted DOY is in the 'gedinumber' vector
  doy %in% sifnumber
})]

sifname <- unlist(strsplit(unlist(lapply(strsplit(siffiles_short, '/'), function(x) x[10])),".tif"))
sifnameshort <- paste0("sif", as.character(as.numeric(unlist(substring(unlist(lapply(strsplit(sifname, '_'), function(x) x[[4]])), 4, 7)))))

siflist <- lapply(siffiles_short, rast)
names(siflist) <- sifnameshort # Changes list names

# Loop through each tif to assign a doyname
for (i in 1:length(siflist)) {
  rastname <- names(siflist)[i]
  
  doy <- as.numeric(gsub("sif", "", rastname))
  
  siflist[[i]]$doymin <- doy
  
}

# Read in the GEDI data ---------------------------------------------------
gedipath <- paste0(wd, "/gedi_amz_2B_", yearid, "/gedi_gridded")
gedi_rast_files <- mixedsort(list.files(gedipath, pattern = glob2rx("gedi*_gridded.tif*"), recursive = TRUE, full.names = TRUE))

gediname <-  unlist(strsplit(unlist(lapply(strsplit(gedi_rast_files, '/'), function(x) x[9])), ".tif"))
gedishortname <- unlist(substring((lapply(strsplit(gediname, '_'), function(x) x[[1]])), 1, 7))
gedinumber <- unlist(substring((lapply(strsplit(gediname, '_'), function(x) x[[1]])), 5, 7))

# Filter GEDI files to match sifnumber DOYs
gedi_rast_files_short <- gedi_rast_files[gedinumber %in% sifnumber]
gedinumber_short <- gedinumber[gedinumber %in% sifnumber]
gedishortname_short <- gedishortname[gedinumber %in% sifnumber]

# Read and name GEDI rasters
gedilist <- lapply(gedi_rast_files_short, rast)
names(gedilist) <- gedishortname_short

# Clean up names and remove old DOY layers
for (i in seq_along(gedilist)) {
  layer_names <- names(gedilist[[i]])
  new_names <- gsub("_median", "", layer_names)
  names(gedilist[[i]]) <- new_names
  gedilist[[i]]$doymin <- NULL
  gedilist[[i]]$doy <- NULL
}

#Set an empty raster for GEDI DOY padding
empty_rast <- gedilist[[1]]
empty_rast[] <- NA

# Pad missing GEDI DOYs with empty rasters
missing_doys <- setdiff(sifnumber, gedinumber_short)
for (doy in missing_doys) {
  gedilist[[paste0("gedi", doy)]] <- empty_rast
}

# Reorder list by DOY to match sifnumber order
gedilist <- gedilist[order(as.numeric(gsub("gedi", "", names(gedilist))))]

# #Read in the MODIS NBAR data ----------------------------------------------------------
# nbarpath <- paste0(wd, "/mcd43c4_data_", yearid, "/compiled")
# nbar_files <- mixedsort(list.files(nbarpath, pattern = "nbar_16day", recursive = T, full.names=T))
# 
# # Extract 'doy' values from filenames in 'siffiles' and filter by 'gedinumber'
# nbarfiles_short <- nbar_files[sapply(nbar_files, function(file) {
#   # Use regular expression to extract the DOY from each file name
#   doy <- sub(".*_16day_doy(\\d{1,3})\\.tif$", "\\1", file)
#   # Check if extracted DOY is in the 'gedinumber' vector
#   doy %in% sifnumber
# })]
# 
# nbarname <- paste0("nbar_", unlist(substring(unlist(strsplit(unlist(lapply(strsplit(nbarfiles_short, '/'), function(x) x[9])), ".tif")), 15, 18)))
# 
# nbarlist <- lapply(nbarfiles_short, rast)
# names(nbarlist) <- c(nbarname)

#Read in the MODIS NBAR data (bands 1 & 2) ---------------------------------------
nbarpath <- paste0(wd, "/mcd19a1cmgl_data_", yearid, "/compiled")
nbar_files <- mixedsort(list.files(nbarpath, pattern = "nbarb1b2_16day", recursive = T, full.names=T))

# Extract 'doy' values from filenames in 'siffiles' and filter by 'sifnumber'
nbarfiles_short <- nbar_files[sapply(nbar_files, function(file) {
  # Use regular expression to extract the DOY from each file name
  doy <- sub(".*_16day_doy(\\d{1,3})\\.tif$", "\\1", file)
  # Check if extracted DOY is in the 'sifnumber' vector
  doy %in% sifnumber
})]

#CHECK THIS!!
nbarname <- paste0("nbarb1b2_", unlist(substring(unlist(strsplit(unlist(lapply(strsplit(nbarfiles_short, '/'), function(x) x[9])), ".tif")), 19, 22)))

nbarlist <- lapply(nbarfiles_short, rast)
names(nbarlist) <- c(nbarname)

#Read in the MODIS NBAR data (bands 11 & 12) ---------------------------------------
nbarpripath <- paste0(wd, "/mcd19a1cmgo_data_", yearid, "/compiled")
nbarpri_files <- mixedsort(list.files(nbarpripath, pattern = "nbarpri_16day", recursive = T, full.names=T))

# Extract 'doy' values from filenames in 'siffiles' and filter by 'sifnumber'
nbarprifiles_short <- nbarpri_files[sapply(nbarpri_files, function(file) {
  # Use regular expression to extract the DOY from each file name
  doy <- sub(".*_16day_doy(\\d{1,3})\\.tif$", "\\1", file)
  # Check if extracted DOY is in the 'sifnumber' vector
  doy %in% sifnumber
})]

#CHECK THIS!!
nbarpriname <- paste0("nbarpri_", unlist(substring(unlist(strsplit(unlist(lapply(strsplit(nbarprifiles_short, '/'), function(x) x[9])), ".tif")), 18, 21)))

nbarprilist <- lapply(nbarprifiles_short, rast)
names(nbarprilist) <- c(nbarpriname)


#Merge MCD19A1CMG Data ----------

nbar_all <- Map(c, nbarlist, nbarprilist)

# Function for 'CCI'
calc_cci <- function(band11, band1) { 
  (band11 - band1) / (band11 + band1) 
}

# Apply function for each combined raster
nbar_cci <- Map(function(r) {
  cci_layer <- lapp(r[[c("refl_b11", "refl_b1")]], fun = calc_cci)
  names(cci_layer) <- "cci"
  return(cci_layer)
}, nbar_all)


# Add CCI layers to nbar_all
nbar_combined <- Map(c, nbar_all, nbar_cci)

# Crop the MODIS data
nbar_crop <- lapply(nbar_combined, function(x) crop(x, amz_vect, mask = T))



#Read in the FPAR data ----------------------------------------------------------------
fparpath<- paste0(wd, "/mod15a2h_fpar_data_", yearid, "/resampled_rast")
fpar_files <- mixedsort(list.files(fparpath, pattern = "resampled_", recursive = T, full.names=T))

# Extract 'doy' values from filenames in 'fpar_files' and filter by 'sifnumber'
fparfiles_short <- fpar_files[sapply(fpar_files, function(file) {
  # Use regular expression to extract the DOY from each file name
  doy <- as.character(as.numeric(sub(".*_FPAR_LAI_\\d{4}(\\d{1,3})\\.tif$", "\\1", file)))
  # Check if extracted DOY is in the 'sifnumber' vector
  doy %in% sifnumber
})]

fparname <- paste0("fpar_doy", as.character(as.numeric(unlist(substring(unlist(strsplit(unlist(lapply(strsplit(fparfiles_short, '/'), function(x) x[9])), ".tif")), 34, 36)))))

fparlist <- lapply(fparfiles_short, rast)
names(fparlist) <- c(fparname)

#Crop the fPAR data
fparlist_crop <- lapply(fparlist, function(x) crop(x, amz_vect, mask = T))



#Read in the IMERG precip data --------------------------------------------------

iprecpath <- paste0(wd, "/gpm_precip_data/gpm_prec_amz")
iprec_files <- mixedsort(list.files(
  iprecpath,
  pattern = paste0("^amz_prec_", yearid, "_\\d{3}\\.tif$"),  # matches .tif only
  recursive = TRUE,
  full.names = TRUE
))

# Extract 'doy' values from filenames in 'fpar_files' and filter by 'sifnumber'
iprecfiles_short <- iprec_files[sapply(iprec_files, function(file) {
  # Extract DOY using regular expression
  match <- regmatches(file, regexpr(paste0("amz_prec_", yearid, "_(\\d{3})\\.tif$"), file, perl = TRUE))
  
  if (length(match) > 0) {
    doy <- as.numeric(sub(paste0("amz_prec_", yearid, "_(\\d{3})\\.tif$"), "\\1", match))
    doy %in% sifnumber
  } else {
    FALSE
  }
})]

# Extract base filenames
basenames <- basename(iprecfiles_short)
doy_vals <- sub("^amz_prec_\\d{4}_(\\d{3})\\.tif$", "\\1", basenames)
doy_numeric <- as.numeric(doy_vals)
iprecname <- paste0("prec_doy", doy_numeric)

ipreclist <- lapply(iprecfiles_short, rast)
names(ipreclist) <- c(iprecname)

#Resample to match sifgrid
ipreclist_resampled <- lapply(ipreclist, function(r) {
  # Select the first two layers
  r_sel <- r[[1:2]]
  
  # Resample to match sifgrid resolution and extent
  r_resampled <- resample(r_sel, sifgrid, method = "bilinear")
  
  # Rename the layers after resampling
  names(r_resampled) <- c("iprec", "iprec_sd")
  
  # Return the renamed, resampled raster
  r_resampled
})

ipreclist_crop <- lapply(ipreclist_resampled, function(x) crop(x, amz_vect, mask = T))


#Read in the PAR data --------------------------------------------------------

parpath <- paste0(wd, "/par_data/processed")

toa_par <- rast(paste0(parpath, "/toa_par_amz_", yearid, ".tif"))
toc_par <- rast(paste0(parpath, "/toc_par_amz_", yearid, ".tif"))
delta_par <- rast(paste0(parpath, "/delta_par_amz_", yearid, ".tif"))

layer_names_toa <- names(toa_par)
layer_names_toc <- names(toc_par)
layer_names_delta <- names(delta_par)

# Extract DOY from each layer name, filter by 'sifnumber'
keep_layers_toa <- sapply(layer_names_toa, function(layer) {
  # Extract DOY and convert to numeric to drop leading zeros
  doy <- as.character(as.numeric(sub(".*_doy(\\d+)$", "\\1", layer)))
  # Check if DOY is in 'sifnumber'
  doy %in% sifnumber
})

# Extract DOY from each layer name, filter by 'sifnumber'
keep_layers_toc <- sapply(layer_names_toc, function(layer) {

  doy <- as.character(as.numeric(sub(".*_doy(\\d+)$", "\\1", layer)))
  # Check if DOY is in 'sifnumber'
  doy %in% sifnumber
})

# Extract DOY from each layer name, filter by 'sifnumber'
keep_layers_delta <- sapply(layer_names_delta, function(layer) {
  
  doy <- as.character(as.numeric(sub(".*_doy(\\d+)$", "\\1", layer)))
  # Check if DOY is in 'sifnumber'
  doy %in% sifnumber
})

# Subset 'toa_par' to keep only matching layers
toa_par <- toa_par[[which(keep_layers_toa)]]

# Subset 'toa_par' to keep only matching layers
toc_par <- toc_par[[which(keep_layers_toc)]]

# Subset 'toa_par' to keep only matching layers
delta_par <- delta_par[[which(keep_layers_delta)]]


#Read in the physical/meteorological datasets ------------------------------------------

soil_amz <- rast(paste0(wd, "/clim_and_soil_", yearid, "/amz_soils.tif")) #6 layers
def_amz <- rast(paste0(wd, "/clim_and_soil_", yearid, "/cwd_", yearid, ".tif")) #12 layers; 12 months, 1 cwd x month
prec_amz <- rast(paste0(wd, "/clim_and_soil_", yearid, "/amz_prec_monthly.tif")) #12 layers; 12 months
tavg_amz <- rast(paste0(wd, "/clim_and_soil_", yearid, "/amz_tavg_monthly.tif")) #12 layers; 12 months
vapr_amz <- rast(paste0(wd, "/clim_and_soil_", yearid, "/amz_vapr_monthly.tif")) #12 layers; 12 months

#Restructure Climate Data -------------------------------------------------------------------

#Restructure the climatological datsets into list format
deflist <- as.list(def_amz)
preclist <- as.list(prec_amz)
tavglist <- as.list(tavg_amz)
vaprlist <- as.list(vapr_amz)

# Loop through the lists and change the layer names
for (i in seq_along(deflist)) {
  names(deflist[[i]])[1] <- "cwd"
}

for (i in seq_along(preclist)) {
  names(preclist[[i]])[1] <- "prec"
}

for (i in seq_along(tavglist)) {
  names(tavglist[[i]])[1] <- "tavg"
}

for (i in seq_along(vaprlist)) {
  names(vaprlist[[i]])[1] <- "vapr"
}

#Name each element of the climatological lists
names(deflist) <-  c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec") #names the items of the list
names(preclist) <-  c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec") #names the items of the list
names(tavglist) <-  c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec") #names the items of the list
names(vaprlist) <-  c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec") #names the items of the list

#Merge the lists together so we have 4 climatic variables per month
climlist <- Map(c, deflist, preclist, tavglist, vaprlist)
#break into months

# Replicate the monthly list to match the length of the bimonthly list
repl_climlist <- rep(climlist, each = 2)  # Replicates each monthly element twice

#Note that there is only one GEDI sampling date within the month of May (due to the 16-day sampling window).
#So we'll delete this individual replicate from the replicate list.
clim_prune <- repl_climlist[-10] #this should be the second May entry

#Test that this worked
names(clim_prune) #All good. Only one May left.

#crop climate date
clim_crop <- lapply(clim_prune, function(x) crop(x, amz_vect, mask = T))

# Convert DOY to Date, then extract the month
sif_months <- unique(format(as.Date(as.numeric(sifnumber) - 1, origin = "2020-01-01"), "%b"))
sif_months <- tolower(sif_months)  # Convert to lowercase for consistency with 'clim_crop' naming

# Filter 'clim_crop' to keep only elements that match 'gedi_months'
clim_crop_filtered <- clim_crop[sapply(names(clim_crop), function(layer_name) {
  # Extract the month abbreviation from the layer name (assuming it matches 'jan', 'feb', etc.)
  month_in_layer <- tolower(substr(layer_name, 1, 3))  # Adjust if month appears in a different position
  # Check if the month is in 'gedi_months'
  month_in_layer %in% sif_months
})]

# Restructure the PAR data ---------------------------------------------------

toaparlist <- terra::as.list(toa_par)
tocparlist <- terra::as.list(toc_par)
deltaparlist <- terra::as.list(delta_par)

parvars <- Map(c, toaparlist, tocparlist, deltaparlist)

# Assuming partoalist is your list of SpatRaster objects
for (i in seq_along(parvars)) {
  # Extracting the day of year from the original name
  doy <- gsub(".*doy", "", names(parvars[[i]][[1]]))
  
  # Rename the list element
  names(parvars)[i] <- paste0("doy", doy)
  
  # Rename the layer within the SpatRaster
  names(parvars[[i]]) <- c("par_toa", "par_toc", "par_delta")
}

# # Crop
# parvars_crop <- lapply(parvars_mw, function(x) crop(x, amz_vect, mask = T))
# Original, before dividing by 1000
parvars_crop <- lapply(parvars, function(x) crop(x, amz_vect, mask = T))

# Combine all list elements into a master dataset ------------------------------------------
# Add stationary data into the dataset (floristic zones, soils, pH, etc)
static_vars <- c(soil_amz, reg_grid)

static_crop <- crop(static_vars, amz_vect, mask = T)

# First append the static variables to the gedilist
for (i in seq_along(gedilist)) {
  gedilist[[i]] <- c(gedilist[[i]], static_crop)
}

#combine.
clim_nirv <- Map(c, clim_crop_filtered, nbar_crop, fparlist_crop, siflist, parvars_crop, ipreclist_crop)

#Mask by landcover type
clim_nirv_lcmsk <- lapply(clim_nirv, function(x) mask(x, lc_tree))

#combine with GEDI data
gedi_clim_nirv <- Map(c, gedilist, clim_nirv_lcmsk)

testset <- gedi_clim_nirv[[8]]
plot(testset[[c(3,28,29,41,48, 67)]], legend = FALSE)
plot(testset[[3]])


## SIF Calculations  -------------------------------------------------------------

# Define the mutation function
#(per Zhang et al 2023, Badgley et al. 2017, Zeng et al 2019
sif_calc_function <- function(raster) {
  # Ensure all required layers are present
  required_layers <- c("sif743_cor", "par_toc", "refl_b2", "ndvi", "nirv", "rfl_781", "rfl_665", "fpar", "toarad743")
  if (!all(required_layers %in% names(raster))) {
    stop("Missing required layers: ", paste(setdiff(required_layers, names(raster)), collapse = ", "))
  }
  
  # Perform calculations
  apar <- raster$fpar * (raster$par_toc * 1000) # APAR, with PAR converted from W to mW
  
  sif_apar <- raster$sif743_cor / apar #SIF yield
  sif_par <- raster$sif743_cor / (raster$par_toc * 1000) # SIF/PAR, with PAR converted from W to mW
  sifs_par <- raster$sif743 / (raster$par_toc * 1000) # SIF/PAR, with PAR converted from W to mW
  sifc_par <- raster$sifcor_csza / (raster$par_toc * 1000) # SIF/PAR, with PAR converted from W to mW
  
  #MODIS CALCULATIONS
  nirvp <- raster$nirv * (raster$par_toc * 1000) #MODIS NIRv
  phif <- raster$sif743_cor / nirvp #MODIS PhiF; Dechant et al 2022 RSE
  fesc <- raster$nirv / raster$fpar #MODIS fesc
  
  #TROPOSIF CALCULATIONS
  sif_rel_tropo <- raster$sif743_cor / raster$toarad743 #SIF rel, radiance, tropo; Zhang et al 2023 GCB
  #sifc_rel_tropoc <- raster$sifcor_csza / raster$toarad_csza
  
  ndvi_tropo <- (raster$rfl_781 - raster$rfl_665) / (raster$rfl_781 + raster$rfl_665) #ndvi, refl
  
  nirv_tropo_refl <- ndvi_tropo * raster$rfl_781 #using TROPO reflectance for NIRv
  nirv_tropo_rad <- ndvi_tropo * raster$toarad743 #using TROPO radiance for NIRv
  #nirv_tropoc_rad <- ndvi_tropo * raster$toarad_csza
  
  #nirvp_tropo_refl <- nirv_tropo_refl * (raster$par_toc * 1000) #NIRvP, using refl
  nirvp_tropo_rad <- nirv_tropo_rad * (raster$par_toc * 1000) #NIRvP, using radiance
  
  #phif_tropo_refl <- raster$sif743_cor / nirvp_tropo_refl #Reflectance approach; Dechant et al 2022 RSE
  phif_tropo_rad <- raster$sif743_cor / nirvp_tropo_rad #Radiance approach
  
  #fesc_tropo_refl <- nirv_tropo_refl / raster$fpar
  fesc_tropo_rad <- nirv_tropo_rad / raster$fpar
  
  raster$apar <- apar
  raster$sif_apar <- sif_apar
  raster$sif_par <- sif_par
  raster$sifs_par <- sifs_par
  raster$sifc_par <- sifc_par
  raster$nirvp <- nirvp
  raster$phif <- phif
  raster$fesc <- fesc
  
  raster$sif_rel_tropo <- sif_rel_tropo
  #raster$sifc_rel_tropoc <- sifc_rel_tropoc
  raster$ndvi_tropo <- ndvi_tropo
  raster$nirv_tropo_refl <- nirv_tropo_refl
  raster$nirv_tropo_rad <- nirv_tropo_rad
  #raster$nirv_tropoc_rad <- nirv_tropoc_rad
  #raster$nirvp_tropo_refl <- nirvp_tropo_refl
  raster$nirvp_tropo_rad <- nirvp_tropo_rad
  #raster$phif_tropo_refl <- phif_tropo_refl
  raster$phif_tropo_rad <- phif_tropo_rad
  #raster$fesc_tropo_refl <- fesc_tropo_refl
  raster$fesc_tropo_rad <- fesc_tropo_rad
  
  return(raster)
}


# Apply the mutation function to each raster in the list
rast_compile <- lapply(gedi_clim_nirv, sif_calc_function)

#test
names(rast_compile[[1]])

#Rename one variable that was shortened in processing
rast_compile <- lapply(rast_compile, function(r) {
  names(r)[1] <- "sensitivity"
  return(r)
})

#testing
rasttest <- rast_compile[[13]]
# plot(rasttest$pai)
# plot(rasttest$sif_par)
# plot(rasttest$sifc_par)
# plot(rasttest$sif_apar)
# plot(rasttest$cci)
# plot(rasttest$iprec)
# plot(rasttest$phif)
# plot(rasttest$fesc)
# plot(rasttest$fesc_tropo_refl)
# plot(rasttest$fesc_tropo_rad)
# plot(rasttest$nirvp)
# plot(rasttest$nirv_tropo_refl)
plot(rasttest$nirv_tropo_rad)
plot(rasttest$nirv_tropoc_rad)
# plot(rasttest$phif_tropo_rad)
# plot(rasttest$phif_tropo_refl)
# plot(rasttest$sif_rel_tropo)

#Extract dataframes -----------------------------------------------------

# Create a dataset where the PAI NA values WERE eliminated (n = thousands)
gedi_df <- do.call(rbind, lapply(rast_compile, function(i) {
  # Convert raster to data frame
  df <- terra::as.data.frame(i, xy = TRUE, cells = FALSE)

  # Filter rows where 'pai' is not NA
  df <- df[!is.na(df$pai), ]

  return(df)
}))

# # Create a dataset where the PAI NA values were NOT eliminated, but SIF/PAR NA values were (n = millions)
gedi_naincl_df <- do.call(rbind, lapply(rast_compile, function(i) {
  # Convert raster to data frame
  df <- terra::as.data.frame(i, xy = TRUE, cells = FALSE)

  # Filter rows where 'sif_par' is not NA
  df <- df[!is.na(df$sif_par), ]

  return(df)
}))


#Write csvs to Box ----------------------------------------------------------------------

write.csv(gedi_df, paste0(boxwd, "/complete_data", "/gedi_df_complete_", yearid, ".csv"), row.names = FALSE)

write.csv(gedi_naincl_df, paste0(boxwd, "/complete_data", "/gedi_df_complete_naincl_", yearid, ".csv"), row.names = FALSE)


#Save individual rasters --------------------------------------------------------------------

rastpath <- paste0(boxwd, "/compiled_rasters/", yearid)

# Loop to save each raster with the numeric string from the element name
for (i in 1:length(rast_compile)) {
  # Get the name of the list element
  element_name <- names(rast_compile)[i]
  
  # Extract the numeric string from the element name using regular expressions
  numeric_string <- gsub("[^0-9]", "", element_name)
  
  # Define the filename using the extracted numeric string
  filename <- paste0("/", "complete_rast_", yearid, numeric_string, ".tif")
  
  # Save the raster with the specified filename
  writeRaster(rast_compile[[i]], file.path(paste0(rastpath, filename)), overwrite = TRUE)
}

#End of script

