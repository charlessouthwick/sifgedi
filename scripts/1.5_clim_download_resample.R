rm(list=ls())
gc()

#Download WorldClim data

library(tidyverse)
library(terra)
library(geodata)
library(RNetCDF)
library(gtools) # for Mixed Sort function

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"

yearid <- "2019"

#Amazon broad extent
amz_ext <-  c(-80.5, -43.3, -21, 9)

#Use the SIF extent for rasterization.
sifgrid <- rast(nrow = 600, ncol = 744, resolution = 0.05, extent = c(-80.5, -43.3, -21, 9), crs = "EPSG:4326")


#pull climate data from WorldClim 2.1
#precip (mm)
wrldprec <- worldclim_global(var = "prec", res = 2.5, path = tempdir(), version="2.1")
prec_amz <- terra::crop(wrldprec, amz_ext)

#temperature (ºC)
wrldtavg <- worldclim_global(var = "tavg", res = 2.5, path = tempdir(), version="2.1")
tavg_amz <- terra::crop(wrldtavg, amz_ext)

#vapor pressure (kPa)
wrldvapr <- worldclim_global(var = "vapr", res = 2.5, path = tempdir(), version="2.1")
vapr_amz <- terra::crop(wrldvapr, amz_ext)

wrld_clim <- rast( list(prec_amz, tavg_amz, vapr_amz)  )
#pull soils data from soilGRIDS database (https://www.isric.org/explore/soilgrids) 30arcsec res

#0-5 cm soil data
wrldvars5 <- soil_world(var = c("nitrogen", "phh2o", "soc"), depth = 5, stat="mean", path = tempdir())
vars5_amz <- terra::crop(wrldvars5, amz_ext)

#15-30cm soil data
wrldvars30 <- soil_world(var = c("nitrogen", "phh2o", "soc"), depth = 30, stat="mean", path = tempdir())
vars30_amz <- terra::crop(wrldvars30, amz_ext)

soil_amz <- rast(list(vars5_amz, vars30_amz))


#resample soil data to same extent as sifgrid

prec_samp <- terra::resample(prec_amz, sifgrid, method = "bilinear")
vapr_samp <- terra::resample(vapr_amz, sifgrid, method = "bilinear")
tavg_samp <- terra::resample(tavg_amz, sifgrid, method = "bilinear")
clim_samp <- terra::resample(wrld_clim, sifgrid, method = "bilinear") #just to have the full sample
soil_samp <- terra::resample(soil_amz, sifgrid, method = "bilinear")

plot(clim_samp)
plot(soil_samp)
plot(vapr_samp)

#save files
writeRaster(prec_samp, paste0(wd, "/clim_and_soil_", yearid, "/amz_prec_monthly.tif"), overwrite = TRUE)

writeRaster(vapr_samp, paste0(wd, "/clim_and_soil_", yearid, "/amz_vapr_monthly.tif"), overwrite = TRUE)

writeRaster(tavg_samp, paste0(wd, "/clim_and_soil_", yearid, "/amz_tavg_monthly.tif"), overwrite = TRUE)

writeRaster(clim_samp, paste0(wd, "/clim_and_soil_", yearid, "/amz_clim_monthly.tif"), overwrite = TRUE)

writeRaster(soil_samp, paste0(wd, "/clim_and_soil_", yearid, "/amz_soils.tif"), overwrite = TRUE)



#Precip from GPM FINAL -----------------------
# rainwd <- "/Users/charlessouthwick/Documents/PhD/sifgedi/clim_and_soil/GPM_IMERG_precip_data/"
# 
# rainlist <- mixedsort(list.files(rainwd, pattern=".HDF5", recursive = TRUE, full.names=TRUE))
# 
# testrain <- rast(paste0(rainwd, "3B-MO.MS.MRG.3IMERG.20200101-S000000-E235959.01.V07B.HDF5"))[[2]]
# testflip <- flip(t(testrain), "v")
# plot(testflip)
# 
# old_ext <- ext(testflip)
# 
# old_ext[2] <- old_ext[2] - 3600
# 
# # Change the extent
# ext(prec_new_ext) <- testflip

 ####################################################################
#TerraClimate - for CWD
#Enter lat and lon ranges
lat.range=c(-21, 9)        #! Ranges instead of point values. Order does not matter
lon.range=c(-80.5, -43.3)

# enter in variable you want to download see: http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html

var="def"

deficit <- paste0(wd, "/terra_climate/TerraClimate_def_", yearid, ".nc")

nc <- open.nc(deficit)
lon <- var.get.nc(nc, "lon")
lat <- var.get.nc(nc, "lat")
lat.range <- sort(lat.range)                              #!sort user input values from low to high
lon.range <-sort(lon.range)
lat.index <- which(lat>=lat.range[1]&lat<=lat.range[2])    #! index values within specified range
lon.index <- which(lon>=lon.range[1]&lon<=lon.range[2])    
lat.n <- length(lat.index)                                #!value for count
lon.n <- length(lon.index)
start <- c(lon.index[1], lat.index[1], 1)
count <- c(lon.n, lat.n, NA)                            #! parameter change: 'NA' instead of '-1' to signify entire dimension


# read in the full period of record using aggregated files

deficit_data <-var.get.nc(nc, variable = var,start = start, count,unpack=TRUE)    #! argument change: 'variable' instead of 'varid'  # Output is now a matrix

close.nc(nc)

#scale_factor <- 0.1 #this is the scale factor information stored in the NetCDF file

defrast <- rast(deficit_data)
crs(defrast) <- "epsg:4326"
ext(defrast) <- c(-80.5, -43.3, -21, 9)

#defrast <- deficit_data * 0.1

#resample to sifgrid extent
defrast_samp <- terra::resample(defrast, sifgrid, method = "bilinear")

names(defrast_samp) <- c("jan_def", "feb_def", "mar_def", "apr_def", "may_def", "jun_def", "jul_def", "aug_def", "sep_def", "oct_def", "nov_def", "dec_def")

writeRaster(defrast_samp, paste0(wd, "/clim_and_soil_", yearid, "/cwd_", yearid, ".tif"), overwrite = TRUE)


