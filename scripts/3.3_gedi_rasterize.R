
rm(list=ls())
gc()

library(tidyverse)
library(terra)
library(gtools)
library(tidyterra)
library(rstatix)
library(parallel)


wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"

#load a SIF file for use as a grid --------------------

yearid <- "2021"

sifgrid <- rast(paste0(wd, "/troposif_data/2020/complete_rast_2020/amz_troposif_rast_doy1.tif"))

#To Calculate approximate area of one gridcell
utm_crs <- "epsg:32720" #UTM Zone 20S
sifgrid_projected <- project(sifgrid, utm_crs)
res_projected <- res(sifgrid_projected)
print(res_projected)
# So one gridcell is roughly 5.58 km x 5.58 km

##GEDI files ------------------------------

gedipath <- paste0(wd, "/gedi_amz_2B_", yearid, "/gedi_shp_processed_20241030")
gedigridpath <- paste0(wd, "/gedi_amz_2B_", yearid, "/gedi_gridded")

gedi_shp_files <- mixedsort(sort(list.files(gedipath, pattern=glob2rx("amz_gedivect_doy*.shp*"), recursive = T, full.names=T)))

gediname <-  unlist(strsplit(unlist(lapply(strsplit(gedi_shp_files, '/'), function(x) x[9])), ".shp"))

gedishortname <- paste0("gedi", unlist(substring((lapply(strsplit(gediname, '_'), function(x) x[[3]])), 4, 6)))

# Initialize total_sum
total_sum <- 0

start_time <-  Sys.time()

# Loop through shapefiles, apply operations, and calculate the total valid returns
gedivectlist <- lapply(seq_along(gedi_shp_files), function(i) {
  # Vectorize shapefiles and filter columns
  current_vect <- vect(gedi_shp_files[[i]]) %>%
    filter(modis_tree >= 75) %>%
    select(-c(solar_azim, beam, pft_class, landsat_tr, solar_elev, l2b_qual_f, doy4day))
  
  # Add the number of valid returns to total_sum
  total_sum <<- total_sum + dim(current_vect)[1]
  
  # Calculate the minimum day of the year for a layer (doy) and add it to the current object
  current_vect$doymin <- min(current_vect$doy)
  
  # Return the filtered object
  return(current_vect)
})

# Print the times for each file
end_time <- Sys.time()

total_time <- difftime(end_time, start_time, units = "mins")
cat(length(gedi_shp_files), "files processed in ", total_time, "minutes\n")


# Assign names
names(gedivectlist) <- gedishortname

# Print the total valid returns
cat("After processing, the GEDI files from", yearid, "produce a total of '", total_sum, "' valid returns used for rasterization and analysis\n", sep = "")

# Test the first element
geditest <- gedivectlist[[1]]
print(geditest)
names(geditest)


# Rasterize each value ------------------------------

#PAI positions of interest
pai_vars <- c("pai", "pai_us", "pai_toc")

# Rasterization process
process_rasterization <- function(gedi_ex) {
  gedi_nams <- names(gedi_ex)
  
  # Calculate rasterization for each element in gediminlist
  gedirast_calc <- lapply(gedi_nams, function(x) {
    rasterize(gedi_ex, sifgrid, field = x, fun = median)
  })
  
  gedibound <- do.call("c", gedirast_calc)
  
  pai_ind <- pai_vars
  
  # Rasterize for the specified variables (using identified indices) and calculate the mean
  paimeanrast <- lapply(pai_ind, function(x) {
    rasterize(gedi_ex, sifgrid, field = x, fun = mean)
  })
  
  paimeanbound <- do.call("c", paimeanrast)
  names(paimeanbound) <- paste0(pai_vars, "_mean")
  
  pai_nams <- names(gedi_ex[, 3]) 
  
  # This function counts the number of returns within a gridcell
  length_pai <- lapply(pai_nams, function(x) {
    rasterize(gedi_ex[, 3], sifgrid, field = x, fun = length)
  })
  
  length_pai_bind <- do.call("c", length_pai)
  names(length_pai_bind) <- "number_of_hits"
  
  # This filters the SpatRaster so that only gridcells with >= 100 returns are used
  length_pai_filt <- clamp(length_pai_bind, lower = 100, values = FALSE)
  
  compile <- c(gedibound, paimeanbound, length_pai_filt)
  
  # Mask the compiled results
  masked_compile <- mask(compile, anyNA(length_pai_filt), maskvalue = TRUE)
  
  return(masked_compile)
}

start_time <-  Sys.time()

# Initialize an empty list to store raster results
gedirast <- lapply(gedivectlist, process_rasterization)

# Print the times for each file
end_time <- Sys.time()

total_time <- difftime(end_time, start_time, units = "mins")
cat(length(gedivectlist), "shapefiles rasterized in ", total_time, "minutes\n")

rm(gedivectlist)
gc() #free up memory

# Rename each list element
names(gedirast) <- c(gedishortname)

#basic summary
freq(gedirast[[1]]$pai_median)
terra::summary(gedirast[[1]]$pai_median)

#Just getting a summary of only the median data
selectlist <- list()
for (i in gedirast) {
  gedi_ex <- i
  gedi_ex_sel <- gedi_ex$pai_median
  gedi_ex_sel$pai_us <- gedi_ex$pai_us_median
  gedi_ex_sel$pai_toc <- gedi_ex$pai_toc_median
  gedi_ex_sel$pai_mean <- gedi_ex$pai_mean
  gedi_ex_sel$doymin <- gedi_ex$doymin_median
  
  selectlist <- c(selectlist, gedi_ex_sel)
}

#This puts all of the gedi data into one raster, then gets a range
spatcollect <- sprc(selectlist)
spatmerge <- merge(spatcollect)
minmax(spatmerge)

#Useful for a quick visualization plot
writeRaster(spatmerge, paste0(wd, "/gedi_pai_rast_one_file_", yearid, ".tif"), overwrite = T)

#Finally, we'll save our rasters separately

for (name in names(gedirast)) {
  # Extract the raster using the name
  raster_data <- gedirast[[name]]
  
  # Save the raster using writeRaster with the same name
  writeRaster(raster_data, file.path(gedigridpath, paste0("/", name, "_gridded.tif")), overwrite = TRUE)
}

