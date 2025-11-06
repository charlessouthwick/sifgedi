# Sample MOD15A2H data with GEDI returns to compare MODIS LAI and GEDI PAI

rm(list=ls())
gc()

library(tidyverse)
library(terra)
library(gtools)
library(tidyterra)
library(rstatix)
library(parallel)


#This code needs ~110 GB of free memory!

yearid <- "2021"

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"
boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"

amz_vect <- vect(paste0(boxwd, "/amz_shps/amz_biome.shp"))
georeg_agg_v <- vect(paste0(boxwd, "/amz_shps/amz_geo_agg_extended.shp"))

#load an fPAR/LAI file for use as a grid --------------------

modgrid <- rast(paste0(wd, "/mod15a2h_fpar_data_2019/final_process/amz_clean_FPAR_LAI_2019001.tif"))

georeg_agg_r <- rasterize(georeg_agg_v, modgrid, field = "region")

##GEDI files ------------------------------

gedipath <- paste0(wd, "/gedi_amz_2B_", yearid, "/gedi_shp_processed_20241030")
gedigridpath <- paste0(wd, "/gedi_amz_2B_", yearid, "/gedi_gridded")

gedi_shp_files <- mixedsort(sort(list.files(gedipath, pattern=glob2rx("amz_gedivect_doy*.shp*"), recursive = T, full.names=T)))

gediname <-  unlist(strsplit(unlist(lapply(strsplit(gedi_shp_files, '/'), function(x) x[9])), ".shp"))

gedishortname <- paste0("gedi", unlist(substring((lapply(strsplit(gediname, '_'), function(x) x[[3]])), 4, 6)))

doy <- as.numeric(gsub("gedi", "", gedishortname))

total_sum <- 0


start_time <- Sys.time()

# Loop through shapefiles, apply operations, and calculate the total valid returns
gedivectlist <- lapply(seq_along(gedi_shp_files), function(i) {
  # Vectorize shapefiles and filter columns
  current_vect <- vect(gedi_shp_files[[i]]) %>%
    select(c(pai, pai_us, pai_toc, doy, modis_tree)) %>%
    filter(modis_tree >= 75) %>% 
    select(-modis_tree)
  
  # Add the number of valid returns to total_sum
  total_sum <<- total_sum + dim(current_vect)[1]
  
  current_vect$doymin <- NULL
  current_vect$doy <- NULL
  
  # Calculate the minimum day of the year for a layer (doy) and add it to the current object
  current_vect$doymin <- doy[i]
  
  # Return the filtered object
  return(current_vect)
})

# Print the times for each file
end_time <- Sys.time()

total_time <- difftime(end_time, start_time, units = "mins")
cat(length(gedi_shp_files), "files processed in ", total_time, "minutes\n")

# Print the total valid returns
cat("After processing, the GEDI files from", yearid, "produce a total of '", total_sum, "' valid returns used for rasterization and analysis\n", sep = "")

# Assign names
names(gedivectlist) <- gedishortname


# Rasterize each value of gedi to the mod15a2h grid --------------------

# PAI positions of interest
pai_vars <- c("pai", "pai_us", "pai_toc")

# Rasterization process
process_rasterization <- function(gedi_ex) {
  gedi_nams <- names(gedi_ex)
  
  # Calculate rasterization for each element in gediminlist
  gedirast_calc <- lapply(gedi_nams, function(x) {
    rasterize(gedi_ex, modgrid, field = x, fun = median)
  })
  
  gedibound <- do.call("c", gedirast_calc)
  
  pai_nams <- names(gedi_ex[, 3]) 
  
  # This function counts the number of returns within a gridcell
  length_pai <- lapply(pai_nams, function(x) {
    rasterize(gedi_ex[, 3], modgrid, field = x, fun = length)
  })
  
  length_pai_bind <- do.call("c", length_pai)
  names(length_pai_bind) <- "number_of_hits"
  
  # This filters the SpatRaster so that only gridcells with >= 10 returns are used
  length_pai_filt <- clamp(length_pai_bind, lower = 10, values = FALSE)
  
  compile <- c(gedibound, length_pai_filt)
  
  # Mask the compiled results
  masked_compile <- mask(compile, anyNA(length_pai_filt), maskvalue = TRUE)
  
  return(masked_compile)
}

start_time <-  Sys.time()
gedirast <- lapply(gedivectlist, process_rasterization)
end_time <- Sys.time()

total_time <- difftime(end_time, start_time, units = "mins")
cat(length(gedivectlist), "shapefiles rasterized in ", total_time, "minutes\n")

rm(gedivectlist)
gc() #free up memory


# Rename each list element
names(gedirast) <- c(gedishortname)

# Load all of the FPAR rasters ---------------------

# note that the mixedsort function is used to assure the files are read in numerically ascending order
fparpath <- paste0(wd, "/mod15a2h_fpar_data_", yearid, "/final_process")

fparfiles <- mixedsort(list.files(fparpath, pattern=".tif", recursive = TRUE, full.names=TRUE))
fparname <- unlist(strsplit(unlist(lapply(strsplit(fparfiles, '/'), function(x) x[9])),".tif"))
fparnameshort <- paste0("fpar", as.character(as.numeric(unlist(substring(unlist(lapply(strsplit(fparname, '_'), function(x) x[[5]])), 5, 7)))))

# Read only the 1st and 4th layers from each raster
fparlist <- lapply(fparfiles, function(file) {
  rast_file <- rast(file)
  subset_rast <- subset(rast_file, c(1, 4))  # Extract layers 1 and 4
  c(subset_rast, georeg_agg = georeg_agg_r)  # Append georeg layer
})

names(fparlist) <- fparnameshort # Changes list names
#Add the georegions as a layer
#fparlist$georeg_agg <- georeg_agg_r

# extract numeric parts from names
gedi_numbers <- as.numeric(sub("gedi", "", names(gedirast)))
fpar_numbers <- as.numeric(sub("fpar", "", names(fparlist)))

# identify matching indices
match_doyind <- which(fpar_numbers %in% gedi_numbers)

# Subset fparlist using the matching indices
selfparlist <- fparlist[match_doyind]

# Check the names of the filtered list
names(selfparlist)

#combinedall data
fpar_gedi <- Map(c, gedirast, selfparlist)

rm(gedirast, selfparlist, fparlist)
gc()


#This next step is quite memory intensive!!-----------------------------

# Combine all rasters into a single data frame
all_df <- do.call(rbind, lapply(fpar_gedi, function(i) {
  # Convert raster to data frame and filter NA values directly
  terra::as.data.frame(i, xy = TRUE, cells = FALSE, na.rm = TRUE)
}))

#Combined dataframe
write.csv(all_df, paste0(boxwd, "/complete_data/gedi_fpar_combined_", yearid, ".csv"), row.names = FALSE)




all_df2 <- all_df %>% filter(number_of_hits > 10)

all_df2 %>%
  ggplot(aes(x = Lai_500m, y = pai_median))+
  geom_point()

all_df2 %>%
  ggplot(aes(x = Lai_500m, y = pai_toc_median))+
  geom_point()

all_df2 %>%
  ggplot(aes(x = Lai_500m, y = pai_us_median))+
  geom_point()



