#Extract GEDI data


library(tidyverse)
library(terra)
library(ncdf4)
library(ncdf4.helpers)
library(raster)
library(rhdf5)
library(bit64)
library(parallel)
library(progressr)

rm(list=ls())
gc()

# Choose one of 2019, 2020, 2021
yearid <- "2021"

#Set a 32GB vector size limit
Sys.setenv(R_MAX_VSIZE = 16 * 1024^3)

print(as.numeric(Sys.getenv("R_MAX_VSIZE")) / (1024^3))  # Should print 32

# How many cores are available?
parallel::detectCores()

#how many cores to use?
num_cores <- 16

#Set up paths ---------------------------------

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"

parent_directory <- paste0(wd, "/gedi_amz_2B_", yearid)
lastpath <- paste0(parent_directory, "/gedi_shp_processed_20241030")

amz_vect <- vect(paste0(wd, "/amz_shps/amz_biome.shp"))
amz_ext <-  c(-80.5, -43.3, -21, 9)

# Define a function to extract all of the GEDI information of interest -----------------------

#This will only work for the particular directory setup in this project
extract_gedi <- function(parent_directory, amz_ext) {
  start_time <- Sys.time()
  
  # Get a list of subdirectories in the parent directory
  subdirectories <- list.dirs(parent_directory, full.names = TRUE, recursive = FALSE)
  
  # Filter subdirectories with names in Y m d format
  date_subdirectories <- subdirectories[grepl("\\d{4}\\.\\d{2}\\.\\d{2}", subdirectories)]
  
  total_files <- sum(sapply(date_subdirectories, function(date_folder) {
    length(list.files(date_folder, pattern = "h5", recursive = TRUE, full.names = TRUE))
  }))
  
  processed_files <- 0  # Initialize the counter for processed files
  
  for (date_folder in date_subdirectories) {
    gedipath <- date_folder
    gediprocpath <- file.path(gedipath, "processed")
    
    gedifiles <- unlist(list.files(gedipath, pattern="h5", recursive = TRUE, full.names=TRUE))
    
    results <- mclapply(seq_along(gedifiles), function(i) {
      gedidf <- NULL
      
      gediname <- unlist(strsplit(unlist(lapply(strsplit(gedifiles[i], "/"), function(x) x[9])),".h5"))
      
      doy <- as.numeric(substring(unlist(lapply(strsplit(gediname, "_"), function(x) x[3])), 5, 7))
      
      temp <- h5ls(gedifiles[i])
      temp <- temp[which(temp$dim != "1"),]
      temp <- temp[-which(temp$dim==""),]
      beams <- unique(unlist(lapply(strsplit(temp$group, "/"), function (x) x[2])))
      
      fn <- gedifiles[i] 
      temp_j <- vector("list", length(beams))
      
      for (j in seq_along(beams)) {
        
        temp <- data.frame(
          beam = as.numeric(h5read(fn, paste("/",beams[j],"/beam",sep=""))),
          channel = as.numeric(h5read(fn, paste("/",beams[j],"/channel",sep=""))),
          lat_lowestmode = as.numeric(h5read(fn, paste("/",beams[j],"/geolocation/lat_lowestmode",sep=""))),
          lon_lowestmode = as.numeric(h5read(fn, paste("/",beams[j],"/geolocation/lon_lowestmode",sep=""))),
          sensitivity = as.numeric(h5read(fn, paste("/",beams[j],"/sensitivity",sep=""))),
          shot_number = as.character(h5read(fn, paste("/",beams[j],"/shot_number",sep=""), bit64conversion="bit64")),
          solar_azimuth = as.numeric(h5read(fn, paste("/",beams[j],"/geolocation/solar_azimuth",sep=""))),
          solar_elevation = as.numeric(h5read(fn, paste("/",beams[j],"/geolocation/solar_elevation",sep=""))),
          landsat_treecover = h5read(fn, paste("/",beams[j],"/land_cover_data/landsat_treecover",sep="")),
          modis_treecover = h5read(fn, paste("/",beams[j],"/land_cover_data/modis_treecover",sep="")),
          pft_class = h5read(fn, paste("/",beams[j],"/land_cover_data/pft_class",sep="")),
          pai = h5read(fn, paste("/",beams[j],"/pai",sep="")),
          cover = h5read(fn, paste("/",beams[j],"/cover",sep="")),
          l2b_qual_flag = as.numeric(h5read(fn, paste("/",beams[j],"/l2b_quality_flag",sep="")))
        )
        
        dz = h5read(fn, paste("/",beams[j],"/ancillary/dz",sep=""))
        
        pai_z = t(h5read(fn, paste("/",beams[j],"/pai_z",sep="")))
        dimnames(pai_z)[[2]] <- paste("pai_", seq(dz, (dim(pai_z)[2])*dz, dz), sep="")
        
        pavd = t(h5read(fn, paste("/",beams[j],"/pavd_z",sep="")))
        dimnames(pavd)[[2]] <- paste("pavd_", seq(dz, (dim(pavd)[2])*dz, dz), sep="")
        
        temp_j[[j]] <- cbind(temp, pai_z, pavd, fn)
      }
      
      temp_j <- do.call(rbind, temp_j)
      gedidf <- temp_j
      
      gedidf2 <- cbind(gedidf, doy)
      
      gedidf3 <- subset(gedidf2, select = -c(fn, channel, shot_number, pai_95:pai_150, pavd_65:pavd_150))
      
      gedi_sub_amz <- subset(gedidf3, lat_lowestmode > amz_ext[3] & lat_lowestmode < amz_ext[4] & lon_lowestmode > amz_ext[1] & lon_lowestmode < amz_ext[2])
      
      fillvalue <- -9999
      gedi_sub_amz[gedi_sub_amz == fillvalue] <- NA
      
      gedi_narm <- gedi_sub_amz[!is.na(gedi_sub_amz$pai),]
      
      #gedi_narm$sdvfp <- apply(gedi_narm[, c(43:72)], MARGIN = 1, FUN = sd)
      #gedi_narm$meanpavd <- apply(gedi_narm[, c(43:72)], MARGIN = 1, FUN = mean)
      gedi_narm$sdvfp <- apply(gedi_narm[, grep("^pavd_[5-9][0-9]?$|^pavd_60$", names(gedi_narm))], MARGIN = 1, FUN = sd)
      
      gedi_narm$meanpavd <- apply(gedi_narm[, grep("^pavd_[5-9][0-9]?$|^pavd_60$", names(gedi_narm))], MARGIN = 1, FUN = mean)
      
      gedi_narm$cvvfp <- gedi_narm$sdvfp / gedi_narm$meanpavd
      
      gedi_narm <- subset(gedi_narm, select = -c(pavd_10:pavd_60))
      
      gedi_qc <- gedi_narm %>% 
        filter(beam > 4) %>% 
        filter(sensitivity > 0.95) %>%
        filter(solar_elevation <= 0) %>%
        filter(l2b_qual_flag > 0) %>% 
        filter(if_else(beam <= 4, cover < 0.95, cover < 0.98)) %>% 
        filter(pavd_5 >= 0) %>%
        filter(pft_class > 0 & pft_class < 6) %>%
        filter(landsat_treecover > 70) %>%
        filter(modis_treecover > 70)
      
      gedi_pai <- gedi_qc %>%
        mutate(
          paivox_5 = pai_5 - pai_10,
          paivox_10 = pai_10 - pai_15,
          paivox_15 = pai_15 - pai_20,
          paivox_20 = pai_20 - pai_25,
          paivox_25 = pai_25 - pai_30,
          paivox_30 = pai_30 - pai_35,
          paivox_35 = pai_35 - pai_40,
          paivox_40 = pai_40 - pai_45,
          paivox_45 = pai_45 - pai_50,
          paivox_50 = pai_50 - pai_55,
          paivox_55 = pai_55 - pai_60,
          paivox_60 = pai_60 - pai_65,
          paivox_65 = pai_65 - pai_70,
          paivox_70 = pai_70 - pai_75,
          paivox_75 = pai_75 - pai_80,
          paivox_80 = pai_80 - pai_85,
          paivox_85 = pai_85 - pai_90
        ) %>% 
        dplyr::select(-c(pai_5:pai_90)) %>% 
        mutate(pai_us = rowSums(across(c(paivox_5:paivox_15))),
               pai_toc = rowSums(across(c(paivox_20:paivox_85))))
      
      write.csv(gedi_pai, file.path(gediprocpath, paste0("amz_", gediname, ".csv")), row.names = FALSE)
      
    }, mc.cores = num_cores) #Change the number of cores
    
  }
  end_time <- Sys.time()
  total_time <- difftime(end_time, start_time, units = "mins")
  cat("Total time taken: ", total_time, "minutes\n")
}


#Extract all GEDI files. THIS TAKES A FEW HOURS! -------------------

extract_gedi(parent_directory, amz_ext)



#Now combined all files into 1 ----------------------

#How many cores to use?
num_cores <- 12 #Could probably go up to 16 on the lab desktop Mac Studio

#What will the coordinate reference system be?
newcrs <- "epsg:4326"


subdirectories <- list.dirs(parent_directory, full.names = TRUE, recursive = FALSE)

# Filter subdirectories with names in Y m d format
date_subdirectories <- subdirectories[grepl("\\d{4}\\.\\d{2}\\.\\d{2}", subdirectories)]

# Function to process each date subdirectory
process_date_subdirectory <- function(date_folder) {
  
  # Extract the day of the year from the folder name
  doy_value <- as.numeric(format(as.POSIXlt(gsub(".*/(\\d{4}\\.\\d{2}\\.\\d{2})", "\\1", date_folder), format="%Y.%m.%d"), "%j"))
  
  # Combine the processing steps
  processed_df <- list.files(paste0(date_folder, "/processed"), pattern = "\\.csv$", full.names = TRUE) %>%
    lapply(read.csv) %>%
    bind_rows() %>%
    mutate(doy4day = as.character(doy_value)) %>%
    vect(., geom = c("lon_lowestmode", "lat_lowestmode"), crs = newcrs) %>%
    terra::crop(., amz_vect)
  
  # Write the processed data frame to a shapefile
  writeVector(
    processed_df,
    file.path(lastpath, paste0("/amz_gedivect_doy", doy_value, ".shp")),
    overwrite = TRUE
  )
  
}

start_time <- Sys.time()  # Capture the start time
# Use mclapply to process multiple date subdirectories in parallel
mclapply(date_subdirectories, process_date_subdirectory, mc.cores = num_cores)  # Use all but one core

end_time <- Sys.time()  # Capture the end time

total_time <- difftime(end_time, start_time, units = "mins")  # Calculate the total time taken in minutes
cat("Created:", length(list.files(lastpath, pattern = ".shp")), "shapefiles; Total time taken:", total_time, "minutes\n")  # Print the total time taken

#lapply(date_subdirectories, process_date_subdirectory)


#test plot
geditest <- vect(paste0(lastpath, "/amz_gedivect_doy321.shp"))
geditest
summary(geditest$pai)
hist(geditest$pai)
summary(geditest$landsat_tr)
hist(geditest$modis_tree)
hist(geditest$landsat_tr)
summary(geditest$pft_class)
hist(geditest$sdvfp)
summary(geditest$sdvfp)


#Note that Beam type creates a low bias, so we are only using power beams.
# testmut <- geditest %>% tidyterra::mutate(beam_type = ifelse(beam > 4, 'power', 'coverage'))
# testdf <- as.data.frame(testmut)
# ggplot(testdf, aes(x = beam_type, y = pai)) +
#   geom_boxplot() +
#   labs(x = "Beam Type", y = "PAI")
# 
# 
# dffilt <- testdf %>% tidyterra::filter(beam > 4)
# 
