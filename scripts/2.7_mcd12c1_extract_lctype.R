

library(terra)


lctypes <- rast("/Users/charlessouthwick/Documents/PhD/sifgedi/mcd12c1_landcover/processed_MCD12C1.A2019001.061.2022170020638_lctype.tif")

amz_shp <- vect("/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi/amz_shps/amz_biome.shp")

# Use ifel to set 255s in layer 2 to NA
mask255 <- ifel(lctypes[[2]] == 255, NA, lctypes[[2]])
maskconf <- ifel(mask255 <= 60, NA, mask255) #filter out low confidence results

# Mask layer 1 with the updated mask layer
lc_masked <- mask(lctypes[[1]], maskconf)

lc <- c(lc_masked, maskconf)

lc_amz <- crop(lc, amz_shp, mask = TRUE)

# Filter out open shrublands, woody savannas, woody savannas, croplands, and built landscapes
lc_tree <- ifel(lc_amz[[1]] > 6, NA, lc_amz[[1]])

plot(lc_tree)

writeRaster(lc_tree, "/Users/charlessouthwick/Documents/PhD/sifgedi/mcd12c1_landcover/mcd12c1_2019_lc_masked_amz.tif", overwrite = T)


