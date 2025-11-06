
# Geospatial wrangling to get region of interest for this study

library(terra)

roi <- vect("/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi/amz_shps/amz_biome.shp")

geo <- vect("/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi/amz_shps/amz_geocrop.shp")

# Dissolve roi into one polygon
roi_union <- aggregate(roi)

# Erase the subregions from roi to get the surrounding shell
roi_gap <- erase(roi_union, geo)

roi_gap$name <- "gap"

# Make sure geo also has a 'name' field or similar ID column
if (!"name" %in% names(geo)) {
  geo$name <- paste0("subregion_", 1:nrow(geo))
}

# Combine the subregions with the gap
geo_extended <- rbind(geo, roi_gap)

plot(geo_extended, col = rainbow(nrow(geo_extended)))

# Split out gap and subregions
gap_poly <- geo_extended[geo_extended$name == "gap", ]
subregions <- geo_extended[geo_extended$name != "gap", ]

# Disaggregate the gap polygon into individual pieces
gap_parts <- disagg(gap_poly)

# There is a problematic chunk that needs to be merged into the NWA region before the for loop below -------------------

# Select the chunk (it's in the west)
problematic_chunk <- gap_parts[27]

# Select the NWA subregion
nwa_subregion <- subregions[subregions$name == "NWA", ]

# Union and dissolve
nwa_merged <- aggregate(union(problematic_chunk, nwa_subregion))

# Set the name
nwa_merged$name <- "NWA"

# Update subregions: remove old NWA and add the new one
subregions <- subregions[subregions$name != "NWA", ]
subregions <- rbind(subregions, nwa_merged)

# Remove the problematic chunk from gap_parts
gap_parts <- gap_parts[-27, ]

# 4. Loop through gap pieces and attach each to closest/intersecting subregion
new_subregions <- list()
k <- 1

for (i in 1:nrow(gap_parts)) {
  gap_piece <- gap_parts[i, ]
  
  # Check for intersecting subregions
  inter_idx <- which(!is.na(relate(gap_piece, subregions, "intersects")))
  
  # Measure distance
  dists <- distance(gap_piece, subregions)
  
  # Check if any distances are exactly zero
  zero_dists <- which(dists == 0)
  
  if (length(zero_dists) > 1) {
    # Choose the second subregion with zero distance (this is patchy but just works)
    subregion <- subregions[zero_dists[2], ]
  } else {
    # Choose the closest subregion
    subregion <- subregions[which.min(dists), ]
  }
  
  # Union and dissolve
  gap_union <- union(gap_piece, subregion)
  gap_union_diss <- aggregate(gap_union)
  
  # Attach correct subregion name
  gap_union_diss$name <- subregion$name
  
  new_subregions[[k]] <- gap_union_diss
  k <- k + 1
}

# Combine all adjusted subregion pieces
subregions_updated <- do.call(rbind, new_subregions)

# Remove old versions of any subregions we updated
subregion_names_updated <- unique(subregions_updated$name)
geo_pruned <- geo_extended[!(geo_extended$name %in% subregion_names_updated | geo_extended$name == "gap"), ]

# Combine with updated subregions
geo_combined <- rbind(geo_pruned, subregions_updated)

# Final dissolve by 'name'
geo_final <- aggregate(geo_combined, by = "name")

# Plot the result
plot(geo_final, col = rainbow(nrow(geo_final)))

geonew <- geo_final
geonew$region <- ifelse(geonew$name %in% c("SA", "SWA", "EA"),
                             "Southern", geonew$name)
amz_geo_agg <- aggregate(geonew, by = "region", dissolve = TRUE)

plot(amz_geo_agg, col = rainbow(length(amz_geo_agg)), main = "Aggregated Regions", border = "black")

# Cleanup
amz_geo_agg <- amz_geo_agg[, !(names(amz_geo_agg) %in% c("mean_mean_ID_", "mean_mean_ID", "mean_agg_n", "agg_n"))]

plot(amz_geo_agg)

#Write the vector
writeVector(amz_geo_agg, "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi/amz_shps/amz_geo_agg_extended.shp", overwrite = TRUE)
