
#Charles Southwick
# Code for plotting maps seen in Figure 1 and Figure 5

library(terra)
library(viridis)



##FIGURE PLOTTING -------------------------------------

#Load processed data to facilitate plotting
boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"
pacedir <- paste0(boxwd, "/pace_vi_data/extracted_cropped")
#compiled_dir <- paste0(boxwd, "/pace_vi_data")

compiled_dir <- paste0(boxwd, "/compiled_rasters")
figdir <- paste0(boxwd, "/figures")

amz_shp <- vect(paste0(boxwd, "/amz_shps/amz_biome.shp"))
wrld_shp <- vect(paste0(boxwd, "/amz_shps/world_countries_epsg4326.shp"))
georeg_agg <- vect(paste0(boxwd, "/amz_shps/amz_geo_agg_extended.shp"))

#Crop South America for use in an inset
sa_ext <- ext(-82, -35, -55, 13)
south_am <- crop(wrld_shp, sa_ext)
# This is the extent to be highlighted
highlight_ext <- ext(amz_shp)

#Load rasters
rel_ampl <- rast(paste0(compiled_dir, "/rs_rel_ampl_across_years.tif"))
paiyr <- rast(paste0(compiled_dir, "/gedi_yrly_rasterized.tif"))
prec_mean_16day <- rast(paste0(compiled_dir, "/amz_mean_16day_precip.tif"))
prec_n_low <- rast(paste0(compiled_dir, "/amz_under50mmcountdoy_precip.tif"))
prec_mean_annual <- rast(paste0(compiled_dir, "/amz_mean_annual_precip.tif"))
prec_first_low <- rast(paste0(compiled_dir, "/amz_firstdoyunder50mm_precip.tif"))

pace_ampl <- rast(paste0(compiled_dir, "/pace_rs_rel_ampl_across_years.tif"))

georeg_agg_r <- rasterize(georeg_agg, prec_n_low, field = "region")


#Clip to 100% change
rel_ampl_clip <- rel_ampl
rel_ampl_clip[rel_ampl_clip > 150] <- 150

pace_ampl_clip <- pace_ampl
pace_ampl_clip[pace_ampl_clip > 150] <- 150

#Time to plot -----------------------------------------------


add_fig_letter_3x2 <- function(letter, x_pos = "left", y_pos = "top", inset = 0.01, cex = 1.5) {
  usr <- par("usr")
  x <- if (x_pos == "left") usr[1] + inset * (usr[2] - usr[1]) else usr[2] - inset * (usr[2] - usr[1])
  y <- if (y_pos == "top")  usr[4] - inset * (usr[4] - usr[3]) else usr[3] + inset * (usr[4] - usr[3])
  text(x, y, labels = letter, font = 1, cex = cex, xpd = NA)
}


add_fig_letter_2x2 <- function(
    letter,
    x_pos = "left",
    y_pos = "top",
    inset_x = 0.06,
    inset_y = 0.06,
    cex = 1.6,
    font = 1
) {
  usr <- par("usr")
  
  # approximate text size for scaling
  sw <- strwidth(letter, cex = cex)
  sh <- strheight(letter, cex = cex)
  
  # X position (slightly beyond plot edge)
  x <- if (x_pos == "left") {
    usr[1] - inset_x * (usr[2] - usr[1])
  } else {
    usr[2] + inset_x * (usr[2] - usr[1])
  }
  
  # Y position (slightly above plot edge)
  y <- if (y_pos == "top") {
    usr[4] + inset_y * (usr[4] - usr[3])
  } else {
    usr[3] - inset_y * (usr[4] - usr[3])
  }
  
  text(x, y, labels = letter, cex = cex, font = font, xpd = NA)
}


# define breaks + labels for legend
brks <- seq(0, 150, by = 30)
lbls <- as.character(brks)
lbls[length(lbls)] <- ">150"

#Plots for publication

#png(paste0(figdir, "/new_fig1_rel_ampl_plots.png"), width = 2000, height = 1500, res = 300)
tiff(paste0(figdir, "/new_fig1_rel_ampl_plots.tiff"), units = 'in', width = 7, height = 5, res = 600)


par(mfrow = c(2, 2))

# Relabel the factor levels
new_levels <- data.frame(
  ID = 0:3,
  region = c("Central", "Northern", "Northwest", "Southern")
)

# assign new categories
levels(georeg_agg_r) <- new_levels
plot(georeg_agg_r$region, main = "Ecoregions")
plot(georeg_agg, add = T, lwd = 1.2, border = "black")
add_fig_letter_2x2("(a)")

# Add inset map in bottom right
inset(
  south_am,
  loc = "bottomright",
  scale = 0.15,
  background = NA,
  perimeter = TRUE,
  pper = list(col = "black", lwd = 0.4),
  box = highlight_ext,
  pbox = list(col = "red", lwd = 1)
)

plot(prec_n_low, col = viridis(100, option = "magma"), main = "Periods with precip < 50 mm")
plot(georeg_agg, add = T, lwd = 1.2, border = "cadetblue")
add_fig_letter_2x2("(b)")

plot(rel_ampl_clip$sif_par, col = viridis(150, option = "cividis", begin = 0.1, direction = 1), range = c(0, 150), main = "Relative change in SIF/PAR", plg = list(at = brks, labels = lbls, title = "Δ%"))
plot(georeg_agg, add = T, lwd = 1, border = "black")
add_fig_letter_2x2("(c)")

plot(rel_ampl_clip$pai_toc, col = viridis(150, option = "cividis", begin = 0.1, direction = 1), range = c(0, 150), main = expression(bold("Relative change in")~bold(PAI[TOC])), plg = list(at = brks, labels = lbls, title = "Δ%"))
plot(georeg_agg, add = T, lwd = 1, border = "black")
add_fig_letter_2x2("(d)")

# Finalize and write to file
dev.off()

par(mfrow = c(1,1))


# PACE Plots ------------------------

# Set output PNG file
#png(paste0(figdir, "/new_pace_rel_ampl_plots.png"), width = 2000, height = 1500, res = 300)
tiff(paste0(figdir, "/new_pace_rel_ampl_plots.tiff"), units = 'in', width = 7, height = 5, res = 600)

par(mfrow = c(3,2))

plot(rel_ampl_clip$sif_par, col = viridis(100, option = "plasma", begin = 0.2, direction = 1), range = c(0, 150), main = "Change in SIF/PAR", plg = list(at = brks, labels = lbls, title = "Δ%"))
plot(georeg_agg, add = T, lwd = 1.2, border = "black")
add_fig_letter_3x2("(a)")

plot(pace_ampl_clip$cire, col = viridis(100, option = "plasma", begin = 0.2, direction = 1), range = c(0, 150), main = expression(bold("Change in PACE CI"[re])), plg = list(at = brks, labels = lbls, title = "Δ%"))
plot(georeg_agg, add = T, lwd = 1.2, border = "black")
add_fig_letter_3x2("(b)")

plot(rel_ampl_clip$fesc, col = viridis(100, option = "plasma", begin = 0.2, direction = 1), range = c(0, 150), main = expression(bold("Change in MODIS F"[esc])), plg = list(at = brks, labels = lbls, title = "Δ%"))
plot(georeg_agg, add = T, lwd = 1.2, border = "black")
add_fig_letter_3x2("(c)")

plot(pace_ampl_clip$car, col = viridis(100, option = "plasma", begin = 0.2, direction = 1), range = c(0, 150), main = "Change in PACE Car", plg = list(at = brks, labels = lbls, title = "Δ%"))
plot(georeg_agg, add = T, lwd = 1.2, border = "black")
add_fig_letter_3x2("(d)")

plot(rel_ampl_clip$modis_lai, col = viridis(100, option = "plasma", begin = 0.2, direction = 1), range = c(0, 150), main = "Change in MODIS LAI", plg = list(at = brks, labels = lbls, title = "Δ%"))
plot(georeg_agg, add = T, lwd = 1.2, border = "black")
add_fig_letter_3x2("(e)")

plot(pace_ampl_clip$chlcar, col = viridis(100, option = "plasma", begin = 0.2, direction = 1), range = c(0, 150), main = "Change in PACE Chl:Car", plg = list(at = brks, labels = lbls, title = "Δ%"))
plot(georeg_agg, add = T, lwd = 1.2, border = "black")
add_fig_letter_3x2("(f)")

# Finalize and write to file
dev.off()



#End of Script

