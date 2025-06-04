


library(sf)
library(dplyr)
library(purrr)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)
library(stringr)
library(cmocean)


# get vessel density/hour rater from AMSA

amsa_r <- terra::rast("Shipping_MeanHours_AUS_2018-2025.tif")
pe <- as.polygons(ext(amsa_r))
pr <- as.polygons(amsa_r > -Inf)
pr <- as.polygons(amsa_r > -Inf, dissolve = TRUE) 


plot(amsa_r)
plot(pe, lwd=5, border='red', add=TRUE)
plot(pr, lwd=1, border='blue', add=TRUE)

plot(pr, lwd=1, border='blue', col = "blue")
plot(pr, border = "red", lwd = 2, col = NA)

# Step 1: Create binary mask of valid cells
valid_cells <- !is.na(amsa_r)

# Step 2: Convert to polygons and dissolve into one feature
outline <- as.polygons(valid_cells, dissolve = TRUE)

# Step 3: Plot
plot(outline, col = NA, border = "red", lwd = 2)







base_dir <- "/Volumes/Ingo_PhD/PhD_Data_Analysis/Shipping/AIS_GMT_data"


# List all .tif files
gmt_files <- list.files(base_dir, pattern = "\\.tif$", full.names = TRUE)

# Load and stack/mosaic them
gmt_stack <- terra::rast(gmt_files)
gmt_stack


# Merge to single raster if necessary
gmt_merged <- if (terra::nlyr(gmt_stack) > 1) terra::mosaic(gmt_stack) else gmt_stack

# Load AMSA raster to get target resolution and extent
amsa_raster <- terra::rast("path/to/your/processed_AMSA_raster.tif")  # UPDATE with actual AMSA raster path

# Resample GMT to match AMSA resolution (0.1°) and extent
gmt_resampled <- terra::resample(gmt_merged, amsa_raster, method = "bilinear")

# Load AMSA coverage polygon shapefile (define exact file path)
amsa_shp <- sf::st_read("/Volumes/Ingo_PhD/PhD_Data_Analysis/Shipping/AIS_AMSA_data/cts_srr_01_2018_pt.shp")  # UPDATE with actual file

# Convert shapefile to SpatVector for terra compatibility
SRR_vect <- terra::vect(amsa_shp)
class(SRR_vect)
SRR_vect_sf <- sf::st_as_sf(SRR_vect)


pe <- as.polygons(ext(z))
pr <- as.polygons(z > -Inf)

plot(z)
plot(pe, lwd=5, border='red', add=TRUE)
plot(pr, lwd=3, border='blue', add=TRUE)

ggplot() +
  geom_sf(data = SRR_vect_sf, fill = NA, color = "black", size = 0.8) +
  coord_sf() +
  labs(title = "AMSA Vector Boundary Outline") +
  theme_minimal()

# Invert the mask: get only PNG area (outside AMSA)
png_only <- terra::mask(gmt_resampled, amsa_vect, inverse = TRUE)

# Merge AMSA raster and PNG raster
combined_raster <- terra::mosaic(amsa_raster, png_only)

# Optional: Save final raster
terra::writeRaster(combined_raster, "combined_AMSA_PNG_hours.tif", overwrite = TRUE)