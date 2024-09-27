library(dplyr)
# library(raster)
library(terra)
library(readr)

rm(list = ls())

# patch finer res bathymetry files first

# Rota Island: Bathymetry PIBHMC
# https://www.soest.hawaii.edu/pibhmc/cms/data-by-location/cnmi-guam/rota-island/rota-island-bathymetry/

# -----------------------------
# 1. Load Fine Resolution Bathymetry Rasters
# -----------------------------

# Define file paths for fine bathymetry rasters
fine_bathymetry_files <- list(
  b1 = "N:/GIS/Projects/CommonMaps/Bathymetry/sai_dball.asc",
  b2 = "N:/GIS/Projects/CommonMaps/Bathymetry/sai_mb_li_db.asc",
  b3 = "N:/GIS/Projects/CommonMaps/Bathymetry/marpi_5m.asc",
  b4 = "N:/GIS/Projects/CommonMaps/Bathymetry/saipan_5m.asc"
)

# Load the fine resolution rasters using `terra::rast`
fine_rasters <- lapply(fine_bathymetry_files, rast)

# Optionally, name the list elements for clarity
names(fine_rasters) <- names(fine_bathymetry_files)

# Plot the fine rasters (optional)
# lapply(names(fine_rasters), function(name) {
#   plot(fine_rasters[[name]], main = paste("Fine Raster:", name))
# })

# -----------------------------
# 2. Resample Rasters to Match `b2`
# -----------------------------

# Identify the target raster (`b2`) for resampling
target_raster <- fine_rasters[["b2"]]

# Resample `b1`, `b3`, and `b4` to match `b2` using nearest neighbor (`"near"`) method
resampled_rasters <- lapply(names(fine_rasters)[names(fine_rasters) != "b2"], function(name) {
  resampled <- resample(fine_rasters[[name]], target_raster, method = "near")
  # Optionally, plot the resampled raster
  # plot(resampled, main = paste("Resampled Raster:", name))
  return(resampled)
})

# Combine resampled rasters with the target raster into a single list
resampled_rasters <- c(resampled_rasters, list(b2 = target_raster))

# -----------------------------
# 3. Compute Mean of Fine Rasters
# -----------------------------

# Stack the resampled fine rasters into a single `SpatRaster` object
fine_stack <- rast(resampled_rasters)

# Compute the cell-wise mean across all layers, ignoring `NA` values
fine_topo <- app(fine_stack, mean, na.rm = TRUE)

# -----------------------------
# 4. Apply Value Constraints
# -----------------------------

# Set values >= 0 to `NA`
fine_topo[fine_topo >= 0] <- NA

# Set values <= -30 to `NA`
fine_topo[fine_topo <= -30] <- NA

plot(fine_topo)

library(raster)

topo_i = readAll(raster(fine_topo))

plot(topo_i)

save(topo_i, file = 'data/gis_bathymetry/sai_merged.RData')
