library(dplyr)
library(terra)
library(readr)

rm(list = ls())

file_paths <- c(
  "N:/GIS/Projects/CommonMaps/Bathymetry/tut_dball.asc",
  "N:/GIS/Projects/CommonMaps/Bathymetry/tut_dbmb.asc",
  # "N:/GIS/Projects/CommonMaps/Bathymetry/Tutuila_5m.asc",
  "N:/GIS/Projects/CommonMaps/Bathymetry/cudem_ninth_amsam/cudem_ninth_amsam_J1078896.tif",
  "N:/GIS/Projects/CommonMaps/Bathymetry/2022_ngs_topobathy_dem_tutuila/2022_ngs_topobathy_dem_tutuila_J1078893_000_000.tif"
  # "N:/GIS/Projects/CommonMaps/Bathymetry/2022_ngs_topobathy_dem_tutuila/2022_ngs_topobathy_dem_tutuila_J1078893_001_000.tif"
)

rasters <- lapply(file_paths, rast)

rasters <- lapply(rasters, function(raster) {
  crop(raster,  ext(530550, 532100, 8416200, 8417300))
})

lapply(rasters, function(r) { plot(r); res(r) })

base <- rasters[[4]]

rasters_resampled <- lapply(rasters, function(r) resample(r, base, method = "near"))

fine_topo <- mean(do.call(c, rasters_resampled), na.rm = TRUE)
fine_topo[fine_topo >= 0] <- NA
fine_topo[fine_topo <= -30] <- NA
plot(fine_topo)

topo_i = readAll(raster(fine_topo))

plot(topo_i)

save(topo_i, file = 'data/gis_bathymetry/tut_merged.RData')
