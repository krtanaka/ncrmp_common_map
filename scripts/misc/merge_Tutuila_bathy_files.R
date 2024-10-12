library(dplyr)
library(terra)
library(readr)

rm(list = ls())

# patch finer res bathymetry files first

b1 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/tut_dball.asc"); plot(b1); res(b1)
b2 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/tut_dbmb.asc"); plot(b2); res(b2)
b3 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/Tutuila_5m.asc"); plot(b3); res(b3)
b4 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_ninth_amsam/cudem_ninth_amsam_J1078896.tif"); plot(b4); res(b4)
b5 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2022_ngs_topobathy_dem_tutuila/2022_ngs_topobathy_dem_tutuila_J1078893_000_000.tif"); plot(b5); res(b5)
b6 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2022_ngs_topobathy_dem_tutuila/2022_ngs_topobathy_dem_tutuila_J1078893_001_000.tif"); plot(b6); res(b6)

base = b3

b1 = resample(b1, base, method = "near"); plot(b1)
b2 = resample(b2, base, method = "near"); plot(b2)
b3 = resample(b3, base, method = "near"); plot(b3) 
b4 = resample(b4, base, method = "near"); plot(b4) 
b5 = resample(b5, base, method = "near"); plot(b5)
b6 = resample(b6, base, method = "near"); plot(b6)

fine_topo <- mean(c(b1, b2, b3, b4, b5, b6), na.rm = TRUE)

fine_topo[fine_topo >= 0] <- NA
fine_topo[fine_topo <= -30] <- NA

plot(fine_topo)
save(topo_i, file = 'data/gis_bathymetry/tut_merged.RData')

# Assign the UTM Zone 2S CRS to fine_topo
crs(fine_topo) <- "EPSG:32702"

# Reproject to WGS84 (lat/lon)
fine_topo_latlon <- project(fine_topo, "EPSG:4326")

# Plot the reprojected raster
plot(fine_topo_latlon)

topo_i = readAll(raster(fine_topo))

plot(topo_i)

save(topo_i, file = 'data/gis_bathymetry/tut_merged.RData')
