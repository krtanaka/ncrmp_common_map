library(dplyr)
library(raster)
library(terra)
library(readr)

rm(list = ls())

# patch finer res bathymetry files first

# Rota Island: Bathymetry PIBHMC
# https://www.soest.hawaii.edu/pibhmc/cms/data-by-location/cnmi-guam/rota-island/rota-island-bathymetry/
# b1 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi2019_aguijan/cnmi2019_isl_dem_hydflt_J1078198.tif"); plot(b1); res(b1)
b2 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi2019_aguijan/cnmi2019_islands_dem_J1078197.tif"); plot(b2); res(b2)
# b3 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi2019_aguijan/cnmi2019_islands_Job1078196.tif"); plot(b3); res(b3)
b4 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi2019_aguijan/cudem_9th_cnmi_J1078199.tif"); plot(b4); res(b4)
# b5 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi2019_aguijan/NOAA_SLR_DEM_CNMI_J1078200.tif"); plot(b5); res(b5)
b6 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi2019_aguijan_rota_saipan_tinian/cnmi2019_islands_dem_J1075168_001_002.tif"); plot(b6); res(b6)

utm = read_csv('data/misc/ncrmp_utm_zones.csv')
island_boxes = read_csv("data/misc/Island_Extents.csv") # Updated Bounding boxes 2021
island_names_codes = read_csv("data/misc/island_name_code.csv")
island_names_codes_boxes = merge(island_names_codes, island_boxes)
rm(island_names_codes, island_boxes)
utm_i = utm %>% subset(Island_Code == "agu")

# determine northern or southern hemisphere
sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')
# sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')

# crs(b2) = crs(b1)
# crs(b3) = crs(b1)

# b1 <- project(b1, sr); plot(b1)
# b2 <- project(b2, sr); plot(b2)
# b3 <- project(b3, sr); plot(b3)
# b4 <- project(b4, sr); plot(b4)
# b5 <- project(b5, sr); plot(b5)
# b6 <- project(b6, sr); plot(b6)

# b1 = resample(b1, b6, method = "near"); plot(b1)
b2 = resample(b2, b4, method = "near"); plot(b2)
# b3 = resample(b3, b6, method = "near"); plot(b3)
# b4 = resample(b4, b6, method = "near"); plot(b4)
b6 = resample(b6, b4, method = "near"); plot(b6)

fine_topo = mean(c(b2, b4, b6), na.rm = T)
# fine_topo = b4

fine_topo[fine_topo >= 0] <- NA
fine_topo[fine_topo <= -30] <- NA

plot(fine_topo)

topo_i = readAll(raster(fine_topo))

plot(topo_i)

save(topo_i, file = 'data/gis_bathymetry/agu_merged.RData')
