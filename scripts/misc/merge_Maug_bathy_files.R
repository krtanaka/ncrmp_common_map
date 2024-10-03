library(dplyr)
library(raster)
library(terra)
library(readr)

rm(list = ls())

# patch finer res bathymetry files first

# Rota Island: Bathymetry PIBHMC
# https://www.soest.hawaii.edu/pibhmc/cms/data-by-location/cnmi-guam/rota-island/rota-island-bathymetry/
b1 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/mau_dball.asc"); plot(b1); res(b1)
b2 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/mau_dbmb_10m.asc"); plot(b2); res(b2)
b3 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/mau_dbmb_5m.asc"); plot(b3); res(b3)
b4 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/maug_10m.asc"); plot(b4); res(b4)

# utm = read_csv('data/misc/ncrmp_utm_zones.csv')
# island_boxes = read_csv("data/misc/Island_Extents.csv") # Updated Bounding boxes 2021
# island_names_codes = read_csv("data/misc/island_name_code.csv")
# island_names_codes_boxes = merge(island_names_codes, island_boxes)
# rm(island_names_codes, island_boxes)
# utm_i = utm %>% subset(Island_Code == "gug")
# 
# # determine northern or southern hemisphere
# sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')
# # sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')
# 
# b1 <- project(b1, sr); plot(b1)
# b2 <- project(b2, sr); plot(b2)

b1 = resample(b1, b3, method = "near"); plot(b1)
b2 = resample(b2, b3, method = "near"); plot(b2)
b4 = resample(b4, b3, method = "near"); plot(b4)

fine_topo = mean(c(b1, b2, b3, b4), na.rm = T)

fine_topo[fine_topo >= 0] <- NA
fine_topo[fine_topo <= -30] <- NA

plot(fine_topo)

topo_i = readAll(raster(fine_topo))

plot(topo_i)

save(topo_i, file = 'data/gis_bathymetry/mau_merged.RData')
