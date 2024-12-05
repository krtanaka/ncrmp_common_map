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

b5 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2022_rainier_mau/H13579_MBAB_6m_S221_100kHz_1of5.tif"); plot(b5); res(b5)
b6 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2022_rainier_mau/H13579_MBAB_2m_2803_300kHz_2of5.tif"); plot(b6); res(b6)
b7 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2022_rainier_mau/H13579_MBAB_3m_2803_200kHz_3of5.tif"); plot(b7); res(b7)
b8 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2022_rainier_mau/H13579_MBAB_2m_2804_300kHz_4of5.tif"); plot(b8); res(b8)
b9 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2022_rainier_mau/H13579_MBAB_3m_2804_200kHz_5of5.tif"); plot(b9); res(b9)

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

b1 = resample(b1, b1, method = "near"); plot(b1)
b2 = resample(b2, b1, method = "near"); plot(b2)
b3 = resample(b3, b1, method = "near"); plot(b3)
b4 = resample(b4, b1, method = "near"); plot(b4)
b5 = resample(b5, b1, method = "near"); plot(b5)
b6 = resample(b6, b1, method = "near"); plot(b6)
b7 = resample(b7, b1, method = "near"); plot(b7)
b8 = resample(b8, b1, method = "near"); plot(b8)
b9 = resample(b9, b1, method = "near"); plot(b9)

fine_topo = mean(c(b1, b2, b3, b4, b5, b6, b7, b8, b9), na.rm = T)

fine_topo[fine_topo >= 0] <- NA
fine_topo[fine_topo <= -30] <- NA

plot(fine_topo)

topo_i = readAll(raster(fine_topo))

plot(topo_i)

save(topo_i, file = 'data/gis_bathymetry/mau_merged.RData')
