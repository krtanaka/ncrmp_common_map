library(dplyr)
library(raster)
library(terra)
library(readr)

rm(list = ls())

# patch finer res bathymetry files first

# Rota Island: Bathymetry PIBHMC
# https://www.soest.hawaii.edu/pibhmc/cms/data-by-location/cnmi-guam/rota-island/rota-island-bathymetry/
b1 = rast("N:/GIS/Projects/SeafloorCalc/Final_Products/agr_inpoo_new/w001001.adf"); plot(b1); res(b1)
b2 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/agrihan_10m.asc"); plot(b2); res(b2)
b3 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/agr_dball_2m.asc"); plot(b3); res(b3)

utm = read_csv('data/misc/ncrmp_utm_zones.csv')
island_boxes = read_csv("data/misc/Island_Extents.csv") # Updated Bounding boxes 2021
island_names_codes = read_csv("data/misc/island_name_code.csv")
island_names_codes_boxes = merge(island_names_codes, island_boxes)
rm(island_names_codes, island_boxes)
utm_i = utm %>% subset(Island_Code == "agr")

# determine northern or southern hemisphere
sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')
# sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')

crs(b2) = crs(b1)
crs(b3) = crs(b1)

b1 <- project(b1, sr); plot(b1)
b2 <- project(b2, sr); plot(b2)
b3 <- project(b3, sr); plot(b3)

b3 = resample(b3, b2, method = "near"); plot(b3)

fine_topo = mean(c(b1, b2, b3), na.rm = T)

fine_topo[fine_topo >= 0] <- NA
fine_topo[fine_topo <= -30] <- NA

plot(fine_topo)

topo_i = readAll(raster(fine_topo))

plot(topo_i)

save(topo_i, file = 'data/gis_bathymetry/agr_merged.RData')
