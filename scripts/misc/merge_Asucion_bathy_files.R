library(dplyr)
library(raster)
library(terra)
library(readr)

rm(list = ls())

# patch finer res bathymetry files first

# Rota Island: Bathymetry PIBHMC
# https://www.soest.hawaii.edu/pibhmc/cms/data-by-location/cnmi-guam/rota-island/rota-island-bathymetry/
b1 = rast("N:/GIS/Projects/SeafloorCalc/Workspace/Asuncion/asc_inpo/w001001.adf"); plot(b1); res(b1)
b2 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/asu_10m_dbmb.asc"); plot(b2); res(b2)
b3 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/asu_5m_dbmb.asc"); plot(b3); res(b3)
b4 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/asu_dball_mos.asc"); plot(b4); res(b4)
b5 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/asuncion_10m.asc"); plot(b5); res(b5)

utm = read_csv('data/misc/ncrmp_utm_zones.csv')
island_boxes = read_csv("data/misc/Island_Extents.csv") # Updated Bounding boxes 2021
island_names_codes = read_csv("data/misc/island_name_code.csv")
island_names_codes_boxes = merge(island_names_codes, island_boxes)
rm(island_names_codes, island_boxes)
utm_i = utm %>% subset(Island_Code == "asc")

# determine northern or southern hemisphere
if (mean(yFromRow(b1, 1:nrow(b1))) > 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')
if (mean(yFromRow(b1, 1:nrow(b1))) < 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')

crs(b2) = crs(b1)
crs(b3) = crs(b1)
crs(b4) = crs(b1)
crs(b5) = crs(b1)

b1 <- project(b1, sr); plot(b1)
b2 <- project(b2, sr); plot(b2)
b3 <- project(b3, sr); plot(b3)
b4 <- project(b4, sr); plot(b4)
b5 <- project(b5, sr); plot(b5)

b1 = resample(b1, b3, method = "near"); plot(b1)
b2 = resample(b2, b3, method = "near"); plot(b2)
b4 = resample(b4, b3, method = "near"); plot(b4)
b5 = resample(b5, b3, method = "near"); plot(b5)

fine_topo = mean(c(b1, b2, b3, b4, b5), na.rm = T)

fine_topo[fine_topo >= 0] <- NA
fine_topo[fine_topo <= -30] <- NA

plot(fine_topo)

topo_i = readAll(raster(fine_topo))

plot(topo_i)

save(topo_i, file = 'data/gis_bathymetry/asc_merged.RData')
