library(dplyr)
library(raster)
library(terra)
library(readr)

rm(list = ls())

b1 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2020_ngs_topobathy_guguan/2020_ngs_topobathy_dem_guguan_J1075176.tif"); plot(b1); res(b1)
b2 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2020_ngs_topobathy_guguan/2020_ngs_topobathy_guguan_Job1075177.tif"); plot(b2); res(b2)
b3 = rast("N:/GIS/Projects/SeafloorCalc/Final_Products/gug_inpo/w001001.adf"); plot(b3); res(b3)

utm = read_csv('data/misc/ncrmp_utm_zones.csv')
island_boxes = read_csv("data/misc/Island_Extents.csv") # Updated Bounding boxes 2021
island_names_codes = read_csv("data/misc/island_name_code.csv")
island_names_codes_boxes = merge(island_names_codes, island_boxes)
rm(island_names_codes, island_boxes)
utm_i = utm %>% subset(Island_Code == "gug")

if (mean(yFromRow(b1, 1:nrow(b1))) > 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')
if (mean(yFromRow(b1, 1:nrow(b1))) < 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')

b1 <- project(b1, sr); plot(b1)
b2 <- project(b2, sr); plot(b2)
b3 <- project(b3, sr); plot(b3)

b1 = resample(b1, b3, method = "near"); plot(b1)
b2 = resample(b2, b3, method = "near"); plot(b2)

fine_topo = mean(c(b1, b2, b3), na.rm = T)

fine_topo[fine_topo >= 0] <- NA
fine_topo[fine_topo <= -30] <- NA

plot(fine_topo)

topo_i = readAll(raster(fine_topo))

plot(topo_i)

save(topo_i, file = 'data/gis_bathymetry/gug_merged.RData')
