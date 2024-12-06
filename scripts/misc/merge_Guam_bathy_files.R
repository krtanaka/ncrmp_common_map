library(dplyr)
library(raster)
library(terra)
library(readr)

rm(list = ls())

b1 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/gua_mb_ld.asc"); plot(b1); res(b1)
b2 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/gua_nthmp_dem_10m_mosaic.tif"); plot(b2); res(b2)
b3 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/guam_5m.asc"); plot(b3); res(b3)
b4 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/Guam_60m.asc"); plot(b4); res(b4)
b5 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/ngdc_bathy_10m_guam_049d_6725_8fc5.nc"); plot(b5); res(b5)

utm = read_csv('data/misc/ncrmp_utm_zones.csv')
island_boxes = read_csv("data/misc/Island_Extents.csv") # Updated Bounding boxes 2021
island_names_codes = read_csv("data/misc/island_name_code.csv")
island_names_codes_boxes = merge(island_names_codes, island_boxes)
rm(island_names_codes, island_boxes)
utm_i = utm %>% subset(Island_Code == "gua")

if (mean(yFromRow(b1, 1:nrow(b1))) > 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')
if (mean(yFromRow(b1, 1:nrow(b1))) < 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')

crs(b5) <- "+proj=utm +zone=55 +datum=WGS84 +units=m +no_defs"

b5 <- project(b5, sr); plot(b5)

res = b3

b1 = resample(b1, res, method = "near"); plot(b1)
b2 = resample(b2, res, method = "near"); plot(b2)
b3 = resample(b3, res, method = "near"); plot(b3)
b4 = resample(b4, res, method = "near"); plot(b4)
b5 = resample(b5, res, method = "near"); plot(b5)

fine_topo = mean(c(b1, b2, b3, b4, b5), na.rm = T)

fine_topo[fine_topo >= 0] <- NA
fine_topo[fine_topo <= -30] <- NA

plot(fine_topo)

topo_i = readAll(raster(fine_topo))

plot(topo_i)

save(topo_i, file = 'data/gis_bathymetry/gua_merged.RData')
