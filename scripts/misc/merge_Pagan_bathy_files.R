library(dplyr)
library(raster)
library(terra)
library(readr)

rm(list = ls())

# patch finer res bathymetry files first
b1 <- list.files("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi2019_fdm_pagan/", pattern = "\\.tif$", full.names = TRUE)
b1 <- lapply(b1, rast)   # Load each .tif as a raster
b1 <- do.call(merge, b1)
b1[b1 >= 0] <- NA
b1[b1 <= -30] <- NA

b2 = rast("N:/GIS/Projects/SeafloorCalc/Final_Products/pag_inpo/w001001.adf")
b2[b2 >= 0] <- NA
b2[b2 <= -30] <- NA

plot(b1); res(b1)
plot(b2); res(b2)

# utm = read_csv('data/misc/ncrmp_utm_zones.csv')
# island_boxes = read_csv("data/misc/Island_Extents.csv") # Updated Bounding boxes 2021
# island_names_codes = read_csv("data/misc/island_name_code.csv")
# island_names_codes_boxes = merge(island_names_codes, island_boxes)
# rm(island_names_codes, island_boxes)
# utm_i = utm %>% subset(Island_Code == "pag")
# 
# # determine northern or southern hemisphere
# sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')
# # sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')
# 
# b1 <- project(b1, sr); plot(b1)
# b2 <- project(b2, sr); plot(b2)
# b2 <- project(b2, sr); plot(b2)

b1 = resample(b1, b2, method = "near"); plot(b1)

fine_topo = mean(c(b1, b2), na.rm = T)

fine_topo[fine_topo >= 0] <- NA
fine_topo[fine_topo <= -30] <- NA

plot(fine_topo)

topo_i = readAll(raster(fine_topo))

plot(topo_i)

save(topo_i, file = 'data/gis_bathymetry/pag_merged.RData')
