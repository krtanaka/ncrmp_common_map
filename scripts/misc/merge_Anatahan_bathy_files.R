library(dplyr)
library(raster)
library(terra)
library(readr)

rm(list = ls())

# patch finer res bathymetry files first

b1 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2020_ngs_topobathy_anatahan/Job1075853_001_001.tif"); plot(b1); res(b1)
b2 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2020_ngs_topobathy_anatahan/Job1075853_001_002.tif"); plot(b2); res(b2)
b3 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2020_ngs_topobathy_anatahan/Job1075853_001_003.tif"); plot(b3); res(b3)
b4 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2020_ngs_topobathy_anatahan/Job1075853_001_004.tif"); plot(b4); res(b4)
b5 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2020_ngs_topobathy_anatahan/Job1075853_002_001.tif"); plot(b5); res(b5)
b6 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2020_ngs_topobathy_anatahan/Job1075853_002_002.tif"); plot(b6); res(b6)
b7 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2020_ngs_topobathy_anatahan/Job1075853_002_003.tif"); plot(b7); res(b7)
b8 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2020_ngs_topobathy_anatahan/Job1075853_002_004.tif"); plot(b8); res(b8)

# b9 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2022_rainier_anatahan/H13586_MBAB_6m_S221_100kHz_1of3.tif"); plot(b9); res(b9)
b10 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2022_rainier_anatahan/H13586_MBAB_2m_2803_300kHz_2of3.tif"); plot(b10); res(b10)
b11 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/2022_rainier_anatahan/H13586_MBAB_2m_2804_300kHz_3of3.tif"); plot(b11); res(b11)

# utm = read_csv('data/misc/ncrmp_utm_zones.csv')
# island_boxes = read_csv("data/misc/Island_Extents.csv") # Updated Bounding boxes 2021
# island_names_codes = read_csv("data/misc/island_name_code.csv")
# island_names_codes_boxes = merge(island_names_codes, island_boxes)
# rm(island_names_codes, island_boxes)
# utm_i = utm %>% subset(Island_Code == "ana")
# 
# # determine northern or southern hemisphere
# sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')
# # sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')

# b1 <- project(b1, sr); plot(b1)
# b2 <- project(b2, sr); plot(b2)
# b3 <- project(b3, sr); plot(b3)
# b4 <- project(b4, sr); plot(b4)
# b5 <- project(b5, sr); plot(b5)
# b6 <- project(b6, sr); plot(b6)
# b7 <- project(b7, sr); plot(b7)
# b8 <- project(b8, sr); plot(b8)
# b9 <- project(b9, sr); plot(b9)
# b10 <- project(b10, sr); plot(b10)
# b11 <- project(b11, sr); plot(b11)

# b9 = resample(b9, b10, method = "near"); plot(b9)

fine_topo_1 <- do.call(mosaic,  list(b1, b2, b3, b4, b5, b6, b7, b8))

fine_topo_1[fine_topo_1 >= 0] <- NA
fine_topo_1[fine_topo_1 <= -30] <- NA

plot(fine_topo_1)

fine_topo_2 = mosaic(b10, b11)

fine_topo_2[fine_topo_2 >= 0] <- NA
fine_topo_2[fine_topo_2 <= -30] <- NA

plot(fine_topo_2)

fine_topo_1 = resample(fine_topo_1, fine_topo_2, method = "near"); plot(fine_topo_1)

fine_topo = mean(c(fine_topo_1, fine_topo_2), na.rm = T)

fine_topo[fine_topo >= 0] <- NA
fine_topo[fine_topo <= -30] <- NA

topo_i = readAll(raster(fine_topo))

plot(topo_i)

save(topo_i, file = 'data/gis_bathymetry/ana_merged.RData')
