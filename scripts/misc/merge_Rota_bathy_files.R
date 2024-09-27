library(dplyr)
library(raster)
library(terra)
library(readr)

rm(list = ls())

# patch finer res bathymetry files first

# Rota Island: Bathymetry PIBHMC
# https://www.soest.hawaii.edu/pibhmc/cms/data-by-location/cnmi-guam/rota-island/rota-island-bathymetry/
b1 = raster("N:/GIS/Projects/CommonMaps/Bathymetry/rot_dball.asc"); plot(b1)
b2 = raster("N:/GIS/Projects/CommonMaps/Bathymetry/rot_dbmb_5m.asc"); plot(b2)
b3 = raster("N:/GIS/Projects/CommonMaps/Bathymetry/Rota_5m_bathymetry.asc"); plot(b3)
b4 = raster("N:/GIS/Projects/CommonMaps/Bathymetry/Rota_60m.asc"); plot(b4)

b1 = resample(b1, b2, method = "ngb"); plot(b1)
b3 = resample(b3, b2, method = "ngb"); plot(b3) 
b4 = resample(b4, b2, method = "ngb"); plot(b4) 

fine_topo = mean(stack(b1, b2, b3, b4), na.rm = T)

fine_topo[fine_topo >= 0] <- NA
fine_topo[fine_topo <= -30] <- NA

plot(fine_topo)

# then bring in coarser bathymetry file, you only need eastern side of the island
topo_1 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/mariana_trench_6_msl_2012.nc")
topo_2 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/Bathymetry_ETOPO_2022_v1_15s_all_units.nc")

utm = read_csv('data/misc/ncrmp_utm_zones.csv')
island_boxes = read_csv("data/misc/Island_Extents.csv") # Updated Bounding boxes 2021
island_names_codes = read_csv("data/misc/island_name_code.csv")
island_names_codes_boxes = merge(island_names_codes, island_boxes)
rm(island_names_codes, island_boxes)

default_proj = crs(topo_1)

box = island_names_codes_boxes %>% subset(Island_Code == "rot")

ymax <- box$ymax[1]
ymin <- box$ymin[1]
xmin <- box$xmin[1]
xmax <- box$xmax[1]

# Define the extent
ext <- ext(xmin, xmax, ymin, ymax)

# Crop the raster using the defined extent
topo_1 <- crop(topo_1, ext)
topo_2 <- crop(topo_2, ext)

crs(topo_1) = default_proj
crs(topo_2) = default_proj

utm_i = utm %>% subset(Island_Code == "rot")

# determine northern or southern hemisphere
if (mean(yFromRow(topo_1, 1:nrow(topo_1))) > 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')
if (mean(yFromRow(topo_1, 1:nrow(topo_1))) < 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')

# determine northern or southern hemisphere
if (mean(yFromRow(topo_2, 1:nrow(topo_2))) > 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')
if (mean(yFromRow(topo_2, 1:nrow(topo_2))) < 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')

topo_1 <- project(topo_1, sr); plot(topo_1)
topo_2 <- project(topo_2, sr); plot(topo_2)

topo_1 = raster(topo_1)
topo_2 = raster(topo_2)
topo_1 = resample(topo_1, b2, method = "ngb"); plot(topo_1) 
topo_2 = resample(topo_2, b2, method = "ngb"); plot(topo_2) 

coarse_topo = mean(stack(topo_1, topo_2), na.rm = T)
coarse_topo[coarse_topo >= 0] <- NA
coarse_topo[coarse_topo <= -30] <- NA
threshold_x <- 307523
coords <- coordinates(coarse_topo)
mask <- coords[, 1] <= threshold_x

coarse_topo[mask] <- NA

plot(coarse_topo)

topo = mean(stack(fine_topo, coarse_topo), na.rm = T)

topo_i = readAll(topo)

plot(topo_i)

save(topo_i, file = 'data/gis_bathymetry/rot_merged.RData')
