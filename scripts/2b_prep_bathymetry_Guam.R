###################################################################
### Prep island bathymetry data using in-house island GIS files ###
###################################################################

rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(tidyr)
library(marmap)
library(lattice)

island_boxes = read_csv("data/misc/Island_Extents.csv") # Updated Bounding boxes 2021
island_names_codes = read_csv("data/misc/island_name_code.csv")
island_names_codes_boxes = merge(island_names_codes, island_boxes)
rm(island_names_codes, island_boxes)

topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/ngdc_bathy_10m_guam_049d_6725_8fc5.nc")

default_proj = crs(topo)

topo = as.data.frame(rasterToPoints(topo))
colnames(topo)[3] = "depth"
topo$depth = as.numeric(as.character(topo$depth))

box = island_names_codes_boxes %>% subset(Island_Code == "gua")

topo = topo %>%
  subset(depth >= -30 & depth <= 0) %>%
  subset(x < box$xmax & x > box$xmin & y < box$ymax & y > box$ymin)

topo = rasterFromXYZ(topo)

crs(topo) = default_proj

utm = read_csv('data/misc/ncrmp_utm_zones.csv')

utm_i = utm %>% subset(Island_Code == "gua")

# determine northern or southern hemisphere
if (mean(extent(topo)[3:4]) > 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')
if (mean(extent(topo)[3:4]) < 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')

topo <- projectRaster(topo, crs = sr)

topo_i <- aggregate(topo, fact = 100/res(topo)); res(topo_i)
plot(topo_i)

save(topo_i, file = 'data/gis_bathymetry/gua.RData')
