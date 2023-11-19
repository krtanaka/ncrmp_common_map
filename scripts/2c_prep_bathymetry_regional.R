#####################################################################
### Prep island bathymetry data using external regional GIS files ###
#####################################################################


rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(tidyr)
library(marmap)
library(lattice)
library(readr)

utm = read_csv('data/misc/ncrmp_utm_zones.csv')

islands = c("gua", "rot", "sai", "tin", "agu")[2:5]; region = "MARIAN" # South Mariana Islands
# islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "sar"); region = "MARIAN" # North Mariana Islands
islands = c("ofu", "ros", "swa", "tau", "tut")[2]; region = "SAMOA" # American Samoa, but Swa is not included
# islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs" # Pacific Remote Island Areas
islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI" # Main Hawaiian Islands
# islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI" # Northern Hawaiian Islands

island_boxes = read_csv("data/misc/Island_Extents.csv") # Updated Bounding boxes 2021
island_names_codes = read_csv("data/misc/island_name_code.csv")
island_names_codes_boxes = merge(island_names_codes, island_boxes)
rm(island_names_codes, island_boxes)

if(region == "MARIAN") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/mariana_trench_6_msl_2012.nc")
if(region == "SAMOA") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/pago_pago_3_mhw_2009.nc")
if(region == "MHI") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/usgsCeCrm10.nc")
if(region == "MHI") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/mhi_mbsyn_bathyonly_50m_v21.nc")
if(region == "MHI") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged.tif")

default_proj = crs(topo)
topo = as.data.frame(rasterToPoints(topo))
colnames(topo)[3] = "depth"
topo$depth = as.numeric(as.character(topo$depth))

# df = topo %>% 
#   mutate(x = round(x, 1), 
#          y = round(y, 1)) %>% 
#   group_by(x, y) %>% 
#   summarise(d = mean(depth))

# wireframe(unclass(as.bathy(df)), 
#           shade = T,
#           aspect = c(length(unique(df$y))/length(unique(df$x)), 0.05),
#           par.box = c(col = "transparent"),
#           scales = list(arrows = FALSE, col = "transparent"), # col="black" is required
#           par.settings = list(axis.line = list(col = 'transparent')),
#           light.source = c(10, 0, 10),
#           zlab = "", 
#           xlab = "",
#           ylab = "",
#           perspective = T,
#           screen = list(z = 10, x = -50, y = 10),
#           zoom = 1.2)

for (i in 1:length(islands)) {
  
  # i = 1
  
  box = island_names_codes_boxes %>% subset(Island_Code == islands[i])

  topo_i = topo %>%
    subset(depth >= -30 & depth <= 0) %>%
    subset(x < box$xmax & x > box$xmin & y < box$ymax & y > box$ymin)
  
  topo_i = rasterFromXYZ(topo_i)
  
  crs(topo_i) = default_proj
  
  utm_i = utm %>% subset(Island_Code == islands[i])
  
  # determine northern or southern hemisphere
  if (mean(extent(topo_i)[3:4]) > 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')
  if (mean(extent(topo_i)[3:4]) < 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')
  
  topo_i <- projectRaster(topo_i, crs = sr)
  
  topo_i = readAll(topo_i)
  # topo_i <- aggregate(topo_i, fact = 100/res(topo_i)); res(topo_i)
  plot(topo_i)
  print(islands[i])
  
  save(topo_i, file = paste0('data/gis_bathymetry/', islands[i], '.RData'))
  
}


