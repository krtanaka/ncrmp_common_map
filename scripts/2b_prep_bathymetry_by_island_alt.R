rm(list = ls())

library(raster)
library(dplyr)

islands = c("swa", "tut")

for (i in 1:length(islands)) {
  
  i = 2
  
  if (islands[i] == "swa") topo = raster("L:/ktanaka/GIS/bathymetry/pibhmc_bathy_40m_swains_ce56_bee8_9b67.nc") #Swain island
  if (islands[i] == "tut") topo = raster("L:/ktanaka/GIS/bathymetry/ngdc_bathy_10m_tutuila_1a99_4a20_d29e.nc") #Tutuila island
  
  default_proj = crs(topo)
  
  topo = as.data.frame(rasterToPoints(topo))
  colnames(topo)[3] = "depth"
  topo$depth = as.numeric(as.character(topo$depth))
  
  topo_i = topo %>% subset(depth >= -30 & depth <= 0)
  colnames(topo_i)[1:2] = c("lon", "lat")
  
  zone <- (floor((topo_i$lon[1] + 180)/6) %% 60) + 1
  
  topo_i = rasterFromXYZ(topo_i[,c("lon", "lat", "depth")]); plot(topo_i)
  crs(topo_i) = default_proj
  
  sr = paste0('+proj=utm +zone=', zone, '+ellps=GRS80 +datum=NAD83 +units=m +no_defs')
  # sr = paste0('+proj=utm +zone=', zone, '+datum=WGS84 +units=m +no_defs')
  
  topo_i <- projectRaster(topo_i, crs = sr); plot(topo_i)
  topo_i <- aggregate(topo_i, fact = 100/res(topo_i)); plot(topo_i)
  
  topo_i = readAll(topo_i)
  
  save(topo_i, file = paste0('data/gis_bathymetry/', islands[i], '.RData'))
  
}
