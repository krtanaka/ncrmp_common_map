###################################################################
### Prep island bathymetry data using external island GIS files ###
###################################################################

rm(list = ls())

library(raster)
library(dplyr)

islands = c("ofu", "ros", "swa", "tau", "tut")

for (i in 1:length(islands)) {
  
  # i = 4
  
  if (islands[i] == "ofu") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/pibhmc_bathy_5m_ofuolosega_98c9_b4c8_bba3.nc") #Ofu And Olosega island
  if (islands[i] == "ros") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/pibhmc_bathy_40m_rose_1488_4a6e_4b0d.nc") #Rose Atoll
  if (islands[i] == "swa") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/pibhmc_bathy_40m_swains_ce56_bee8_9b67.nc") #Swain island
  if (islands[i] == "tau") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/pibhmc_bathy_5m_tau_82ff_d595_4f15.nc") #Tau island
  if (islands[i] == "tut") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/ngdc_bathy_10m_tutuila_1a99_4a20_d29e.nc") #Tutuila island
  
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
