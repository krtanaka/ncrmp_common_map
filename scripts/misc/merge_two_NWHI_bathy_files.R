# merge two bathymetry files for NWHI islands to fill in missing areas

rm(list = ls())

library(raster)
islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI" # Northern Hawaiian Islands

for (i in 1:length(islands)) {
  
  # i = 7
  
  # Northwestern Hawaiian Islands Bathymetry PIBHMC
  # https://www.soest.hawaii.edu/pibhmc/cms/
  load(paste0("data/gis_bathymetry/", islands[i], ".RData")); b1 = topo_i; plot(b1)
  
  # Northwestern Hawaiian Islands Multibeam Bathymetry Synthesis: 60-m Bathymetry
  # https://pae-paha.pacioos.hawaii.edu/erddap/info/hurl_bathy_60m_nwhi/index.html
  load(paste0("data/gis_bathymetry/alt/", islands[i], ".RData")); b2 = topo_i; plot(b2)
  
  # resample 2m file at 5m
  b2 = resample(b2, b1, method = "ngb") 

  topo = mean(stack(b1, b2), na.rm = T)
  
  plot(topo)
  
  topo_i = readAll(topo)
  
  save(topo_i, file = paste0("data/gis_bathymetry/", islands[i], "_merged.RData"))
  
  # # alt approach
  # 
  # library(raster)
  # library(terrainr)
  # 
  # files =
  #   merge_rasters(list("N:/GIS/Projects/CommonMaps/Bathymetry/how_dball.asc",
  #                      "N:/GIS/Projects/CommonMaps/Bathymetry/Howland_5m.asc"),
  #                 output_raster = "N:/GIS/Projects/CommonMaps/Bathymetry/how_merged_5m.tif",
  #                 overwrite = T)
  # 
  # plot(raster("N:/GIS/Projects/CommonMaps/Bathymetry/how_merged_5m.tif"))
  
}
