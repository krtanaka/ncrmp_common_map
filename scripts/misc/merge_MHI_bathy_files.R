# merge CUDEM, CRM and MBSYN bathymetry files for MHI islands to fill in potential missing areas

rm(list = ls())

library(raster)
islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah")

for (i in 1:length(islands)) {
  
  gc()
  
  # i = 7
  
  load(paste0("data/gis_bathymetry/", islands[i], ".RData")); b1 = topo_i
  load(paste0("data/gis_bathymetry/alt/", islands[i], "_crm.RData")); b2 = topo_i
  # load(paste0("data/gis_bathymetry/alt/", islands[i], "_mbsyn.RData")); b3 = topo_i; plot(b3)
  
  # resample 2m file at 5m
  b2 = resample(b2, b1, method = "ngb") 
  # b3 = resample(b3, b1, method = "ngb") 
  
  topo = mean(stack(b1, b2), na.rm = T)
  # topo = mean(stack(b1, b3), na.rm = T)
  # topo = mean(stack(topo, b3), na.rm = T)
  
  # par(mfrow = c(2,2))
  # plot(b1, main = "3m CUDEM 2023", col = matlab.like(100))
  # plot(topo, main = "3m CUDEM 2023 & 30m CRM 2023", col = matlab.like(100))
  # plot(abs(b1-b2), main = "abs(CUDEM-CRM)", col = matlab.like(100))
  # hist(abs(b1-b2), main = "abs(CUDEM-CRM)")

  topo_i = readAll(topo)
  
  save(topo_i, file = paste0("data/gis_bathymetry/", islands[i], "_merged.RData"))
  
  gc()
  
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
