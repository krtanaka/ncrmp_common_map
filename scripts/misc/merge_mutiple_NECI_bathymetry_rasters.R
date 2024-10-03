library(terra)
library(terrainr)
library(tools)

rm(list = ls())

list_folders <- list.files('/Users/kisei.tanaka/Desktop/', full.names = TRUE)
list_folders <- list_folders[file.info(list_folders)$isdir]

for (l in head(list_folders, -2)) {
  
  # l = list_folders[7]
  
  folder_name <- basename(l)
  
  files <- list_files_with_exts(l, "tif")
  
  # path = paste0("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi2019_islands/", "51m_res/") # Local without VPNs
  # path = paste0("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi2019_islands/", "30m_res/") # Local without VPNs
  # path = paste0("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi2019_islands/", "9m_res/") # Local without VPNs
  # path = paste0("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi2019_islands/", "3m_res/") # Local without VPNs
  path <- paste0("N:/GIS/Projects/CommonMaps/Bathymetry/", folder_name, "/3m_res/")
  
  if (!dir.exists(path)) dir.create(path, recursive = T)
  
  for (f in 1:length(files)) {
    
    # f = 1
    topo <- rast(files[f])
    
    # fact = 10 gives you 30m,
    # fact = 17 gives you 51m, 
    # fact = 5 gives you 13m
    # fact = 3 gives you 9m
    
    topo = terra::aggregate(topo, fact = 3, core = 14)
    
    topo[topo >= 0] <- NA
    topo[topo <= -30] <- NA
    topo = project(topo, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(topo)
    writeRaster(topo, paste0(path, basename(files[f])), overwrite = TRUE)
    
  }
  
  # files <- list_files_with_exts("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi2019_islands/51m_res/", "tif")
  # merge_rasters(files[1:16], output_raster = "N:/GIS/Projects/CommonMaps/Bathymetry/cnmi_merged_51m_res.tif", overwrite = T)
  # plot(rast("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi_merged_51m_res.tif"))
  # 
  # files <- list_files_with_exts("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi2019_islands/30m_res/", "tif")
  # merge_rasters(files[1:16], output_raster = "N:/GIS/Projects/CommonMaps/Bathymetry/cnmi_merged_30m_res.tif", overwrite = T)
  # plot(rast("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi_merged_30m_res.tif"))
  # 
  # files <- list_files_with_exts("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi2019_islands/9m_res/", "tif")
  # merge_rasters(files[1:16], output_raster = "N:/GIS/Projects/CommonMaps/Bathymetry/cnmi_merged_9m_res.tif", overwrite = T)
  # plot(rast("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi_merged_9m_res.tif"))
  
  files <- list_files_with_exts(path, "tif")
  merge_rasters(files[1:length(files)], output_raster = paste0("N:/GIS/Projects/CommonMaps/Bathymetry/", folder_name, "_merged.tif"), overwrite = T)
  plot(rast(paste0("N:/GIS/Projects/CommonMaps/Bathymetry/", folder_name, "_merged.tif")))
  
  # # calculate rugosity
  # df = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi_merged_3m_res.tif")
  # df = terra::aggregate(df, fun = "sd", fact = 2, core = 14) # fact = 10 gives you 30m, fact = 17 gives you 51m, fact = 5 gives you 13m
  # df = project(df, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  # plot(df)
  # writeRaster(df, "N:/GIS/Projects/CommonMaps/Bathymetry/cnmi_merged_6m_res_rugosity.tif", overwrite = TRUE)

}


