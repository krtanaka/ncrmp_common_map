library(terra)
library(terrainr)
library(tools)

rm(list = ls())

# Hawaii: Continuously Updated Digital Elevation Model (CUDEM) - 1/9 Arc-Second Resolution Bathymetric-Topographic Tiles
files <- list_files_with_exts("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_Job823441/", "tif")

path = paste0("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_Job823441/", "51m_res/") # Local without VPNs
path = paste0("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_Job823441/", "30m_res/") # Local without VPNs
path = paste0("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_Job823441/", "9m_res/") # Local without VPNs
path = paste0("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_Job823441/", "3m_res/") # Local without VPNs

if (!dir.exists(path)) dir.create(path, recursive = T)

for (f in 1:length(files)) {
  
  # f = 5
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

files <- list_files_with_exts("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_Job823441/51m_res/", "tif")
merge_rasters(files[1:16], output_raster = "N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged_51m_res.tif", overwrite = T)
plot(rast("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged_51m_res.tif"))

files <- list_files_with_exts("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_Job823441/30m_res/", "tif")
merge_rasters(files[1:16], output_raster = "N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged_30m_res.tif", overwrite = T)
plot(rast("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged_30m_res.tif"))

files <- list_files_with_exts("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_Job823441/9m_res/", "tif")
merge_rasters(files[1:16], output_raster = "N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged_9m_res.tif", overwrite = T)
plot(rast("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged_9m_res.tif"))

files <- list_files_with_exts("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_Job823441/3m_res/", "tif")
merge_rasters(files[1:16], output_raster = "N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged_3m_res.tif", overwrite = T)
plot(rast("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged_3m_res.tif"))

# calculate rugosity
df = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged_3m_res.tif")
df = terra::aggregate(df, fun = "sd", fact = 2, core = 14) # fact = 10 gives you 30m, fact = 17 gives you 51m, fact = 5 gives you 13m
df = project(df, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(df)
writeRaster(df, "N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged_6m_res_rugosity.tif", overwrite = TRUE)
