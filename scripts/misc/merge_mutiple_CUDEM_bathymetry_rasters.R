library(terra)
library(terrainr)
library(tools)

rm(list = ls())

# Hawaii: Continuously Updated Digital Elevation Model (CUDEM) - 1/9 Arc-Second Resolution Bathymetric-Topographic Tiles
files <- list_files_with_exts("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_Job823441/", "tif")

path = paste0("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_Job823441/", "51m_res/") # Local without VPNs
if (!dir.exists(path)) dir.create(path, recursive = T)

for (f in 1:length(files)) {
  
  # f = 5
  topo <- rast(files[f])
  topo = terra::aggregate(topo, fact = 17, core = 8) # fact = 10 gives you 30m, fact = 17 gives you 51m
  topo[topo >= 0] <- NA
  topo[topo <= -30] <- NA
  topo = project(topo, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  plot(topo)
  writeRaster(topo, paste0(path, basename(files[f])), overwrite = TRUE)
  
}

files <- list_files_with_exts("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_Job823441/51m_res/", "tif")

merge_rasters(files[1:16], output_raster = "N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged.tif", overwrite = T)
plot(rast("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged.tif"))
