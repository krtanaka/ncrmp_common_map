library(terra)
library(terrainr)
library(tools)

# Hawaii: Continuously Updated Digital Elevation Model (CUDEM) - 1/9 Arc-Second Resolution Bathymetric-Topographic Tiles
files <- list_files_with_exts("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_Job823441/", "tif")

for (f in 1:length(files)) {
  
  f = 11
  
  # topo = raster::raster(files[f])
  topo <- rast(files[f])
  
  topo[topo > 0] <- NA
  topo[topo > -30] <- NA
  plot(topo)
  
  
  
}

merge_rasters(files[1:16], output_raster = "N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged.tif", overwrite = T)
plot(raster("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged.tif"))
topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged.tif")
