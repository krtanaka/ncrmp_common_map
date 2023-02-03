library(raster)
library(terrainr)
library(tools)

# Hawaii: Continuously Updated Digital Elevation Model (CUDEM) - 1/9 Arc-Second Resolution Bathymetric-Topographic Tiles

files <- list_files_with_exts("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_Job823441/", "tif")

merge_rasters(files[1:16], output_raster = "N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged.tif", overwrite = T)

plot(raster("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged.tif"))

topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged.tif")

# if depth raster files contains no below sea-level cells (e.g. Swa), don't subset them
if(min(values(topo), na.rm = T) <= 0) {
  
  topo[topo <= -30] <- NA
  topo[topo >= 0] <- NA  
  
}