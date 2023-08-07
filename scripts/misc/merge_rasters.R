# alt approach

library(raster)
library(terrainr)

files = 
  merge_rasters(list("N:/GIS/Projects/CommonMaps/Bathymetry/how_dball.asc", 
                     "N:/GIS/Projects/CommonMaps/Bathymetry/Howland_5m.asc"), 
                output_raster = "N:/GIS/Projects/CommonMaps/Bathymetry/how_merged_5m.tif",
                overwrite = T)

plot(raster("N:/GIS/Projects/CommonMaps/Bathymetry/how_merged_5m.tif"))
