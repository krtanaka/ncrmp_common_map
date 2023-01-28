library(raster)
library(terrainr)

files <- list_files_with_exts("/Users/kisei.tanaka/Desktop/hawaii_bty_5m/", "adf")

merge_rasters(files[1:5], output_raster = "/Users/kisei.tanaka/Desktop/test.tif", overwrite = T)

plot(raster("/Users/kisei.tanaka/Desktop/test.tif"))
