library(raster)
library(rgdal)
library(rgeos)
library(dplyr)

rm(list = ls())

spatial_resolution = 100 # spatial resolution in m

shp_path = "L:/ktanaka/GIS" # pc

# Hard/Soft Bottom Substrate ----------------------------------------------

shp_list = list.files(path = paste0(shp_path, "/hardsoft/"), pattern = "\\.shp$", full.names = T); shp_list
# shp_list = shp_list[c(1:7, 12:14)]; shp_list

for (shp_i in 1:length(shp_list)) {
  
  start = Sys.time()
  
  # shp_i = 1
  
  dat <- shapefile(shp_list[shp_i])
  
  # dat <- spTransform(dat, CRS('+proj=utm +zone=55 +datum=WGS84 +units=km +no_defs'))
  dat <- spTransform(dat, CRS('+proj=utm +zone=55 +datum=WGS84 +units=m +no_defs'))
  # dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84'))
  
  dat <- dat[dat$HardSoft %in% c("Hard", "hard", "Unknown", "unknown"),]
  
  # get names
  nam <- unique(dat$HardSoft); nam
  
  # create a data.frame
  nam_df <- data.frame(ID = 1:length(nam), nam = nam); nam_df
  
  # Place IDs
  dat$ID <- nam_df$ID[match(dat$HardSoft,nam_df$nam)]
  
  # Define RasterLayer object
  r.raster <- raster()
  
  # Define raster extent
  extent(r.raster) <- extent(dat)
  
  # Define pixel size
  res(r.raster) <- spatial_resolution
  
  # rasterize
  ras <- rasterize(x = dat, y = r.raster, field = "ID")
  
  # ratify raster
  r <- ratify(ras)
  
  # Create levels
  rat <- levels(r)[[1]]
  rat$names <- nam_df$nam
  rat$IDs <- nam_df$ID
  levels(r) <- rat
  
  # rasterVis::levelplot(r)
  
  # island_name = tolower(substr(shp_list[shp_i], 25, nchar(shp_list[shp_i])-18))
  island_name = tolower(substr(shp_list[shp_i], 25, nchar(shp_list[shp_i])))
  
  raster = readAll(r)
  
  table = nam_df
  
  raster_and_table = list(raster, table)
  
  save(raster_and_table, file = paste0("data/gis_hardsoft/raster_alt/", island_name, ".RData"))
  
  end = Sys.time()
  
  time = end - start
  
  print(paste0(island_name, "...done...took ", time, "..."))
  
}

# Reef Zones --------------------------------------------------------------

shp_list = list.files(path = paste0(shp_path, "/reefzone/"), pattern = "\\.shp$", full.names = T); shp_list
shp_list = shp_list[c(1, 9:11)]; shp_list

for (shp_i in 1:length(shp_list)) {
  
  start = Sys.time()
  
  # shp_i = 3
  
  dat <- shapefile(shp_list[shp_i])
  
  # dat <- spTransform(dat, CRS('+proj=utm +zone=55 +datum=WGS84 +units=km +no_defs'))
  dat <- spTransform(dat, CRS('+proj=utm +zone=55 +datum=WGS84 +units=m +no_defs'))
  # dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84'))
  
  names(dat)[2] = "ZONE_CODE"
  
  table(dat$ZONE_CODE)
  
  dat <- dat[dat$ZONE_CODE %in% c("Backreef", "Forereef", "Lagoon", "BRF", "FRF", "LAG"),]
  
  # get names
  nam <- unique(dat$ZONE_CODE)
  
  # create a data.frame
  nam_df <- data.frame(ID = 1:length(nam), nam = nam)
  
  # Place IDs
  dat$ID <- nam_df$ID[match(dat$ZONE_CODE,nam_df$nam)]
  
  # Define RasterLayer object
  r.raster <- raster()
  
  # Define raster extent
  extent(r.raster) <- extent(dat)
  
  # Define pixel size
  res(r.raster) <- spatial_resolution
  
  # rasterize
  ras <- rasterize(x = dat, y = r.raster, field = "ID")
  
  # ratify raster
  r <- ratify(ras)
  
  # Create levels
  rat <- levels(r)[[1]]
  rat$names <- nam_df$nam
  rat$IDs <- nam_df$ID
  levels(r) <- rat
  
  # rasterVis::levelplot(r)
  
  island_name = tolower(substr(shp_list[shp_i], 25, nchar(shp_list[shp_i])-21))
  
  raster = readAll(r)
  
  table = nam_df
  
  raster_and_table = list(raster, table)
  
  save(raster_and_table, file = paste0("data/gis_reef/raster_alt/", island_name, ".RData"))
  
  end = Sys.time()
  
  time = end - start
  
  print(paste0(island_name, "...done...took ", time, "..."))
  
}

# Sub-Island Sector -------------------------------------------------------

shp_list = list.files(path = paste0(shp_path, "/sector/"), pattern = "\\.shp$", full.names = T); shp_list
shp_list = shp_list[1]; shp_list

for (shp_i in 1:length(shp_list)) {
  
  start = Sys.time()
  
  # shp_i = 1
  
  dat <- shapefile(shp_list[shp_i])
  
  # dat <- spTransform(dat, CRS('+proj=utm +zone=55 +datum=WGS84 +units=km +no_defs'))
  dat <- spTransform(dat, CRS('+proj=utm +zone=55 +datum=WGS84 +units=m +no_defs'))
  # dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84'))
  
  dat <- dat[dat$Sector != c("Land"),]
  dat <- dat[dat$Sector != c("Harbor"),]
  
  # get names
  nam <- unique(dat$Sector)
  
  # create a data.frame
  nam_df <- data.frame(ID = 1:length(nam), nam = nam)
  
  # Place IDs
  dat$ID <- nam_df$ID[match(dat$Sector, nam_df$nam)]
  
  # Define RasterLayer object
  r.raster <- raster()
  
  # Define raster extent
  extent(r.raster) <- extent(dat)
  
  # Define pixel size
  res(r.raster) <- spatial_resolution
  
  # rasterize
  ras <- rasterize(x = dat, y = r.raster, field = "ID")
  
  # ratify raster
  r <- ratify(ras)
  
  # Create levels
  rat <- levels(r)[[1]]
  rat$names <- nam_df$nam
  rat$IDs <- nam_df$ID
  levels(r) <- rat
  
  # rasterVis::levelplot(r)
  
  island_name = tolower(substr(shp_list[shp_i], 23, nchar(shp_list[shp_i])-37))
  
  raster = readAll(r)
  
  table = nam_df
  
  raster_and_table = list(raster, table)
  
  save(raster_and_table, file = paste0("data/gis_sector/raster_alt/", island_name, ".RData"))
  
  end = Sys.time()
  
  time = end - start
  
  print(paste0(island_name, "...done...took ", time, "..."))
  
  
}
