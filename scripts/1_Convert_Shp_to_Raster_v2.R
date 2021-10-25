library(raster)
library(rgdal)
library(rgeos)
library(dplyr)

rm(list = ls())

spatial_resolution = 100 # spatial resolution in m

shp_path = "L:/ktanaka/GIS" # pc

S.MARI = c("gua", "rot", "sai", "tin", "agu")
N.MARI = c("agr", "ala", "asc", "gug", "fdp", "mau", "sar")
AMSM = c("ofu", "ros", "swa", "tau", "tut")
PRIA = c("bak", "how", "jar", "joh", "kin", "pal", "wak")
MHI = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah")
NWHI = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr")

# Hard/Soft Bottom Substrate ----------------------------------------------

shp_list = list.files(path = paste0(shp_path, "/hardsoft/"), pattern = "\\.shp$", full.names = T); shp_list
# shp_list = shp_list[c(1:7, 12:14)]; shp_list

for (shp_i in 1:length(shp_list)) {
  
  start = Sys.time()
  
  # shp_i = 1
  
  dat <- shapefile(shp_list[shp_i], verbose = T)
  
  # dat <- spTransform(dat, CRS('+proj=utm +zone=55 +datum=WGS84 +units=km +no_defs'))
  dat <- spTransform(dat, CRS('+proj=utm +zone=55 +datum=WGS84 +units=m +no_defs'))
  # dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84'))
  
  dat = dat[c(names(dat) %in% c("HardSoft"))]
  
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
  
  island_name = tolower(substr(shp_list[shp_i], 25, 27))
  
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
# shp_list = shp_list[c(1, 9:11)]; shp_list

for (shp_i in 1:length(shp_list)) {
  
  start = Sys.time()
  
  # shp_i = 5
  
  dat <- shapefile(shp_list[shp_i], verbose = T)
  
  # dat <- spTransform(dat, CRS('+proj=utm +zone=55 +datum=WGS84 +units=km +no_defs'))
  dat <- spTransform(dat, CRS('+proj=utm +zone=55 +datum=WGS84 +units=m +no_defs'))
  # dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84'))
  
  dat = dat[c(names(dat) %in% c("Zone", "REEF_ZONE"))]
  
  names(dat) = "Reef"
  
  table(dat$Reef)
  
  dat <- dat[dat$Reef %in% c("Backreef", "Forereef", "Lagoon", "BRF", "FRF", "LAG"),]
  
  # get names
  nam <- unique(dat$Reef)
  
  # create a data.frame
  nam_df <- data.frame(ID = 1:length(nam), nam = nam)
  
  # Place IDs
  dat$ID <- nam_df$ID[match(dat$Reef,nam_df$nam)]
  
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
  
  island_name = tolower(substr(shp_list[shp_i], 25, 27))

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
shp_list = shp_list[c(10, 12)]; shp_list

for (shp_i in 1:length(shp_list)) {
  
  start = Sys.time()
  
  # shp_i = 4
  
  dat <- shapefile(shp_list[shp_i], verbose = T)
  
  zone <- spTransform(dat, CRS('+proj=longlat +datum=WGS84'))
  zone = as.data.frame(zone@polygons[[2]]@Polygons[[1]]@coords)
  zone <- (floor((zone$V1[1] + 180)/6) %% 60) + 1
  
  # dat <- spTransform(dat, CRS('+proj=utm +zone=55 +datum=WGS84 +units=km +no_defs'))
  dat <- spTransform(dat, CRS(paste0('+proj=utm +zone=', zone, ' +datum=WGS84 +units=m +no_defs')))
  # dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84'))
  
  dat = dat[c(names(dat) %in% c("SEC_NAME", "Sanctuary"))]
  
  names(dat) = "Sector"
  
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
  
  island_name = tolower(substr(shp_list[shp_i], 23, 25))
  
  raster = readAll(r)
  
  table = nam_df
  
  raster_and_table = list(raster, table)
  
  save(raster_and_table, file = paste0("data/gis_sector/raster_alt/", island_name, ".RData"))
  
  end = Sys.time()
  
  time = end - start
  
  print(paste0(island_name, "...done...took ", time, "..."))

}
