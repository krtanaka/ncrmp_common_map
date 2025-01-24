#######################################################################
### Convert Hardsoft, Reefzones, Island sector shapefiles to raster ###
#######################################################################

# Load required libraries
library(raster)      # For working with raster data
library(dplyr)       # For data manipulation
library(readr)       # For reading CSV files
library(colorRamps)  # For color ramp functions
library(ggrepel)     # For repelling overlapping text labels in ggplot2
library(sf)
library(sp)

# Clear the workspace
rm(list = ls())

# Set the target spatial resolution in meters
spatial_resolution = 100

# Define the path to the GIS Common Map shapefiles (PICSWORDFISH)
shp_path = "N:/GIS/Projects/CommonMaps"

# Read a CSV file containing UTM zone data for each island
utm = read_csv('data/misc/ncrmp_utm_zones.csv')

# Hard/Soft Bottom Substrate ----------------------------------------------

shp_list = list.files(path = paste0(shp_path, "/hardsoft/"), pattern = "\\.shp$", full.names = T); shp_list
# shp_list = shp_list[c(1:7, 12:14)]; shp_list

for (shp_i in 1:length(shp_list)) {
  
  start = Sys.time()
  
  # shp_i = 7
  
  island_name = tolower(substr(shp_list[shp_i], 37, 39)); island_name
  
  utm_i = utm %>% subset(Island_Code == island_name)
  
  # dat = shapefile(shp_list[shp_i], verbose = T)
  
  # import as sf dataframe if shp takes too long to read (e.g., ffs)
  dat = st_read(shp_list[shp_i])
  # dat = as(dat, "SpatialPolygonsDataFrame")
  dat = as(dat, "Spatial")
  
  # proj4string(dat) <- CRS("+proj=longlat +datum=WGS84")
  dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84'))
  
  # plot(dat); degAxis(1); degAxis(2)
  
  # determine northern or southern hemisphere
  if (median((dat@bbox[2,])) > 0) dat <- spTransform(dat, CRS(paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')))
  if (median((dat@bbox[2,])) < 0) dat <- spTransform(dat, CRS(paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')))
  
  # plot(dat); axis(1); axis(2)
  
  dat = dat[c(names(dat) %in% c("HardSoft", "hard_soft"))]
  
  names(dat) = "HardSoft"
  
  # dat <- dat[dat$HardSoft %in% c("Hard", "hard", "Unknown", "unknown"),]
  
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
  
  raster = readAll(r)
  
  table = nam_df
  
  r_df <- as.data.frame(rasterToPoints(r))
  colnames(r_df) <- c("x", "y", "ID")
  r_df = merge(r_df, table)
  
  ggplot() +  
    geom_raster(data = r_df, aes(x, y, fill = nam), show.legend = T) + 
    coord_equal()
  
  raster_and_table = list(raster, table)
  
  save(raster_and_table, file = paste0("data/gis_hardsoft/", island_name, ".RData"))
  
  end = Sys.time()
  
  time = end - start
  
  cat(paste0(island_name, "...done...took ", time, " seconds...\n"))
  
}

# Reef Zones --------------------------------------------------------------

shp_list = list.files(path = paste0(shp_path, "/reefzone/"), pattern = "\\.shp$", full.names = T); shp_list
# shp_list = shp_list[c(1, 9:11)]; shp_list

for (shp_i in 1:length(shp_list)) {
  
  start = Sys.time()
  
  # shp_i = 29
  
  island_name = tolower(substr(shp_list[shp_i], 37, 39)); island_name
  
  utm_i = utm %>% subset(Island_Code == island_name)
  
  # dat = shapefile(shp_list[shp_i], verbose = T)
  
  # import as sf dataframe if shp takes too long to read (e.g., ffs)
  dat = st_read(shp_list[shp_i])
  # dat = as(dat, "SpatialPolygonsDataFrame")
  dat = as(dat, "Spatial")
  
  # proj4string(dat) <- CRS("+proj=longlat +datum=WGS84")
  dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84'))
  
  # plot(dat); degAxis(1); degAxis(2)
  
  # determine northern or southern hemisphere
  if (median((dat@bbox[2,])) > 0) dat <- spTransform(dat, CRS(paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')))
  if (median((dat@bbox[2,])) < 0) dat <- spTransform(dat, CRS(paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')))
  
  # plot(dat); axis(1); axis(2)
  
  dat = dat[c(names(dat) %in% c("Zone", "Zones", "REEF_ZONE"))]
  
  names(dat) = "Reef"
  
  table(dat$Reef)
  
  # dat <- dat[dat$Reef %in% c("Backreef", "Forereef", "Lagoon", "BRF", "FRF", "LAG"),]
  
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
  
  raster = readAll(r)
  
  table = nam_df
  
  r_df <- as.data.frame(rasterToPoints(r))
  colnames(r_df) <- c("x", "y", "ID")
  r_df = merge(r_df, table)
  
  ggplot() +  
    geom_raster(data = r_df, aes(x, y, fill = nam), show.legend = T) + 
    coord_equal() + 
    scale_fill_viridis_d("") + 
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) 
    
  
  raster_and_table = list(raster, table)
  
  save(raster_and_table, file = paste0("data/gis_reef/", island_name, ".RData"))
  
  end = Sys.time()
  
  time = end - start
  
  cat(paste0(island_name, "...done...took ", time, " seconds...\n"))
  
}

# Sub-Island Sector -------------------------------------------------------

shp_list = list.files(path = paste0(shp_path, "/sector/"), pattern = "\\.shp$", full.names = T); shp_list
shp_list = shp_list[!shp_list %in% grep(paste0('NMSAS_PY', collapse = "|"), shp_list, value = T)]; shp_list # process NMSAS_PY (A. Samoa) separately, see 1b

for (shp_i in 1:length(shp_list)) {
  
  start = Sys.time()
  
  # shp_i = 1
  
  island_name = tolower(substr(shp_list[shp_i], 35, 37)); island_name
  
  utm_i = utm %>% subset(Island_Code == island_name)
  
  # dat = shapefile(shp_list[shp_i], verbose = T)
  
  # import as sf dataframe if shp takes too long to read (e.g., ffs)
  dat = st_read(shp_list[shp_i])
  # dat = as(dat, "SpatialPolygonsDataFrame")
  dat = as(dat, "Spatial")
  
  # proj4string(dat) <- CRS("+proj=longlat +datum=WGS84")
  dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84'))
  
  # plot(dat); degAxis(1); degAxis(2)
  
  # determine northern or southern hemisphere
  if (median((dat@bbox[2,])) > 0) dat <- spTransform(dat, CRS(paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')))
  if (median((dat@bbox[2,])) < 0) dat <- spTransform(dat, CRS(paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')))
  
  # plot(dat); axis(1); axis(2)
  
  dat = dat[c(names(dat) %in% c("SEC_NAME", "Sanctuary"))]
  
  names(dat) = "Sector"
  
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
  
  raster = readAll(r)
  
  table = nam_df
  
  r_df <- as.data.frame(rasterToPoints(r))
  colnames(r_df) <- c("x", "y", "ID")
  r_df = merge(r_df, table)
  r_df_label = r_df %>% group_by(nam) %>% summarise(x = median(x), y = median(y))
  
  ggplot() +  
    geom_raster(data = r_df, aes(x, y, fill = nam), show.legend = F) + 
    geom_text_repel(data = r_df_label, aes(x, y, label = nam))
  
  raster_and_table = list(raster, table)
  
  save(raster_and_table, file = paste0("data/gis_sector/", island_name, ".RData"))
  
  end = Sys.time()
  
  time = end - start
  
  cat(paste0(island_name, "...done...took ", time, " seconds...\n"))
  
}
