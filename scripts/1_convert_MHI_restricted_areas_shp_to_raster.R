############################################
### Convert rstr_PY shapefile to raster ###
############################################

# Load required libraries
library(raster)      # For working with raster data
# library(rgeos)       # For geometric operations on spatial data
library(dplyr)       # For data manipulation
library(readr)       # For reading CSV files
library(colorRamps)  # For color ramp functions
library(ggrepel)     # For repelling overlapping text labels in ggplot2
library(sf)
library(sp)         # For simple features, a modern approach to spatial data
library(patchwork)   # For combining ggplot2 plots

# Clear the workspace
rm(list = ls())

# Set the target spatial resolution in meters
spatial_resolution = 100

# Set the buffer distance in degrees (used for buffering operations)
buffer_distance = 0.005

# Define the path to the shapefiles
shp_path = "N:/GIS/Projects/CommonMaps/Restricted areas_2016/"

# List all shapefiles in the specified path
shp_list = list.files(path = shp_path, pattern = "\\.shp$", full.names = TRUE)

# Select specific shapefiles from the list (1st, 3rd, 4th, and 5th)
shp_list = shp_list[c(1, 3:5)]; shp_list

##################################
### CFR_RestrictedAreas_Hawaii ###
##################################

dat <- shapefile(shp_list[1], verbose = T); plot(dat)
# dat <- gBuffer(dat, byid = TRUE, width = buffer_distance)
dat <- st_as_sf(dat)
dat <- st_buffer(dat, dist = buffer_distance)
dat <- as(dat, "Spatial"); plot(dat)

unique(dat$Name)

dat <- subset(dat, Name %in% c("Honolulu International Airport North Section Security Zone", 
                               "Honolulu International Airport South Section Security Zone",
                               "Pearl Harbor Prohibited Area",
                               # "MCB Danger Zone",
                               "MCB Hawaii Buffer Zone",
                               "Restricted Anchorage A",
                               "Restricted Anchorage B",
                               "Restricted Anchorage C",
                               "Restricted Anchorage D",
                               "Barbers Point Offshore Moorings Security Zone",
                               "Pacific Missile Range Facility at Barking Sands Safety Zone"))

ggplot(st_as_sf(dat)) +
  geom_sf(aes(fill = Name)) +
  coord_sf() +
  annotation_map(map_data("world"))

rstr <- as.data.frame(dat)
rstr = rstr$Name; rstr
rstr_name = gsub(" ", "_", rstr)
rstr_name = gsub("/", "_", rstr_name)
rstr_name = gsub("'", "", rstr_name)

dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84')); plot(dat); degAxis(1); degAxis(2)
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84"); plot(dat); degAxis(1); degAxis(2)

dat_i <- spTransform(dat,  CRS('+proj=utm +zone=4 +datum=WGS84 +units=m +no_defs +north'))

plot(dat_i); axis(1); axis(2)

dat_i = dat_i[c(names(dat_i) %in% c("Name"))]

names(dat_i) = "Name"

# get names
nam <- unique(dat_i$Name)

# create a data.frame
nam_df <- data.frame(ID = 1:length(nam), nam = nam)

# Place IDs
dat_i$ID <- nam_df$ID[match(dat_i$Name, nam_df$nam)]

# Define RasterLayer object
r.raster <- raster()

# Define raster extent
extent(r.raster) <- extent(dat_i)

# Define pixel size
res(r.raster) <- spatial_resolution

# rasterize
ras <- rasterize(x = dat_i, y = r.raster, field = "ID")

# ratify raster
r <- ratify(ras)

# Create levels
rat <- levels(r)[[1]]
rat$names <- nam_df$nam[rat$ID]
rat$IDs <- nam_df$ID[rat$ID]
levels(r) <- rat

raster = readAll(r)

table = nam_df[rat$ID,]

r_df <- as.data.frame(rasterToPoints(r))
colnames(r_df) <- c("x", "y", "ID")
r_df = merge(r_df, table)
r_df_label = r_df %>% group_by(nam) %>% summarise(x = median(x), y = median(y))

ggplot() +  
  geom_raster(data = r_df, aes(x, y, fill = nam), show.legend = F) + 
  geom_text_repel(data = r_df_label, aes(x, y, label = nam), max.overlaps = Inf)

raster_and_table = list(raster, table)

save(raster_and_table, file = "data/gis_sector/mhi_restricted_areas_kau_oah_a.RData")


#################
### eCFR_Area ###
#################

dat <- shapefile(shp_list[2], verbose = T); plot(dat)
# dat <- gBuffer(dat, byid = TRUE, width = buffer_distance)
dat <- st_as_sf(dat)
dat <- st_buffer(dat, dist = buffer_distance)
dat <- as(dat, "Spatial"); plot(dat)

unique(dat$SafetyZone)

# Zones to keep
dat <- subset(dat, SafetyZone %in% c("Honolulu International Airport North Section", 
                                     "Honolulu International Airport South Section",
                                     "Pacific Missile Range Facility",
                                     "Barbers Point Offshore Moorings"))

ggplot(st_as_sf(dat)) +
  geom_sf(aes(fill = SafetyZone)) +
  annotation_map(map_data("world")) + 
  scale_fill_discrete("")

rstr <- as.data.frame(dat)
rstr = rstr$SafetyZone; rstr
rstr_name = gsub(" ", "_", rstr)
rstr_name = gsub("/", "_", rstr_name)
rstr_name = gsub("'", "", rstr_name)

dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84')); plot(dat); degAxis(1); degAxis(2)
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84"); plot(dat); degAxis(1); degAxis(2)

dat_i <- spTransform(dat,  CRS('+proj=utm +zone=4 +datum=WGS84 +units=m +no_defs +north'))

plot(dat_i); axis(1); axis(2)

dat_i = dat_i[c(names(dat_i) %in% c("SafetyZone"))]

names(dat_i) = "Name"

# get names
nam <- unique(dat_i$Name)

# create a data.frame
nam_df <- data.frame(ID = 1:length(nam), nam = nam)

# Place IDs
dat_i$ID <- nam_df$ID[match(dat_i$Name, nam_df$nam)]

# Define RasterLayer object
r.raster <- raster()

# Define raster extent
extent(r.raster) <- extent(dat_i)

# Define pixel size
res(r.raster) <- spatial_resolution

# rasterize
ras <- rasterize(x = dat_i, y = r.raster, field = "ID")

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
  geom_text_repel(data = r_df_label, aes(x, y, label = nam), max.overlaps = Inf)

raster_and_table = list(raster, table)

save(raster_and_table, file = "data/gis_sector/mhi_restricted_areas_kau_oah_b.RData")


#######################################
### RestrictedAreas_Hawaii - zone 4 ###
#######################################

sites = c(#"Paiko Lagoon WS",   
          "Ahihi-Kinau NA Reserve",
          #"Pupukea MLCD", 
          #"Waikiki MLCD", 
          # "Molokini Shoal MLCD", 
          # "Manele-Hulopoe MLCD", 
          # "Honolua-Mokuleia Bay MLCD", 
          "Hanauma Bay MLCD")

dat <- shapefile(shp_list[4], verbose = T)
dat = dat %>% subset(Site_Label %in% sites); plot(dat)
# dat <- gBuffer(dat, byid = TRUE, width = buffer_distance)
dat <- st_as_sf(dat)
dat <- st_buffer(dat, dist = buffer_distance)
dat <- as(dat, "Spatial"); plot(dat)

unique(dat$Site_Label)

ggplot(st_as_sf(dat)) +
  geom_sf(aes(fill = Site_Name)) +
  coord_sf() +
  annotation_map(map_data("world")) + 
  scale_fill_discrete("")

dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84')); plot(dat); degAxis(1); degAxis(2); maps::map(add = T, col = "blue", fill = T)
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84"); plot(dat); degAxis(1); degAxis(2); maps::map(add = T, col = "blue", fill = T)

dat_i <- spTransform(dat,  CRS('+proj=utm +zone=4 +datum=WGS84 +units=m +no_defs +north'))
# dat_i <- spTransform(dat,  CRS('+proj=utm +zone=5 +datum=WGS84 +units=m +no_defs +north'))

plot(dat_i); axis(1); axis(2)

dat_i = dat_i[c(names(dat_i) %in% c("Site_Label"))]

names(dat_i) = "Name"

# get names
nam <- unique(dat_i$Name)

# create a data.frame
nam_df <- data.frame(ID = 1:length(nam), nam = nam)

# Place IDs
dat_i$ID <- nam_df$ID[match(dat_i$Name, nam_df$nam)]

# Define RasterLayer object
r.raster <- raster()

# Define raster extent
extent(r.raster) <- extent(dat_i)

# Define pixel size
res(r.raster) <- spatial_resolution

# rasterize
ras <- rasterize(x = dat_i, y = r.raster, field = "ID")

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
  geom_text_repel(data = r_df_label, aes(x, y, label = nam), max.overlaps = Inf)

raster_and_table = list(raster, table)

save(raster_and_table, file = "data/gis_sector/mhi_restricted_areas_oah_lan_mai.RData")


#######################################
### RestrictedAreas_Hawaii - zone 5 ###
#######################################

sites = c("Lapakahi MLCD", 
          "Waialea Bay MLCD", 
          "Old Kona Airport MLCD", 
          "Wai'opae Tidepools MLCD", 
          "Kealakekua Bay MLCD")

dat <- shapefile(shp_list[4], verbose = T)
dat = dat %>% subset(Site_Label %in% sites); plot(dat)
# dat <- gBuffer(dat, byid = TRUE, width = buffer_distance)
dat <- st_as_sf(dat)
dat <- st_buffer(dat, dist = buffer_distance)
dat <- as(dat, "Spatial"); plot(dat)

ggplot(st_as_sf(dat)) +
  geom_sf(aes(fill = Site_Name)) +
  coord_sf() +
  annotation_map(map_data("world")) + 
  scale_fill_discrete("")

dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84')); plot(dat); degAxis(1); degAxis(2)
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84"); plot(dat); degAxis(1); degAxis(2)

dat_i <- spTransform(dat,  CRS('+proj=utm +zone=5 +datum=WGS84 +units=m +no_defs +north'))

plot(dat_i); axis(1); axis(2)

dat_i = dat_i[c(names(dat_i) %in% c("Site_Label"))]

names(dat_i) = "Name"

# get names
nam <- unique(dat_i$Name)

# create a data.frame
nam_df <- data.frame(ID = 1:length(nam), nam = nam)

# Place IDs
dat_i$ID <- nam_df$ID[match(dat_i$Name, nam_df$nam)]

# Define RasterLayer object
r.raster <- raster()

# Define raster extent
extent(r.raster) <- extent(dat_i)

# Define pixel size
res(r.raster) <- spatial_resolution

# rasterize
ras <- rasterize(x = dat_i, y = r.raster, field = "ID")

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
  geom_text_repel(data = r_df_label, aes(x, y, label = nam), max.overlaps = Inf)

raster_and_table = list(raster, table)

save(raster_and_table, file = "data/gis_sector/mhi_restricted_areas_haw.RData")


############################################
### mokapu_peninsula_500yds-polygon      ###
### polygon created via Google Earth Pro ### 
### exported as kmz, converted to shp    ###
############################################

dat <- shapefile(shp_list[3], verbose = T); plot(dat)
# dat <- gBuffer(dat, byid = TRUE, width = buffer_distance)
dat <- st_as_sf(dat)
dat <- st_buffer(dat, dist = buffer_distance)
dat <- as(dat, "Spatial"); plot(dat)

ggplot(st_as_sf(dat)) +
  geom_sf(aes(fill = Name)) +
  coord_sf() +
  annotation_map(map_data("world")) + 
  scale_fill_discrete("")

dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84')); plot(dat); degAxis(1); degAxis(2)
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84"); plot(dat); degAxis(1); degAxis(2)

dat_i <- spTransform(dat,  CRS('+proj=utm +zone=4 +datum=WGS84 +units=m +no_defs +north'))

plot(dat_i); axis(1); axis(2)

names(dat_i) = "Name"

# get names
nam <- unique(dat_i$Name)

# create a data.frame
nam_df <- data.frame(ID = 1:length(nam), nam = nam)

# Place IDs
dat_i$ID <- nam_df$ID[match(dat_i$Name, nam_df$nam)]

# Define RasterLayer object
r.raster <- raster()

# Define raster extent
extent(r.raster) <- extent(dat_i)

# Define pixel size
res(r.raster) <- spatial_resolution

# rasterize
ras <- rasterize(x = dat_i, y = r.raster, field = "ID")

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
  geom_text_repel(data = r_df_label, aes(x, y, label = nam), max.overlaps = Inf)

raster_and_table = list(raster, table)

save(raster_and_table, file = "data/gis_sector/mhi_restricted_areas_oah_mokapu.RData")
