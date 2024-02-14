############################################
### Convert rstr_PY shapefile to raster ###
############################################

library(raster)
library(rgeos)
library(dplyr)
library(readr)
library(colorRamps)
library(ggrepel)
library(sf)
library(patchwork)

rm(list = ls())

spatial_resolution = 100 # target spatial resolution in m
buffer_distance = 0.005

# shp_path = "L:/ktanaka/GIS"
shp_path = "N:/GIS/Projects/CommonMaps/Restricted areas_2016/"

shp_list = list.files(path = shp_path, pattern = "\\.shp$", full.names = T)
shp_list = shp_list[c(1, 3:4)]; shp_list

##################################
### CFR_RestrictedAreas_Hawaii ###
##################################

dat <- shapefile(shp_list[1], verbose = T)
dat <- gBuffer(dat, byid = TRUE, width = buffer_distance)

png("outputs/CFR_RestrictedAreas_Hawaii.png", height = 18, width = 20, units = "in", res = 100)

p1 = ggplot(st_as_sf(dat)) +
  geom_sf(aes(fill = Type)) +
  coord_sf() +
  annotation_map(map_data("world")) + 
  scale_fill_discrete("") + 
  ggtitle("CFR Restricted Areas Hawaii") + 
  theme(legend.position = c(0.05, 0.15))

p2 = ggplot(st_as_sf(dat)) +
  geom_sf(aes(fill = Name)) +
  coord_sf() +
  annotation_map(map_data("world")) + 
  scale_fill_discrete("") + 
  theme(legend.position = c(0.2, 0.25))

p1/p2

dev.off()

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

dat <- shapefile(shp_list[2], verbose = T)
dat <- gBuffer(dat, byid = TRUE, width = buffer_distance)

png("outputs/eCFR_Area.png", height = 9, width = 18, units = "in", res = 100)
ggplot(st_as_sf(dat)) +
  geom_sf(aes(fill = SafetyZone)) +
  coord_sf() +
  annotation_map(map_data("world")) + 
  scale_fill_discrete("") + 
  ggtitle("eCFR_Area") + 
  theme(legend.position = c(0.1, 0.2))
dev.off()

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

sites = c("Paiko Lagoon WS",   
          "Ahihi-Kinau NA Reserve",
          "Pupukea MLCD", 
          "Waikiki MLCD", 
          "Molokini Shoal MLCD", 
          "Manele-Hulopoe MLCD", 
          "Honolua-Mokuleia Bay MLCD", 
          "Hanauma Bay MLCD")

dat <- shapefile(shp_list[3], verbose = T)
dat = dat %>% subset(Site_Label %in% sites)
dat <- gBuffer(dat, byid = TRUE, width = buffer_distance)

png("outputs/RestrictedAreas_Hawaii_a.png", height = 9, width = 13, units = "in", res = 100)
ggplot(st_as_sf(dat)) +
  geom_sf(aes(fill = Site_Name)) +
  coord_sf() +
  annotation_map(map_data("world")) + 
  scale_fill_discrete("") + 
  ggtitle("Restricted Areas Hawaii") + 
  theme(legend.position = c(0.15, 0.15))
dev.off()

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

save(raster_and_table, file = "data/gis_sector/mhi_restricted_areas_oah_lan_mau.RData")


#######################################
### RestrictedAreas_Hawaii - zone 5 ###
#######################################

sites = c("Lapakahi MLCD", 
          "Waialea Bay MLCD", 
          "Old Kona Airport MLCD", 
          "Wai'opae Tidepools MLCD", 
          "Kealakekua Bay MLCD")

dat <- shapefile(shp_list[3], verbose = T)
dat = dat %>% subset(Site_Label %in% sites)
dat <- gBuffer(dat, byid = TRUE, width = buffer_distance)

png("outputs/RestrictedAreas_Hawaii_b.png", height = 9, width = 13, units = "in", res = 100)
ggplot(st_as_sf(dat)) +
  geom_sf(aes(fill = Site_Name)) +
  coord_sf() +
  annotation_map(map_data("world")) + 
  scale_fill_discrete("") + 
  ggtitle("Restricted Areas Hawaii") + 
  theme(legend.position = c(0.8, 0.85))
dev.off()


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
