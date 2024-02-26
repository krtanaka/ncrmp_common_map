########################################################
### Convert Swain island bathymetry shapefile raster ###
########################################################

library(raster)
library(rgeos)
library(dplyr)
library(readr)
library(colorRamps)
library(ggplot2)
library(ggrepel)

rm(list = ls())

spatial_resolution = 10 # spatial resolution in m

shp_path = "N:/GIS/Projects/CommonMaps"

utm = read_csv('data/misc/ncrmp_utm_zones.csv')

island_name = "swa"

utm_i = utm %>% subset(Island_Code == island_name)

dat <- shapefile("N:/GIS/Projects/CommonMaps/Bathymetry/Swains_depth_bins.shp", verbose = T)

# proj4string(dat) <- CRS("+proj=longlat +datum=WGS84"); plot(dat); degAxis(1); degAxis(2)
dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84')); plot(dat); degAxis(1); degAxis(2)
dat <- spTransform(dat, CRS(paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs + south'))); plot(dat); axis(1); axis(2)

dat = subset(dat, Contour != "coral reef awash LW")

# get names
nam <- unique(dat$Contour); nam

# create a data.frame
nam_df <- data.frame(ID = 1:length(nam), nam = nam); nam_df

# Place IDs
dat$ID <- nam_df$ID[match(dat$Contour, nam_df$nam)]

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

save(raster_and_table, file = paste0("data/gis_bathymetry/", island_name, "_alt.RData"))
