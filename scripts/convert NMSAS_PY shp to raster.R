library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(readr)
library(colorRamps)

rm(list = ls())

spatial_resolution = 100 # spatial resolution in m

shp_path = "L:/ktanaka/GIS" # pc

utm = read_csv('data/ncrmp_utm_zones.csv')

shp_list = list.files(path = paste0(shp_path, "/sector/"), pattern = "\\.shp$", full.names = T); shp_list

island_name = tolower(substr(shp_list[10], 23, 30)); island_name

utm_i = utm %>% subset(Island_Code == island_name)

dat <- shapefile(shp_list[10], verbose = T)

dat_fortify = fortify(dat)


dat.df <- as.data.frame(dat)
dat.df$.id <- as.numeric(rownames(dat.df )) 



r <- raster(ncol=100, nrow=100)
r.polys <- rasterize(dat, r, field = dat@data[1,4], fun = "mean", 
                     update = TRUE, updateValue = "NA")
plot(r.polys)

dat <- spTransform(dat, CRS(paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs')))
# dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84'))

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

# rasterVis::levelplot(r)
plot(r, col = matlab.like(length(unique(r))))

raster = readAll(r)

table = nam_df

raster_and_table = list(raster, table)

save(raster_and_table, file = paste0("data/gis_sector/", island_name, ".RData"))

end = Sys.time()

time = end - start

print(paste0(island_name, "...done...took ", time, "..."))

