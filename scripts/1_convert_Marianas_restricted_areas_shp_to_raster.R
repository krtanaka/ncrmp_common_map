############################################
### Convert rstr_PY shapefile to raster ###
############################################

# Load required libraries
library(raster)      # For working with raster data
library(dplyr)       # For data manipulation
library(readr)       # For reading CSV files
library(colorRamps)  # For color ramp functions
library(ggrepel)     # For repelling overlapping text labels in ggplot2
library(sf)
library(sp)         # For simple features, a modern approach to spatial data
library(patchwork)   # For combining ggplot2 plots
library(maps)

# Clear the workspace
rm(list = ls())

# Set the target spatial resolution in meters
spatial_resolution = 10

# Set the buffer distance in degrees (used for buffering operations)
buffer_distance = 0.005

# Define the path to the shapefiles
shp_path = "N:/GIS/Projects/CommonMaps/Restricted_Areas/Marianas//"

# List all shapefiles in the specified path
shp_list = list.files(path = shp_path, pattern = "\\.shp$", full.names = TRUE)

# # Select specific shapefiles from the list (1st, 3rd, 4th, and 5th)
# shp_list = shp_list[c(1, 3:5)]; shp_list

##################################
### CFR_RestrictedAreas_Hawaii ###
##################################

dat <- shapefile(shp_list[1], verbose = T)
# dat <- gBuffer(dat, byid = TRUE, width = buffer_distance)
dat <- st_as_sf(dat)
dat <- st_buffer(dat, dist = buffer_distance)
dat <- as(dat, "Spatial"); plot(dat)
dat$Anc_Name = ifelse(is.na(dat$Anc_Name), "No_Name", dat$Anc_Name)

unique(dat$Anc_Name)

# dat <- subset(dat, Name %in% c(#"Honolulu International Airport North Section Security Zone", 
#   #"Honolulu International Airport South Section Security Zone",
#   # "Pearl Harbor Prohibited Area",
#   # "MCB Danger Zone",
#   "MCB Hawaii Buffer Zone",
#   "Restricted Anchorage A",
#   "Restricted Anchorage B",
#   "Restricted Anchorage C",
#   "Restricted Anchorage D",
#   "Barbers Point Offshore Moorings Security Zone"
#   # "Pacific Missile Range Facility at Barking Sands Safety Zone"
# ))

ggplot(st_as_sf(dat)) +
  annotation_map(map_data("world")) + 
  geom_sf(aes(fill = Anc_Name)) +
  coord_sf()

ggsave(last_plot(), file = "outputs/maps/Anc_Names_Saipan.png")

rstr <- as.data.frame(dat)
rstr = rstr$Anc_Name; rstr
rstr_name = gsub(" ", "_", rstr)
rstr_name = gsub("/", "_", rstr_name)
rstr_name = gsub("'", "", rstr_name)

dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84')); plot(dat); degAxis(1); degAxis(2)
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84"); plot(dat); degAxis(1); degAxis(2)

dat_i <- spTransform(dat,  CRS('+proj=utm +zone=55 +datum=WGS84 +units=m +no_defs +north'))

plot(dat_i); axis(1); axis(2)

dat_i = dat_i[c(names(dat_i) %in% c("Anc_Name"))]

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
  geom_text_repel(data = r_df_label, aes(x, y, label = nam), max.overlaps = Inf) + 
  coord_fixed()

raster_and_table = list(raster, table)

save(raster_and_table, file = "data/gis_sector/marianas_restricted_areas_sai.RData")

