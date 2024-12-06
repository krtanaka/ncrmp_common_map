###################################################################
### Prep island bathymetry data using in-house island GIS files ###
###################################################################

# Clear the workspace
rm(list = ls())

# Load required libraries
library(dplyr)       # For data manipulation
library(ggplot2)     # For creating plots
library(raster)      # For working with raster data
library(tidyr)       # For tidying data
library(marmap)      # For working with bathymetric data
library(lattice)     # For creating trellis plots
library(readr)       # For reading CSV files

# Read in CSV files containing island extents and island name codes
island_boxes = read_csv("data/misc/Island_Extents.csv") # Updated Bounding boxes 2021
island_names_codes = read_csv("data/misc/island_name_code.csv")

# Merge the two data frames on common columns
island_names_codes_boxes = merge(island_names_codes, island_boxes)

# Remove the original data frames to free up memory
rm(island_names_codes, island_boxes)

# Load the bathymetry raster file for Guam
topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/ngdc_bathy_10m_guam_049d_6725_8fc5.nc")

# Store the default projection of the raster
default_proj = crs(topo)

# Convert the raster data to a data frame
topo = as.data.frame(rasterToPoints(topo))
colnames(topo)[3] = "depth"

# Convert the depth values to numeric
topo$depth = as.numeric(as.character(topo$depth))

# Subset the island box for Guam
box = island_names_codes_boxes %>% subset(Island_Code == "gua")

# Subset the topo data frame for depth between -30 and 0 meters and within the bounding box of Guam
topo = topo %>%
  subset(depth >= -30 & depth <= 0) %>%
  subset(x < box$xmax & x > box$xmin & y < box$ymax & y > box$ymin)

# Convert the subsetted data frame back to a raster
topo = rasterFromXYZ(topo)

# Set the projection of the new raster to the default projection
crs(topo) = default_proj

# Read in the UTM zones CSV file
utm = read_csv('data/misc/ncrmp_utm_zones.csv')

# Subset the UTM data frame for Guam
utm_i = utm %>% subset(Island_Code == "gua")

# Determine whether the raster is in the northern or southern hemisphere based on the mean latitude
if (mean(extent(topo)[3:4]) > 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')
if (mean(extent(topo)[3:4]) < 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')

# Project the raster to the determined UTM coordinate reference system
topo <- projectRaster(topo, crs = sr)

# topo_i <- aggregate(topo, fact = 100/res(topo))
topo_i = topo
plot(topo_i); res(topo_i)

save(topo_i, file = 'data/gis_bathymetry/gua.RData')
