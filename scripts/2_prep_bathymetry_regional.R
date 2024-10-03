#####################################################################
### Prep island bathymetry data using external regional GIS files ###
#####################################################################

# Clear the workspace
rm(list = ls())

library(dplyr)       # For data manipulation
library(ggplot2)     # For creating plots
library(raster)      # For working with raster data
library(tidyr)       # For tidying data
library(marmap)      # For working with bathymetric data
library(lattice)     # For creating trellis plots
library(readr)       # For reading CSV files
library(terra)

# Read in the UTM zones CSV file
utm = read_csv('data/misc/ncrmp_utm_zones.csv')

# Define the islands and region for which to prepare bathymetry data
islands = c("gua", "rot", "sai", "tin", "agu")[2:5]; region = "MARIAN" # South Mariana Islands
islands = c("agr", "ala", "ana", "asc", "gug", "fdp", "mau", "sar"); region = "MARIAN" # North Mariana Islands
islands = c("ofu", "ros", "swa", "tau", "tut")[2]; region = "SAMOA" # American Samoa, but Swa is not included
# islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs" # Pacific Remote Island Areas
islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI" # Main Hawaiian Islands
islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI" # Northern Hawaiian Islands

# Read in CSV files containing island extents and island name codes
island_boxes = read_csv("data/misc/Island_Extents.csv") # Updated Bounding boxes 2021
island_names_codes = read_csv("data/misc/island_name_code.csv")

# Merge the two data frames on common columns
island_names_codes_boxes = merge(island_names_codes, island_boxes)

# Remove the original data frames to free up memory
rm(island_names_codes, island_boxes)

# Load the appropriate bathymetry raster file based on the selected region
if(region == "MARIAN") topo = rast("N:/GIS/Projects/CommonMaps/Bathymetry/mariana_trench_6_msl_2012.nc")
if(region == "MARIAN") topo = rast("N:/GIS/Projects/CommonMaps/Bathymetry/Bathymetry_ETOPO_2022_v1_15s_all_units.nc")
if(region == "MARIAN") topo = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi_merged_3m_res.tif")
if(region == "SAMOA") topo = rast("N:/GIS/Projects/CommonMaps/Bathymetry/pago_pago_3_mhw_2009.nc")
if(region == "NWHI") topo = rast("N:/GIS/Projects/CommonMaps/Bathymetry/Bathymetry_HURL_NWHI_60m_all_units.nc")
# if(region == "MHI") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/usgsCeCrm10.nc") #outdated
if(region == "MHI") topo = rast("N:/GIS/Projects/CommonMaps/Bathymetry/crm_vol10_2023.nc") #new CRM data at 1 arc second
if(region == "MHI") topo = rast("N:/GIS/Projects/CommonMaps/Bathymetry/mhi_mbsyn_bathyonly_50m_v21_0.001deg.tif")
# if(region == "MHI") topo = rast("N:/GIS/Projects/CommonMaps/Bathymetry/Bathymetry_ETOPO_2022_v1_15s_all_units.nc")
if(region == "MHI") topo = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged_51m_res.tif")
if(region == "MHI") topo = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged_30m_res.tif")
if(region == "MHI") topo = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged_9m_res.tif")
if(region == "MHI") topo = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_HI_merged_3m_res.tif")

default_proj = crs(topo)

# Define the function to set values not between -30 and 0 to NA
set_na <- function(x) {
  x[x < -30 | x > 0] <- NA
  return(x)
}

# topo = clamp(topo, lower = -30, upper = 0, values = TRUE)

# df = topo %>%
#   mutate(x = round(x, 1),
#          y = round(y, 1)) %>%
#   group_by(x, y) %>%
#   summarise(d = mean(depth))

# wireframe(unclass(as.bathy(df)),
#           shade = T,
#           aspect = c(length(unique(df$y))/length(unique(df$x)), 0.05),
#           par.box = c(col = "transparent"),
#           scales = list(arrows = FALSE, col = "transparent"), # col="black" is required
#           par.settings = list(axis.line = list(col = 'transparent')),
#           light.source = c(10, 0, 10),
#           zlab = "",
#           xlab = "",
#           ylab = "",
#           perspective = T,
#           screen = list(z = 10, x = -50, y = 10),
#           zoom = 1.2)

for (i in 1:length(islands)) {
  
  # i = 1
  
  # if (file.exists(paste0('data/gis_bathymetry/', islands[i], '.RData'))) {
  #   
  #   cat("Bathymetry data for this island already exists, skipping this island...\n")
  #   next
  #   
  # }
  
  box = island_names_codes_boxes %>% subset(Island_Code == islands[i])
  
  ymax <- box$ymax[1]
  ymin <- box$ymin[1]
  xmin <- box$xmin[1]
  xmax <- box$xmax[1]
  
  # Define the extent
  ext <- ext(xmin, xmax, ymin, ymax)
  
  # Crop the raster using the defined extent
  topo_i <- crop(topo, ext)
  
  # Apply the function to the raster
  topo_i <- app(topo_i, set_na)
  
  # topo_i = terra::as.data.frame(topo_i, xy = TRUE)
  # colnames(topo_i)[3] = "depth"
  # topo_i$depth = as.numeric(as.character(topo_i$depth))
  
  # topo_i = topo %>%
  #   subset(x < box$xmax & x > box$xmin & y < box$ymax & y > box$ymin) %>% 
  #   subset(depth >= -30 & depth <= 0)
  
  gc()
  
  # topo_i = rasterFromXYZ(topo_i)
  
  crs(topo_i) = default_proj
  
  utm_i = utm %>% subset(Island_Code == islands[i])
  
  # determine northern or southern hemisphere
  if (mean(yFromRow(topo_i, 1:nrow(topo_i))) > 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')
  if (mean(yFromRow(topo_i, 1:nrow(topo_i))) < 0) sr = paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')
  
  # topo_i <- projectRaster(topo_i, crs = sr)
  topo_i <- project(topo_i, sr)
  
  topo_i = readAll(raster(topo_i))
  # topo_i <- aggregate(topo_i, fact = 100/res(topo_i)); res(topo_i)
  plot(topo_i)
  print(islands[i])
  
  save(topo_i, file = paste0('data/gis_bathymetry/', islands[i], '.RData'))
  # save(topo_i, file = paste0('data/gis_bathymetry/alt/', islands[i], '_crm.RData'))
  # save(topo_i, file = paste0('data/gis_bathymetry/alt/', islands[i], '_mbsyn.RData'))
  
  gc()
  
}


