############################################
### Convert NMSAS_PY shapefile to raster ###
############################################

# Load required libraries
library(raster)      # For working with raster data
library(dplyr)       # For data manipulation
library(readr)       # For reading CSV files
library(colorRamps)  # For color ramp functions
library(ggrepel)     # For repelling overlapping text labels in ggplot2


# Clear the workspace
rm(list = ls())

# Set the target spatial resolution in meters
spatial_resolution = 10

# Define the path to the shapefile
shp_path = "N:/GIS/Projects/CommonMaps/Sector/"

# List all shapefiles in the specified path
shp_list = list.files(path = shp_path, pattern = "\\.shp$", full.names = TRUE); shp_list

# Extract the island name from the 10th shapefile's filename
island_name = tolower(substr(shp_list[10], 35, 42)); island_name

# Load the 10th shapefile and plot it
dat <- shapefile(shp_list[10], verbose = TRUE); plot(dat); degAxis(1); degAxis(2); maps::map(add = TRUE, col = "blue", fill = TRUE)

# Convert the shapefile data to a data frame and extract the 'Label' column
nmsas <- as.data.frame(dat)
nmsas = nmsas$Label; nmsas

# Transform the coordinate reference system of the shapefile to WGS84
dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84')); plot(dat); degAxis(1); degAxis(2)
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84"); plot(dat); degAxis(1); degAxis(2)

# Read a CSV file containing UTM zone data
utm = read_csv('data/misc/ncrmp_utm_zones.csv')

# Define a vector of island names
island_name = c("ros", "swa", "tau")

for (i in 1:length(island_name)) {
  
  start = Sys.time()
  
  # i = 1
  
  if (island_name[i] == "ros") island_sector = "Muliava Sanctuary Unit"
  if (island_name[i] == "swa") island_sector = c("Swains Island Sanctuary Unit", "Swains Open")
  if (island_name[i] == "tau") island_sector = c("Ta'u Sanctuary Unit", "Tau Open")

  utm_i = utm %>% subset(Island_Code == island_name[i])
  
  dat_i = subset(dat, Label %in% island_sector)
  
  # determine northern or southern hemisphere
  if (median((dat_i@bbox[2,])) > 0) dat_i <- spTransform(dat_i,  CRS(paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')))
  if (median((dat_i@bbox[2,])) < 0) dat_i <- spTransform(dat_i,  CRS(paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')))

  plot(dat_i); axis(1); axis(2)
  
  dat_i = dat_i[c(names(dat_i) %in% c("Label"))]
  
  names(dat_i) = "Sector"
  
  # get names
  nam <- unique(dat_i$Sector)
  
  # create a data.frame
  nam_df <- data.frame(ID = 1:length(nam), nam = nam)
  
  # Place IDs
  dat_i$ID <- nam_df$ID[match(dat_i$Sector, nam_df$nam)]
  
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
    geom_text_repel(data = r_df_label, aes(x, y, label = nam), box.padding = 2)
  
  raster_and_table = list(raster, table)
  
  nmsas_name = gsub(" ", "_", nmsas[i])
  nmsas_name = gsub("/", "_", nmsas_name)
  nmsas_name = gsub("'", "", nmsas_name)
  
  save(raster_and_table, file = paste0("data/gis_sector/", island_name[i], ".RData"))
  
  end = Sys.time()
  
  time = end - start
  
  print(paste0(nmsas[i], "...done...took ", time, "..."))
  
}
