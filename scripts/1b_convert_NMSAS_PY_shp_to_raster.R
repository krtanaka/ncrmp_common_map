############################################
### Convert NMSAS_PY shapefile to raster ###
############################################

library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(readr)
library(colorRamps)

rm(list = ls())

spatial_resolution = 100 # spatial resolution in m

shp_path = "L:/ktanaka/GIS" # pc

shp_list = list.files(path = paste0(shp_path, "/sector/"), pattern = "\\.shp$", full.names = T); shp_list

island_name = tolower(substr(shp_list[10], 23, 30)); island_name

dat <- shapefile(shp_list[10], verbose = T); plot(dat); degAxis(1); degAxis(2)
nmsas <- as.data.frame(dat)
nmsas = nmsas$Label
nmsas = nmsas[c(1, 2, 5)]; nmsas # take out Tutulia sectors

dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84')); plot(dat); degAxis(1); degAxis(2)
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84"); plot(dat); degAxis(1); degAxis(2)

utm = read_csv('data/misc/ncrmp_utm_zones.csv')

for (i in 1:length(nmsas)) {
  
  start = Sys.time()
  
  # i = 1

  if (nmsas[i] == "Muliava Sanctuary Unit") island_name = "ros"
  if (nmsas[i] == "Swains Island Sanctuary Unit") island_name = "swa"
  if (nmsas[i] == "Fagalua/Fogama'a Sanctuary Unit") island_name = "tut"
  if (nmsas[i] == "Fagatele Bay Sanctuary Unit") island_name = "tut"
  if (nmsas[i] == "Ta'u Sanctuary Unit") island_name = "tau"
  if (nmsas[i] == "Aunu'u Sanctuary Unit B") island_name = "tut"
  if (nmsas[i] == "Aunu'u Sanctuary Unit A") island_name = "tut"
  
  utm_i = utm %>% subset(Island_Code == island_name)
  
  dat_i = subset(dat, Label == nmsas[i])

  dat_i <- spTransform(dat_i, CRS(paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs'))); plot(dat_i); axis(1); axis(2)
  
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
    geom_text_repel(data = r_df_label, aes(x, y, label = nam)) + 
    coord_equal() + 
    theme_light()
  
  raster_and_table = list(raster, table)
  
  nmsas_name = gsub(" ", "_", nmsas[i])
  nmsas_name = gsub("/", "_", nmsas_name)
  nmsas_name = gsub("'", "", nmsas_name)
  
  # save(raster_and_table, file = paste0("data/gis_sector/", nmsas_name, "_NMSAS.RData"))
  save(raster_and_table, file = paste0("data/gis_sector/", island_name, ".RData"))
  
  end = Sys.time()
  
  time = end - start
  
  print(paste0(nmsas[i], "...done...took ", time, "..."))
  
}