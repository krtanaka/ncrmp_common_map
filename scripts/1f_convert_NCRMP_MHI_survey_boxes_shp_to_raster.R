#######################################################
### Convert NCRMP MHI Survey Boxes shapefile raster ###
#######################################################

library(raster)
library(dplyr)
library(readr)
library(colorRamps)
library(ggrepel)

rm(list = ls())

spatial_resolution = 100 # spatial resolution in m

shp_path = "L:/ktanaka/GIS/survey_boxes/" # pc
shp_path = "N:/GIS/Projects/CommonMaps/survey_boxes/"

shp_list = list.files(path = shp_path, pattern = "\\.shp$", full.names = T)[1]; shp_list
dat <- shapefile(shp_list, verbose = T); plot(dat); degAxis(1); degAxis(2)

island_name <- as.data.frame(dat)
island_name = island_name$Island

dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84')); plot(dat); degAxis(1); degAxis(2)
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84"); plot(dat); degAxis(1); degAxis(2)

utm = read_csv('data/misc/ncrmp_utm_zones.csv')

island_name = unique(island_name)

for (i in 1:length(island_name)) {
  
  start = Sys.time()
  
  # i = 2
  
  utm_i = utm %>% subset(Island == island_name[i])
  
  dat_i = subset(dat, Island == island_name[i])
  
  # determine northern or southern hemisphere
  if (median((dat_i@bbox[2,])) > 0) dat_i <- spTransform(dat_i, CRS(paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +north')))
  if (median((dat_i@bbox[2,])) < 0) dat_i <- spTransform(dat_i, CRS(paste0('+proj=utm +zone=', utm_i$UTM_Zone, ' +datum=WGS84 +units=m +no_defs +south')))
  
  plot(dat_i); axis(1); axis(2)
  
  dat_i = dat_i[c(names(dat_i) %in% c("Survey_Box"))]
  
  names(dat_i) = "Survey_Box"
  
  # get names
  nam <- unique(dat_i$Survey_Box)
  
  # create a data.frame
  nam_df <- data.frame(ID = 1:length(nam), nam = nam)
  
  # Place IDs
  dat_i$ID <- nam_df$ID[match(dat_i$Survey_Box, nam_df$nam)]
  
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
    coord_equal()
  
  if (file.exists(paste0("data/gis_bathymetry/", island_name[i], ".RData"))) {
    
    load(paste0("data/gis_bathymetry/", island_name[i], ".RData"))
    raster = resample(raster, topo_i, method = "ngb")
    raster = readAll(raster)
    
  }
  
  raster_and_table = list(raster, table)
  
  save(raster_and_table, file = paste0("data/gis_survey_boxes/", utm_i$Island_Code, ".RData"))
  
  end = Sys.time()
  
  time = end - start
  
  cat(paste0(island_name[i], "...done...took ", time, " seconds...\n"))
  
}
