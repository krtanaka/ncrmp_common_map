###############################################
### generate stratified-random survey sites ###
###############################################

rm(list = ls())

# library(SimSurvey)
library(raster)
library(data.table)
library(ggplot2)
library(dplyr)
library(patchwork)
library(colorRamps)
library(readr)
library(ggrepel)
library(ggnewscale)
library(ggspatial)
library(ggthemes)
library(ggmap)

select = dplyr::select

utm = read_csv('data/misc/ncrmp_utm_zones.csv')

########################################################################
### do some parameter settings to simulate stratified random surveys ###
########################################################################

# n_sims = 100 # number of simulations
effort_level = c("low", "mid", "high")[2] # define sampling effort (low, mid, high)
min_sets = 1 # minimum number of sets per strat
max_sets = 50
trawl_dim = c(0.01, 0.0353) # 0.000353 sq.km (353 sq.m) from two 15-m diameter survey cylinders
resample_cells = F

##################################################################
### determine number of sites you want to deploy @ each island ###
##################################################################

load('data/misc/survey_effort_ncrmp_2000-2020.RData')
island_name_code = read_csv('data/misc/island_name_code.csv')
survey_effort = data.frame(Island = survey_effort$Island, Effort = survey_effort[[effort_level]])
survey_effort = merge(island_name_code, survey_effort); head(survey_effort); tail(survey_effort)

#################################
### Read in Island Boundaries ###
#################################
load('data/gis_island_boundaries/ncrmp_islands_shp.RData')
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

##################################
###  select islands & regions  ###
##################################

islands = c("gua", "rot", "sai", "tin", "agu"); region = "S.MARIAN"                           # South Mariana Islands
islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "pag", "sar"); region = "N.MARIAN"      # North Mariana Islands
islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                              # American Samoa
islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"                # Pacific Remote Island Areas
islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"    # Main Hawaiian Islands
islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"                 # Northern Hawaiian Islands

#######################################################################
### the last known site number from data management for each island ###
#######################################################################

site_num = read_csv("data/misc/V_NCRMP_MAX_SITE_NUM_DATA.csv") %>%
  mutate(MAX_SITE_NUM = sprintf("%04d", MAX_SITE_NUM),
         ISLANDCODE = tolower(ISLANDCODE)) %>% 
  select(ISLANDCODE, MAX_SITE_NUM)

set.seed(2024)

select = dplyr::select

ggmap::register_google("AIzaSyDpirvA5gB7bmbEbwB1Pk__6jiV4SXAEcY")

#################################################################
### Generate survey site tables & maps, check outputs/ folder ###
#################################################################

for (i in 1:length(islands)) {
  
  # i = 1
  
  # survey domain with sector & reef & hard_unknown & 3 depth bins
  load(paste0("data/survey_grid_ncrmp/survey_grid_", islands[i], ".RData"))#; plot(survey_grid_ncrmp)
  
  cat(paste0("generating survey site for ", islands[i], "...\n"))
  
  total_sample = survey_effort %>% subset(Island_Code == islands[i])
  
  if (dim(total_sample)[1] == 0){
    
    total_sample = 100
    
  } else {
    
    total_sample = total_sample$Effort*2
    
  }
  
  cat(paste0("target sampling effort = ", total_sample, "...\n"))
  
  n <- id <- division <- strat <- N <- strat_sets <- cell_sets <- NULL
  
  # cells <- data.table(rasterToPoints(survey_grid_ncrmp))
  cells <- data.table(terra::as.data.frame(survey_grid_ncrmp, xy = T, cells = T, na.rm = T))
  
  # add modeled trophic biomass variability, summarize by strata
  load(paste0("data/spc/modeled_piscivore_variability_", region, ".RData")) # modeled at original grid scale
  load(paste0("data/spc/modeled_planktivore_variability_", region, ".RData")) # modeled at original grid scale
  load(paste0("data/spc/modeled_secondary_variability_", region, ".RData")) # modeled at original grid scale
  load(paste0("data/spc/modeled_primary_variability_", region, ".RData")) # modeled at original grid scale
  load(paste0("data/spc/modeled_total_variability_", region, ".RData")) # modeled at original grid scale
  
  cells$sd_piscivore = predict(g_piscivore, cells)
  sd_piscivore = cells[,c("strat", "sd_piscivore")]
  sd_piscivore = sd_piscivore %>% group_by(strat) %>% summarise(sd_piscivore = mean(sd_piscivore, na.rm = T))
  
  cells$sd_planktivore = predict(g_planktivore, cells)
  sd_planktivore = cells[,c("strat", "sd_planktivore")]
  sd_planktivore = sd_planktivore %>% group_by(strat) %>% summarise(sd_planktivore = mean(sd_planktivore, na.rm = T))
  
  cells$sd_primary = predict(g_primary, cells); sd = cells[,c("strat", "sd_primary")]
  sd_primary = cells[,c("strat", "sd_primary")]
  sd_primary = sd_primary %>% group_by(strat) %>% summarise(sd_primary = mean(sd_primary, na.rm = T))
  
  cells$sd_secondary = predict(g_secondary, cells); sd = cells[,c("strat", "sd_secondary")]
  sd_secondary = cells[,c("strat", "sd_secondary")]
  sd_secondary = sd_secondary %>% group_by(strat) %>% summarise(sd_secondary = mean(sd_secondary, na.rm = T))
  
  cells$sd_total = predict(g_total, cells)
  sd_total = cells[,c("strat", "sd_total")]
  sd_total = sd_total %>% group_by(strat) %>% summarise(sd_total = mean(sd_total, na.rm = T))
  
  strat_det <- cells[, list(strat_cells = .N), by = "strat"]; strat_det
  strat_det$tow_area <- prod(trawl_dim); strat_det
  strat_det$cell_area <- prod(res(survey_grid_ncrmp)); strat_det
  strat_det$strat_area <- strat_det$strat_cells * prod(res(survey_grid_ncrmp)); strat_det
  
  strat_det = right_join(strat_det, sd_piscivore); strat_det
  strat_det = right_join(strat_det, sd_planktivore); strat_det
  strat_det = right_join(strat_det, sd_primary); strat_det
  strat_det = right_join(strat_det, sd_secondary); strat_det
  strat_det = right_join(strat_det, sd_total); strat_det
  
  strat_det$sd = rowMeans(strat_det[, 6:10]); strat_det
  
  ## allocate sampling units by area * sd
  strat_det$weight = abs(strat_det$strat_area * strat_det$sd); strat_det
  strat_det$strat_sets = round((total_sample * strat_det$weight) / sum(strat_det$weight), 0); strat_det
  
  ## allocate sampling units by area
  # strat_det$strat_sets = round((total_sample * strat_det$strat_area) / sum(strat_det$strat_area), 0); strat_det
  
  # make sure minimum number of sets per strat is not 0.
  strat_det$strat_sets[strat_det$strat_sets < min_sets] <- min_sets; strat_det
  # strat_det$strat_sets[strat_det$strat_sets > max_sets] <- max_sets; strat_det
  
  strat_table = strat_det %>% dplyr::select(strat, strat_sets); strat_table
  
  # add "strat" "strat_cells" "tow_area" ...
  strat_det = strat_det[, c("strat", "strat_cells", "tow_area", "cell_area", "strat_area", "strat_sets")]
  
  if (any(grepl(paste0(islands[i], "_itinerary"),list.files("outputs/sector_keys/")))) {
    
    load(paste0("outputs/sector_keys/", islands[i], "_itinerary.Rdata"))
    
    # tab$strat_sets_alt = tab$MAP_SITES %>% as.numeric()
    tab$strat_sets_alt = tab$FISH_MAX_SITES %>% as.numeric()
    
    tab$strat_sets_alt <- ifelse(tab$strat_sets_alt < 10, tab$strat_sets_alt * 3, tab$strat_sets_alt * 2)
    
    tab = tab[, c("strat", "strat_sets_alt")]
    
    strat_det = left_join(strat_det, tab)
    
    strat_det$strat_sets_adj = ifelse(strat_det$strat_sets > strat_det$strat_sets_alt, 
                                      strat_det$strat_sets, 
                                      strat_det$strat_sets_alt)
    
    strat_det$strat_sets_adj = ifelse(is.na(strat_det$strat_sets_adj), strat_det$strat_sets, strat_det$strat_sets_adj)
    
    strat_det = strat_det[, c("strat", "strat_cells", "tow_area", "cell_area", "strat_area", "strat_sets_adj")]
    
    colnames(strat_det)[6] = "strat_sets"
    
  }
  
  strat_det = strat_det %>% 
    mutate(strat_sets = ifelse(strat_sets > strat_cells, strat_cells, strat_sets))
  
  strat_set = strat_det %>% select(strat, strat_area, strat_sets) %>% 
    mutate(island = islands[i],
           region = region)
  
  keys = read_csv(paste0("outputs/tables/strata_keys_", region, "_", islands[i], ".csv"))
  
  strat_set = left_join(strat_set, keys) %>% 
    select(region, island, depth_bin, sector_id, reef_id, strat, strat_nam, strat_area, strat_sets)
  
  cat(paste0("saving strata set for ", region, " ", islands[i], " to CSV...\n"))
  readr::write_csv(strat_set, file = paste0("outputs/tables/strata_set_", region, "_", islands[i], ".csv"))
  
  cells <- merge(cells, strat_det, by = c("strat")) 
  
  utm_i = utm %>% subset(Island_Code == islands[i])
  
  utmcoor <- SpatialPoints(cbind(cells$x, cells$y), proj4string = CRS(paste0("+proj=utm +units=km +zone=", utm_i$UTM_Zone, " ", utm_i$Hemisphere)))
  longlatcoor <- spTransform(utmcoor,CRS("+proj=longlat"))
  cells$longitude <- coordinates(longlatcoor)[,1]
  cells$latitude <- coordinates(longlatcoor)[,2]
  
  # subset "cells" to create site locations
  sets <- cells[, .SD[sample(.N, size = unique(strat_sets), replace = resample_cells)], by = c("strat")]
  
  # remove sites that are closer than 100 m
  nearby_sites <- data.frame(longitude = sets$longitude, latitude = sets$latitude)#; plot(nearby_sites, pch = 20, col = 2, axes = F)
  
  library(geosphere)
  remove_close_points <- function(data, threshold = 100) {
    keep <- rep(TRUE, nrow(data))
    for (i in 1:(nrow(data) - 1)) {
      if (keep[i]) {
        dists <- distm(data[i, c("longitude", "latitude")], data[(i+1):nrow(data), c("longitude", "latitude")], fun = distHaversine)
        close_points <- which(dists < threshold)
        if (length(close_points) > 0) {
          keep[(i + close_points)] <- FALSE
        }
      }
    }
    data[keep, ]
  }
  
  nearby_sites <- remove_close_points(nearby_sites, 100)#; points(nearby_sites, pch = 20, col = 4)
  
  colnames(nearby_sites) = c("longitude", "latitude")
  nearby_sites$latitude = round(nearby_sites$latitude, 4)
  nearby_sites$longitude = round(nearby_sites$longitude, 4)
  
  sets$latitude = round(sets$latitude, 4)
  sets$longitude = round(sets$longitude, 4)
  
  cat(paste0("removing ", nrow(sets) - nrow(nearby_sites), " sites to maintain a minimum distance of 100 m between each site...\n"))
  
  sets = inner_join(sets, nearby_sites)
  
  if (islands[i] %in% unique(site_num$ISLANDCODE) ) {
    
    id = site_num %>% filter(ISLANDCODE == islands[i])
    id = id$MAX_SITE_NUM %>% as.numeric()
    id = seq(id,id + dim(sets)[1]-1,1)
    id = sprintf("s_%04d", id)
    id = gsub("s",  toupper(islands[i]), id)
    id = paste0(id, "A")
    
  }else{
    
    id <- seq(1,dim(sets)[1],1)
    id = sprintf("s_%04d", id)
    id = gsub("s",  islands[i], id)
    id = paste0(id, "A")
    
  }
  
  # count number of distinct sim*year*cell combinations
  sets[, `:=`(cell_sets, .N), by = c("cell")]
  sets$set <- seq(nrow(sets))
  sets = sets  %>% 
    mutate(id = id) %>% 
    dplyr::select(id, x, y, longitude, latitude, depth, strat)
  
  sets$depth_bin = ""
  sets$depth_bin = ifelse(sets$depth >= 0  & sets$depth <= 6, "SHAL", sets$depth_bin) 
  sets$depth_bin = ifelse(sets$depth > 6  & sets$depth <= 18, "MID", sets$depth_bin) 
  sets$depth_bin = ifelse(sets$depth > 18, "DEEP", sets$depth_bin) 
  
  sets$SITE_NO = substr(sets$id, 5, 8)
  
  colnames(sets)[1] = c("site_id")
  sets = sets[,c("site_id", "SITE_NO", "x", "y", "longitude", "latitude", "depth",  "depth_bin", "strat")]
  
  sets_i = sets
  
  key_i = read_csv(paste0("outputs/tables/strata_keys_", region, "_", islands[i], ".csv")) %>%
    mutate(depth_bin = stringr::str_replace(depth_bin, "MIDD", "MID"))
  
  sets_i = left_join(sets_i, key_i)
  
  colnames(sets_i) = toupper(colnames(sets_i))
  
  sets_i = sets_i[,c("SITE_ID", "SITE_NO", "X", "Y", "LONGITUDE", "LATITUDE", "DEPTH_BIN", "REEF_ID")]
  
  sets_i <- sets_i %>% 
    mutate_if(is.numeric, round, digits = 4) %>% 
    mutate_if(is.character, toupper)
  
  cat(paste0("\n\n\n... saving survey table for ", region, " ", islands[i], " to CSV ...\n\n\n"))
  readr::write_csv(sets_i, file = paste0("outputs/tables/survey_table_", region, "_", islands[i], ".csv"))
  
  # #########################################
  # ## Export set table as two columns pdf ##
  # #########################################
  # 
  # # Add additional row to sets with odd numbers so they can be evenly split
  # if(nrow(sets) %% 2 == 1){
  # 
  #   blankrow <- data.frame(matrix(ncol = 8, nrow = 1))
  #   colnames(blankrow) <- colnames(sets)
  #   sets <- rbind(sets, blankrow)
  # 
  # }
  # 
  # # Format and split sets to create two columns
  # sets$depth <- round(sets$depth, digits = 2)
  # sets_print <- select(sets, "id", "longitude","latitude","depth","strat","depth_bin")
  # 
  # sets1 <- data.frame(split(sets_print, factor(sort(rank(row.names(sets_print))%%2))))
  # sets1 <- sets1[,1:6]
  # colnames(sets1) <- colnames(sets_print)
  # sets2 <- anti_join(sets_print, sets1)
  # list = seq.int((nrow(sets2) + 1 ), nrow(sets))
  # 
  # # Print table
  # if(dim(sets1)[1] < 30) {
  # 
  #   page_height = 14.5 # Margins for smaller site lists i.e Rota
  #   page_width = 10.5 # Margins for smaller site lists i.e Rota
  # 
  # } else {
  # 
  #   page_height = 21.75 # Margins for larger site lists i.e. Guam
  #   page_width = 15.75 # Margins for larger site lists i.e. Guam
  # 
  # }
  # 
  # library(grid)
  # library(gridExtra)
  # pdf(paste0("outputs/tables/survey_table_", region, "_", islands[i], ".pdf"), height = page_height, width = page_width)
  # sets1 <- tableGrob(sets1)
  # sets2 <- tableGrob(sets2, rows = list)
  # grid.arrange(rectGrob(), rectGrob(), ncol = 2)
  # grid.arrange(sets1, sets2, nrow = 1, ncol = 2, newpage = F)
  # dev.off()
  
  bathymetry = cells %>% 
    ggplot(aes(x, y)) +
    geom_raster(aes(fill = depth)) + 
    # coord_fixed() +
    theme_map() + 
    scale_fill_viridis_c("Depth (m)", limits = c(0, 30), direction = -1) + 
    theme(panel.background = element_rect(fill = "gray10"),
          panel.grid = element_line(color = "gray15"),
          legend.background = element_rect(fill = "transparent"), 
          legend.text = element_text(color = "white"),           
          legend.title = element_text(color = "white"))
  
  variability = cells %>% 
    ggplot(aes(x, y)) +
    geom_raster(aes(fill = sd_total)) + 
    # coord_fixed() +
    theme_map() + 
    scale_fill_viridis_c("Var") + 
    theme(panel.background = element_rect(fill = "gray10"),
          panel.grid = element_line(color = "gray15"),
          legend.background = element_rect(fill = "transparent"), 
          legend.text = element_text(color = "white"),           
          legend.title = element_text(color = "white"))
  
  area = cells %>% 
    ggplot(aes(x, y)) +
    geom_raster(aes(fill = strat_area )) + 
    # coord_fixed() +
    theme_map() + 
    scale_fill_viridis_c("Area (km2)") + 
    theme(panel.background = element_rect(fill = "gray10"),
          panel.grid = element_line(color = "gray15"),
          legend.background = element_rect(fill = "transparent"), 
          legend.text = element_text(color = "white"),           
          legend.title = element_text(color = "white"))
  
  # png(paste0("outputs/maps/survey_layers_", islands[i], ".png"), height = 5, width = 15, res = 500, units = "in")
  # print(bathymetry + variability + area)
  # dev.off()
  
  isl_shp = island_name_code %>% subset(Island_Code == islands[i])
  
  #######################################
  ### Read Island shape and coastline ###
  #######################################
  ISL_this = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(isl_shp)),]
  if (islands[i] == "tut") ISL_this = ISL_bounds[which(ISL_bounds$ISLAND %in% c(toupper(isl_shp), "AUNUU")),]
  if (islands[i] == "ofu") ISL_this = ISL_bounds[which(ISL_bounds$ISLAND %in% c(toupper(isl_shp), "OLOSEGA")),]
  # ISL_this_utm = spTransform(ISL_this,CRS(paste0("+proj=utm +units=km +zone=", zone)))
  # ISL_this_sf = st_transform(st_as_sf(ISL_this), crs = paste0("+proj=utm +units=km +zone=", zone))
  
  ######################################
  ### Read Island 5km buffer sectors ###
  ######################################
  load(paste0('data/gis_5km_buffer/', islands[i], '.RData'))
  buffer = raster_and_table[[1]]
  buffer_name = raster_and_table[[2]]
  buffer <- data.table(rasterToPoints(buffer))
  utmcoor <- SpatialPoints(cbind(buffer$x, buffer$y), proj4string = CRS(paste0("+proj=utm +units=m +zone=", utm_i$UTM_Zone, " ", utm_i$Hemisphere)))
  longlatcoor <- spTransform(utmcoor,CRS("+proj=longlat"))
  buffer$longitude <- coordinates(longlatcoor)[,1]
  buffer$latitude <- coordinates(longlatcoor)[,2]
  colnames(buffer)[3] = "ID"
  buffer = merge(buffer, buffer_name)
  buffer = buffer %>%
    mutate(longitude = round(longitude, 3), 
           latitude = round(latitude, 3)) %>% 
    dplyr::select(longitude, latitude, nam) %>% 
    unique()
  colnames(buffer)[3] = "sector_nam"
  buffer_label = buffer %>% 
    group_by(sector_nam) %>% 
    summarise(longitude = quantile(longitude, 0.5), latitude = quantile(latitude, 0.5))
  
  ################################
  ### Read Island survey boxes ###
  ################################
  if (file.exists(paste0('data/gis_survey_boxes/', islands[i], '.RData'))) {
    
    load(paste0('data/gis_survey_boxes/', islands[i], '.RData'))
    boxes = raster_and_table[[1]]
    boxes_name = raster_and_table[[2]]
    boxes <- data.table(rasterToPoints(boxes)); colnames(boxes)[3] = "ID"
    utmcoor <- SpatialPoints(cbind(boxes$x, boxes$y), proj4string = CRS(paste0("+proj=utm +units=m +zone=", utm_i$UTM_Zone, " ", utm_i$Hemishpere)))
    longlatcoor <- spTransform(utmcoor,CRS("+proj=longlat"))
    boxes$longitude <- coordinates(longlatcoor)[,1]
    boxes$latitude <- coordinates(longlatcoor)[,2]
    boxes = merge(boxes, boxes_name)
    colnames(boxes)[6] = "boxes_nam"
    boxes_label = boxes %>% group_by(boxes_nam) %>% summarise(longitude = median(longitude), latitude = median(latitude))
    
    # Convex hulls for survey boxes
    ddply = plyr::ddply
    df <- boxes
    find_hull <- function(boxes) boxes[chull(boxes$latitude, boxes$longitude), ]
    boxes_hulls <- ddply(df, "boxes_nam", find_hull)
    
    # Split the data frame by ID
    split_data <- split(boxes_hulls, boxes_hulls$ID)
    
    # Function to create polygons
    create_polygon <- function(df) {
      coords <- df[, c("longitude", "latitude")]
      coords <- rbind(coords, coords[1, ])  # Close the polygon
      Polygons(list(Polygon(coords)), as.character(df$ID[1]))
    }
    
    # Apply the function to each group
    polygons_list <- lapply(split_data, create_polygon)
    
    # Combine into SpatialPolygons
    sp_polygons <- SpatialPolygons(polygons_list)
    
    # Create a data frame for the polygons
    polygons_df <- do.call(rbind, lapply(split_data, function(df) df[1, ]))
    row.names(polygons_df) <- as.character(polygons_df$ID)
    
    # Create the SpatialPolygonsDataFrame
    boxes_hulls_polygon <- SpatialPolygonsDataFrame(sp_polygons, polygons_df)
    
    Switch = T
    
  } else {
    
    Switch = F
    
  }
  
  total_area = unique(cells$cell_area)*dim(cells)[1]
  
  map_direction = c("NW", "NE", "SW", "SE")
  
  map_list = list()
  
  for (d in 1:length(map_direction)) {
    
    # d = 2
    
    map_i = map_direction[d]
    
    space = 0.01
    
    if (map_direction[d] == "NW") ext = c(min(sets$longitude, na.rm = T) - space, median(sets$longitude, na.rm = T) + space, median(sets$latitude, na.rm = T) - space, max(sets$latitude, na.rm = T) + space)
    if (map_direction[d] == "NE") ext = c(median(sets$longitude, na.rm = T) - space, max(sets$longitude, na.rm = T) + space, median(sets$latitude, na.rm = T) - space, max(sets$latitude, na.rm = T) + space)
    if (map_direction[d] == "SW") ext = c(min(sets$longitude, na.rm = T) - space, median(sets$longitude, na.rm = T) + space, min(sets$latitude, na.rm = T) - space, median(sets$latitude, na.rm = T) + space)
    if (map_direction[d] == "SE") ext = c(median(sets$longitude, na.rm = T) - space, max(sets$longitude, na.rm = T) + space, min(sets$latitude, na.rm = T) - space, median(sets$latitude, na.rm = T) + space)
    
    sets_i = sets %>% subset(longitude > ext[1] & longitude < ext[2] & latitude > ext[3] & latitude < ext[4])
    
    # # clip survey boxes and coast lines
    # tryCatch({
    #   
    #   ISL_this_i <- crop(ISL_this, extent(ext))
    #   boxes_hulls_polygon_i <- crop(boxes_hulls_polygon, extent(ext))
    #   boxes_hulls_polygon_i_nam = boxes %>% 
    #     subset(longitude > ext[1] & longitude < ext[2] & latitude > ext[3] & latitude < ext[4]) %>% 
    #     group_by(boxes_nam) %>% 
    #     summarise(longitude = median(longitude), latitude = median(latitude))
    #   
    # }, error = function(e){
    #   
    #   print("No land shp available in this extent. Use full extent instead")
    #   ISL_this_i <- crop(ISL_this, extent(ISL_this))
    #   boxes_hulls_polygon_i <- crop(boxes_hulls_polygon, extent(boxes_hulls_polygon))
    #   boxes_hulls_polygon_i_nam = boxes %>% 
    #     group_by(boxes_nam) %>% 
    #     summarise(longitude = median(longitude), latitude = median(latitude))
    # })
    
    # use ggmap
    tryCatch({
      
      map = get_map(location = c(mean(sets_i$longitude, na.rm = T), mean(sets_i$latitude, na.rm = T)),
                    maptype = "satellite",
                    # zoom = utm_i$Satellite,
                    zoom = 12,
                    # color = "bw",
                    force = T)
      
    }, error = function(e){
      
      print("No sets available in this extent. Use full extent instead")
      map <- get_map(location = c(mean(sets$longitude, na.rm = T), mean(sets$latitude, na.rm = T)),
                     maptype = "satellite",
                     # zoom = utm_i$Satellite,
                     # color = "bw",
                     force = T)
    })
    
    
    # remove sector label outside of extent
    
    if (map_direction[d] == "NW") {
      
      buffer_label_i = buffer %>% 
        subset(longitude > ext[1] & longitude < ext[2] & latitude > ext[3] & latitude < ext[4]) %>% 
        group_by(sector_nam) %>% 
        summarise(longitude = quantile(longitude, 0.4), 
                  latitude = quantile(latitude, 0.6))
    }
    
    if (map_direction[d] == "NE") {
      
      buffer_label_i = buffer %>% 
        subset(longitude > ext[1] & longitude < ext[2] & latitude > ext[3] & latitude < ext[4]) %>% 
        group_by(sector_nam) %>% 
        summarise(longitude = quantile(longitude, 0.6), 
                  latitude = quantile(latitude, 0.6))
    }
    
    if (map_direction[d] == "SW") {
      
      buffer_label_i = buffer %>% 
        subset(longitude > ext[1] & longitude < ext[2] & latitude > ext[3] & latitude < ext[4]) %>% 
        group_by(sector_nam) %>% 
        summarise(longitude = quantile(longitude, 0.4), 
                  latitude = quantile(latitude, 0.4))
    }
    
    if (map_direction[d] == "SE") {
      
      buffer_label_i = buffer %>% 
        subset(longitude > ext[1] & longitude < ext[2] & latitude > ext[3] & latitude < ext[4]) %>% 
        group_by(sector_nam) %>% 
        summarise(longitude = quantile(longitude, 0.6), 
                  latitude = quantile(latitude, 0.4))
    }
    
    map_i = 
      
      # ggplot() +
      ggmap(map) +
      
      # add land and coastline
      # geom_polygon(data = ISL_this_i, aes(long, lat, group = group), fill = "gray50", color = NA, alpha = 0.9) + 
      # geom_path(data = ISL_this_i, aes(long, lat, group = group), inherit.aes = F, size = 0.01, color = "gray10") +
      
      # add survey boxes
      # {if(Switch) geom_polygon(data = boxes_hulls_polygon_i, aes(long, lat, group = group), inherit.aes = F, size = 1, alpha = 0.2)} +
      {if(Switch) geom_path(data = boxes_hulls_polygon_i, aes(long, lat, group = group), inherit.aes = F, size = 0.5, color = "gray10")} +
      {if(Switch) geom_text_repel(data = boxes_hulls_polygon_i_nam, aes(longitude, latitude, label = boxes_nam), color = "black", size = 15)} + 
      
      # display if there is more than 1 island sector
      {if (length(unique(buffer$sector_nam)) > 1) {
        
        geom_raster(data = buffer %>% mutate(across(c(latitude, longitude), round, digits = 3)) %>% distinct(), 
                    aes(longitude, latitude, fill = sector_nam), show.legend = F, alpha = 0.2)}
        
      } + 
      
      {if (length(unique(buffer$sector_nam)) > 1) {
        
        geom_label_repel(data = buffer_label_i, 
                         aes(longitude, latitude, label = sector_nam, fill = sector_nam, fontface = 'bold'), 
                         alpha = 0.8, color = "white", size = 10, max.overlaps = Inf, show.legend = F)}
        
      } + 
      
      scale_fill_discrete("") +
      scale_color_discrete("") +
      
      new_scale_color() +
      new_scale_fill() + 
      
      geom_spatial_point(data = sets_i, aes(longitude, latitude, shape = depth_bin, fill = depth_bin), size = 5, crs = 4326) +
      # geom_point(data = sets_i, aes(longitude, latitude, shape = depth_bin, fill = depth_bin), size = 5) + 
      
      scale_fill_manual(name = "", values = c("red", "goldenrod1", "green3"), na.translate = F) + 
      scale_shape_manual(name = "", values = c(24, 22, 21), na.translate = F) +
      annotation_scale(location = "br", width_hint = 0.2, text_col = "gray20", bar_cols = "white", size = 5) +  # new_scale_color() +
      
      geom_label_repel(data = sets_i, 
                       aes(longitude, latitude, label = SITE_NO),
                       size = 5,
                       label.size = NA, 
                       alpha = 0.75, 
                       fontface = 'bold', 
                       color = 'black',
                       max.overlaps = Inf,
                       segment.color = "gray20",
                       box.padding = unit(0.8, "lines"),
                       point.padding = unit(0.3, "lines")) +
      
      # ggtitle(paste0(map_direction[d])) +
      theme(plot.title = element_text(size = 30, face = "bold")) + 
      
      coord_sf(crs = 4326) + 
      
      scale_x_continuous(sec.axis = dup_axis(), "", limits = ext[1:2], expand = c(-0.001, 0)) +
      scale_y_continuous(sec.axis = dup_axis(), "", limits = ext[3:4], expand = c(-0.001, 0))
    
    map_full =     
      ggplot() +
      geom_polygon(data = ISL_this, aes(long, lat, group = group), fill = "darkgrey", color = NA, alpha = 0.9) +
      geom_rect(aes(xmin =  ext[1], xmax =  ext[2], ymin =  ext[3], ymax =  ext[4]), color = "red", fill = NA, size = 2) +
      coord_sf(crs = "+proj=lonlat +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") + 
      theme_inset()
    
    if(diff(ext[1:2]) < diff(ext[3:4])) pdf(paste0("outputs/maps/survey_map_", region, "_", islands[i], "_", map_direction[d], ".pdf"), height = 22, width = 17)
    if(diff(ext[1:2]) > diff(ext[3:4])) pdf(paste0("outputs/maps/survey_map_", region, "_", islands[i], "_", map_direction[d], ".pdf"), height = 17, width = 22)
    
    # print(map_full / map_i + plot_layout(heights = c(1, 4)))
    print(map_i + inset_element(map_full, left = 0, bottom = 0.9, right = 0.2, top = 1, align_to = 'full'))
    
    dev.off()
    
    map_list[[length(map_list)+1]] = map_i
    
  }
  
  # Get map
  ext = c(min(sets$longitude, na.rm = T) - 0.001, max(sets$longitude, na.rm = T) + 0.001, min(sets$latitude, na.rm = T) - 0.001, max(sets$latitude, na.rm = T) + 0.001)
  map <- get_map(location = c(left = ext[1], bottom = ext[3], right = ext[2], top = ext[4]), maptype = 'satellite')
  map <- get_map(location = c(mean(sets$longitude, na.rm = T),
                              mean(sets$latitude, na.rm = T)),
                 # zoom = utm_i$Satellite,
                 maptype = 'satellite')
  
  whole_map = 
    
    # ggplot() +
    ggmap(map) +
    
    geom_path(data = ISL_this, aes(long, lat, group = group), inherit.aes = F, size = 0.01, color = "gray10") + # coastline
    geom_polygon(data = ISL_this, aes(long, lat, group = group), fill = "gray50", color = NA, alpha = 0.9) + # land shapefile

    geom_raster(data = cells %>% mutate(across(c(latitude, longitude), round, digits = 3)), aes(longitude, latitude, fill = factor(strat)), alpha = 0.8) + # stratum
    
    scale_fill_discrete("Strata") + 
    scale_color_discrete("Strata") + 
    
    new_scale_color() +
    new_scale_fill() +
    
    {if ( length(unique(buffer$sector_nam)) > 1) {
      
      geom_raster(data = buffer %>% mutate(across(c(latitude, longitude), round, digits = 3)), 
                  aes(longitude, latitude, fill = sector_nam), alpha = 0.2, show.legend = F)}
      
    } + 
    
    # island sectors
    {if ( length(unique(buffer$sector_nam)) > 1) {
      
      geom_label_repel(data = buffer_label, aes(longitude, latitude, label = sector_nam, fill = sector_nam, fontface = 'bold'), color = "white", max.overlaps = Inf, show.legend = F)}
      
    } +
    
    scale_fill_discrete("") + 
    scale_color_discrete("") + 
    
    new_scale_color() +
    new_scale_fill() +
    
    # geom_tile(data = boxes, aes(longitude, latitude, fill = boxes_nam), width = 0.001, height = 0.001, alpha = 0.2, show.legend = F) + # survey boxes fill
    {if(Switch) geom_polygon(data = boxes_hulls, aes(longitude, latitude, fill = boxes_nam, color = boxes_nam), alpha = 0.01, size = 1, show.legend = F)} + # survey boxes hull
    {if(Switch) geom_text_repel(data = boxes_label, aes(longitude, latitude, label = boxes_nam, color = boxes_nam, fontface = 'bold'), max.overlaps = Inf, show.legend = F)} +
    {if(Switch) scale_fill_discrete()} + 
    {if(Switch) scale_color_discrete()} + 
    {if(Switch) new_scale_color()} +
    {if(Switch) new_scale_fill()} +
    
    # geom_point(data = sets, aes(longitude, latitude, shape = depth_bin, color = depth_bin)) +
    # geom_spatial_point(data = sets, aes(longitude, latitude, shape = depth_bin, fill = depth_bin), size = 3, crs = 4326) + 
    # scale_fill_manual(name = "Depth", values = c("red", "goldenrod1", "green3"), na.translate = F) + # in geom_spatial_point make size = 9 ONLY for Guam
    # scale_shape_manual(name = "Depth", values = c(24, 22, 21), na.translate = F) +
    annotation_scale(location = "br", width_hint = 0.2, text_col = "gray20", bar_cols = "gray20", size = 5) +  # new_scale_color() +
    # new_scale_fill() +      
    
    # geom_label_repel(data = sets,
    #                  aes(longitude, latitude, label = id),
    #                  size = 2,
    #                  label.size = NA,
    #                  alpha = 0.75,
    #                  fontface = 'bold',
    #                  color = 'black',
    #                  max.overlaps = Inf,
    #                  segment.size = 0.2,
  #                  direction = "both",
  #                  # nudge_y = 0.005,
  #                  # nudge_x = 0.005,
  #                  box.padding = unit(0.8, "lines"),
  #                  point.padding = unit(0.3, "lines")) +
  
  # coord_fixed() +
  # coord_map() + 
  coord_sf(crs = 4326) + 
    
    scale_x_continuous(sec.axis = dup_axis(), "", limits = range(pretty(buffer$longitude) + c(-0.1, 0.1))) +
    scale_y_continuous(sec.axis = dup_axis(), "", limits = range(pretty(buffer$latitude) + c(-0.1, 0.1))) +
    
    theme(legend.position = "bottom",
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10)) +   
    labs(
      title = "",
      subtitle = paste0(paste0("Island = ", toupper(as.character(isl_shp[1])),"\n",
                               # "Number of strata = ", length(unique(cells$strat)), "\n",
                               # "Target survey effort = ", total_sample, " sites \n",
                               "Target effort = ", sum(strat_det$strat_sets), " sites")))
  
  if(diff(ext[1:2]) < diff(ext[3:4])) pdf(paste0("outputs/maps/survey_map_", region, "_", islands[i], ".pdf"), height = 22, width = 17)
  if(diff(ext[1:2]) > diff(ext[3:4])) pdf(paste0("outputs/maps/survey_map_", region, "_", islands[i], ".pdf"), height = 17, width = 22)
  print(whole_map)
  dev.off()
  
  library(pdftools)
  pdf_combine(c(paste0("outputs/maps/survey_map_", region, "_", islands[i], ".pdf"), 
                paste0("outputs/maps/survey_map_", region, "_", islands[i], "_NE.pdf"),
                paste0("outputs/maps/survey_map_", region, "_", islands[i], "_NW.pdf"),
                paste0("outputs/maps/survey_map_", region, "_", islands[i], "_SE.pdf"),
                paste0("outputs/maps/survey_map_", region, "_", islands[i], "_SW.pdf")),
              # paste0("outputs/tables/survey_table_", region, "_", islands[i], ".pdf")),
              output = paste0("outputs/maps/survey_maps_", region, "_", islands[i], ".pdf"))
  
  file.remove(paste0("outputs/maps/survey_map_", region, "_", islands[i], ".pdf"))
  file.remove(paste0("outputs/maps/survey_map_", region, "_", islands[i], "_NE.pdf"))
  file.remove(paste0("outputs/maps/survey_map_", region, "_", islands[i], "_NW.pdf"))
  file.remove(paste0("outputs/maps/survey_map_", region, "_", islands[i], "_SE.pdf"))
  file.remove(paste0("outputs/maps/survey_map_", region, "_", islands[i], "_SW.pdf"))
  file.remove(paste0("outputs/tables/survey_table_", region, "_", islands[i], ".pdf"))
  
}
