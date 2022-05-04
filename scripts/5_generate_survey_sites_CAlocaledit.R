###############################################
### generate stratified-random survey sites ###
###############################################
rm(list = ls())

library(SimSurvey)
library(raster)
library(data.table)
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggdark)
library(colorRamps)
library(readr)
library(ggrepel)
library(ggnewscale)
library(ggspatial)
library(sf)
library(geosphere)
library(grid)
library(gridExtra)
library(sp)


utm = read_csv('data/misc/ncrmp_utm_zones.csv')
utm$Island_Code = toupper(utm$Island_Code)


##################################
###  select islands & regions  ###
##################################

islands = c("GUA", "ROT", "SAI", "TIN", "AGU"); region = "S.MARIAN"                           # South Mariana Islands
#islands = c("AGR", "ALA", "ASC","GUG", "FDP", "MAU", "PAG", "SAR"); region = "N.MARIAN"      # North Mariana Islands
#islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                             # American Samoa
# islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"              # Pacific Remote Island Areas
# islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"  # Main Hawaiian Islands
# islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"               # Northern Hawaiian Islands


########################################################################
### do some parameter settings to simulate stratified random surveys ###
########################################################################

# n_sims = 100 # number of simulations
effort_level = c("low", "mid", "high")[3] # define sampling effort (low, mid, high)
# min_sets = 10 # minimum number of sets per strat. For benthic - set up min/max sets for each island 
# max_sets = 20 # maximum number of sets per strat 
trawl_dim = c(0.01, 0.0353) # 0.000353 sq.km (353 sq.m) from two 15-m diameter survey cylinders
resample_cells = F


##################################################################
### determine number of sites you want to deploy @ each island ###
##################################################################

load('data/misc/survey_effort_ncrmp_2000-2020.RData')
island_name_code = read_csv('data/misc/island_name_code.csv')
island_name_code$Island_Code = toupper(island_name_code$Island_Code)
survey_effort = data.frame(Island = survey_effort$Island, Effort = survey_effort[[effort_level]])
survey_effort = merge(island_name_code, survey_effort); head(survey_effort); tail(survey_effort)


#################################
### Read in Island Boundaries ###
#################################
load('data/gis_island_boundaries/ncrmp_islands_shp.RData')
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


#################################################################
### Generate survey site tables & maps, check outputs/ folder ###
#################################################################

#for (i in 1:length(islands)) {
  
  i = 2
  
  # survey domain with sector & reef & hard_unknown & 3 depth bins
  load(paste0("data/survey_grid_ncrmp/survey_grid_", islands[i], ".RData")) 
  
  total_sample = survey_effort %>% subset(Island_Code == islands[i])
  
  if (dim(total_sample)[1] == 0){
    
    total_sample = 100
    
  }else {
    
    total_sample = total_sample$Effort*3
    
  }
  
  n <- id <- division <- strat <- N <- strat_sets <- cell_sets <- NULL
  
  cells <- data.table(rasterToPoints(survey_grid_ncrmp))
  
  
  # Remove sites > 80 feet (24.38 m) deep and sites < 3 feet (0.91 m)
  cells <- cells %>% filter(depth < 24.4) %>% filter(depth > 0.95) 
  
  
  # add modeled trophic biomass variability, summarize by strata
  load(paste0("data/rea/modeled_survey_variability_", region, ".RData")) # modeled at original grid scale
  cells$sd = predict(g, cells); sd = cells[,c("strat", "sd")]; sd = sd %>% group_by(strat) %>% summarise(sd = mean(sd, na.rm = T))
  
  strat_det <- cells[, list(strat_cells = .N), by = "strat"]; strat_det
  strat_det$tow_area <- prod(trawl_dim); strat_det
  strat_det$cell_area <- prod(res(survey_grid_ncrmp)); strat_det
  strat_det$strat_area <- strat_det$strat_cells * prod(res(survey_grid_ncrmp)); strat_det
  strat_det = right_join(strat_det, sd); strat_det
  
  # allocate sampling units by area * sd
  strat_det$weight = abs(strat_det$strat_area * strat_det$sd); strat_det
  strat_det$strat_sets = round((total_sample * strat_det$weight) / sum(strat_det$weight), 0); strat_det 
  
  
  ## GUAM ONLY: choose min sets per strat based on the strat
  # # NOTE: 100m resolution (comes out to 102m in script 2a) is used for Guam 2022
  # # Available cells within MPAs (need to map 4 sites per depth):
  # # Piti Bomb = 6 shallow, 14 mid, 21 deep
  # # Tumon Bay = 5 shallow, 13 mid, 20 deep
  # # Pati Point = 18 shallow, 11, 15
  # # Achang = 12 mid, 4 shallow, 19 deep
  # # Strats 2 (Guam Harbor), 9 (east side mid depth, has 4 cells), 10 (Guam Harbor) have cells < 12
  # # No strat 17 = removed because it was Sasa Bay?
  # 
  # # Remove Guam Harbor strats
  #  cells <- filter(cells, strat != 2 & strat != 10)
  #  strat_det <- filter(strat_det, strat != 2 & strat != 10); strat_det
  # 
  # # Isolate strats within MPAs
  #  strat_det_mpa <- filter(strat_det, strat == 6 | strat == 14 | strat == 21 | strat == 5 | strat == 13 | strat == 20 |
  #                          strat == 18 | strat == 11 | strat == 3 | strat == 12 | strat == 4 | strat == 19) ; strat_det_mpa
  # 
  # # Set strat sets manually
  #  strat_det$strat_sets[strat_det$strat_sets < 5 & strat_det$strat == 9] <- 4; strat_det # sector with smallest available cells
  #  strat_det$strat_sets[strat_det$strat %in% strat_det_mpa$strat] <- 4; strat_det # set strat sets for MPAs
  #  strat_det$strat_sets[strat_det$strat_sets < 4] <- 8; strat_det
  #  strat_det$strat_sets[strat_det$strat_sets > 12] <- 12; strat_det # sectors with most available cells
                             
  
  ## SAIPAN ONLY: lower max sets so it does exceed 80 sites when adding east side samples later on
  # # Need to plot 21 deep, 33 mid, 18 shallow = 72 total
  # strat_det$strat_sets <- ifelse(strat_det$strat == 1, 20, 18); strat_det
  
  
  ## ASUNCION ONLY: Set min/max sets by strata 
  # # Need to plot minimum 9 shallow, 30 mid, 9 deep = 48 total
  # strat_det$strat_sets <- ifelse(strat_det$strat == 2, 30, 15); strat_det

  
  ## ROTA ONLY: Set min/max sets by strata
  # # Need to plot minimum 27 shallow, 12 mid, 9 deep = 48 total
  # # FYI - cell size for 2022 maps is 100m
  # strat_det$strat_sets <- ifelse(strat_det$strat == 1, 15, 6); strat_det

  
  ## PAGAN ONLY: make sure minimum number of sets per strat is not 0 or 1
  # # Need minimum 21 deep, 30 mid, and 21 deep mapped = 72 total
  # strat_det$strat_sets <- ifelse(strat_det$strat == 2, 32, 23); strat_det

  
  ## TINIAN ONLY: make sure minimum number of sets per strat is not 0 or 1
  # # Need minimum 21 deep, 30 mid, and 21 deep mapped = 72 total
  # cells <- cells[cells$y > 1649] # remove cluster of sites south of Tinian
  # 
  # strat_det$strat_sets <- case_when(
  #                         strat_det$strat == 1 ~ 21,
  #                         strat_det$strat == 2 ~ 29,
  #                         strat_det$strat == 3 ~ 20)
  # strat_det
  
  
  ## MAUG ONLY: Set min/max sets by strata 
  # # Need to map minimum 15 shallow, 36 mid, 21 deep = 72 total
  # # Maug cells are set to 70m (using script 2a) for 2022 benthic map
  # strat_det$strat_sets <- case_when(
  #                         strat_det$strat == 1 ~ 17,
  #                         strat_det$strat == 2 ~ 37,
  #                         strat_det$strat == 3 ~ 22)
  # strat_det
   
  
  ## Add "strat" "strat_cells" "tow_area" ...
  strat_det = strat_det[,c("strat", "strat_cells", "tow_area", "cell_area", "strat_area", "strat_sets")]
  cells <- merge(cells, strat_det, by = c("strat")) 
  
  utm_i = utm %>% subset(Island_Code == islands[i])
  
  utmcoor <- SpatialPoints(cbind(cells$x, cells$y), proj4string = CRS(paste0("+proj=utm +units=km +zone=", utm_i$UTM_Zone)))
  longlatcoor <- spTransform(utmcoor,CRS("+proj=longlat"))
  cells$longitude <- coordinates(longlatcoor)[,1]
  cells$latitude <- coordinates(longlatcoor)[,2]
  
  # Subset "cells" to create site locations
  sets <- cells[, .SD[sample(.N, strat_sets, replace = resample_cells)], 
                by = c("strat")] 
  
  
  ## GUAM ONLY: Add several more sites to the NW region of GUA_EAST_OPEN
  # cells_NE <- cells[cells$strat == 16 | cells$strat == 9 | cells$strat == 23]; head(cells_NE) # Subset
  # cells_NE <- cells_NE[cells_NE$latitude > 13.53] ; head(cells_NE)
  # 
  # cells_NE$strat_sets <- case_when(cells_NE$strat == 16 ~ 3,
  #                                  cells_NE$strat == 9 ~ 3,
  #                                  cells_NE$strat == 23 ~ 3)  # Recalculate strat_det
  # 
  # sets_NE <- cells_NE[, .SD[sample(.N, strat_sets, replace = T)],
  #                                            by = c("strat")]
  # 
  # sets_full <- rbind(sets, sets_NE) # Merge base sets with add-on sets
  # sets <- distinct(sets_full)
  

  ## SAIPAN ONLY: Make sure there is adequate site allocation in the north and southeast
  # # Across entire island, need to plot 21 deep, 33 mid, 18 shallow = 72 total
  #
  # cells_NE <- cells[cells$longitude > 145.75 & cells$latitude > 15.16] ; head(cells_NE)
  #
  # cells_NE$strat_sets <- ifelse(cells_NE$strat == 2, 6, 6)
  # sets_NE <- cells_NE[, .SD[sample(.N, strat_sets, replace = T)],
  #                         by = c("strat")]
  # sets_NE <- distinct(sets_NE) # shouldn't be necessary unless we have to switch replace = T when there are limited available cells
  # 
  # cells_SE <- cells[cells$longitude > 145.7 & cells$latitude < 15.16] ; head(cells_SE)
  # 
  # cells_SE$strat_sets <- ifelse(cells_SE$strat == 2, 6, 6)
  # sets_SE <- cells_SE[, .SD[sample(.N, strat_sets, replace = T)],
  #                   by = c("strat")]
  # sets_SE <- distinct(sets_SE)
  # 
  # cells_NW <- cells[cells$longitude > 145.7 & cells$latitude > 15.25]; head(cells_NW)
  #
  # cells_NW$strat_sets <- ifelse(cells_NW$strat == 2, 5, 5)
  # sets_NW <- cells_NW[, .SD[sample(.N, strat_sets, replace = T)], # strat 1 has n = 0 so make replace = T and remove duplicates next
  #                      by = c("strat")]
  # sets_NW <- distinct(sets_NW)
  # 
  # # Merge the sets baseline dataframe to the regional sets dataframes
  # sets_full <- rbind(sets, sets_NE, sets_NW, sets_SE) # Merge base sets with add-on sets
  # sets <- distinct(sets_full)
  # sets_det <- sets %>% group_by(strat) %>% summarise(n_sites = n()); sets_det
  
  
  ## TINIAN ONLY: Add sites to the eastern tip
  # # Need minimum 21 shallow, 30 mid, and 21 deep mapped = 72 total
  # cells_E <- cells[cells$longitude > 145.65 & cells$latitude < 15.03 & cells$latitude > 14.98]
  # cells_E$strat_sets <- ifelse(cells_E$strat_sets == 1, 3, 3) # no shallow sites present
  # 
  # sets_E <- cells_E[, .SD[sample(.N, strat_sets, replace = T)], by = c("strat")]
  # sets_E <- distinct(sets_E)
  # 
  # sets_full <- rbind(sets, sets_E) # Merge base sets with add-on set
  # sets <- distinct(sets_full)

   
  ## ASUNCION ONLY: Select all available shallow strata on the east side of the island
  # # Need to plot at least 9 shallow, 30 mid, 9 deep = 48 total
  #  cells_E <- cells[cells$longitude > 145.4] ; head(cells_E)
  #  sets_E <- filter(cells_E, strat == 1)
  # 
  # # Merge the sets baseline dataframe to the regional sets dataframe
  #  sets_full <- rbind(sets, sets_E) 
  #  sets <- distinct(sets_full) # remove sites in the same cell

  
  ## ROTA ONLY: Make sure there is adequate sampling on east side
  # # Min sites needed for entire island: 48 sites = 27 shallow, 12 mid, 9 deep
  # strat_det$strat_sets <- ifelse(strat_det$strat == 2, 8, 6); strat_det # max number of shallow sites in a regional subset = 4
  # 
  # cells_SE <- cells[cells$latitude < 14.133 & cells$longitude > 145.12]; head(cells_SE) # 4 shallow sites
  # sets_SE <- cells_SE[, .SD[sample(.N, strat_sets, replace = T)], by = c("strat")]
  # sets_SE <- distinct(sets_SE) # Adds approximately 4 shallow, 5 mid, 3 deep
  # 
  # cells_NE <- cells[cells$latitude < 14.1965 & cells$longitude > 145.23]; head(cells_NE) # 2 shallow sites
  # sets_NE <- cells_NE[, .SD[sample(.N, strat_sets, replace = T)], by = c("strat")]
  # sets_NE <- distinct(sets_NE) # Adds approximately 2 shallow, 9 mid, 5 deep
  # 
  # cells_MID <- cells[cells$longitude < 145.278 & cells$longitude > 145.22 & cells$latitude > 14.196]; head(cells_MID)
  # sets_MID <- cells_MID[, .SD[sample(.N, 2, replace = T)], by = c("strat")] # only want a small number of sites (no shallow present)
  # sets_MID <- distinct(sets_MID) # Adds approximately 1 shallow, 2 mid, 2 deep
  # 
  # # Merge the sets baseline dataframe to the regional sets dataframe
  # sets_full <- rbind(sets, sets_NE, sets_SE, sets_MID) # Merge base sets with add-on sets
  # sets <- distinct(sets_full)
  # sets_det <- sets %>% group_by(strat) %>% summarise(n_sites = n()); sets_det

  
  ## Create site names
  #  if (islands[isl] == "gua") topo = raster("L:/ktanaka/GIS/bathymetry/gua_nthmp_dem_10m_mosaic.tif") # Guam
  
  if(region == "S.MARIAN") {    
    
    start_code_smar = c(2649,829,1819,824,0) # Specific to 2022 surveys, based on previously used fish codes
    id <- seq(1,dim(sets)[1],1) + start_code_smar[i]
    #id = sprintf("s_%04d", id) # Add three-leter code to the site name
    sets$id = gsub("s",  islands[i], id)
    
  } else {
    
    start_code_nmar = c(0000,0000,0579,0000,0000,1199,1249,0000)
    id <- seq(1,dim(sets)[1],1) + start_code_nmar[i]
    #id = sprintf("s_%04d", id)
    sets$id = gsub("s",  islands[i], id)
    
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
  
  readr::write_csv(sets, path = paste0("outputs/table/survey_table_", region, "_", islands[i], ".csv"))
  
  
  
  ## Spot check site locations
  ggplot() + 
    geom_spatial_point(data = sets, aes(longitude, latitude, shape = depth_bin, fill = depth_bin), color = "black", size = 6, crs = 4326) + 
    scale_fill_manual(name = "Depth", values = c("red", "goldenrod1", "green3")) + # make size = 9 ONLY for Guam
    scale_shape_manual(name = "Depth", values = c(24,22,21))


  
  ## Export set table as two columns
  # Add additional row to sets with odd numbers so they can be evenly split
  if(nrow(sets) %% 2 == 1){     
    
    blankrow <- data.frame(matrix(ncol = 8, nrow = 1))
    colnames(blankrow) <- colnames(sets)
    sets <- rbind(sets, blankrow)
  }
 
  # Format and split sets to create two columns   
  sets$depth <- round(sets$depth, digits = 2)
  sets_print <- select(sets, "id", "longitude","latitude","depth","strat","depth_bin") # Add "strat" for Guam
  
  # GUAM ONLY: add in strat names
  # sets_print$strat <- case_when(sets_print$strat == 5 ~ "Tumon Bay", 
  #                               sets_print$strat == 13 ~ "Tumon Bay",
  #                               sets_print$strat == 20 ~ "Tumon Bay",
  #                               sets_print$strat == 11 ~ "Pati Point",
  #                               sets_print$strat == 18 ~ "Pati Point",
  #                               sets_print$strat == 3 ~ "Pati Point",
  #                               sets_print$strat == 21 ~ "Piti Bomb",
  #                               sets_print$strat == 14 ~ "Piti Bomb",
  #                               sets_print$strat == 6 ~ "Piti Bomb",
  #                               sets_print$strat == 4 ~ "Achang",
  #                               sets_print$strat == 12 ~ "Achang",
  #                               sets_print$strat == 19 ~ "Achang",
  #                               sets_print$strat == 15 ~ "East Open",
  #                               sets_print$strat == 7 ~ "East Open",
  #                               sets_print$strat == 1 ~ "East Open",
  #                               sets_print$strat == 22 ~ "East Open",
  #                               sets_print$strat == 9 ~ "East Open",
  #                               sets_print$strat == 23 ~ "West Open",
  #                               sets_print$strat == 16 ~ "West Open",
  #                               sets_print$strat == 8 ~ "West Open") # missing = 2 (gua harbor),10 (gua harbor),17 (not present in cells)
  
  sets1 <- data.frame(split(sets_print, factor(sort(rank(row.names(sets_print))%%2))))
  sets1 <- sets1[,1:6]
  colnames(sets1) <- colnames(sets_print)
  sets2 <- anti_join(sets_print, sets1)
  list = seq.int((nrow(sets2) + 1 ), nrow(sets))
  
  # Print table  
  if(dim(sets1)[1] < 30) {
    
    page_height = dim(sets)[1]/6.5 # Margins for larger site lists i.e. Guam 
    
  } else {
    
    page_height = dim(sets)[1]/7 # Margins for smaller site lists i.e Rota
    
  }
  
  pdf(paste0("outputs/table/survey_table_", region, "_", islands[i], ".pdf"), height = page_height, width = 16)

  sets1 <- tableGrob(sets1)
  sets2 <- tableGrob(sets2, rows = list)
  grid.arrange(rectGrob(), rectGrob(), ncol = 2)
  grid.arrange(sets1, sets2, nrow=1, ncol = 2, newpage = FALSE)
  dev.off()
    

 
  ## Plot Island shapefiles  
  (bathymetry = cells %>% 
      ggplot(aes(x, y)) +
      geom_raster(aes(fill = depth)) + 
      scale_fill_gradientn(colours = matlab.like(30), "Depth (m)") + 
      ylab("Northings (km)") + xlab("Eastings (km)") + 
      coord_fixed() +
      theme_light() +
      theme(legend.position = "right"))
  
  (strata = cells %>% 
      ggplot(aes(x, y)) +
      geom_raster(aes(fill = factor(strat))) + 
      scale_fill_discrete("Strata") + 
      ylab("Northings (km)") + xlab("Eastings (km)") + 
      coord_fixed() +
      theme_light() +
      theme(legend.position = "right"))
  
  (variability = cells %>% 
      ggplot(aes(x, y)) +
      geom_raster(aes(fill = sd)) + 
      scale_fill_gradientn(colours = matlab.like(100), "var") + 
      ylab("Northings (km)") + xlab("Eastings (km)") +
      coord_fixed() +
      theme_light() +
      theme(legend.position = "right"))
  
  (area = cells %>% 
      ggplot(aes(x, y)) +
      geom_raster(aes(fill = strat_area )) + 
      scale_fill_gradientn(colours = matlab.like(100), "Area (sq.km)") + 
      ylab("Northings (km)") + xlab("Eastings (km)") +
      coord_fixed() +
      theme_light() +
      theme(legend.position = "right"))
  
  # (bathymetry + area) / (variability + strata)
  
  
  
  isl_shp = island_name_code %>% subset(Island_Code == islands[i])
  
  #######################################
  ### Read Island shape and coastline ###
  #######################################
  ISL_this = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(isl_shp)),]
  # ISL_this_utm = spTransform(ISL_this,CRS(paste0("+proj=utm +units=km +zone=", zone)))
  # ISL_this_sf = st_transform(st_as_sf(ISL_this), crs = paste0("+proj=utm +units=km +zone=", zone))
  
  
  ######################################
  ### Read Island 5km buffer sectors ###
  ######################################
  load(paste0('data/gis_5km_buffer/', islands[i], '.RData'))
  buffer = raster_and_table[[1]]
  buffer_name = raster_and_table[[2]]
  buffer <- data.table(rasterToPoints(buffer))
  utmcoor <- SpatialPoints(cbind(buffer$x, buffer$y), proj4string = CRS(paste0("+proj=utm +units=m +zone=", utm_i$UTM_Zone)))
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
  buffer_label = buffer %>% group_by(sector_nam) %>% summarise(longitude = median(longitude), latitude = median(latitude))
  
  
  
  
  ################################
  ### Read Island survey boxes ###
  ################################
  if (file.exists(paste0('data/gis_survey_boxes/', islands[i], '.RData'))) {
    
    load(paste0('data/gis_survey_boxes/', islands[i], '.RData'))
    boxes = raster_and_table[[1]]
    boxes_name = raster_and_table[[2]]
    boxes <- data.table(rasterToPoints(boxes)); colnames(boxes)[3] = "ID"
    utmcoor <- SpatialPoints(cbind(boxes$x, boxes$y), proj4string = CRS(paste0("+proj=utm +units=m +zone=", utm_i$UTM_Zone)))
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
    
    Switch = T
    
  } else {
    
    
    Switch = F
    
  }

  
  
  ####################################################
  #### GUAM: Manually Choose sector and box color ####
  ####################################################
  unique(buffer$sector_nam)
  buffer <- buffer %>% mutate(color = case_when(
                              sector_nam == "GUA_EAST_OPEN" ~ "blue3",
                              sector_nam == "GUA_WEST_OPEN" ~ "cyan3",
                              sector_nam == "GUA_PATI_POINT" ~ "chartreuse1",
                              sector_nam == "GUA_TUMON_BAY" ~ "tomato1",
                              sector_nam == "GUA_PITI_BOMB" ~ "red1",
                              sector_nam == "GUA_SASA_BAY" ~ "goldenrod1",
                              sector_nam == "GUA_ACHANG" ~ "hotpink1",
                              sector_nam == "GUA_HARBOR" ~ "goldenrod4",
                              sector_nam == "Rota" ~ "cyan3",
                              sector_nam == "Tinian" ~ "cyan3",
                              sector_nam == "Pagan" ~ "cyan3",
                              sector_nam == "Asuncion" ~ "cyan3",
                              sector_nam == "Maug" ~ "cyan3",
                              sector_nam == "Saipan" ~ "cyan3"
                              ))
  
  buffer_label <- buffer_label %>% mutate(color = case_when(
                                          sector_nam == "GUA_EAST_OPEN" ~ "blue3",
                                          sector_nam == "GUA_WEST_OPEN" ~ "cyan3",
                                          sector_nam == "GUA_PATI_POINT" ~ "chartreuse1",
                                          sector_nam == "GUA_TUMON_BAY" ~ "tomato1",
                                          sector_nam == "GUA_PITI_BOMB" ~ "red1",
                                          sector_nam == "GUA_SASA_BAY" ~ "goldenrod1",
                                          sector_nam == "GUA_ACHANG" ~ "hotpink1",
                                          sector_nam == "GUA_HARBOR" ~ "goldenrod4",
                                          sector_nam == "Rota" ~ "cyan3",
                                          sector_nam == "Tinian" ~ "cyan3",
                                          sector_nam == "Pagan" ~ "cyan3",
                                          sector_nam == "Asuncion" ~ "cyan3",
                                          sector_nam == "Maug" ~ "cyan3",
                                          sector_nam == "Saipan" ~ "cyan3"
                                          ))


  ##########################################
  #### Import and format MPA shapefiles ####
  ##########################################
  # May be worth adding to an earlier script. These shapefiles were copied to N:\GIS\Projects\CommonMaps\Marianas_Benthic_Shapefiles_Copy\MPAs
  # Saipan
  managaha_isl <- st_read("//Picgoldfish/general/Benthic/GIS/Projects/MARAMP 2017/MPAs/CNMI/Saipan/Managaha_Island.shp") # Managaha marine conservation area
  managaha <- st_transform(managaha$geometry, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  managaha_coords <- as.data.frame(sf::st_coordinates(managaha)) # Extract coordinates from shapefiles
  
  managaha <- st_read("//Picgoldfish/general/Benthic/GIS/Projects/MARAMP 2017/MPAs/CNMI/Saipan/managaha.shp") # Managaha island
  managaha_isl <- st_transform(managaha_isl$geometry, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  managaha_isl_coords <- as.data.frame(sf::st_coordinates(managaha_isl)) 
  
  bird_isl <- st_read("//Picgoldfish/general/Benthic/GIS/Projects/MARAMP 2017/MPAs/CNMI/Saipan/Bird_Island.shp") # Bird island sanctuary
  bird_isl <- st_transform(bird_isl$geometry, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  bird_isl_coords <- as.data.frame(sf::st_coordinates(bird_isl)) 
  
  forb_isl <- st_read("//Picgoldfish/general/Benthic/GIS/Projects/MARAMP 2017/MPAs/CNMI/Saipan/Forbidden_Island.shp") # Forbidden Island sanctuary
  forb_isl <- st_transform(forb_isl$geometry, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  forb_isl_coords <- as.data.frame(sf::st_coordinates(forb_isl)) 
  
  bank <- st_read("//Picgoldfish/general/Benthic/GIS/Projects/MARAMP 2017/MPAs/CNMI/Saipan/conservation_mitigation_bank.shp")
  bank <- st_transform(bank$geometry, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  bank_coords <- as.data.frame(sf::st_coordinates(bank)) 
  
  # lighthouse <- st_read("//Picgoldfish/general/Benthic/GIS/Projects/MARAMP 2017/MPAs/CNMI/Saipan/Light_House_trochus.shp") # Lighthouse reef torchus sanctuary
  # laolao <- st_read("//Picgoldfish/general/Benthic/GIS/Projects/MARAMP 2017/MPAs/CNMI/Saipan/Laolao_Bay_Sea_Cucumber.shp") # Laolao bay sea cucumber sanctuary
  
  
  # Tinian
  mpa <- st_read("//Picgoldfish/general/Benthic/GIS/Projects/MARAMP 2017/MPAs/CNMI/Tinian/MPA.shp") # Tinian marine reserve
  mpa <- st_transform(mpa$geometry, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  mpa_coords <- as.data.frame(sf::st_coordinates(mpa)) 
  
  #tin_mpa <- st_read("//Picgoldfish/general/Benthic/GIS/Projects/MARAMP 2017/MPAs/CNMI/Tinian/Tinian_MPA.shp") # Tinian marine reserve -no projected CRS
  #tin_mpa_approx <- st_read("//Picgoldfish/general/Benthic/GIS/Projects/MARAMP 2017/MPAs/CNMI/Tinian/Tinian_MPA_Approximate_2011.shp") # Smaller than the above Tinian marine reserve shp
  
  
  # Rota
  con <- st_read("//Picgoldfish/general/Benthic/GIS/Projects/MARAMP 2017/MPAs/CNMI/Rota/Rota_con.shp")
  con <- st_transform(con$geometry, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  con_coords <- as.data.frame(sf::st_coordinates(con)) 
  
  
  
  
  ##################################
  #### Add inset map for Saipan ####
  ##################################
  
  (sai_inset =

    ggplotGrob(

    ggplot() +

    geom_path(data = ISL_this, aes(long, lat, group = group), inherit.aes = F, size = 0.01, color = "darkgrey") + # coastline
    geom_polygon(data = ISL_this, aes(long, lat, group = group), fill = "darkgrey", color = NA, alpha = 0.9) + # land shapefile

    #geom_tile(data = buffer, aes(longitude, latitude, fill = color), width = 0.001, height = 0.001, alpha = 0.3, show.legend = F) +
      
    geom_polygon(data = managaha_isl_coords, aes(X, Y, fill = L1, group=L1), fill = "red", alpha = 0.25) + # Saipan
    geom_polygon(data = managaha_coords, aes(X, Y, fill = L1, group=L1), fill = "red", alpha = 0.25) + # Saipan
    geom_polygon(data = forb_isl_coords, aes(X, Y, fill = L1 ,group=L2), fill = "red", alpha = 0.25) + # Saipan
    geom_polygon(data = bird_isl_coords, aes(X, Y, fill = L1, group=L2), fill = "red", alpha = 0.25) + # Saipan

    scale_fill_identity() +
    scale_color_identity() +

    {if(Switch) geom_polygon(data = boxes_hulls, aes(longitude, latitude, fill = boxes_nam, color = "black"), alpha = 0.01, size = 1, show.legend = F)} + # survey boxes hull
    {if(Switch) geom_text_repel(data = boxes_label, aes(longitude, latitude, label = boxes_nam, color = "black", fontface = 'bold'), size = 5, max.overlaps = Inf, show.legend = F)} +

     #annotation_scale(location = "br", width_hint = 0.2) +
     coord_sf(crs = 4326) +

     theme_bw() +
     theme(axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank())
    )

  )

  
  ################################
  #### Add inset map for Guam ####
  ################################

  (gua_inset =

     ggplotGrob(

       ggplot() +

         geom_path(data = ISL_this, aes(long, lat, group = group), inherit.aes = F, size = 0.01, color = "darkgrey") + # coastline
         geom_polygon(data = ISL_this, aes(long, lat, group = group), fill = "darkgrey", color = NA, alpha = 0.9) + # land shapefile

         #geom_tile(data = buffer, aes(longitude, latitude, fill = color), width = 0.001, height = 0.001, alpha = 0.3, show.legend = F) +

         scale_fill_identity() +
         scale_color_identity() +

         {if(Switch) geom_polygon(data = boxes_hulls, aes(longitude, latitude, fill = boxes_nam, color = "black"), alpha = 0.01, size = 1, show.legend = F)} + # survey boxes hull
         {if(Switch) geom_text_repel(data = boxes_label, aes(longitude, latitude, label = boxes_nam, color = "black", fontface = 'bold'), max.overlaps = Inf, show.legend = F)} +

         annotation_scale(location = "br", width_hint = 0.2) +
         coord_sf(crs = 4326) +

         theme_bw() +
         theme(axis.ticks.x=element_blank(),
               axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.y=element_blank(),
               axis.title.y=element_blank(),
               axis.text.y=element_blank())
     )

  )

    
  ########################
  #### Split Guam Map ####
  ########################
  #  a <- st_as_sf(ISL_this);a$geometry #find min and max lat/long
  
  # North region
  ISL_this <- crop(ISL_this, extent(144.75, 145.0, 13.42, 13.7))

  buffer = buffer %>% subset(longitude > 144.75 &
                               longitude < 145 &
                               latitude > 13.42 &
                               latitude < 13.7)

  buffer_label = buffer_label %>% subset(longitude > 144.75 &
                                           longitude < 145 &
                                           latitude > 13.42 &
                                           latitude < 13.7)

  boxes_hulls = boxes_hulls %>% subset(longitude > 144.75 &
                                         longitude < 145 &
                                         latitude > 13.42 &
                                         latitude < 13.7) %>%
                                         filter(boxes_nam != "GUA_F" & boxes_nam != "GUA_C")

  boxes_label = boxes_label %>% subset(longitude > 144.75 &
                                         longitude < 145 &
                                         latitude > 13.42 &
                                         latitude < 13.7)

  sets_north = sets %>% subset(longitude > 144.75 &
                           longitude < 145 &
                           latitude > 13.42 &
                           latitude < 13.7)

  #South
  ISL_this <- crop(ISL_this, extent(144.53, 144.81, 13.15, 13.51))

  buffer = buffer %>% subset(longitude > 144.53 &
                               longitude < 144.81 &
                               latitude > 13.15 &
                               latitude < 13.51)

  buffer_label = buffer_label %>% subset(longitude > 144.53 &
                                           longitude < 144.81 &
                                           latitude > 13.15 &
                                           latitude < 13.51)

  boxes_hulls = boxes_hulls %>% subset(longitude > 144.53 &
                                         longitude < 144.82 &
                                         latitude > 13.15 &
                                         latitude < 13.51) %>%
                                         filter(boxes_nam != "GUA_G" & boxes_nam != "GUA_B")

  boxes_label = boxes_label %>% subset(longitude > 144.53 &
                                         longitude < 144.81 &
                                         latitude > 13.15 &
                                         latitude < 13.51) %>%
                                         filter(boxes_nam != "GUA_G")

  sets_south = sets %>% subset(longitude > 144.53 &
                           longitude < 144.81 &
                           latitude > 13.15 &
                           latitude < 13.51)

  
  ##########################
  #### Split Saipan Map ####
  ##########################
  
  # a <- st_as_sf(ISL_this);a$geometry #find min and max lat/long
  # #xmin: 145.6895 ymin: 15.09136 xmax: 145.8304 ymax: 15.2901
  
  #North region
  ISL_this <- crop(ISL_this, extent(145.65, 145.86, 15.17, 15.31))

  buffer = buffer %>% subset(longitude > 145.65 &
                               longitude < 145.86 &
                               latitude > 15.17 &
                               latitude < 15.31)

  # buffer_label = buffer_label %>% subset(longitude > 145.69 &
  #                                          longitude < 145.83 &
  #                                          latitude > 15.2 &
  #                                          latitude < 15.3)

  boxes_hulls = boxes_hulls %>% subset(longitude > 145.65 &
                                         longitude < 145.86 &
                                         latitude > 15.17 &
                                         latitude < 15.31) %>%
                                         filter(boxes_nam != "SAI_C" & boxes_nam != "SAI_B" )

  boxes_label = boxes_label %>% subset(longitude > 145.65 &
                                         longitude < 145.86 &
                                         latitude > 15.17 &
                                         latitude < 15.31)

  sets_north = sets %>% subset(longitude > 145.65 &
                           longitude < 145.86 &
                           latitude > 15.17 &
                           latitude < 15.31)

  #South region
  ISL_this <- crop(ISL_this, extent(145.65, 145.86, 15.07, 15.2))

  buffer = buffer %>% subset(longitude > 145.65 &
                               longitude < 145.86 &
                               latitude > 15.07 &
                               latitude < 15.2)

  boxes_hulls = boxes_hulls %>% subset(longitude > 145.65 &
                               longitude < 145.86 &
                               latitude > 15.07 &
                               latitude < 15.2) %>%
                               filter(boxes_nam != "SAI_D")

  boxes_label = boxes_label %>% subset(longitude > 145.65 &
                               longitude < 145.86 &
                               latitude > 15.07 &
                               latitude < 15.2)

  sets_south = sets %>% subset(longitude > 145.65 &   #Have to split sets south and north because I resample after one half of the map is made
                               longitude < 145.86 &
                               latitude > 15.07 &
                               latitude < 15.2)


  #####################################################
  #### Change Sector Label Lat/Long for Guam MPAs ####
  #####################################################

  buffer_label_repel <- filter(buffer_label, sector_nam == "GUA_ACHANG" | sector_nam == "GUA_PITI_BOMB" | sector_nam == "GUA_WEST_OPEN" |
                                 sector_nam == "GUA_TUMON_BAY" | sector_nam =="GUA_PATI_POINT" |  sector_nam =="GUA_HARBOR")
  buffer_label <- anti_join(buffer_label, buffer_label_repel)
  #buffer_label_repel<- droplevels(buffer_label_repel$latitude)
 # buffer_label_repel$latitude <- NA
 # buffer_label_repel$longitude <- NA
  
  
 buffer_label_repel$latitude <- case_when(buffer_label_repel$sector_nam == "GUA_ACHANG" ~ 13.2225,
                                          buffer_label_repel$sector_nam == "GUA_PITI_BOMB" ~ 13.497,
                                          buffer_label_repel$sector_nam == "GUA_WEST_OPEN" ~ 13.48,
                                          buffer_label_repel$sector_nam == "GUA_PATI_POINT" ~ 13.623,
                                          buffer_label_repel$sector_nam == "GUA_TUMON_BAY" ~ 13.535,
                                          buffer_label_repel$sector_nam == "GUA_HARBOR" ~ 13.45578)
       
 buffer_label_repel$longitude <- case_when(buffer_label_repel$sector_nam == "GUA_ACHANG" ~ 144.7,
                                           buffer_label_repel$sector_nam == "GUA_PITI_BOMB" ~ 144.69,
                                           buffer_label_repel$sector_nam == "GUA_WEST_OPEN" ~ 144.62,
                                           buffer_label_repel$sector_nam == "GUA_PATI_POINT" ~ 144.965,
                                           buffer_label_repel$sector_nam == "GUA_TUMON_BAY" ~ 144.76,
                                           buffer_label_repel$sector_nam == "GUA_HARBOR" ~ 144.647)
                                    
  
  ####################################
  #### Change Box Label Lat/Longs ####
  ####################################
 
 # Guam:
 # boxes_label$longitude <- case_when(boxes_label$boxes_nam == "GUA_A" ~ 144.91,
 #                                  boxes_label$boxes_nam == "GUA_B" ~ 144.862,
 #                                  boxes_label$boxes_nam == "GUA_C" ~ 144.794,
 #                                  boxes_label$boxes_nam == "GUA_D" ~ 144.76,
 #                                  boxes_label$boxes_nam == "GUA_E" ~ 144.632,
 #                                  boxes_label$boxes_nam == "GUA_F" ~ 144.612,
 #                                  boxes_label$boxes_nam == "GUA_G" ~ 144.851)
 # 
 # boxes_label$latitude <- case_when(boxes_label$boxes_nam == "GUA_A" ~ 13.63,
 #                                    boxes_label$boxes_nam == "GUA_B" ~ 13.464,
 #                                    boxes_label$boxes_nam == "GUA_C" ~ 13.39,
 #                                    boxes_label$boxes_nam == "GUA_D" ~ 13.241,
 #                                    boxes_label$boxes_nam == "GUA_E" ~ 13.39,
 #                                    boxes_label$boxes_nam == "GUA_F" ~ 13.464,
 #                                   boxes_label$boxes_nam == "GUA_G" ~ 13.61)
 
 # Rota:
 # boxes_label$longitude <- case_when(boxes_label$boxes_nam == "ROT_A" ~ 	145.1955,
 #                                    boxes_label$boxes_nam == "ROT_B" ~ 145.253,
 #                                    boxes_label$boxes_nam == "ROT_C" ~  145.1754)
 # 
 # boxes_label$latitude <- case_when(boxes_label$boxes_nam == "ROT_A" ~ 14.1722,
 #                                    boxes_label$boxes_nam == "ROT_B" ~ 14.1635,
 #                                    boxes_label$boxes_nam == "ROT_C" ~ 14.11868)
 
 
 # Pagan:
 # boxes_label$longitude <- case_when(boxes_label$boxes_nam == "PAG_A" ~ 	145.7895,
 #                                    boxes_label$boxes_nam == "PAG_B" ~ 	145.7674,
 #                                    boxes_label$boxes_nam == "PAG_C" ~  145.734)
 # 
 # boxes_label$latitude <- case_when(boxes_label$boxes_nam == "PAG_A" ~ 18.15779,
 #                                    boxes_label$boxes_nam == "PAG_B" ~ 	18.07981,
 #                                    boxes_label$boxes_nam == "PAG_C" ~  18.10372)

 
 # Tinian:
 # boxes_label$latitude <- case_when(boxes_label$boxes_nam == "TIN_A" ~ 	15.05106,
 #                                    boxes_label$boxes_nam == "TIN_B" ~ 	15.00353,
 #                                    boxes_label$boxes_nam == "TIN_C" ~  14.9748)
 # 
 # boxes_label$longitude <- case_when(boxes_label$boxes_nam == "TIN_A" ~ 145.605,
 #                                    boxes_label$boxes_nam == "TIN_B" ~ 	145.6576,
 #                                    boxes_label$boxes_nam == "TIN_C" ~  145.6189)
 

 
  ########################
  #### Plot Final Map ####
  ########################
  
  
  (site_location = 
     
     ggplot() + 

     # geom_tile(data = cells, aes(longitude, latitude, fill = factor(strat)), alpha = 0.3, width = 0.001, height = 0.001) + # stratum - only needed for Guam
     geom_polygon(data = ISL_this, aes(long, lat, group = group), fill = "darkgrey", color = NA, alpha = 0.9) + # land shapefile
     geom_path(data = ISL_this, aes(long, lat, group = group), inherit.aes = F, size = 0.01, color = "darkgrey") + # coastline
     
     # geom_tile(data = boxes, aes(longitude, latitude, fill = boxes_nam), width = 0.001, height = 0.001, alpha = 0.2, show.legend = F) + # survey boxes fill
     {if(Switch) geom_polygon(data = boxes_hulls, aes(longitude, latitude, fill = boxes_nam, color = "black"), alpha = 0.01, size = 1, show.legend = F)} + # survey boxes hull
     {if(Switch) geom_text_repel(data = boxes_label, aes(longitude, latitude, label = boxes_nam, color = "black", fontface = 'bold'), size = 12, max.overlaps = Inf, show.legend = F)} +
     {if(Switch) scale_fill_identity()} + 
     {if(Switch) scale_color_identity()} + 
     {if(Switch) new_scale_color()} +
     {if(Switch) new_scale_fill()} +
     
     # GUAM ONLY: add sector and box names 
     # geom_tile(data = buffer, aes(longitude, latitude, fill = color), width = 0.001, height = 0.001, alpha = 0.25, show.legend = F) + # GUAM ONLY: island sectors
     # geom_label_repel(data = buffer_label_repel, aes(longitude, latitude, label = sector_nam, fill = color, fontface = 'bold'), size = 12, color = "white", max.overlaps = Inf, show.legend = F, segment.alpha = 0) + # GUAM ONLY: MPA names
     # geom_label(data = buffer_label, aes(longitude, latitude, label = sector_nam, fill = color, fontface = 'bold'), size = 12, color = "white",show.legend = F) + # GUAM ONLY: Sector names not reformatted
     
     # SAIPAN, ROTA AND TINIAN ONLY: add MPAs
     # geom_polygon(data = managaha_isl_coords, aes(X, Y, color = "black", group=L1), fill = NA, size = 1) + # North Saipan
     # geom_path(data = con_coords, aes(X, Y, color = "black", group=L2), fill = NA) + # Rota
     # geom_path(data = mpa_coords, aes(X, Y, color = "black", group=L2), fill = NA) + # Tinian
     
     # SAIPAN and GUAM ONLY: Add map insets to islands split between two maps
     # annotation_custom(grob = gua_inset, xmin = 144.935, xmax = 145.015, ymin = 13.41, ymax = 13.49) + # Guam north inset
     # annotation_custom(grob = gua_inset, xmin = 144.78, xmax = 144.8285, ymin = 13.185, ymax = 13.24) + # Guam south inset
     # annotation_custom(grob = sai_inset, xmin = 145.822, xmax = 145.862, ymin = 15.166, ymax = 15.21) + # Saipan north inset
     # annotation_custom(grob = sai_inset, xmin = 145.783, xmax = 145.82, ymin = 15.07, ymax = 15.12) + # Saipan south inset
     
     scale_fill_identity() + 
     scale_color_identity() +
     
     new_scale_color() +
     new_scale_fill() +
     
     # Add site points
     geom_spatial_point(data = sets, aes(longitude, latitude, shape = depth_bin, fill = depth_bin), color = "black", size = 10, crs = 4326) + # Add _north or _south for Saipan and Guam
     scale_fill_manual(name = "Depth", values = c("red", "goldenrod1", "green3"), na.translate = F) + # in geom_spatial_point make size = 9 ONLY for Guam
     scale_shape_manual(name = "Depth", values = c(24,22,21), na.translate = F) +
     annotation_scale(location = "br", width_hint = 0.2, height = unit(0.8, "cm"), text_cex = 2) +
     
     # Add site names
     geom_label_repel(data = sets, # Add _north or _south for Saipan and Guam
                      aes(longitude, latitude, label = id),
                      fill = "white",
                      size = 10,
                      label.size = NA, 
                      alpha = 0.75, 
                      fontface = 'bold', 
                      color = 'black',
                      max.overlaps = Inf,
                      segment.size = 0.5,
                      direction = "both", 
                      # nudge_y = 0.005,
                      # nudge_x = 0.005,
                      box.padding = unit(0.4, "lines"),
                      point.padding = unit(0.3, "lines")) +
     
     
     # coord_fixed() +
     # coord_map() + 
     coord_sf(crs = 4326) + 
     
     # scale_x_continuous(sec.axis = dup_axis(), breaks = scales::pretty_breaks(n = 20), "Longitude (dec deg)") +
     # scale_y_continuous(sec.axis = dup_axis(), breaks = scales::pretty_breaks(n = 20), "Latitude (dec deg)") +
     
     # scale_x_continuous(sec.axis = dup_axis(), "") + # Use for all Islands not listed below
     # scale_y_continuous(sec.axis = dup_axis(), "") +

     # MAUG, ASUNCION, PAGAN AND TINIAN ONLY: Reduce blank area for smaller islands
     # xlim(145.2,145.245) + # Maug
     # ylim(20.005,20.041) +
     # xlim(145.38,145.427) + # Asuncion
     # ylim(19.669,19.714) +
     # xlim(145.681,145.83) + # Pagan
     # ylim(18.02,18.19) +
     # xlim(145.55,145.68) + # Tinian
     # ylim(14.918,15.113) +
     
     theme_bw() +
     
     theme(legend.position = c(0.95,0.95), 
           legend.title = element_text(size = 22, face = "bold"),
           legend.text = element_text(size = 22),
           axis.text = element_text(size = 15),
           axis.title = element_text(size = 10),
           #legend.title = element_blank(),
           panel.grid = element_blank()) + 
     
     labs(
       title = "",
       subtitle = paste0(paste0("Island = ", toupper(as.character(isl_shp[1])),"\n"), 
                                #"Target survey effort = ", total_sample, " sites \n",
                                # "Total survey effort = ", sum(strat_det$strat_sets), " sites \n"),
                                "Number of strata = ", length(unique(cells$strat)))) +
                                

     theme(plot.subtitle = element_text(size = 30))
     
     )
     
   

  ## Print map(s)      
  # pdf(paste0("outputs/survey_layers_", islands[i], ".pdf"), height = 10, width = 10)
  # print((bathymetry + strata) / (area + variability))
  # dev.off()
  
  pdf(paste0("outputs/map/survey_map_", region, "_", islands[i], ".pdf"), height = 38, width = 28)
  print(site_location)
  dev.off()
  
  library(pdftools)
  pdf_combine(c(paste0("outputs/map/survey_map_", region, "_", islands[i], ".pdf"), 
                paste0("outputs/table/survey_table_", region, "_", islands[i], ".pdf")), 
              output = paste0("outputs/survey_map_table_", region, "_", islands[i], ".pdf"))
  
  
  
  # (Area = cells %>% 
  #     group_by(strat) %>% 
  #     summarise(strat_area = mean(strat_area))%>% 
  #     ggplot(aes(x = factor(strat), y = as.numeric(as.character(strat_area)), fill = strat_area)) +
  #     geom_bar(stat = "identity", position = position_dodge(), show.legend = F) + 
  #     xlab("Strat") + ylab("Strat_Area (sq.km)") + 
  #     scale_fill_viridis_c() + 
  #     coord_flip() + 
  #     theme_minimal())
  # 
  # (SD = cells %>% 
  #     group_by(strat) %>% 
  #     summarise(sd = mean(sd))%>% 
  #     ggplot(aes(x = factor(strat), y = as.numeric(as.character(sd)), fill = sd)) +
  #     geom_bar(stat = "identity", position = position_dodge(), show.legend = F) + 
  #     scale_fill_viridis_c() + 
  #     xlab("Strat") + ylab("S.D.") + 
  #     coord_flip() + 
  #     theme_minimal())
  # 
  # (Site_allocation = cells %>% 
  #     group_by(strat) %>% 
  #     summarise(strat_sets  = mean(strat_sets ))%>% 
  #     ggplot(aes(x = factor(strat), y = as.numeric(as.character(strat_sets )), fill = factor(strat_sets))) +
  #     geom_bar(stat = "identity", position = position_dodge(), show.legend = F) + 
  #     scale_fill_viridis_d() + 
  #     xlab("Strat") + ylab("Site_allocation") + 
  #     coord_flip() + 
  #     theme_minimal())
  
#}



