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
library(ggmap)
library(sf)

utm = read_csv('data/misc/ncrmp_utm_zones.csv')
#utm$Island_Code = toupper(utm$Island_Code)

########################################################################
### do some parameter settings to simulate stratified random surveys ###
########################################################################

# n_sims = 100 # number of simulations
effort_level = c("low", "mid", "high")[3] # define sampling effort (low, mid, high)
min_sets = 6 # minimum number of sets per strat
max_sets = 26
# shal_sets = 12 # JAR: 16
# mid_sets = 39
# deep_sets = 31
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

islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                              # American Samoa
# islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"                # Pacific Remote Island Areas
# islands = c("GUA", "ROT", "SAI", "TIN", "AGU"); region = "S.MARIAN"                           # South Mariana Islands
# islands = c("AGR", "ALA", "ASC","GUG", "FDP", "MAU", "PAG", "SAR"); region = "N.MARIAN"      # North Mariana Islands
# islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"  # Main Hawaiian Islands
# islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"               # Northern Hawaiian Islands

set.seed(2022)

ggmap::register_google("AIzaSyDpirvA5gB7bmbEbwB1Pk__6jiV4SXAEcY")

select = dplyr::select

#################################################################
### Generate survey site tables & maps, check outputs/ folder ###
#################################################################

# for (i in 1:length(islands)) {
  
   i = 5
  
  # survey domain with sector & reef & hard_unknown & 3 depth bins
  load(paste0("data/survey_grid_ncrmp/survey_grid_", islands[i], ".RData")); plot(survey_grid_ncrmp)
  
  total_sample = survey_effort %>% subset(Island_Code == islands[i])
  
  if (dim(total_sample)[1] == 0){
    
    total_sample = 100
    
  } else {
    
    total_sample = total_sample$Effort # Lists Baker as survey effort 35. Fish going to 40 sites in 2023
    
  }
  
  n <- id <- division <- strat <- N <- strat_sets <- cell_sets <- NULL
  
  cells <- data.table(rasterToPoints(survey_grid_ncrmp))
  cells <- cells %>% filter(depth > .8)
  
  #### Add modeled trophic biomass variability, summarize by strata ####
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
  # strat_det$strat_sets[strat_det$strat_sets > max_sets] <- max_sets; strat_det
  # strat_det$strat_sets[strat_det$strat_sets < min_sets] <- min_sets; strat_det
  

  #### Island-Specific Site Selection ####
  # BAKER
  # In 2018 fish maps: 30 deep, 33 mid and 37 shallow
  # For 2023 allocation, they're visiting 12 deep, 14 mid, 14 shallow
  # count(cells, strat)
  # strat_det[strat_det$strat == 1, "strat_sets"] <- 19 # leave room for additions later
  # strat_det[strat_det$strat == 2, "strat_sets"] <- 19
  # strat_det[strat_det$strat == 3, "strat_sets"] <- 16; strat_det
  
  
  # # HOWLAND
  # # In 2018:
  # # For 2023 allocation: 18 shallow, 13 mid, 9 deep
  # # Only 13 cells for shallow are available. Changing shallow bin to go up to 6.4 would fix the allocation issue
  # cells$strat <- ifelse(cells$depth < 6.45 & cells$depth > 6, as.numeric(cells$strat) - 1, as.numeric(cells$strat))
  # count(cells, strat)
  # 
  # strat_det[strat_det$strat == 1, "strat_sets"] <- 24 # using all cells available
  # strat_det[strat_det$strat == 2, "strat_sets"] <- 50
  # strat_det[strat_det$strat == 3, "strat_sets"] <- 50; strat_det
  
  
  # TUTUILLA
  
  # TUT_NW_OPEN: 1=SHAL, 9=MID, 17=DEEP
  # TUT_FAGALUA: 2=SHAL, 10-MID, 18=DEEP
  # TUT_FAGATELE: 3=SHAL, 11=MID, 19=DEEP
  # TUT_AUNUUA: 4=SHAL, 12=MID, 20=DEEP
  # TUT_AUNUUB: 5=SHAL, 13=MID, 21=DEEP
  # TUT_NE_OPEN: 6=SHAL, 14=MID, 22=DEEP
  # TUT_SE_OPEN: 7=SHAL, 15=MID, 23=DEEP
  # TUT_SW_OPEN: 8=SHAL, 16=MID, 24=DEEP
  # 
  # For benthic allocation:
  # AUNUU: deep=9, mid=9, shallow=6
  # Fagalua/Fagatele: deep=9, mid=9, shallow=9
  # NE: deep=15, mid=24, shallow=12
  # NW: deep=12, mid-12, shallow=6
  # SE: deep=9, mid=39, shallow=30
  # SW: deep=6, mid=9, shallow=9

  count(cells, strat)
  strat_det[strat_det$strat == 1, "strat_sets"] <- 6 # Northwest
  strat_det[strat_det$strat == 9, "strat_sets"] <- 12
  strat_det[strat_det$strat == 17, "strat_sets"] <- 12

  strat_det[strat_det$strat == 2, "strat_sets"] <- 6 # Fagalua
  strat_det[strat_det$strat == 10, "strat_sets"] <- 6
  strat_det[strat_det$strat == 18, "strat_sets"] <- 6 

  strat_det[strat_det$strat == 3, "strat_sets"] <- 6 # Fagatele; only has 3 cells available
  strat_det[strat_det$strat == 11, "strat_sets"] <- 6
  strat_det[strat_det$strat == 19, "strat_sets"] <- 6  

  strat_det[strat_det$strat == 4, "strat_sets"] <- 5 # Anuua A
  strat_det[strat_det$strat == 12, "strat_sets"] <- 5
  strat_det[strat_det$strat == 20, "strat_sets"] <- 5

  strat_det[strat_det$strat == 5, "strat_sets"] <- 5 # Anuua B
  strat_det[strat_det$strat == 13, "strat_sets"] <- 5
  strat_det[strat_det$strat == 21, "strat_sets"] <- 5

  strat_det[strat_det$strat == 6, "strat_sets"] <- 13 # Northeast
  strat_det[strat_det$strat == 14, "strat_sets"] <- 25
  strat_det[strat_det$strat == 22, "strat_sets"] <- 16

  strat_det[strat_det$strat == 7, "strat_sets"] <- 30 # Southeast
  strat_det[strat_det$strat == 15, "strat_sets"] <- 39
  strat_det[strat_det$strat == 23, "strat_sets"] <- 9 # only has 10 cells available

  strat_det[strat_det$strat == 8, "strat_sets"] <- 9 # Southwest
  strat_det[strat_det$strat == 16, "strat_sets"] <- 9
  strat_det[strat_det$strat == 24, "strat_sets"] <- 6; strat_det
  
  
  # # OFU & OLOSEGA
  # count(cells, strat)
  # strat_det[strat_det$strat == 1, "strat_sets"] <- 17
  # strat_det[strat_det$strat == 2, "strat_sets"] <- 35
  # strat_det[strat_det$strat == 3, "strat_sets"] <- 20; strat_det
  
  
  # # Rose
  # count(cells, strat)
  # 
  # strat_det[strat_det$strat == 1, "strat_sets"] <- 12 # Shallow backreef
  # strat_det[strat_det$strat == 2, "strat_sets"] <- 9 # Shallow forereef
  # strat_det[strat_det$strat == 3, "strat_sets"] <- 9 # Shallow lagoon
  # strat_det[strat_det$strat == 4, "strat_sets"] <- 9 # Mid backreef
  # strat_det[strat_det$strat == 5, "strat_sets"] <- 15 # Mid forereef
  # strat_det[strat_det$strat == 6, "strat_sets"] <- 9 # Mid lagoon
  # strat_det[strat_det$strat == 7, "strat_sets"] <- 9 # Deep backreef
  # strat_det[strat_det$strat == 8, "strat_sets"] <- 9 # Deep forereef
  # strat_det[strat_det$strat == 9, "strat_sets"] <- 9; strat_det # Deep lagoon
  
  
  # # SWAINS
  # count(cells, strat)
  # strat_det[strat_det$strat == 1, "strat_sets"] <- 40 # Shallow sanctuary
  # strat_det[strat_det$strat == 2, "strat_sets"] <- 5 # Shallow open
  # strat_det[strat_det$strat == 3, "strat_sets"] <- 25 # Mid sanctuary
  # strat_det[strat_det$strat == 4, "strat_sets"] <- 6 # Mid open
  # strat_det[strat_det$strat == 5, "strat_sets"] <- 16 # Deep sanctuary
  # strat_det[strat_det$strat == 6, "strat_sets"] <- 4; strat_det # Deep open
  
  
  ## TAU
  # count(cells, strat)
  # strat_det[strat_det$strat == 1, "strat_sets"] <- 10 # Shallow sanctuary
  # strat_det[strat_det$strat == 2, "strat_sets"] <- 24 # Shallow open
  # strat_det[strat_det$strat == 3, "strat_sets"] <- 6 # Mid sanctuary
  # strat_det[strat_det$strat == 4, "strat_sets"] <- 16 # Mid open
  # strat_det[strat_det$strat == 5, "strat_sets"] <- 4 # Deep sanctuary
  # strat_det[strat_det$strat == 6, "strat_sets"] <- 6; strat_det # Deep open
  
  
  
  #### Create Sets ####
  
  strat_table = strat_det %>% dplyr::select(strat, strat_sets); strat_table
  
  # Add "strat" "strat_cells" "tow_area" ...
  strat_det = strat_det[, c("strat", "strat_cells", "tow_area", "cell_area", "strat_area", "strat_sets")]
  cells <- merge(cells, strat_det, by = c("strat")) 
  
  utm_i = utm %>% subset(Island_Code == islands[i])
  
  utmcoor <- SpatialPoints(cbind(cells$x, cells$y), proj4string = CRS(paste0("+proj=utm +units=km +zone=", utm_i$UTM_Zone, " ", utm_i$Hemishpere)))
  longlatcoor <- spTransform(utmcoor,CRS("+proj=longlat"))
  cells$longitude <- coordinates(longlatcoor)[,1]
  cells$latitude <- coordinates(longlatcoor)[,2]
  
  # Subset "cells" to create site locations
  sets <- cells[, .SD[sample(.N, size = unique(strat_sets), replace = resample_cells)], 
                by = c("strat")]
  
  #### Remove sites that are closer than 100 m ####
  sets_full <- sets
  nearby_sites <- data.frame(longitude = sets$longitude, latitude = sets$latitude); plot(nearby_sites, pch = 20, col = 2)
  coordinates(nearby_sites) <- c('longitude', 'latitude')
  proj4string(nearby_sites) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  nearby_sites <- spTransform(nearby_sites, CRS(paste0("+proj=utm +units=m +zone=", utm_i$UTM_Zone, " ", utm_i$Hemishpere)))##
  
  library(rgeos)
  points_matrix <- gWithinDistance(nearby_sites, dist = 100, byid = T)
  points_matrix[lower.tri(points_matrix, diag = T)] <- NA
  points_matrix
  
  colSums(points_matrix, na.rm = T) == 0
  
  v <- colSums(points_matrix, na.rm = T) == 0
  nearby_sites = nearby_sites[v, ]
  
  nearby_sites <- spTransform(nearby_sites, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  nearby_sites = as.data.frame(nearby_sites)
  points(nearby_sites, pch = 20, col = 4)
  
  nearby_sites$latitude = round(nearby_sites$latitude, 4)
  nearby_sites$longitude = round(nearby_sites$longitude, 4)
  sets$latitude = round(sets$latitude, 4)
  sets$longitude = round(sets$longitude, 4)
  
  setsa = inner_join(sets, nearby_sites)
  
  
  #### Manually add in sites ####
  
  # BAKER
  # Add sites to the south and west
  # cells_S  <- cells[cells$latitude < 0.19 & cells$longitude < -176.47] ; dim(cells_S)
  # cells_W  <- cells[cells$longitude < -176.485] ; dim(cells_W)
  # 
  # cells_S$strat_sets <- case_when(cells_S$strat == 1 ~ 2,
  #                                 cells_S$strat == 2 ~ 2,
  #                                 cells_S$strat == 3 ~ 2) # Recalculate strat_det
  # cells_W$strat_sets <- case_when(cells_W$strat == 1 ~ 2,
  #                                 cells_W$strat == 2 ~ 2,
  #                                 cells_W$strat == 3 ~ 2) # Recalculate strat_det
  # 
  # sets_S <- cells_S[, .SD[sample(.N, strat_sets, replace = T)], by = c("strat")]
  # sets_W <- cells_W[, .SD[sample(.N, strat_sets, replace = T)], by = c("strat")]
  # 
  # sets_full <- rbind(sets, sets_S, sets_W) # Merge base sets with add-on sets
  # sets <- distinct(sets_full)
  
  
  # # Swains
  # count(sets, strat) # Need 36 shallow, 21 mid, 9 deep. Have 34 shallow, 19 mid, 6 deep
  # sets_full <- sets_full %>% select( c(x,y,longitude,latitude,depth,strat))
  # sets_lost <- anti_join(sets_full, sets, by = c("x","y")); sets_lost$strat
  # cells_avail <- anti_join(cells, sets_full)
  # cells_avail$strat_sets <- case_when(cells_avail$strat == 3 ~ 8,
  #                                     cells_avail$strat == 5 ~ 5)
  # cells_avail[is.na(cells_avail)] <- 0
  # sets_avail <- cells_avail[, .SD[sample(.N, strat_sets, replace = F)], by = c("strat")]
  # # sets_avail <- sets_avail %>% select(c(x,y,longitude,latitude,depth,strat))
  # sets <- rbind(sets, sets_avail)
  # 
  # 
  # cells_D <- cells[cells$latitude > -11.047 & cells$longitude < -171.072 & cells$depth > 18] ; dim(cells_D)
  # cells_D$strat_sets <- case_when(cells_D$strat == 5 ~ 1,
  #                                 cells_D$strat == 6 ~ 0)
  # sets_D <- cells_D[, .SD[sample(3, strat_sets, replace = F)], by = c("strat")] 
  # sets <- sets %>% select(-depth_bin)
  # sets_D <- sets_D %>% select(c(x,y,longitude,latitude,depth,strat))
  # sets <- rbind(sets, sets_D) 
  
  
  # # Ofu & Olosega
  # sets_full <- sets_full %>% select( c(x,y,longitude,latitude,depth,strat))
  # sets_lost <- anti_join(sets_full, sets, by = c("x","y")); sets_lost$strat
  # cells_avail <- anti_join(cells, sets_full)
  # cells_avail <- cells_avail %>% filter(strat == 2)
  # 
  # sets_2 <- cells_avail[, .SD[sample(4, replace = F)], by = c("strat")]
  # 
  # sets <- rbind(sets, sets_2)
  
  
  # # Rose
  # sets_full <- sets_full %>% select( c(x,y,longitude,latitude,depth,strat))
  # sets_lost <- anti_join(sets_full, sets, by = c("x","y")); sets_lost$strat
  # cells_avail <- anti_join(cells, sets_full)
  # count(cells_avail, strat)
  # count(sets, strat) # need 12 shallow backreef (1), 15 mid forereef (5), everything else needs 9 
  # 
  # cells_avail$strat_sets <- case_when(cells_avail$strat == 2 ~ 1,
  #                                     cells_avail$strat == 3 ~ 1,
  #                                     cells_avail$strat == 4 ~ 6,
  #                                     cells_avail$strat == 5 ~ 1,
  #                                     cells_avail$strat == 6 ~ 1) # Recalculate strat_det
  # cells_avail <- na.omit(cells_avail)
  # sets_avail <- cells_avail[, .SD[sample(.N, strat_sets, replace = F)], by = c("strat")]
  # # sets_avail <- sets_avail %>% select(x,y,longitude,latitude,strat)
  # sets_full <- rbind(sets, sets_avail) # Merge base sets with add-on sets
  # sets <- distinct(sets_full)
  # 
  # sets %>% group_by(strat) %>% summarise(n = n()) # Summary table
  
  
  # Tutuilla
  count(sets,strat)
  sets_lost <- anti_join(sets_full, sets, by = c("x","y")); count(sets_lost,strat)
  cells_avail <- anti_join(cells, sets_full)
  cells_avail$strat_sets <- case_when(cells_avail$strat == 5 ~ 2,
                                      cells_avail$strat == 10 ~ 1,
                                      cells_avail$strat == 13 ~ 1,
                                      cells_avail$strat == 5 ~ 2,
                                      cells_avail$strat == 10 ~ 1,
                                      cells_avail$strat == 11 ~ 1)
  cells_avail <- na.omit(cells_avail)
  sets_avail <- cells_avail[, .SD[sample(.N, strat_sets, replace = F)], by = c("strat")]
  sets <- rbind(sets, sets_avail) 
  
  # add sites based on region
  cells_D <- cells[cells$latitude < -14.31 & cells$longitude > -170.67 & cells$strat == 23] ; dim(cells_D)
  cells_D$strat_sets <- case_when(cells_D$strat == 23 ~ 2)
  sets_D <- cells_D[, .SD[sample(3, strat_sets, replace = F)], by = c("strat")]
  sets <- sets %>% select(-depth_bin)
  # sets_D <- sets_D %>% select(c(x,y,longitude,latitude,depth,strat))
  sets <- rbind(sets, sets_D)
  
  cells_S <- cells[cells$latitude > -14.28 & cells$longitude > -170.75 & cells$longitude < -170.67 & cells$depth <=6] ; dim(cells_S)
  cells_S$strat_sets <- case_when(cells_S$strat == 1 ~ 1,
                                  cells_S$strat == 6 ~ 1,)
  sets_S <- cells_S[, .SD[sample(.N, strat_sets, replace = F)], by = c("strat")]
  sets <- sets %>% select(-depth_bin)
  # sets_S <- sets_S %>% select(c(x,y,longitude,latitude,depth,strat))
  sets <- rbind(sets, sets_S)
  
  
  #### Format sets dataframe ####
  
  
  # Count number of distinct sim*year*cell combinations
  sets[, `:=`(cell_sets, .N), by = c("cell")]
  sets$set <- seq(nrow(sets))
  sets = sets  %>% 
    mutate(id = id) %>% 
    dplyr::select(id, x, y, longitude, latitude, depth, strat)
  
  # Add depth bin
  sets$depth_bin = ""
  sets$depth_bin = ifelse(sets$depth >= 0  & sets$depth <= 6, "SHAL", sets$depth_bin)
  sets$depth_bin = ifelse(sets$depth > 6  & sets$depth <= 18, "MID", sets$depth_bin)
  sets$depth_bin = ifelse(sets$depth > 18, "DEEP", sets$depth_bin) 
  
  
  # Spot check site locations
  ggplot() + 
    geom_spatial_point(data = sets, aes(longitude,latitude, shape = depth_bin, fill = depth_bin), color = "black", size = 3, crs = 4326, show.legend = F) + 
    scale_fill_manual(name = "Depth", values = c("red", "goldenrod1", "green3")) + 
    # geom_label_repel(data = sets, aes(longitude, latitude, label = strat), fill = NA, point.padding = unit(.1, "lines")) +
    scale_shape_manual(name = "Depth", values = c(24,22,21)) +
    coord_sf(crs = 4326) +
    ggtitle("sets")
  

  
  # Format site codes
  if(region == "PRIAs") {    
    
    start_code_pria = c(2026,1052,2079,2121,5150,5134,2048) # Based on most recent fish codes. Start benthic codes AFTER
   id <- seq(1,dim(sets)[1],1) + start_code_pria[i]
   # sets$id = gsub("s",  islands[i], id)
    id_full = sprintf("s_%04d", id) # Add three-leter code to the site name
    sets$id_full = gsub("s",  toupper(islands[i]), id_full)
  #  sets$id = gsub("s",  islands[i], id_full)
    
    
  } else {
    
    start_code_samoa = c(1165,5030,837,1161,5280)
    id <- seq(1,dim(sets)[1],1) + start_code_samoa[i]
    sets$id = gsub("s",  islands[i], id)
    # id_full <- seq(1,dim(sets)[1],1) + start_code_samoa[i]
    id_full = sprintf("s_%04d", id)
    sets$id = id
    sets$id_full = gsub("s",  toupper(islands[i]), id_full)
    
  }
  
  # id = sprintf("s_%04d", id)
  # id = gsub("s",  islands[i], id)
  
  sets$depth <- sets$depth*3.28084
  

  
  #### Export set table ####
  
  readr::write_csv(sets, path = paste0("outputs/table/survey_table_", region, "_", islands[i], ".csv")) 
  
  # Add additional row to sets with odd numbers so they can be evenly split
  if(nrow(sets) %% 2 == 1){     
    
    blankrow <- data.frame(matrix(ncol = 9, nrow = 1))
    colnames(blankrow) <- colnames(sets)
    sets <- rbind(sets, blankrow)
    
  }
  
  # Format and split sets to create two columns   
  sets$depth <- round(sets$depth, digits = 2)
  sets_print <- select(sets, "id", "longitude","latitude","depth","depth_bin")
  
  sets1 <- data.frame(split(sets_print, factor(sort(rank(row.names(sets_print))%%3))))
  sets1 <- sets1[,1:5]
  colnames(sets1) <- colnames(sets_print)
  sets2 <- sets[83:164,]
  sets2 <- select(sets2, c("id", "longitude","latitude","depth","depth_bin"))
  sets3 <- sets[165:246,]
  sets3 <- select(sets3, c("id", "longitude","latitude","depth","depth_bin"))
  # sets2 <- anti_join(sets_print, sets1)
  # list = seq.int((nrow(sets1) + 1 ), nrow(sets1))
  list = seq.int(82)
  
  # Print table  
  if(dim(sets1)[1] < 70) {
    
    page_height = 15 # Margins for smaller site lists 
    page_width = 12 # Margins for smaller site lists 
    
  } else {
    
    page_height = 23 # Margins for larger site lists 
    page_width = 13 # Margins for larger site lists  
    
  }
 
  library(grid)
  library(gridExtra)
  pdf(paste0("outputs/table/survey_table_", region, "_", islands[i], ".pdf"), height = page_height, width = page_width)
  sets1 <- tableGrob(sets1)
  sets2 <- tableGrob(sets2)
  sets3 <- tableGrob(sets3)
  grid.arrange(rectGrob(), rectGrob(), ncol = 3)
  grid.arrange(sets1, sets2, sets3, nrow = 1, ncol = 3, newpage = F)
  dev.off()
  
  
  #### Visualize Island Inputs ####
  
  (bathymetry = cells %>% 
      ggplot(aes(x, y)) +
      geom_raster(aes(fill = depth)) + 
      coord_fixed())
  
  (strata = cells %>% #filter(depth > 18) %>%
      ggplot(aes(x, y)) +
      geom_raster(aes(fill = factor(strat))) + 
      coord_fixed())
  
  (variability = cells %>% 
      ggplot(aes(x, y)) +
      geom_raster(aes(fill = sd)) + 
      coord_fixed())
  
  (area = cells %>% 
      ggplot(aes(x, y)) +
      geom_raster(aes(fill = strat_area )) + 
      coord_fixed())
  
  
  #######################################
  ### Read Island shape and coastline ###
  #######################################
  island_name_code$Island_Code <- toupper(island_name_code$Island_Code)
  isl_shp = island_name_code %>% subset(Island_Code == toupper(islands[i]))
  ISL_this = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(isl_shp)),]
  # ISL_this_utm = spTransform(ISL_this,CRS(paste0("+proj=utm +units=km +zone=", zone)))
  # ISL_this_sf = st_transform(st_as_sf(ISL_this), crs = paste0("+proj=utm +units=km +zone=", zone))
  if (islands[i] == "tut") ISL_this = ISL_bounds[which(ISL_bounds$ISLAND %in% c(toupper(isl_shp), "AUNUU")),]
  if (islands[i] == "ofu") ISL_this = ISL_bounds[which(ISL_bounds$ISLAND %in% c(toupper(isl_shp), "OLOSEGA")),]
  
  # rose <- st_read("C:/Users/Corinne.Amir/Documents/REA/Random Site Maps/ASRAMP23/Shapefiles/ROS_coastline/Sector/ros_sector.shp")
  # rose <- st_transform(rose$geometry, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  # rose <- rose[1,]
  # rose_coords <- as.data.frame(sf::st_coordinates(rose)) 
 
  
  ######################################
  ### Read Island 5km buffer sectors ###
  ######################################
  load(paste0('data/gis_5km_buffer/', islands[i], '.RData'))
  buffer = raster_and_table[[1]]
  buffer_name = raster_and_table[[2]]
  buffer <- data.table(rasterToPoints(buffer))
  utmcoor <- SpatialPoints(cbind(buffer$x, buffer$y), proj4string = CRS(paste0("+proj=utm +units=m +zone=", utm_i$UTM_Zone, " ", utm_i$Hemishpere)))
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
  buffer_label = buffer %>% group_by(sector_nam) %>% summarise(longitude = quantile(longitude, 0.9), latitude = quantile(latitude, 0.9)) %>% filter(sector_nam != "TUT_PAGOPAGO")
  buffer = filter(buffer, sector_nam == "TUT_AUNUU_B" |sector_nam ==  "TUT_AUNUU_A" | sector_nam == "TUT_FAGATELE" | sector_nam == "TUT_FAGALUA")
  
  ################################################
  ### Island survey boxes and Google Map Stuff ###
  ################################################
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
    
    Switch = T
    
  } else {
    
    
    Switch = F
    
  }
  
  total_area = unique(cells$cell_area)*dim(cells)[1]
  
  map_direction = c("NW", "NE", "SW", "SE")
  
  map_list = list()
  
  for (d in 1:length(map_direction)) {
    
    # d = 1
    
    map_i = map_direction[d]
    
    space = 0.001
    
    if (map_direction[d] == "NW") ext = c(min(sets$longitude, na.rm = T) - space, median(sets$longitude, na.rm = T) + space, median(sets$latitude, na.rm = T) - space, max(sets$latitude, na.rm = T) + space)
    if (map_direction[d] == "NE") ext = c(median(sets$longitude, na.rm = T) - space, max(sets$longitude, na.rm = T) + space, median(sets$latitude, na.rm = T) - space, max(sets$latitude, na.rm = T) + space)
    if (map_direction[d] == "SW") ext = c(min(sets$longitude, na.rm = T) - space, median(sets$longitude, na.rm = T) + space, min(sets$latitude, na.rm = T) - space, median(sets$latitude, na.rm = T) + space)
    if (map_direction[d] == "SE") ext = c(median(sets$longitude, na.rm = T) - space, max(sets$longitude, na.rm = T) + space, min(sets$latitude, na.rm = T) - space, median(sets$latitude, na.rm = T) + space)
    
    sets_i = sets %>% subset(longitude > ext[1] & longitude < ext[2] & latitude > ext[3] & latitude < ext[4])
    
    # use coastline shp file
    tryCatch({
      
      ISL_this_i <- crop(ISL_this, extent(ext)); plot(ISL_this_i)
      
    }, error = function(e){
      
      print("No land shp available in this extent. Use full extent instead")
      ISL_this_i <- crop(ISL_this, extent(ISL_this)); plot(ISL_this_i)
      
    })
    
    # use ggmap
    tryCatch({
      
      map = get_map(location = c(mean(sets_i$longitude, na.rm = T), mean(sets_i$latitude, na.rm = T)),
                    maptype = "satellite",
                    zoom = utm_i$Satellite,
                    # color = "bw",
                    force = T)
      
    }, error = function(e){
      
      print("No sets available in this extent. Use full extent instead")
      map <- get_map(location = c(mean(sets$longitude, na.rm = T), mean(sets$latitude, na.rm = T)),
                     maptype = "satellite",
                     zoom = utm_i$Satellite,
                     # color = "bw",
                     force = T)
    })
    
    
    map_i = 
      
      # gglot() +
      ggmap(map) +
      
      geom_polygon(data = ISL_this_i, aes(long, lat, group = group), fill = "darkgrey", color = NA, alpha = 0.9) + # land shapefile
      
      # display if there is more than 1 island sector
      
      {if (length(unique(buffer$sector_nam)) > 1) {
        
        geom_tile(data = buffer, aes(longitude, latitude, fill = sector_nam), width = 0.001, height = 0.001, alpha = 0.3, show.legend = T)}
        
      } + 
      
      {if (length(unique(buffer$sector_nam)) > 1) {
        
        geom_label_repel(data = buffer_label, 
                         aes(longitude, latitude, label = sector_nam, fill = sector_nam, fontface = 'bold'), 
                         alpha = 0.5, color = "white", max.overlaps = Inf, show.legend = F)}
        
      } + 
      
      scale_fill_discrete("") +
      scale_color_discrete("") +
      
      new_scale_color() +
      new_scale_fill() + 
      
      geom_spatial_point(data = sets_i, aes(longitude, latitude, shape = depth_bin, fill = depth_bin), size = 5, crs = 4326) +
      # geom_point(data = sets_i, aes(longitude, latitude, shape = depth_bin, fill = depth_bin), size = 5) + 
      
      scale_fill_manual(name = "Depth", values = c("red", "goldenrod1", "green3"), na.translate = F) + 
      scale_shape_manual(name = "Depth", values = c(24, 22, 21), na.translate = F) +
      annotation_scale(location = "br", width_hint = 0.2) +
      
      geom_label_repel(data = sets_i, 
                       aes(longitude, latitude, label = id),
                       size = 5,
                       label.size = NA, 
                       alpha = 0.75, 
                       fontface = 'bold', 
                       color = 'black',
                       max.overlaps = Inf,
                       segment.size = 0.2,
                       box.padding = unit(0.8, "lines"),
                       point.padding = unit(0.3, "lines")) +
      
      ggtitle(paste0(map_direction[d])) + 
      theme(plot.title = element_text(size = 30, face = "bold")) + 
      
      coord_sf(crs = 4326) + 
      
      scale_x_continuous(sec.axis = dup_axis(), "", limits = ext[1:2]) +
      scale_y_continuous(sec.axis = dup_axis(), "", limits = ext[3:4]) 
    
    pdf(paste0("outputs/map/survey_map_", region, "_", islands[i], "_", map_direction[d], ".pdf"), height = 21.75, width = 15.75)
    print(map_i)
    dev.off()
    
    map_list[[length(map_list)+1]] = map_i
    
  }
  
  # Get map
  ext = c(min(sets$longitude, na.rm = T) - 0.01, max(sets$longitude, na.rm = T) + 0.01, min(sets$latitude, na.rm = T) - 0.01, max(sets$latitude, na.rm = T) + 0.01)
  # map <- get_map(location = c(left = ext[1], bottom = ext[3], right = ext[2], top = ext[4]), maptype = 'satellite')
  map <- get_map(location = c(mean(sets$longitude, na.rm = T),
                              mean(sets$latitude, na.rm = T)), 
                 zoom = utm_i$Satellite,
                 maptype = 'satellite')
  
  
  #######################
  #### Add MPA boxes ####
  #######################
  # mpa_swa <- st_read("C:/Users/Corinne.Amir/Documents/REA/Random Site Maps/ASRAMP23/Shapefiles/SWA_mpa/SWA_sector.shp") #
  # mpa_swa <- st_transform(mpa_swa$geometry, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  # mpa_swa_coords <- as.data.frame(sf::st_coordinates(mpa_swa)) # Extract coordinates from shapefiles
  # mpa_swa_coords <- mpa_swa_coords %>% filter(L2 == 2 | L2 == 3) 
  # mpa_swa_coords$mpa <- replicate(n = nrow(mpa_tau_coords), "Tau Island Sanctuary Unit")

  mpa_tau <- st_read("C:/Users/Corinne.Amir/Documents/REA/Random Site Maps/ASRAMP23/Shapefiles/TAU_mpa/TAU_sector.shp") #
  mpa_tau <- st_transform(mpa_tau$geometry, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  mpa_tau <- mpa_tau[2,]
  mpa_tau_coords <- as.data.frame(sf::st_coordinates(mpa_tau)) # Extract coordinates from shapefiles
  mpa_tau_coords$mpa <- replicate(n = nrow(mpa_tau_coords), "Tau Island Sanctuary Unit")
  
  setwd("C:/Users/Corinne.Amir/Documents/GitHub/ncrmp_common_maps/")
  
  
  ##################################
  ### Read Tutuilla survey boxes ###
  ##################################
  require(rgdal)
  library(magrittr)
  shp_path <- "C:/Users/Corinne.Amir/Documents/REA/Random Site Maps/ASRAMP23/Shapefiles/TUT_surveybox"
  box <- readOGR(dsn = shp_path)
  box <- st_as_sf(box)
  
  box_label <- st_read(dsn = shp_path) # Extract box labels
  centroid <- st_centroid(box_label)
  centroid <- centroid %>%
    dplyr::mutate(long = sf::st_coordinates(.)[,1],
                  lat = sf::st_coordinates(.)[,2])
  
  setwd("C:/Users/Corinne.Amir/Documents/GitHub/ncrmp_common_maps/")
  
  ############################
  #### Add Tutuilla inset ####
  ############################
  (tut_inset =
     
     ggplotGrob(
       
       ggplot() +
         
         geom_path(data = ISL_this, aes(long, lat, group = group), inherit.aes = F, size = 0.01, color = "darkgrey") + # coastline
         geom_polygon(data = ISL_this, aes(long, lat, group = group), fill = "darkgrey", color = NA, alpha = 0.9) + # land shapefile
         
         #geom_tile(data = buffer, aes(longitude, latitude, fill = color), width = 0.001, height = 0.001, alpha = 0.3, show.legend = F) +
         
         scale_fill_identity() +
         scale_color_identity() +
         
         # {if(Switch) geom_polygon(data = boxes_hulls, aes(longitude, latitude, fill = boxes_nam, color = "black"), alpha = 0.01, size = 1, show.legend = F)} + # survey boxes hull
         # {if(Switch) geom_text_repel(data = boxes_label, aes(longitude, latitude, label = boxes_nam, color = "black", fontface = 'bold'), max.overlaps = Inf, show.legend = F)} +
         
         geom_sf(data = box, fill = NA, color = "black", size = 1) +
         geom_label(data = centroid, aes(long,lat,label = Box_Name), fill = NA, fontface = "plain", label.size = NA, size = 6) +
       
         # geom_rect(aes(xmin = -170.58, xmax = -170.52, ymin = -14.31, ymax = -14.26), fill = NA, size = 1, color = "black") + # Aunu'u
         # geom_rect(aes(xmin = -170.72, xmax = -170.56, ymin = -14.32, ymax = -14.2), fill = NA, size = 1, color = "black") + # East
         
         annotation_scale(location = "br", width_hint = 0.2) +
         coord_sf(crs = 4326) +
         
         theme_bw() +
         theme(axis.ticks.x=element_blank(),
               axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.y=element_blank(),
               axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               panel.grid.major = element_blank(), 
               panel.grid.minor = element_blank(),
               panel.background = element_blank())
     )
   
  )
  
  
  ###############################
  ###### Split Tutuila Map ######
  ###############################
  #### Northwest region ####
  ISL_this <- crop(ISL_this, extent(-170.88,-170.69, -14.325 , -14.21))
  
  buffer = buffer %>% subset(longitude > -170.88 &
                               longitude < -170.69 &
                               latitude > -14.325 &
                               latitude < -14.21)
  
  buffer_label[buffer_label$sector_nam =="TUT_NW_OPEN", "longitude"] <- -170.77
  buffer_label[buffer_label$sector_nam =="TUT_NE_OPEN", "longitude"] <- -170.702
  buffer_label[buffer_label$sector_nam =="TUT_NE_OPEN", "latitude"] <- -14.241
  # buffer_label[buffer_label$sector_nam =="TUT_PAGOPAGO", "longitude"] <- -170.726
  # buffer_label[buffer_label$sector_nam =="TUT_PAGOPAGO", "latitude"] <- -14.31
  buffer_label = buffer_label %>% subset(longitude > -170.88 &
                                           longitude < -170.69 &
                                           latitude > -14.325 &
                                           latitude < -14.21)
  
  boxes_label = boxes_label %>% subset(longitude > -170.88 &
                                         longitude < -170.69 &
                                         latitude > -14.325 &
                                         latitude < -14.21)

  box <- subset(box, Box_Name == "A" | Box_Name == "B" )
  centroid$long <- ifelse(centroid$Box_Name == "A", -170.732, centroid$long)
  centroid$lat <- ifelse(centroid$Box_Name == "A", -14.269, centroid$lat)
  centroid <- centroid %>% filter(Box_Name == "A")

  sets_nw = sets %>% subset(longitude > -170.88 &
                                 longitude < -170.69 &
                                 latitude > -14.325 &
                                 latitude < -14.21)
  
  #### South region ####
  ISL_this <- crop(ISL_this, extent(-170.88,-170.7581, -14.4 , -14.315))
  
  buffer = buffer %>% subset(longitude > -170.88 &
                               longitude < -170.7581 &
                               latitude >  -14.4 &
                               latitude < -14.315)
  
  buffer_label[buffer_label$sector_nam =="TUT_SW_OPEN", "latitude"] <- -14.35
  buffer_label[buffer_label$sector_nam =="TUT_SW_OPEN", "longitude"] <- -170.83
  buffer_label[buffer_label$sector_nam =="TUT_FAGATELE", "latitude"] <- -14.349
  buffer_label[buffer_label$sector_nam =="TUT_FAGATELE", "longitude"] <- -170.76
  buffer_label = buffer_label %>% subset(longitude > -170.88 &
                                           longitude < -170.7597 &
                                           latitude >  -14.4 &
                                           latitude < -14.315)
  
  
  box <- subset(box, Box_Name == "A" | Box_Name == "B" )
  
  centroid <- centroid %>% filter(Box_Name == "A")
  # boxes_hulls = boxes_hulls %>% subset(longitude > -170.88 &
  #                                        longitude < -170.69 &
  #                                        latitude > -14.325 &
  #                                        latitude < -14.21) 
  # 
  # boxes_label = boxes_label %>% subset(longitude > -170.88 &
  #                                        longitude < -170.69 &
  #                                        latitude > -14.325 &
  #                                        latitude < -14.21)
  
  sets_s = sets %>% subset(longitude > -170.88 &
                              longitude < -170.7581 &
                              latitude >  -14.4 &
                              latitude < -14.315)
  
  #### Southwest region ####
  ISL_this <- crop(ISL_this, extent(-170.88,-170.66, -14.4 , -14.315))
  
  buffer = buffer %>% subset(longitude > -170.88 &
                               longitude < -170.66 &
                               latitude >  -14.4 &
                               latitude < -14.315)
  
  buffer_label[buffer_label$sector_nam =="TUT_SW_OPEN", "latitude"] <- -14.35
  buffer_label[buffer_label$sector_nam =="TUT_SW_OPEN", "longitude"] <- -170.83
  buffer_label[buffer_label$sector_nam =="TUT_FAGATELE", "latitude"] <- -14.357
  buffer_label[buffer_label$sector_nam =="TUT_FAGATELE", "longitude"] <- -170.763
  buffer_label[buffer_label$sector_nam =="TUT_FAGALUA", "latitude"] <- -14.376
  buffer_label[buffer_label$sector_nam =="TUT_FAGALUA", "longitude"] <- -170.74
  buffer_label[buffer_label$sector_nam =="TUT_SE_OPEN", "latitude"] <- -14.36
  buffer_label[buffer_label$sector_nam =="TUT_SE_OPEN", "longitude"] <- -170.7
  buffer_label = buffer_label %>% subset(longitude > -170.88 &
                                           longitude < -170.6 &
                                           latitude >  -14.4 &
                                           latitude < -14.315)
  
  boxes_hulls = boxes_hulls %>% subset(longitude > -170.88 &
                                         longitude < -170.69 &
                                         latitude > -14.325 &
                                         latitude < -14.21) 
  
  boxes_label = boxes_label %>% subset(longitude > -170.88 &
                                         longitude < -170.69 &
                                         latitude > -14.325 &
                                         latitude < -14.21)
  
  sets_sw = sets %>% subset(longitude > -170.88 &
                              longitude < -170.66 &
                              latitude >  -14.4 &
                              latitude < -14.315)
  
  #### West Coast region ####
  ISL_this <- crop(ISL_this, extent(-170.88,-170.7581, -14.4 , -14.315))
  
  buffer = buffer %>% subset(longitude > -170.88 &
                             longitude > -170.69 &
                             latitude >  -14.4 &
                             latitude < -14.315)
  
  boxes_hulls = boxes_hulls %>% subset(longitude > -170.88 &
                                         longitude < -170.69 &
                                         latitude > -14.325 &
                                         latitude < -14.21) 
  box <- crop(box, extent(-170.88,-170.7581, -14.4 , -14.315))
  
  boxes_label = boxes_label %>% subset(longitude > -170.88 &
                                         longitude < -170.69 &
                                         latitude > -14.325 &
                                         latitude < -14.21)
  centroid = centroid %>% subset(long > -170.88 &
                                   long < -170.7581 &
                                   lat > -14.4 &
                                   lat < -14.315)
  
  
  sets_s = sets %>% subset(longitude > -170.88 &
                             longitude < -170.7581 &
                             longitude < -170.758 &
                             latitude >  -14.4 &
                             latitude < -14.315)
  
  #### Fagatele/Fagaalu region ####
  ISL_this <- crop(ISL_this, extent(-170.772,-170.74, -14.4 , -14.355))
                               
  buffer = buffer %>% subset(longitude > -170.772 &
                             longitude < -170.738 &
                             latitude >  -14.4 &
                             latitude < -14.315)
  
  buffer_label[buffer_label$sector_nam =="TUT_FAGATELE", "latitude"] <- -14.3588
  buffer_label[buffer_label$sector_nam =="TUT_FAGATELE", "longitude"] <- -170.7641
  buffer_label[buffer_label$sector_nam =="TUT_FAGALUA", "latitude"] <- -14.3562161
  buffer_label[buffer_label$sector_nam =="TUT_FAGALUA", "longitude"] <- -170.751627
  buffer_label = buffer_label %>% subset(longitude > -170.779 &
                                           longitude < -170.73 &
                                           latitude >  -14.4 &
                                           latitude < -14.354)
  
  buffer_label[buffer_label$sector_nam =="TUT_SW_OPEN", "latitude"] <- -14.35
  buffer_label[buffer_label$sector_nam =="TUT_SW_OPEN", "longitude"] <- -170.83
  buffer_label[buffer_label$sector_nam =="TUT_FAGATELE", "latitude"] <- -14.357
  buffer_label[buffer_label$sector_nam =="TUT_FAGATELE", "longitude"] <- -170.763
  buffer_label[buffer_label$sector_nam =="TUT_FAGALUA", "latitude"] <- -14.376
  buffer_label[buffer_label$sector_nam =="TUT_FAGALUA", "longitude"] <- -170.74
  # Remove survey box and labels
  
  
  sets_ff = setsa %>% subset(longitude > -170.772 &
                              longitude < -170.738 &
                              latitude >  -14.4 &
                              latitude < -14.358)
  
  
  
  
  #### Aunu'u region ####
  ISL_this <- crop(ISL_this, extent(-170.62,-170.38, -14.31, -14.2))
  
  buffer = buffer %>% subset(longitude > -170.6 &
                               longitude < -170.38 &
                               latitude >  -14.31 &
                               latitude <  -14.26)
  
  buffer_label[buffer_label$sector_nam =="TUT_NE_OPEN", "latitude"] <- -14.25
  buffer_label[buffer_label$sector_nam =="TUT_NE_OPEN", "longitude"] <- -170.54
  buffer_label[buffer_label$sector_nam =="TUT_AUNUU_A", "latitude"] <- -14.303
  buffer_label[buffer_label$sector_nam =="TUT_AUNUU_A", "longitude"] <- -170.557
  buffer_label[buffer_label$sector_nam =="TUT_AUNUU_B", "latitude"] <- -14.265
  buffer_label[buffer_label$sector_nam =="TUT_AUNUU_B", "longitude"] <- -170.539
  buffer_label[buffer_label$sector_nam =="TUT_SE_OPEN", "latitude"] <- -14.294
  buffer_label[buffer_label$sector_nam =="TUT_SE_OPEN", "longitude"] <- -170.60
  buffer_label = buffer_label %>% subset(longitude > -170.58 &
                                           longitude < -170.38 &
                                           latitude >  -14.31 &
                                           latitude <  -14.26)
  
  box <- subset(box, Box_Name == "C")
  centroid <- centroid %>% filter(Box_Name == "C")
  
  sets_a = sets %>% subset(longitude > -170.62 &
                              longitude < -170.38 &
                              latitude >  -14.31 &
                              latitude <  -14.2)
  
  #### East region ####
  ISL_this <- crop(ISL_this, extent(-170.72,-170.56, -14.32, -14.2))
  
  buffer = buffer %>% subset(longitude > -170.72 &
                               longitude < -170.55 &
                               latitude >  -14.32 &
                               latitude <  -14.2)
  
  buffer_label[buffer_label$sector_nam =="TUT_NE_OPEN", "latitude"] <- -14.24
  buffer_label[buffer_label$sector_nam =="TUT_NE_OPEN", "longitude"] <- -170.63
  buffer_label[buffer_label$sector_nam =="TUT_SE_OPEN", "latitude"] <- -14.31
  buffer_label[buffer_label$sector_nam =="TUT_SE_OPEN", "longitude"] <- -170.64
  buffer_label = buffer_label %>% subset(longitude > -170.72 &
                                           longitude < -170.55 &
                                           latitude >  -14.32 &
                                           latitude <  -14.2)
  
  boxes_hulls = boxes_hulls %>% subset(longitude > -170.72 &
                                         longitude < -170.37 &
                                         latitude >  -14.32 &
                                         latitude <  -14.2) 
  
  boxes_label = boxes_label %>% subset(longitude > -170.72 &
                                         longitude < -170.56 &
                                         latitude >  -14.32 &
                                         latitude <  -14.2)
  
  sets_e = sets %>% subset(longitude > -170.72 &
                              longitude < -170.55 &
                              latitude >  -14.32 &
                              latitude <  -14.2)
  #####################
  ######################################
  #### Load previously created maps ####
  #####################################
  
  setwd("C:/Users/Corinne.Amir/Documents/REA/Random Site Maps/ASRAMP23/tables/")
  sets <- read.csv("survey_table_PRIAs_bak_1-20-2023.csv")
  sets <- read.csv("survey_table_PRIAs_how_1-20-2023.csv")
  sets <- read.csv("survey_table_SAMOA_ofu.csv")
  sets <- read.csv("survey_table_SAMOA_tau.csv")
  
  
  setwd("C:/Users/Corinne.Amir/Documents/GitHub/ncrmp_common_maps/")
  
  
  ################################
  ####### Create final map #######
  ################################
   (whole_map = 
      
      ggplot() + 
      
      # ggmap(map) + 
      # geom_tile(data = cells, aes(longitude, latitude, fill = factor(strat)), alpha = 0.5, width = 0.001, height = 0.001) + # stratum
      
      {if ( length(unique(buffer$sector_nam)) > 1) {

        geom_tile(data = buffer, aes(longitude, latitude, fill = sector_nam), width = 0.001, height = 0.001, alpha = 0.3, show.legend = F)}

      } +
      
      geom_path(data = ISL_this, aes(long, lat, group = group), inherit.aes = F, size = 1, color = "black") + # coastline
      geom_polygon(data = ISL_this, aes(long, lat, group = group), fill = "darkgrey", color = NA, alpha = 0.9) + # land shapefile
      
      # scale_fill_viridis_d("Strata") +
      # scale_color_viridis_d("Strata") +
      
      new_scale_color() +
      new_scale_fill() +

      # island sectors
      {if ( length(unique(buffer$sector_nam)) > 1) {

        geom_label_repel(data = buffer_label, aes(longitude, latitude, label = sector_nam, fill = sector_nam, fontface = 'bold'), color = "white", size = 8, max.overlaps = Inf, show.legend = F)}

      } +

      scale_fill_discrete("") +
      scale_color_discrete("") +

      new_scale_color() +
      new_scale_fill() +
      
      # geom_tile(data = boxes, aes(longitude, latitude, fill = boxes_nam), width = 0.001, height = 0.001, alpha = 0.2, show.legend = F) + # survey boxes fill
      # {if(Switch) geom_polygon(data = boxes_hulls, aes(longitude, latitude, fill = boxes_nam, color = boxes_nam), alpha = 0.01, size = 1, show.legend = F)} + # survey boxes hull
      # {if(Switch) geom_text_repel(data = boxes_label, aes(longitude, latitude, label = boxes_nam, color = boxes_nam, fontface = 'bold'), max.overlaps = Inf, show.legend = F)} +
      # {if(Switch) scale_fill_discrete()} +
      # {if(Switch) scale_color_discrete()} +
      # {if(Switch) new_scale_color()} +
      # {if(Switch) new_scale_fill()} +
    
      # geom_sf(data = box, fill = NA, color = "black", size = 1) +
      # # geom_polygon(data = box, aes(X, Y), fill = NA, color = "black", size = 1) + # TUT survey boxes
      # geom_label(data = centroid, aes(long,lat,label = Box_Name), fill = NA, fontface = "bold", label.size = NA, size = 8) +
    
      # geom_path(data = rose_coords, aes(X, Y, group = L1), color = "black") +
      # geom_path(data = mpa_swa_coords, aes(X, Y,  group = L2, color = "black")) +
      # scale_color_manual(name = "MPA", values = "black", label = paste0("non-MPA"), na.translate = F) +
      # geom_path(data = mpa_tau_coords, aes(X, Y,  group = L1, color = "black")) + # MPA boxes
      # scale_color_manual(name = "MPA", values = "black", label = paste0("Tau Island" ,"\n","Sanctuary", "\n", "Unit"), na.translate = F) +

       # geom_point(data = sets, aes(longitude, latitude, shape = depth_bin, color = depth_bin)) +
       geom_spatial_point(data = sets, aes(longitude, latitude, shape = depth_bin, fill = depth_bin), size = 4, crs = 4326) +
       scale_fill_manual(name = "Depth", values = c("red", "goldenrod1", "green3"), na.translate = F) + # in geom_spatial_point make size = 9 ONLY for Guam
       scale_shape_manual(name = "Depth", values = c(24, 22, 21), na.translate = F) +
       annotation_scale(location = "bl", width_hint = 0.2, height = unit(0.7, "cm"), text_cex = 1) + # not working with google map
       new_scale_fill() +

       geom_label_repel(data = sets,
                     aes(longitude, latitude, label = id),
                     fill = NA,
                     size = 4,
                     label.size = NA,
                     alpha = 0.75,
                     fontface = 'bold',
                     color = 'black',
                     max.overlaps = Inf,
                     segment.size = 0.5,
                     direction = "both",
                     # nudge_y = 0.005,
                     # nudge_x = 0.005,
                     # box.padding = unit(0.3, "lines"),
                     point.padding = unit(0.19, "lines")) +
    
      # coord_fixed() +
      # coord_map() + 
      coord_sf(crs = 4326) + 
      
      # scale_x_continuous(sec.axis = dup_axis(), "", limits = range(pretty(buffer$longitude))) + # Palmyra
      # scale_y_continuous(sec.axis = dup_axis(), "", limits = range(pretty(buffer$latitude))) +
      # scale_y_continuous(labels = unicode_minus) +
      # ylim(-14.31,-14.243) + # TUT_NW = (-14.33,-14.23), PAL = (5.855,5.9), KIN = (6.375,6.443), BAK = (.184,.21), ROS = (.184,.21), SWA = (-11.07,-11.042)
      # xlim(-170.6,-170.51) + # TUT_NW = (-170.865,-170.69), BAKER = (-176.491,-176.455), OFU = (-169.689,-169.596), TAU = (-169.52,-169.4), SWA = (-171.095,-171.061)
      
      # # TUTUILLA ONLY: Add map insets 
      # annotation_custom(grob = tut_inset, xmin = -170.873, xmax = -170.81, ymin = -14.27, ymax = -14.212) + # Northwest
      # annotation_custom(grob = tut_inset, xmin = -170.54, xmax = -170.502, ymin = -14.313, ymax = -14.299) + # Aunu'u
      # annotation_custom(grob = tut_inset, xmin = -170.57, xmax =-170.52, ymin = -14.321, ymax = -14.305) + # East
      # annotation_custom(grob = tut_inset, xmin = -170.7495, xmax = -170.738, ymin = -14.376, ymax = -14.3705) + # Fagalua/Fagatele
    
      
      #theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            panel.border = element_rect(color = "black", fill = NA, size = 1)) +
      
      theme(legend.position = c(0.94,0.93), # SWA = (0.93,0.89), PAL = (0.95,0.88), TUT_SW = (0.95,0.9), ROS = (0.92,0.93), KIN & other TUT = (0.95,0.94),
            # legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12),
            axis.text.y = element_text(angle = 90, vjust=1, hjust=.5),
            axis.text = element_text(size = 10),
            # axis.title = element_text(size = 10),
            legend.title = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank())  + 
      
      # theme(legend.position = "bottom",
      #       axis.text = element_text(size = 10),
      #       axis.title = element_text(size = 10)) +
      # theme(plot.subtitle=element_text(size=15, hjust=0, color="black")) +
     
      labs(title =  paste0("2023 Benthic REA: ", toupper(as.character(isl_shp[1])), " (Aunu'u)"))

   # labs(title = "",
   #      subtitle = paste0(paste0("2023 Fish REA", "\n",
   #       "Island = ", toupper(as.character(isl_shp[1]))) #,"\n",
         # "Region = SOUTHWEST", "\n", # TUTUILLA ONLY
         # "Total survey effort = ",
       # "Number of strata = ", length(unique(cells$strat)), "\n",
       # "Target survey effort = ", total_sample, " sites \n",
        # nrow(sets), " sites")))
   )
    
  
  pdf(paste0("outputs/map/survey_map_", region, "_", islands[i], ".pdf"), paper = 'USr', height = 21.75, width = 15.75)
  # paper = 'USr',
  print(whole_map)
  
  library(pdftools)
  pdf_combine(c(paste0("outputs/map/survey_map_", region, "_", islands[i], ".pdf"), 
               # paste0("outputs/map/survey_map_", region, "_", islands[i], "_NE.pdf"),
               # paste0("outputs/map/survey_map_", region, "_", islands[i], "_NW.pdf"),
               # paste0("outputs/map/survey_map_", region, "_", islands[i], "_SE.pdf"),
               # paste0("outputs/map/survey_map_", region, "_", islands[i], "_SW.pdf"),
               paste0("outputs/table/survey_table_", region, "_", islands[i], ".pdf")),
               output = paste0("outputs/map/survey_map_table_", region, "_", islands[i], ".pdf"))
  
  dev.off()
  
  # file.remove(paste0("outputs/map/survey_map_", region, "_", islands[i], ".pdf"))
  # file.remove(paste0("outputs/map/survey_map_", region, "_", islands[i], "_NE.pdf"))
  # file.remove(paste0("outputs/map/survey_map_", region, "_", islands[i], "_NW.pdf"))
  # file.remove(paste0("outputs/map/survey_map_", region, "_", islands[i], "_SE.pdf"))
  # file.remove(paste0("outputs/map/survey_map_", region, "_", islands[i], "_SW.pdf"))
  # file.remove(paste0("outputs/table/survey_table_", region, "_", islands[i], ".pdf"))
  
# }

#### Remove overlapping sites that previous code did not catch ####
  
sets <- sets %>% filter(id != 5299) %>% filter(id != 5399) %>% filter(id != 5513) %>% filter(id != 5463) %>% 
                 filter(id != 5467) %>% filter(id != 5315) %>% filter(id != 5303) %>% filter(id != 5293) %>%
                 filter(id != 5293) 
count(sets, strat)
# RE-EXPORT SET TABLE!!!

