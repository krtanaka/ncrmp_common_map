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
max_sets = 40
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

# islands = c("gua", "rot", "sai", "tin", "agu"); region = "S.MARIAN"                           # South Mariana Islands
# islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "pag", "sar"); region = "N.MARIAN"      # North Mariana Islands
islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                              # American Samoa
# islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"                # Pacific Remote Island Areas
# islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"    # Main Hawaiian Islands
# islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"                 # Northern Hawaiian Islands

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

# for (i in 1:length(islands)) {

i = 5

# survey domain with sector & reef & hard_unknown & 3 depth bins
load(paste0("data/survey_grid_ncrmp/survey_grid_", islands[i], ".RData"))#; plot(survey_grid_ncrmp)

cat(paste0("generating survey site for ", islands[i], "...\n"))

total_sample = survey_effort %>% subset(Island_Code == islands[i])

if (dim(total_sample)[1] == 0){
  
  total_sample = 100
  
} else {
  
  total_sample = total_sample$Effort*2
  
}

total_sample = 50

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

nearby_sites <- remove_close_points(nearby_sites, 50)#; points(nearby_sites, pch = 20, col = 4)

colnames(nearby_sites) = c("longitude", "latitude")
nearby_sites$latitude = round(nearby_sites$latitude, 4)
nearby_sites$longitude = round(nearby_sites$longitude, 4)

sets$latitude = round(sets$latitude, 4)
sets$longitude = round(sets$longitude, 4)

cat(paste0("removing ", nrow(sets) - nrow(nearby_sites), " sites to maintain a minimum distance of 100 m between each site...\n"))

sets = inner_join(sets, nearby_sites)

# remove sites east of -170.705
sets = sets %>% filter(longitude <= -170.705)

#TUT-6061 and above
id = sprintf("s_%04d", seq(6061, 6061 + dim(sets)[1] - 1, 1))
id = gsub("s",  islands[i], id)
id = paste0(id, "A")

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

bathymetry = cells %>% 
  ggplot(aes(x, y)) +
  geom_raster(aes(fill = depth)) + 
  # coord_fixed() +
  theme_map() + 
  scale_fill_viridis_c("Depth (m)", direction = -1) + 
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

png(paste0("outputs/maps/survey_layers_", islands[i], ".png"), height = 5, width = 15, res = 500, units = "in")
print(bathymetry + variability + area)
dev.off()

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

d = 2

map_i = map_direction[d]

space = 0.01

if (map_direction[d] == "NW") ext = c(min(sets$longitude, na.rm = T) - space, median(sets$longitude, na.rm = T) + space, median(sets$latitude, na.rm = T) - space, max(sets$latitude, na.rm = T) + space)
if (map_direction[d] == "NE") ext = c(median(sets$longitude, na.rm = T) - space, max(sets$longitude, na.rm = T) + space, median(sets$latitude, na.rm = T) - space, max(sets$latitude, na.rm = T) + space)
if (map_direction[d] == "SW") ext = c(min(sets$longitude, na.rm = T) - space, median(sets$longitude, na.rm = T) + space, min(sets$latitude, na.rm = T) - space, median(sets$latitude, na.rm = T) + space)
if (map_direction[d] == "SE") ext = c(median(sets$longitude, na.rm = T) - space, max(sets$longitude, na.rm = T) + space, min(sets$latitude, na.rm = T) - space, median(sets$latitude, na.rm = T) + space)

sets_i = sets %>% subset(longitude > ext[1] & longitude < ext[2] & latitude > ext[3] & latitude < ext[4])

ISL_this_i <- crop(ISL_this, extent(ext))

map = get_map(location = c(mean(sets_i$longitude, na.rm = T), mean(sets_i$latitude, na.rm = T)),
              maptype = "satellite",
              zoom = 15,
              # color = "bw",
              force = T)

map_i = 
  ggmap(map) +
  geom_spatial_point(data = sets_i, aes(longitude, latitude, shape = depth_bin, fill = depth_bin), size = 5, crs = 4326) +
  scale_fill_manual(name = "", values = c("red", "goldenrod1", "green3"), na.translate = F) + 
  scale_shape_manual(name = "", values = c(24, 22, 21), na.translate = F) +
  annotation_scale(location = "br", width_hint = 0.2, text_col = "white", bar_cols = "white", size = 10) +  # new_scale_color() +
  geom_label_repel(data = sets_i, 
                   aes(longitude, latitude, label = SITE_NO),
                   size = 5,
                   label.size = NA, 
                   alpha = 0.75, 
                   fontface = 'bold', 
                   color = 'black',
                   max.overlaps = Inf,
                   segment.color = "white",
                   box.padding = unit(0.8, "lines"),
                   point.padding = unit(0.3, "lines")) +
  coord_sf(crs = 4326) + 
  scale_x_continuous(sec.axis = dup_axis(), "", limits = c(-170.718, -170.701)) +
  scale_y_continuous(sec.axis = dup_axis(), "", limits = c(-14.327, -14.315))

map_i

pdf(paste0("outputs/maps/survey_map_", region, "_", islands[i], ".pdf"), height = 17, width = 22)
print(map_i)
dev.off()
