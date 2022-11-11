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

utm = read_csv('data/misc/ncrmp_utm_zones.csv')


##################################
###  select islands & regions  ###
##################################

islands = c("gua", "rot", "sai", "tin", "agu"); region = "S.MARIAN"                           # South Mariana Islands
islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "pag", "sar"); region = "N.MARIAN"      # North Mariana Islands
islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                              # American Samoa
islands = c("bak", "how", 
            # "jar", # shapeifle projections do not match with bathymetry projections
            "joh", "kin", "pal", "wak"); region = "PRIAs"                # Pacific Remote Island Areas
# islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"  # Main Hawaiian Islands
# islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"               # Northern Hawaiian Islands


########################################################################
### do some parameter settings to simulate stratified random surveys ###
########################################################################

# n_sims = 100 # number of simulations
effort_level = c("low", "mid", "high")[2] # define sampling effort (low, mid, high)
min_sets = 1 # minimum number of sets per strat
max_sets = 30
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

set.seed(2022)

#################################################################
### Generate survey site tables & maps, check outputs/ folder ###
#################################################################

for (i in 1:length(islands)) {
  
  # i = 4
  
  # survey domain with sector & reef & hard_unknown & 3 depth bins
  load(paste0("data/survey_grid_ncrmp/survey_grid_", islands[i], ".RData")); plot(survey_grid_ncrmp)
  
  total_sample = survey_effort %>% subset(Island_Code == islands[i])
  
  if (dim(total_sample)[1] == 0){
    
    total_sample = 100
    
  } else {
    
    total_sample = total_sample$Effort*2
    
  }
  
  n <- id <- division <- strat <- N <- strat_sets <- cell_sets <- NULL
  
  cells <- data.table(rasterToPoints(survey_grid_ncrmp))
  
  # add modeled trophic biomass variability, summarize by strata
  load(paste0("data/rea/modeled_survey_variability_", region, ".RData")) # modeled at original grid scale
  cells$sd = predict(g, cells); sd = cells[,c("strat", "sd")]; sd = sd %>% group_by(strat) %>% summarise(sd = mean(sd, na.rm = T))
  
  strat_det <- cells[, list(strat_cells = .N), by = "strat"]; strat_det
  strat_det$tow_area <- prod(trawl_dim); strat_det
  strat_det$cell_area <- prod(res(survey_grid_ncrmp)); strat_det
  strat_det$strat_area <- strat_det$strat_cells * prod(res(survey_grid_ncrmp)); strat_det
  strat_det = right_join(strat_det, sd); strat_det
  
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
  strat_det = strat_det[,c("strat", "strat_cells", "tow_area", "cell_area", "strat_area", "strat_sets")]
  cells <- merge(cells, strat_det, by = c("strat")) 
  
  utm_i = utm %>% subset(Island_Code == islands[i])
  
  utmcoor <- SpatialPoints(cbind(cells$x, cells$y), proj4string = CRS(paste0("+proj=utm +units=km +zone=", utm_i$UTM_Zone)))
  longlatcoor <- spTransform(utmcoor,CRS("+proj=longlat"))
  cells$longitude <- coordinates(longlatcoor)[,1]
  cells$latitude <- coordinates(longlatcoor)[,2]
  
  # subset "cells" to create site locations
  sets <- cells[, .SD[sample(.N, size = unique(strat_sets), replace = resample_cells)], 
                by = c("strat")]
  
  id <- seq(1,dim(sets)[1],1)
  id = sprintf("s_%04d", id)
  id = gsub("s",  islands[i], id)
  
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
  
  page_height = ifelse(dim(sets)[1] > 30, dim(sets)[1]/3, dim(sets)[1])
  
  library(gridExtra)
  pdf(paste0("outputs/table/survey_table_", region, "_", islands[i], ".pdf"), height = page_height, width = 10)
  grid.table(sets)
  dev.off()
  
  (bathymetry = cells %>% 
      ggplot(aes(x, y)) +
      geom_raster(aes(fill = depth)) + 
      coord_fixed())
  
  (strata = cells %>% 
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
  buffer_label = buffer %>% group_by(sector_nam) %>% summarise(longitude = quantile(longitude, 0.9), latitude = quantile(latitude, 0.9))
  
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
  
  # ISL_this <- crop(ISL_this, extent(144.62, 144.71, 13.24, 13.65))
  # 
  # buffer = buffer %>% subset(longitude > 144.62 &  
  #                              longitude < 144.71 & 
  #                              latitude > 13.24 & 
  #                              latitude < 13.65)
  # 
  # buffer_label = buffer_label %>% subset(longitude > 144.62 &  
  #                                          longitude < 144.71 & 
  #                                          latitude > 13.24 & 
  #                                          latitude < 13.65)
  # 
  # boxes_hulls = boxes_hulls %>% subset(longitude > 144.62 &  
  #                                        longitude < 144.71 & 
  #                                        latitude > 13.24 & 
  #                                        latitude < 13.65)
  # 
  # boxes_label = boxes_label %>% subset(longitude > 144.62 &  
  #                                        longitude < 144.71 & 
  #                                        latitude > 13.24 & 
  #                                        latitude < 13.65)
  # 
  # sets = sets %>% subset(longitude > 144.62 &  
  #                          longitude < 144.71 & 
  #                          latitude > 13.24 & 
  #                          latitude < 13.65)
  
  (site_location = 
      
      ggplot() + 
      
      geom_path(data = ISL_this, aes(long, lat, group = group), inherit.aes = F, size = 0.01, color = "darkgrey") + # coastline
      geom_polygon(data = ISL_this, aes(long, lat, group = group), fill = "darkgrey", color = NA, alpha = 0.9) + # land shapefile
      
      # geom_tile(data = cells, aes(longitude, latitude, fill = factor(strat)), alpha = 0.3, width = 0.001, height = 0.001) + # stratum
      
      geom_tile(data = buffer, aes(longitude, latitude, fill = sector_nam), width = 0.001, height = 0.001, alpha = 0.3, show.legend = F) + # island sectors
      geom_label_repel(data = buffer_label, aes(longitude, latitude, label = sector_nam, fill = sector_nam, fontface = 'bold'), color = "white", max.overlaps = Inf, show.legend = F) +
      
      scale_fill_discrete() + 
      scale_color_discrete() + 
      
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
      geom_spatial_point(data = sets, aes(longitude, latitude, shape = depth_bin, color = depth_bin),  crs = 4326) + 
      annotation_scale(location = "br", width_hint = 0.2) +
      
      # new_scale_color() +
      # new_scale_fill() +      
      
      geom_label_repel(data = sets, 
                       aes(longitude, latitude, label = id),
                       size = 2,
                       label.size = NA, 
                       alpha = 0.75, 
                       fontface = 'bold', 
                       color = 'black',
                       max.overlaps = Inf,
                       segment.size = 0.2,
                       direction = "both", 
                       # nudge_y = 0.005,
                       # nudge_x = 0.005,
                       box.padding = unit(0.8, "lines"),
                       point.padding = unit(0.3, "lines")) +
      
      # coord_fixed() +
      # coord_map() + 
      coord_sf(crs = 4326) + 
      
      # scale_x_continuous(sec.axis = dup_axis(), breaks = scales::pretty_breaks(n = 20), "Longitude (dec deg)") +
      # scale_y_continuous(sec.axis = dup_axis(), breaks = scales::pretty_breaks(n = 20), "Latitude (dec deg)") +
      
      scale_x_continuous(sec.axis = dup_axis(), "", limits = range(pretty(buffer$longitude))) +
      scale_y_continuous(sec.axis = dup_axis(), "", limits = range(pretty(buffer$latitude))) +
      
      theme_bw() +
      
      theme(legend.position = "bottom",
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10),
            panel.grid = element_blank()) +   
      labs(
        title = "",
        subtitle = paste0(paste0("Island = ", toupper(as.character(isl_shp[1])),"\n",
                                 # "Number of strata = ", length(unique(cells$strat)), "\n",
                                 # "Target survey effort = ", total_sample, " sites \n",
                                 "Total survey effort = ", sum(strat_det$strat_sets), " sites"))))
  
  # pdf(paste0("outputs/survey_layers_", islands[i], ".pdf"), height = 10, width = 10)
  # print((bathymetry + strata) / (area + variability))
  # dev.off()
  
  total_area = unique(cells$cell_area)*dim(cells)[1]
  
  size = ifelse(total_area < 18, 15, round(total_area/3))
  
  pdf(paste0("outputs/map/survey_map_", region, "_", islands[i], ".pdf"), height = size, width = size)
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
  
}
