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

utm = read_csv('data/misc/ncrmp_utm_zones.csv')


##################################
###  select islands & regions  ###
##################################

islands = c("gua", "rot", "sai", "tin", "agu"); region = "S.MARIAN"                         # South Mariana Islands
islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "sar"); region = "N.MARIAN"           # North Mariana Islands
# islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                            # American Samoa
# islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"              # Pacific Remote Island Areas
# islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"  # Main Hawaiian Islands
# islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"               # Northern Hawaiian Islands


########################################################################
### do some parameter settings to simulate stratified random surveys ###
########################################################################

# n_sims = 100 # number of simulations
effort_level = c("low", "mid", "high")[2] # define sampling effort (low, mid, high)
min_sets = 1 # minimum number of sets per strat
trawl_dim = c(0.01, 0.0353) # 0.000353 sq.km (353 sq.m) from two 15-m diameter survey cylinders
resample_cells = F


##################################################################
### determine number of sites you want to deploy @ each island ###
##################################################################

load('data/misc/survey_effort_ncrmp_2000-2020.RData')
island_name_code = read_csv('data/misc/island_name_code.csv')
survey_effort = data.frame(Island = survey_effort$Island, Effort = survey_effort[[effort_level]])
survey_effort = merge(island_name_code, survey_effort); head(survey_effort); tail(survey_effort)


#################################################################
### Generate survey site tables & maps, check outputs/ folder ###
#################################################################

for (i in 1:length(islands)) {
  
  # i = 1
  
  # survey domain with sector & reef & hard_unknown & 3 depth bins
  load(paste0("data/survey_grid_ncrmp/survey_grid_", islands[i], ".RData")) 
  
  total_sample = survey_effort %>% subset(Island_Code == islands[i])
  
  if (dim(total_sample)[1] == 0){
    
    total_sample = 10

  }else {
    
    total_sample = total_sample$Effort
    
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
  
  # make sure minimum number of sets per strat is not 0 or 1
  strat_det$strat_sets[strat_det$strat_sets < min_sets] <- min_sets; strat_det
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
  sets <- cells[, .SD[sample(.N, strat_sets, replace = resample_cells)], 
                by = c("strat")]
  
  id <- seq(1,dim(sets)[1],1)
  id = sprintf("site_%02d", id)
  
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
  
  readr::write_csv(sets, file = paste0("outputs/survey_table_", islands[i], "_", effort_level, ".csv"))
  
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
      scale_fill_gradientn(colours = matlab.like(100), "var(total_fish_density)") + 
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

  (site_location = 
      ggplot() + 
      # geom_point(data = sets, aes(longitude, latitude, shape = depth_bin, color = depth_bin)) +  
      # geom_text_repel(data = sets, aes(longitude, latitude, label = id), max.overlaps = Inf) +
      # geom_tile(data = cells, aes(longitude, latitude, fill = factor(strat)), alpha = 0.5, width = 0.001, height = 0.001) +
      # ylab("Latitude (dec deg)") + xlab("Longitude (dec deg)") +
      geom_point(data = sets, aes(x, y, shape = depth_bin, color = depth_bin)) +
      geom_text_repel(data = sets, aes(x, y, label = id), max.overlaps = Inf) +
      geom_raster(data = cells, aes(x, y, fill = factor(strat)), alpha = 0.5) +
      ylab("Northings (km)") + xlab("Eastings (km)") +
      coord_fixed() +
      # scale_x_continuous(sec.axis = dup_axis()) +
      # scale_y_continuous(sec.axis = dup_axis()) + 
      
      scale_fill_discrete("Strata") + 
      theme_light() +
      theme(legend.position = "right") + 
      labs(
        title = "",
        subtitle = paste0(paste0("Island = ", toupper(islands[i]),"\n", 
                                 "Number of strata = ", length(unique(cells$strat)), "\n", 
                                 "Target survey effort = ", total_sample, " sites \n",
                                 "Total survey effort = ", sum(strat_det$strat_sets), " sites"))))
  
  # pdf(paste0("outputs/survey_layers_", islands[i], ".pdf"), height = 10, width = 10)
  # print((bathymetry + strata) / (area + variability))
  # dev.off()
  
  pdf(paste0("outputs/survey_location_", islands[i], "_", effort_level, ".pdf"), height = 10, width = 10)
  print(site_location)
  dev.off()
  
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

