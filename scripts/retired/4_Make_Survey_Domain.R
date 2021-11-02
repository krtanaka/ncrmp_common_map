########################################
### prep survey_grid for simulations ###
########################################

rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(sp)
library(rgdal)
library(tidyr)
library(patchwork)
library(SimSurvey)
library(sf)

islands = c("gua", "rot", "sai", "tin")

for (isl in 1:length(islands)) {
  
  # isl = 1
  
  load(paste0("data/gis_bathymetry/raster/", islands[isl], ".RData"))
  
  df = topo; rm(topo)
  
  df$longitude = df$x
  df$latitude = df$y
  
  df = df %>% 
    group_by(longitude, latitude) %>% 
    summarise(depth = mean(depth))
  
  df$cell = 1:dim(df)[1]; df$cell = as.numeric(df$cell)
  df$division = as.numeric(1)
  
  ###########################################################
  ### import sector/reefzones shapefile                   ###
  ### adjust resolutions and merge with bathymetry data   ###
  ### these are outputs from "convert_shp_to_data.frame.R ###
  ###########################################################
  
  if (isl == 1) {
    load("data/gis_sector/raster/gua.RData"); sector = raster_and_table[[1]]; sector_name = raster_and_table[[2]]
    load("data/gis_reef/raster/gua.RData"); reef = raster_and_table[[1]]; reef_name = raster_and_table[[2]]
    load("data/gis_hardsoft/raster/gua.RData"); hardsoft = raster_and_table[[1]]; hardsoft_name = raster_and_table[[2]]
  }
  if (isl == 2) { # using reef raster as a placeholder bc there is no sector for this island
    load("data/gis_reef/raster/rot.RData"); sector = raster_and_table[[1]] ; sector_name = raster_and_table[[2]] 
    load("data/gis_reef/raster/rot.RData"); reef = raster_and_table[[1]]; reef_name = raster_and_table[[2]]
    load("data/gis_hardsoft/raster/rot.RData"); hardsoft = raster_and_table[[1]]; hardsoft_name = raster_and_table[[2]]
  }
  if (isl == 3) { # using reef raster as a placeholder bc there is no sector for this island
    load("data/gis_reef/raster/sai.RData"); sector = raster_and_table[[1]] ; sector_name = raster_and_table[[2]] 
    load("data/gis_reef/raster/sai.RData"); reef = raster_and_table[[1]]; reef_name = raster_and_table[[2]]
    load("data/gis_hardsoft/raster/sai.RData"); hardsoft = raster_and_table[[1]]; hardsoft_name = raster_and_table[[2]]
  }
  if (isl == 4) { # using reef raster as a placeholder bc there is no sector for this island
    load("data/gis_reef/raster/tin.RData"); sector = raster_and_table[[1]] ; sector_name = raster_and_table[[2]] 
    load("data/gis_reef/raster/tin.RData"); reef = raster_and_table[[1]]; reef_name = raster_and_table[[2]]
    load("data/gis_hardsoft/raster/tin.RData"); hardsoft = raster_and_table[[1]]; hardsoft_name = raster_and_table[[2]]
  }
  
  rm(raster_and_table)
  
  sector = rasterToPoints(sector) %>% as.data.frame(); colnames(sector) = c("x", "y", "z")
  reef = rasterToPoints(reef) %>% as.data.frame(); colnames(reef) = c("x", "y", "z")
  hardsoft = rasterToPoints(hardsoft) %>% as.data.frame(); colnames(hardsoft) = c("x", "y", "z")
  
  if (isl %in% c(2:4)) {
    
    sector$z = 1
    sector_name = data.frame(SEC_NAME = "single_sector",
                             i = 1)
  }
  
  # merge hardsoft -----------------------------------------------------------
  
  hardsoft$lon = hardsoft$x
  hardsoft$lat = hardsoft$y
  
  hardsoft$hardsoft_name = as.numeric(as.factor(hardsoft$z))
  hardsoft = as.matrix(hardsoft[,c("lon", "lat", "hardsoft_name")])
  e = extent(hardsoft[,1:2])
  
  crm_res = rasterFromXYZ(df[,c("longitude", "latitude", "cell")])
  dim(crm_res); crm_res
  
  res = 10  # rasterize it, but be careful with resolutions, lower = better but more missing points
  r <- raster(e, ncol = round((dim(crm_res)[2]/res), digits = 0), nrow = round(dim(crm_res)[1]/res, digits = 0))
  hardsoft <- rasterize(hardsoft[, 1:2], r, hardsoft[,3], fun = mean)
  dim(hardsoft)
  hardsoft
  default_proj = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  crs(hardsoft) = default_proj
  plot(hardsoft); summary(hardsoft)
  
  hardsoft = resample(hardsoft, crm_res, method = "bilinear") 
  
  crm_res = as.data.frame(rasterToPoints(crm_res))
  hardsoft = as.data.frame(rasterToPoints(hardsoft))
  
  hardsoft = left_join(crm_res, hardsoft)
  colnames(hardsoft) = c("longitude", "latitude", "cell", "hardsoft")
  summary(hardsoft)
  
  bottom_hard = hardsoft_name[1,2]
  bottom_hard = unlist(strsplit(bottom_hard$i, ","))
  bottom_unknown = hardsoft_name[2,2]
  bottom_unknown = unlist(strsplit(bottom_unknown$i, ","))
  
  hardsoft$hardsoft = as.character(round(hardsoft$hardsoft, 0))
  
  # #separate hard and unknown bottom type
  # hardsoft$hardsoft = ifelse(hardsoft$hardsoft %in% bottom_hard, "hard", hardsoft$hardsoft)
  # hardsoft$hardsoft = ifelse(hardsoft$hardsoft %in% bottom_unknown, "unknown", hardsoft$hardsoft)
  
  #merge hard and unknown bottom type
  hardsoft$hardsoft = ifelse(hardsoft$hardsoft %in% bottom_hard, "hard_or_unknown", hardsoft$hardsoft)
  hardsoft$hardsoft = ifelse(hardsoft$hardsoft %in% bottom_unknown, "hard_or_unknown", hardsoft$hardsoft)
  
  hardsoft %>% 
    ggplot(aes(longitude, latitude, fill = hardsoft)) + 
    geom_tile() + 
    coord_fixed() + 
    scale_fill_discrete("hardsoft") + 
    ggdark::dark_theme_minimal()
  
  df = merge(df, hardsoft, by = c("cell"))
  
  df = df[!is.na(df$hardsoft), ]
  
  colnames(df)[2:3] = c("longitude", "latitude")
  
  
  # merge sectors -----------------------------------------------------------
  
  sector$lon = sector$x
  sector$lat = sector$y
  
  sector$sector_name = as.numeric(as.factor(sector$z))
  sector = as.matrix(sector[,c("lon", "lat", "sector_name")])
  e = extent(sector[,1:2])
  
  crm_res = rasterFromXYZ(df[,c("longitude", "latitude", "cell")])
  dim(crm_res); crm_res
  
  res = 10  # rasterize it, but be careful with resolutions, lower = better but more missing points
  r <- raster(e, ncol = round((dim(crm_res)[2]/res), digits = 0), nrow = round(dim(crm_res)[1]/res, digits = 0))
  sector <- rasterize(sector[, 1:2], r, sector[,3], fun = mean)
  dim(sector)
  sector
  default_proj = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  crs(sector) = default_proj
  plot(sector); summary(sector)
  
  sector = resample(sector, crm_res, method = "bilinear") 
  
  crm_res = as.data.frame(rasterToPoints(crm_res))
  sector = as.data.frame(rasterToPoints(sector))
  
  sector = left_join(crm_res, sector)
  colnames(sector) = c("longitude", "latitude", "cell", "sector")
  summary(sector)
  
  sector$sector = as.character(round(sector$sector, 0))
  
  sector_name = as.data.frame(sector_name)
  sector_name$SEC_NAME = tolower(sector_name$SEC_NAME)
  colnames(sector_name) = c("sector_name", "sector")
  
  sector = merge(sector, sector_name, all = T)
  sector$sector_name = ifelse(is.na(sector$sector_name), "Unnamed", sector$sector_name)
  
  sector %>% 
    ggplot(aes(longitude, latitude, fill = sector_name)) + 
    geom_tile() + 
    coord_fixed() + 
    ggdark::dark_theme_minimal()
  
  df = merge(df, sector, by = c("cell"))
  
  df = df[!is.na(df$sector), ]
  
  colnames(df)[2:3] = c("longitude", "latitude")
  
  # merge reefzones ---------------------------------------------------------
  
  reef$lon = reef$x
  reef$lat = reef$y
  
  reef$reef_zone = as.numeric(as.factor(reef$z))
  reef = as.matrix(reef[,c("lon", "lat", "reef_zone")])
  e = extent(reef[,1:2])
  
  crm_res = rasterFromXYZ(df[,c("longitude", "latitude", "cell")])
  dim(crm_res); crm_res
  
  res = 10  # rasterize it, but be careful with resolutions, lower = better but more missing points
  r <- raster(e, ncol = round((dim(crm_res)[2]/res), digits = 0), nrow = round(dim(crm_res)[1]/res, digits = 0))
  reef <- rasterize(reef[, 1:2], r, reef[,3], fun = mean)
  dim(reef)
  reef
  default_proj = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  crs(reef) = default_proj
  plot(reef)
  
  reef = resample(reef, crm_res, method = "bilinear") 
  
  crm_res = as.data.frame(rasterToPoints(crm_res))
  reef = as.data.frame(rasterToPoints(reef))
  
  reef = left_join(crm_res, reef)
  colnames(reef) = c("longitude", "latitude", "cell", "reef")
  summary(reef)
  
  reef_name = as.data.frame(reef_name)
  
  reef_name_clean = NULL
  
  for (i in 1:dim(reef_name)[1]) {
    
    # i = 1
    
    reefname_sub =   data.frame(reef_name = as.character(reef_name[i,1]),
                                reef = unlist(strsplit(as.character(reef_name[i,2]), ",")))
    
    reef_name_clean = rbind(reef_name_clean, reefname_sub)
    
  }
  
  reef$reef = as.character(round(reef$reef, 0))
  
  reef = merge(reef, reef_name_clean)
  
  reef %>% 
    ggplot(aes(longitude, latitude, fill = reef_name)) + 
    geom_tile() + 
    coord_fixed() + 
    scale_fill_discrete("reef_zones") + 
    ggdark::dark_theme_minimal()
  
  df = merge(df, reef, by = "cell")
  
  df = df[!is.na(df$reef), ]
  
  df = df[ , -which(names(df) %in% c("longitude.y", "latitude.y", "longitude.y", "latitude.y"))]
  
  # make strata by depth * sector * reef type -------------------------------
  
  df$depth_bin = ""
  df$depth_bin = ifelse(df$depth <= 0  & df$depth >= -6, 1L, df$depth_bin) 
  df$depth_bin = ifelse(df$depth < -6  & df$depth >= -18, 2L, df$depth_bin) 
  df$depth_bin = ifelse(df$depth < -18, 3L, df$depth_bin) 
  
  df$sector = as.character(df$sector_name)
  df$reef = as.character(df$reef_name)
  
  df$strat = paste(df$depth_bin, 
                   df$sector,
                   df$reef,
                   sep = "_")
  
  df$strat = as.numeric(as.factor(df$strat))
  
  df$depth = as.numeric(df$depth*-1)
  
  colnames(df)[2:3] = c("longitude", "latitude")
  
  df$longitude = df$longitude * 0.001
  df$latitude = df$latitude * 0.001
  
  # utmcoor <- SpatialPoints(cbind(df$longitude, df$latitude), proj4string = CRS("+proj=utm +units=km +zone=55"))
  # longlatcoor <- spTransform(utmcoor,CRS("+proj=longlat"))
  # df$longitude <- coordinates(longlatcoor)[,1]
  # df$latitude <- coordinates(longlatcoor)[,2]
  
  (depth = df %>% 
      ggplot( aes(longitude, latitude, fill = depth_bin)) + 
      # geom_tile(height = 0.001, width = 0.001) +
      geom_raster() + 
      # scale_fill_gradientn(colours = colorRamps::matlab.like(100), "Bathymetry (m)") +
      scale_fill_discrete( "depth_bins") +
      coord_fixed() +
      ggdark::dark_theme_minimal() +
      theme(axis.title = element_blank(),
            legend.position = "right"))
  
  (sector = df %>% 
      ggplot( aes(longitude, latitude, fill = factor(sector))) + 
      geom_raster() +
      scale_fill_discrete("sector") +
      coord_fixed() +
      ggdark::dark_theme_minimal() +
      theme(axis.title = element_blank(),
            legend.position = "right"))
  
  (reef = df %>% 
      ggplot( aes(longitude, latitude, fill = as.factor(reef))) + 
      geom_raster() +
      scale_fill_discrete("reef") +
      coord_fixed() +
      theme_minimal() + 
      ggdark::dark_theme_minimal() +
      theme(axis.title = element_blank(),
            legend.position = "right"))
  
  (strata = df %>% 
      ggplot( aes(longitude, latitude, fill = factor(strat))) + 
      geom_raster() +
      scale_fill_discrete("Strata") +
      coord_fixed() +
      ggdark::dark_theme_minimal() +
      theme(axis.title = element_blank(),
            legend.position = "right"))
  
  df = as.data.frame(df)
  
  cell = rasterFromXYZ(df[,c("longitude", "latitude", "cell")]); plot(cell)
  division = rasterFromXYZ(df[,c("longitude", "latitude", "division")]); plot(division)
  strat = rasterFromXYZ(df[,c("longitude", "latitude", "strat")]); plot(strat)
  depth = rasterFromXYZ(df[,c("longitude", "latitude", "depth")]); plot(depth)
  
  values = raster::values
  
  survey_grid_ncrmp = stack(cell, division, strat, depth)
  survey_grid_ncrmp$strat = round(survey_grid_ncrmp$strat, digits = 0)
  # values(survey_grid_ncrmp$strat) = ifelse(values(survey_grid_ncrmp$strat) > 3, 3, values(survey_grid_ncrmp$strat))
  values(survey_grid_ncrmp$division) = ifelse(is.na(values(survey_grid_ncrmp$division)), NA, 1)
  
  sp::spplot(survey_grid$cell) #SimSurvey example
  sp::spplot(survey_grid_ncrmp$cell)
  
  sp::spplot(survey_grid$division) #SimSurvey example
  sp::spplot(survey_grid_ncrmp$division)
  
  sp::spplot(survey_grid$strat) #SimSurvey example
  sp::spplot(survey_grid_ncrmp$strat)
  
  sp::spplot(survey_grid$depth) #SimSurvey example
  sp::spplot(survey_grid_ncrmp$depth)
  
  p <- raster::rasterToPolygons(survey_grid$strat, dissolve = TRUE)
  sp::plot(p)
  
  p <- raster::rasterToPolygons(survey_grid_ncrmp$strat, dissolve = TRUE)
  sp::plot(p)
  
  survey_grid_ncrmp = readAll(survey_grid_ncrmp)
  
  save(survey_grid_ncrmp, file = paste0("data/survey_grid_w_sector_reef/survey_grid_", islands[isl], ".RData"))
  
}