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
library(readr)

utm = read_csv('data/misc/ncrmp_utm_zones.csv')

islands = c("gua", "rot", "sai", "tin", "agu"); region = "S.MARIAN"                           # South Mariana Islands
islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "sar"); region = "N.MARIAN"             # North Mariana Islands
islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                            # American Samoa
# islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"              # Pacific Remote Island Areas
# islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"  # Main Hawaiian Islands
# islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"               # Northern Hawaiian Islands

for (isl in 1:length(islands)) {
  
  # isl = 1
  
  load(paste0("data/gis_bathymetry/", islands[isl], ".RData"))
  
  ###########################################################
  ### import sector/reefzones shapefile                   ###
  ### adjust resolutions and merge with bathymetry data   ###
  ###########################################################
  
  ### Island Sector ###
  if (file.exists(paste0("data/gis_sector/", islands[isl], ".RData"))) {
    
    load(paste0("data/gis_sector/", islands[isl], ".RData"))
    sector = raster_and_table[[1]]; sector_name = raster_and_table[[2]]
    # remove_id = sector_name %>% subset(sector_name$nam %in% c("Land"))
    # remove_id = remove_id$ID
    # sector[sector %in% remove_id] <- NA
    
  } else {
    
    # using bathymetry raster as a placeholder bc there is no sector for this island
    load(paste0("data/gis_bathymetry/", islands[isl], ".RData"))
    sector = topo_i
    sector[sector <= 0] <- 1
    sector_name = data.frame(ID = 1L, 
                             nam = paste0(islands[isl], "_sector"))
    
  }
  
  ### Reef Zones ###
  if (file.exists(paste0("data/gis_reef/", islands[isl], ".RData"))) {
    
    load(paste0("data/gis_reef/", islands[isl], ".RData"))
    reef = raster_and_table[[1]]; reef_name = raster_and_table[[2]]
    # remove_id = reef_name %>% subset(reef_name$nam %in% c("Land", "Land", "Reef Crest/Reef Flat"))
    # remove_id = remove_id$ID
    # reef[reef %in% remove_id] <- NA
    
  } else {
    
    # using bathymetry raster as a placeholder bc there is no reef data for this island
    load(paste0("data/gis_bathymetry/", islands[isl], ".RData"))
    reef = topo_i
    reef[reef <= 0] <- 1
    reef_name = data.frame(ID = 1L, 
                           nam = "forereef")
    
  }
  
  ### Bottom Substrate ###
  if (file.exists(paste0("data/gis_hardsoft/", islands[isl], ".RData"))) {
    
    load(paste0("data/gis_hardsoft/", islands[isl], ".RData"))
    hardsoft = raster_and_table[[1]]; hardsoft_name = raster_and_table[[2]]
    # remove_id = hardsoft_name %>% subset(hardsoft_name$nam %in% c("Land", "Other", "Soft"))
    # remove_id = remove_id$ID
    # hardsoft[hardsoft %in% remove_id] <- NA
    
  } else {
    
    # using bathymetry raster as a placeholder bc there is no reef data for this island
    load(paste0("data/gis_bathymetry/", islands[isl], ".RData"))
    hardsoft = topo_i
    hardsoft[hardsoft <= 0] <- 1
    hardsoft_name = data.frame(ID = 1L, 
                               nam = paste0(islands[isl], "_hardsoft"))
    
  }
  
  hardsoft = resample(hardsoft, topo_i, method = "ngb") 
  sector = resample(sector, topo_i, method = "ngb") 
  reef = resample(reef, topo_i, method = "ngb") 
  bathymetry = resample(topo_i, topo_i, method = "ngb") 
  
  df = stack(hardsoft, sector, reef, bathymetry)
  df = as.data.frame(rasterToPoints(df))
  
  colnames(df) = c("longitude", "latitude", "hardsoft", "sector", "reef", "depth")
  
  df = na.omit(df)
  
  df$cell = 1:dim(df)[1]; df$cell = as.numeric(df$cell)
  df$division = as.numeric(1)
  
  df$depth_bin = ""
  df$depth_bin = ifelse(df$depth <= 0  & df$depth >= -6, 1L, df$depth_bin) 
  df$depth_bin = ifelse(df$depth < -6  & df$depth >= -18, 2L, df$depth_bin) 
  df$depth_bin = ifelse(df$depth < -18, 3L, df$depth_bin) 
  
  df$hardsoft = round(df$hardsoft, 0)
  df$reef = round(df$reef, 0)
  df$sector = round(df$sector, 0)
  
  df$sector = as.character(df$sector)
  df$reef = as.character(df$reef)
  
  df$depth = as.numeric(df$depth*-1)
  
  df$longitude = df$longitude * 0.001
  df$latitude = df$latitude * 0.001
  
  df = as.data.frame(df)
  
  colnames(sector_name) = c("sector", "sector_id")
  colnames(reef_name) = c("reef", "reef_id")
  colnames(hardsoft_name) = c("hardsoft", "hardsoft_id")
  
  sector_name$sector_id = tolower(sector_name$sector_id)
  reef_name$reef_id = tolower(reef_name$reef_id)
  hardsoft_name$hardsoft_id = tolower(hardsoft_name$hardsoft_id)
  
  df = merge(df, sector_name)
  df = merge(df, reef_name)
  df = merge(df, hardsoft_name)
  
  (depth = df %>% 
      ggplot( aes(longitude, latitude, fill = depth_bin)) + 
      geom_raster() + 
      scale_fill_discrete( "depth_bins") +
      ggdark::dark_theme_minimal())
  
  (sector = df %>% 
      ggplot( aes(longitude, latitude, fill = sector_id)) + 
      geom_raster() +
      scale_fill_discrete("sector") +
      ggdark::dark_theme_minimal())
  
  (reef = df %>% 
      ggplot( aes(longitude, latitude, fill = reef_id)) + 
      geom_raster() +
      scale_fill_discrete("reef") +
      ggdark::dark_theme_minimal())
  
  (hardsoft = df %>% 
      ggplot( aes(longitude, latitude, fill = hardsoft_id)) + 
      geom_raster() +
      scale_fill_discrete("hardsoft") +
      ggdark::dark_theme_minimal())
  
  df = df %>%
    subset(sector_id != "GUA_LAND") %>% # filter sector
    subset(reef_id %in% c( "forereef")) %>% # filter land and Reef Crest/Reef Flat
    subset(hardsoft_id %in% c("hard", "unknown")) # filter for sector
  
  df$strat = paste(df$depth_bin, 
                   df$sector,
                   df$reef,
                   sep = "_")
  
  df$strat = as.numeric(as.factor(df$strat))
  
  ################################################################
  ### create table to match strata to numbers for output table ###
  ################################################################
  tab <- df 
  tab$depth_bin_value = ""
  tab$depth_bin_value = ifelse(tab$depth_bin == 1, "SHAL", tab$depth_bin_value) 
  tab$depth_bin_value = ifelse(tab$depth_bin == 2, "MIDD", tab$depth_bin_value) 
  tab$depth_bin_value = ifelse(tab$depth_bin == 3, "DEEP", tab$depth_bin_value) 
  tab <- tab %>% dplyr::select(sector_id,reef_id,strat,depth_bin_value)
  tab <- tab %>% filter(!duplicated(tab))
  save(tab,file = paste0("outputs/sector_key_", islands[isl], ".RData"))
  
  (strata = df %>% 
      ggplot( aes(longitude, latitude, fill = factor(strat))) + 
      geom_raster() +
      scale_fill_discrete("Strata") +
      ggdark::dark_theme_minimal())
  
  cell = rasterFromXYZ(df[,c("longitude", "latitude", "cell")]); plot(cell)
  division = rasterFromXYZ(df[,c("longitude", "latitude", "division")]); plot(division)
  strat = rasterFromXYZ(df[,c("longitude", "latitude", "strat")]); plot(strat)
  depth = rasterFromXYZ(df[,c("longitude", "latitude", "depth")]); plot(depth)
  
  values = raster::values
  
  survey_grid_ncrmp = stack(cell, division, strat, depth)
  survey_grid_ncrmp$strat = round(survey_grid_ncrmp$strat, digits = 0)
  values(survey_grid_ncrmp$division) = ifelse(is.na(values(survey_grid_ncrmp$division)), NA, 1)
  
  survey_grid_ncrmp = readAll(survey_grid_ncrmp)
  
  # default_proj = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  # crs(survey_grid_ncrmp) = default_proj

  p <- raster::rasterToPolygons(survey_grid$strat, dissolve = TRUE); sp::plot(p)
  p <- raster::rasterToPolygons(survey_grid_ncrmp$strat, dissolve = TRUE); sp::plot(p)
  
  survey_grid_ncrmp = readAll(survey_grid_ncrmp)
  save(survey_grid_ncrmp, file = paste0("data/survey_grid_ncrmp/survey_grid_", islands[isl], ".RData"))
  
  print(paste0("... ", islands[isl], " survey domain generated ..."))
  
}
