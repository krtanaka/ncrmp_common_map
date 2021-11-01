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

utm = read_csv('data/ncrmp_utm_zones.csv')

islands = c("gua", "rot", "sai", "tin", "agu"); region = "MARIAN"                           # South Mariana Islands
# islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "sar"); region = "MARIAN"             # North Mariana Islands
# islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                            # American Samoa
# islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"              # Pacific Remote Island Areas
# islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"  # Main Hawaiian Islands
# islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"               # Northern Hawaiian Islands

for (isl in 1:length(islands)) {
  
  isl = 1
  
  load(paste0("data/gis_bathymetry/", islands[isl], ".RData"))
  
  ###########################################################
  ### import sector/reefzones shapefile                   ###
  ### adjust resolutions and merge with bathymetry data   ###
  ###########################################################
  
  ### Island Sector ###
  if (file.exists(paste0("data/gis_sector/", islands[isl], ".RData"))) {
    
    load(paste0("data/gis_sector/", islands[isl], ".RData"))
    sector = raster_and_table[[1]]; sector_name = raster_and_table[[2]]
    remove_id = sector_name %>% subset(sector_name$nam %in% c("Land"))
    remove_id = remove_id$ID
    sector[sector %in% remove_id] <- NA
    
  } else {
    
    # using bathymetry raster as a placeholder bc there is no sector for this island
    load(paste0("data/gis_bathymetry/", islands[isl], ".RData"))
    sector = topo_i
    sector[sector <= 0] <- 1
    
  }
  
  ### Reef Zones ###
  if (file.exists(paste0("data/gis_reef/", islands[isl], ".RData"))) {
    
    load(paste0("data/gis_reef/", islands[isl], ".RData"))
    reef = raster_and_table[[1]]; reef_name = raster_and_table[[2]]
    remove_id = reef_name %>% subset(reef_name$nam %in% c("Land", "Land", "Reef Crest/Reef Flat"))
    remove_id = remove_id$ID
    reef[reef %in% remove_id] <- NA
    
  } else {
    
    # using bathymetry raster as a placeholder bc there is no reef data for this island
    load(paste0("data/gis_bathymetry/", islands[isl], ".RData"))
    reef = topo_i
    reef[reef <= 0] <- 1
    
  }
  
  ### Bottom Substrate ###
  if (file.exists(paste0("data/gis_hardsoft/", islands[isl], ".RData"))) {
    
    load(paste0("data/gis_hardsoft/", islands[isl], ".RData"))
    hardsoft = raster_and_table[[1]]; hardsoft_name = raster_and_table[[2]]
    remove_id = hardsoft_name %>% subset(hardsoft_name$nam %in% c("Land", "Other", "Soft"))
    remove_id = remove_id$ID
    hardsoft[hardsoft %in% remove_id] <- NA
    
  } else {
    
    # using bathymetry raster as a placeholder bc there is no reef data for this island
    load(paste0("data/gis_bathymetry/", islands[isl], ".RData"))
    hardsoft = topo_i
    hardsoft[hardsoft <= 0] <- 1
    
  }
  
  default_proj = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  
  crs(topo_i) = default_proj
  crs(sector) = default_proj
  crs(reef) = default_proj
  crs(hardsoft) = default_proj
  
  names(topo_i) = "depth"
  names(sector) = "sector"
  names(reef) = "reef"
  names(hardsoft) = "hardsoft"
  
  sector = resample(sector, topo_i, method = "bilinear"); plot(sector)
  reef = resample(reef, topo_i, method = "bilinear"); plot(reef)
  hardsoft = resample(hardsoft, topo_i, method = "bilinear"); plot(hardsoft)
  
  ncrmp = stack(sector, topo_i)
  ncrmp = stack(reef, ncrmp)
  ncrmp = stack(hardsoft, ncrmp)
  
  plot(ncrmp)
  
  ncrmp = rasterToPoints(ncrmp) %>% as.data.frame() %>% na.omit(); head(ncrmp)
  
  ncrmp$depth_bin = ""
  ncrmp$depth_bin = ifelse(ncrmp$depth <= 0  & ncrmp$depth >= -6, 1L, ncrmp$depth_bin) 
  ncrmp$depth_bin = ifelse(ncrmp$depth < -6  & ncrmp$depth >= -18, 2L, ncrmp$depth_bin) 
  ncrmp$depth_bin = ifelse(ncrmp$depth < -18, 3L, ncrmp$depth_bin) 
  
  ncrmp$sector = as.character(round(ncrmp$sector, 0))
  ncrmp$reef = as.character(round(ncrmp$reef, 0))
  
  ncrmp$strat = paste(ncrmp$depth_bin, 
                      ncrmp$sector,
                      ncrmp$reef,
                      sep = "_")
  
  ncrmp$strat = as.numeric(as.factor(ncrmp$strat))
  
  ncrmp$depth = as.numeric(ncrmp$depth*-1)
  
  colnames(ncrmp)[1:2] = c("longitude", "latitude")
  
  ncrmp$longitude = ncrmp$longitude * 0.001
  ncrmp$latitude = ncrmp$latitude * 0.001
  
  (depth = ncrmp %>% 
      ggplot( aes(longitude, latitude, fill = depth_bin)) + 
      geom_raster() + 
      scale_fill_discrete( "depth_bins") +
      coord_fixed() +
      ggdark::dark_theme_minimal() +
      theme(axis.title = element_blank(),
            legend.position = "right"))
  
  (sector = ncrmp %>% 
      ggplot( aes(longitude, latitude, fill = factor(sector))) + 
      geom_raster() +
      scale_fill_discrete("sector") +
      coord_fixed() +
      ggdark::dark_theme_minimal() +
      theme(axis.title = element_blank(),
            legend.position = "right"))
  
  (reef = ncrmp %>% 
      ggplot( aes(longitude, latitude, fill = as.factor(reef))) + 
      geom_raster() +
      scale_fill_discrete("reef") +
      coord_fixed() +
      theme_minimal() + 
      ggdark::dark_theme_minimal() +
      theme(axis.title = element_blank(),
            legend.position = "right"))
  
  (strata = ncrmp %>% 
      ggplot( aes(longitude, latitude, fill = factor(strat))) + 
      geom_raster() +
      scale_fill_discrete("Strata") +
      coord_fixed() +
      ggdark::dark_theme_minimal() +
      theme(axis.title = element_blank(),
            legend.position = "right"))
  
  ncrmp = as.data.frame(ncrmp)
  
  strat = rasterFromXYZ(ncrmp[,c("longitude", "latitude", "strat")]); plot(strat)
  depth = rasterFromXYZ(ncrmp[,c("longitude", "latitude", "depth")]); plot(depth)
  
  values = raster::values
  
  survey_grid_ncrmp = stack(strat, depth)
  survey_grid_ncrmp$strat = round(survey_grid_ncrmp$strat, digits = 0)
  
  sp::spplot(survey_grid$strat) #SimSurvey example
  sp::spplot(survey_grid_ncrmp$strat)
  
  sp::spplot(survey_grid$depth) #SimSurvey example
  sp::spplot(survey_grid_ncrmp$depth)
  
  p <- raster::rasterToPolygons(survey_grid$strat, dissolve = TRUE); sp::plot(p)
  p <- raster::rasterToPolygons(survey_grid_ncrmp$strat, dissolve = TRUE); sp::plot(p)
  
  survey_grid_ncrmp = readAll(survey_grid_ncrmp)
  
  save(survey_grid_ncrmp, file = paste0("data/survey_grid_ncrmp/survey_grid_", islands[isl], ".RData"))
  
  print(paste0("...", islands[isl], " survey domain generated..."))
  
}
