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

# islands = c("gua", "rot", "sai", "tin", "agu"); region = "S.MARIAN"                           # South Mariana Islands
# islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "pag", "sar"); region = "N.MARIAN"      # North Mariana Islands
# islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                              # American Samoa
# islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"                # Pacific Remote Island Areas
islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah")[7]; region = "MHI"  # Main Hawaiian Islands
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
    remove_id = sector_name %>% subset(sector_name$nam %in% c("TUT_PAGOPAGO", "TUT_LAND"))
    remove_id = remove_id$ID
    sector[sector %in% remove_id] <- NA
    
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
                               nam = "hard")
    
  }
  
  ### 5km buffer ###
  if (file.exists(paste0("data/gis_5km_buffer/", islands[isl], ".RData"))) {
    
    load(paste0("data/gis_5km_buffer/", islands[isl], ".RData"))
    buffer = raster_and_table[[1]]; buffer_name = raster_and_table[[2]]
    # remove_id = hardsoft_name %>% subset(hardsoft_name$nam %in% c("Land", "Other", "Soft"))
    # remove_id = remove_id$ID
    # hardsoft[hardsoft %in% remove_id] <- NA
    
  } else {
    
    # using bathymetry raster as a placeholder bc there is no reef data for this island
    load(paste0("data/gis_bathymetry/", islands[isl], ".RData"))
    buffer = topo_i
    buffer[buffer <= 0] <- 1
    buffer_name = data.frame(ID = 1L, 
                             nam = paste0(islands[isl], "_5km_buffer"))
    
  }
  
  ### survey boxes ###
  if (file.exists(paste0("data/gis_survey_boxes/", islands[isl], ".RData"))) {
    
    load(paste0("data/gis_survey_boxes/", islands[isl], ".RData"))
    boxes = raster_and_table[[1]]; boxes_name = raster_and_table[[2]]
    
  } else {
    
    # using bathymetry raster as a placeholder bc there is no reef data for this island
    load(paste0("data/gis_bathymetry/", islands[isl], ".RData"))
    boxes = topo_i
    boxes[boxes <= 0] <- 1
    boxes_name = data.frame(ID = 1L, 
                            nam = paste0(islands[isl], "_box"))
    
  }
  
  hardsoft = resample(hardsoft, topo_i, method = "ngb") 
  sector = resample(sector, topo_i, method = "ngb") 
  reef = resample(reef, topo_i, method = "ngb") 
  bathymetry = resample(topo_i, topo_i, method = "ngb") 
  buffer = resample(buffer, topo_i, method = "ngb")
  boxes = resample(boxes, topo_i, method = "ngb")
  
  df = stack(hardsoft, sector, reef, bathymetry, buffer)
  
  df = as.data.frame(rasterToPoints(df))

  colnames(df) = c("longitude", "latitude", "hardsoft", "sector", "reef", "depth", "buffer")
  
  df = na.omit(df)
  
  df$cell = 1:dim(df)[1]; df$cell = as.numeric(df$cell)
  df$division = as.numeric(1)
  
  df$depth_bin = ""
  df$depth_bin = ifelse(df$depth <= 0  & df$depth >= -6, "shallow", df$depth_bin) 
  df$depth_bin = ifelse(df$depth < -6  & df$depth >= -18, "mid", df$depth_bin) 
  df$depth_bin = ifelse(df$depth < -18, "deep", df$depth_bin) 

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
  
  df = df %>% 
    subset(longitude < 718 & longitude > 704 & latitude > 2341)
    
  (depth = df %>% 
      ggplot( aes(longitude, latitude, fill = depth_bin)) + 
      geom_raster() + 
      coord_fixed())
  
  (sector = df %>% 
      ggplot( aes(longitude, latitude, fill = sector_id)) + 
      geom_raster() +
      coord_fixed())
  
  (reef = df %>% 
      ggplot( aes(longitude, latitude, fill = reef_id)) + 
      geom_raster() +
      coord_fixed())
  
  (hardsoft = df %>% 
      ggplot( aes(longitude, latitude, fill = hardsoft_id)) + 
      geom_raster() +
      coord_fixed())
  
  df$strat = paste(df$depth_bin, 
                   df$sector_id,
                   df$reef_id,
                   sep = "_")
  
  (strata = df %>% 
      ggplot( aes(longitude, latitude, fill = strat)) + 
      geom_raster() +
      coord_fixed())
  
  survey_grid_kalaupapa = df
  save(survey_grid_kalaupapa, file = paste0("data/survey_grid_ncrmp/survey_grid_kalaupapa.RData"))
  
  print(paste0("... ", islands[isl], " survey domain generated ..."))
  
}
