########################################
### prep survey_grid for simulations ###
########################################

rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(sp)
library(tidyr)
library(sf)
library(readr)
library(concaveman)
library(patchwork)

utm = read_csv('data/misc/ncrmp_utm_zones.csv')

islands = c("gua", "rot", "sai", "tin", "agu"); region = "S.MARIAN"                           # South Mariana Islands
islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "pag", "sar"); region = "N.MARIAN"      # North Mariana Islands
islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                              # American Samoa
islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"                # Pacific Remote Island Areas
islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"    # Main Hawaiian Islands
islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"                 # Northern Hawaiian Islands

for (isl in 1:length(islands)) {
  
  # isl = 2
  
  load(paste0("data/gis_bathymetry/", islands[isl], ".RData"))
  
  cat(paste0("generating ", islands[isl], " survey domain ...\n"))
  
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
  
  ### MHI restricted areas ###
  if (region == "MHI") {
    
    load("data/gis_sector/mhi_restricted_areas_haw.RData"); restricted_haw = raster_and_table
    load("data/gis_sector/mhi_restricted_areas_kau_oah_a.RData"); restricted_kau_oah_a = raster_and_table
    load("data/gis_sector/mhi_restricted_areas_kau_oah_b.RData"); restricted_kau_oah_b = raster_and_table
    load("data/gis_sector/mhi_restricted_areas_oah_lan_mau.RData"); restricted_oah_lan_mau = raster_and_table
    load("data/gis_sector/mhi_restricted_areas_oah_mokapu.RData"); restricted_oah_mokapu = raster_and_table
    
    restricted_haw_areas = restricted_haw[[1]]; restricted_haw_names = restricted_haw[[2]]
    restricted_kau_oah_a_areas = restricted_kau_oah_a[[1]]; restricted_kau_oah_a_names = restricted_kau_oah_a[[2]]
    restricted_kau_oah_b_areas = restricted_kau_oah_b[[1]]; restricted_kau_oah_b_names = restricted_kau_oah_b[[2]]
    restricted_oah_lan_mau_areas = restricted_oah_lan_mau[[1]]; restricted_oah_lan_mau_names = restricted_oah_lan_mau[[2]]
    restricted_oah_mokapu_areas = restricted_oah_mokapu[[1]]; restricted_oah_mokapu_names = restricted_oah_mokapu[[2]]
    
    restricted_haw_names$ID  = as.character(restricted_haw_names$ID)
    restricted_kau_oah_a_names$ID = as.character(restricted_kau_oah_a_names$ID)
    restricted_kau_oah_b_names$ID = as.character(restricted_kau_oah_b_names$ID)
    restricted_oah_lan_mau_names$ID = as.character(restricted_oah_lan_mau_names$ID)
    restricted_oah_lan_mau_names$ID = as.character(restricted_oah_lan_mau_names$ID)
    restricted_oah_mokapu_names$ID = as.character(restricted_oah_mokapu_names$ID)
    
    colnames(restricted_haw_names) = c("restricted_haw_areas", "restricted_haw_areas_id")
    colnames(restricted_kau_oah_a_names) = c("restricted_kau_oah_a_areas", "restricted_kau_oah_a_names_id")
    colnames(restricted_kau_oah_b_names) = c("restricted_kau_oah_b_areas", "restricted_kau_oah_b_names_id")
    colnames(restricted_oah_lan_mau_names) = c("restricted_oah_lan_mau_areas", "restricted_oah_lan_mau_names_id")
    colnames(restricted_oah_mokapu_names) = c("restricted_oah_mokapu_areas", "restricted_oah_mokapu_names_id")
    
    restricted_haw_names$restricted_haw_areas_id <- gsub(" ", "_", tolower(restricted_haw_names$restricted_haw_areas_id))
    restricted_kau_oah_a_names$restricted_kau_oah_a_names_id <- gsub(" ", "_", tolower(restricted_kau_oah_a_names$restricted_kau_oah_a_names_id))
    restricted_kau_oah_b_names$restricted_kau_oah_b_names_id <- gsub(" ", "_", tolower(restricted_kau_oah_b_names$restricted_kau_oah_b_names_id))
    restricted_oah_lan_mau_names$restricted_oah_lan_mau_names_id <- gsub(" ", "_", tolower(restricted_oah_lan_mau_names$restricted_oah_lan_mau_names_id))
    restricted_oah_mokapu_names$restricted_oah_mokapu_names_id <- gsub(" ", "_", tolower(restricted_oah_mokapu_names$restricted_oah_mokapu_names_id))
    
  } 
  
  # Resample raster layers to match 'topo_i' resolution using nearest-neighbor method
  cat("resampling base raster layers to match original bathymetry resolution... this may take some time...\n")
  hardsoft <- resample(hardsoft, topo_i, method = "ngb")
  sector <- resample(sector, topo_i, method = "ngb")
  reef <- resample(reef, topo_i, method = "ngb")
  bathymetry <- resample(topo_i, topo_i, method = "ngb")
  buffer <- resample(buffer, topo_i, method = "ngb")
  boxes <- resample(boxes, topo_i, method = "ngb")
  
  # Resample restricted areas based on the current island
  if (islands[isl] %in% c("haw")) {
    
    restricted_haw_areas = resample(restricted_haw_areas, topo_i, method = "ngb")
    
  } 
  
  if (islands[isl] %in% c("oah")) {
    
    restricted_oah_mokapu_areas = resample(restricted_oah_mokapu_areas, topo_i, method = "ngb")
    
  } 
  
  if (islands[isl] %in% c("kau", "oah")) {
    
    restricted_kau_oah_a_areas = resample(restricted_kau_oah_a_areas, topo_i, method = "ngb")
    restricted_kau_oah_b_areas = resample(restricted_kau_oah_b_areas, topo_i, method = "ngb")
    
  }
  
  if (islands[isl] %in% c("oah", "lan", "mau")) {
    
    restricted_oah_lan_mau_areas = resample(restricted_oah_lan_mau_areas, topo_i, method = "ngb")
    
  } 
  
  # Stack and name layers based on the current island
  df <- stack(hardsoft, sector, reef, bathymetry, buffer)
  names(df) <- c("hardsoft", "sector", "reef", "depth", "buffer")
  
  if (islands[isl] == "tut") {
    
    df = stack(hardsoft, sector, reef, bathymetry)
    names(df) <- c("hardsoft", "sector", "reef", "depth")
    
  }
  
  if (islands[isl] == "ros") {
    
    df <- stack(hardsoft, reef, bathymetry, buffer)
    names(df) <- c("hardsoft", "reef", "depth", "buffer")
    df$sector <- 1L  # Set sector to 1 for 'ros' island
    
  }
  
  if (islands[isl] %in% c("haw")) {
    
    df = stack(hardsoft, sector, reef, bathymetry, buffer, restricted_haw_areas)
    names(df) <- c("hardsoft", "sector", "reef", "depth", "buffer", "restricted_haw_areas")
    
  } 
  
  if (islands[isl] %in% c("oah")) {
    
    df = stack(hardsoft, sector, reef, bathymetry, buffer, 
               restricted_kau_oah_a_areas, 
               restricted_kau_oah_b_areas, 
               restricted_oah_lan_mau_areas,
               restricted_oah_mokapu_areas)
    
    names(df) <- c("hardsoft", "sector", "reef", "depth", "buffer", 
                   "restricted_kau_oah_a_areas", 
                   "restricted_kau_oah_b_areas", 
                   "restricted_oah_lan_mau_areas",
                   "restricted_oah_mokapu_areas")
  } 
  
  if (islands[isl] %in% c("kau")) {
    
    df = stack(hardsoft, sector, reef, bathymetry, buffer, 
               restricted_kau_oah_a_areas, 
               restricted_kau_oah_b_areas)
    
    names(df) <- c("hardsoft", "sector", "reef", "depth", "buffer", 
                   "restricted_kau_oah_a_areas", 
                   "restricted_kau_oah_b_areas")
  } 
  
  if (islands[isl] %in% c("mau", "lan")) {
    
    df = stack(hardsoft, sector, reef, bathymetry, buffer, restricted_oah_lan_mau_areas)
    names(df) <- c("hardsoft", "sector", "reef", "depth", "buffer", "restricted_oah_lan_mau_areas")
    
  } 
  
  # Convert raster to data frame and add cell numbers
  df <- as.data.frame(rasterToPoints(df))
  df$cell <- 1:nrow(df)
  df$cell <- as.numeric(df$cell)
  
  # Add a division column with a constant value of 1
  df$division = as.numeric(1)
  
  # Classify depth into bins
  df$depth_bin <- ""
  df$depth_bin <- ifelse(df$depth <= 0 & df$depth >= -6, "shallow", df$depth_bin)
  df$depth_bin <- ifelse(df$depth < -6 & df$depth >= -18, "mid", df$depth_bin)
  df$depth_bin <- ifelse(df$depth < -18, "deep", df$depth_bin)
  df <- df %>% filter(!is.na(depth_bin))
  
  # Convert depth to positive values
  df$depth <- as.numeric(df$depth * -1)
  
  # Round and convert hardsoft, reef, and sector values to character
  df$hardsoft <- as.character(round(df$hardsoft, 0))
  df$reef <- as.character(round(df$reef, 0))
  df$sector <- as.character(round(df$sector, 0))
  
  # Rename UTM coordinates to longitude and latitude for now
  df$longitude <- df$x * 0.001
  df$latitude <- df$y * 0.001
  
  # Convert columns starting with 'restricted_' to character
  df <- df %>% mutate_at(vars(starts_with("restricted_")), as.character)
  
  # Rename columns in sector_name, reef_name, and hardsoft_name data frames
  colnames(sector_name) <- c("sector", "sector_id")
  colnames(reef_name) <- c("reef", "reef_id")
  colnames(hardsoft_name) <- c("hardsoft", "hardsoft_id")
  
  # Convert IDs to lowercase
  sector_name$sector_id <- tolower(sector_name$sector_id)
  reef_name$reef_id <- tolower(reef_name$reef_id)
  hardsoft_name$hardsoft_id <- tolower(hardsoft_name$hardsoft_id)
  
  # Convert feature columns to character
  sector_name$sector <- as.character(sector_name$sector)
  reef_name$reef <- as.character(reef_name$reef)
  hardsoft_name$hardsoft <- as.character(hardsoft_name$hardsoft)
  
  # Join additional information to the main data frame
  df <- left_join(df, sector_name)
  df <- left_join(df, reef_name)
  df <- left_join(df, hardsoft_name)
  
  # Loop through restricted dataframes and perform left join
  if (region == 'MHI') {
    
    for (df_to_join in list(
      
      restricted_haw_names,
      restricted_kau_oah_a_names,
      restricted_kau_oah_b_names,
      restricted_oah_lan_mau_names,
      restricted_oah_mokapu_names
      
    )) {
      
      tryCatch({
        
        df <- left_join(df, df_to_join)
        
      }, error = function(e) {
        
        # Ignore errors and continue
        message("Error ignored:", e$message)
        
      })
    }
    
    # Filter out rows with NA in all restricted columns
    df = df %>% filter(if_all(.cols = contains("restricted_"), ~ is.na(.)))
    
  }
  
  # Create plots for depth_bin, sector_id, reef_id, and hardsoft_id
  p1 = df %>% 
    ggplot(aes(x, y, fill = depth_bin)) + 
    geom_raster() + 
    coord_fixed() +
    theme(panel.background = element_rect(fill = "gray10"),
          panel.grid = element_line(color = "gray15"))
  
  p2 = df %>% 
    ggplot(aes(x, y,  fill = sector_id)) + 
    geom_raster() +
    coord_fixed() +
    theme(panel.background = element_rect(fill = "gray10"),
          panel.grid = element_line(color = "gray15"))
  
  p3 = df %>% 
    ggplot(aes(x, y,  fill = reef_id)) + 
    geom_raster() +
    coord_fixed() +
    theme(panel.background = element_rect(fill = "gray10"),
          panel.grid = element_line(color = "gray15"))
  
  p4 = df %>% 
    ggplot(aes(x, y, fill = hardsoft_id)) + 
    geom_raster() +
    coord_fixed() +
    theme(panel.background = element_rect(fill = "gray10"),
          panel.grid = element_line(color = "gray15"))
  
  png(paste0("outputs/map/base_layers_", islands[isl], ".png"), height = 15, width = 15, res = 500, units = "in")
  print((p1 + p2) / (p3 + p4))
  dev.off()
  
  if (islands[isl] %in% c("kin", "ros")) {
    
    df = df %>%
      subset(reef_id %in% c( "forereef", "backreef", "lagoon", "protected slope")) %>% # filter land and Reef Crest/Reef Flat
      subset(hardsoft_id %in% c("hard", "unknown")) # filter for sector
    
  } else if (islands[isl] == "swa") {
    
    df = df %>%
      subset(reef_id %in% c( "forereef", "reef crest/reef flat")) %>% # keep reef crest/reef flat to emphasize east side of swain
      subset(hardsoft_id %in% c("hard", "unknown")) # filter for sector
    
  } else {
    
    df = df %>%
      subset(sector_id != "GUA_LAND") %>% # filter sector
      subset(reef_id %in% c( "forereef")) %>% # filter land and Reef Crest/Reef Flat
      subset(hardsoft_id %in% c("hard", "unknown")) # filter for sector
    
  }
  
  df$strat = paste(df$depth_bin, 
                   df$sector_id,
                   df$reef_id,
                   sep = "_")
  
  df$strat = as.numeric(as.factor(df$strat))
  
  ################################################################
  ### create table to match strata to numbers for output table ###
  ################################################################
  tab <- df 
  tab$depth_bin_value = ""
  tab$depth_bin_value = ifelse(tab$depth_bin == "shallow", "SHAL", tab$depth_bin_value) 
  tab$depth_bin_value = ifelse(tab$depth_bin == "mid", "MIDD", tab$depth_bin_value) 
  tab$depth_bin_value = ifelse(tab$depth_bin == "deep", "DEEP", tab$depth_bin_value) 
  tab <- tab %>% dplyr::select(sector_id,reef_id,strat,depth_bin_value)
  tab <- tab %>% filter(!duplicated(tab))
  save(tab,file = paste0("outputs/sector_key/", islands[isl], ".RData"))
  
  png(paste0("outputs/map/strata_", islands[isl], ".png"), height = 10, width = 10, res = 500, units = "in")
  
  print(df %>% 
          ggplot( aes(longitude, latitude, fill = factor(strat))) + 
          geom_raster() + 
          scale_fill_discrete("strata") + 
          coord_fixed() +
          theme(panel.background = element_rect(fill = "gray10"),
                panel.grid = element_line(color = "gray15")))
  
  dev.off()
  
  cell = rasterFromXYZ(df[,c("longitude", "latitude", "cell")])#; plot(cell)
  division = rasterFromXYZ(df[,c("longitude", "latitude", "division")])#; plot(division)
  strat = rasterFromXYZ(df[,c("longitude", "latitude", "strat")])#; plot(strat)
  depth = rasterFromXYZ(df[,c("longitude", "latitude", "depth")])#; plot(depth)
  
  values = raster::values
  
  survey_grid_ncrmp = stack(cell, division, strat, depth)
  survey_grid_ncrmp$strat = round(survey_grid_ncrmp$strat, digits = 0)
  values(survey_grid_ncrmp$division) = ifelse(is.na(values(survey_grid_ncrmp$division)), NA, 1)
  
  survey_grid_ncrmp = readAll(survey_grid_ncrmp)
  
  save(survey_grid_ncrmp, file = paste0("data/survey_grid_ncrmp/survey_grid_", islands[isl], ".RData"))
  
  cat(paste0("... ", islands[isl], " survey domain generated ...\n"))
  
  default_proj = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  crs(survey_grid_ncrmp) = default_proj
  
  # # export strata as shapefiles
  # p <- raster::rasterToPolygons(survey_grid_ncrmp$strat, dissolve = TRUE); sp::plot(p)
  # sf_object <- st_as_sf(p)
  # shapefile_dir <- dirname(paste0("outputs/shapefiles/", islands[isl], "_strata.shp"))
  # if (!file.exists(shapefile_dir)) dir.create(shapefile_dir)
  # st_write(sf_object, dsn = shapefile_dir, layer = basename(shapefile_name), driver = "ESRI Shapefile", append = FALSE)
  
}
