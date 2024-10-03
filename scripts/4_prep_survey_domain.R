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
library(ggthemes)
library(data.table)
library(terra)

utm = read_csv('data/misc/ncrmp_utm_zones.csv')

islands = c("gua", "rot", "sai", "tin", "agu"); region = "S.MARIAN"                           # South Mariana Islands
islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "pag", "sar"); region = "N.MARIAN"      # North Mariana Islands
islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                              # American Samoa
islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"                # Pacific Remote Island Areas
islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"    # Main Hawaiian Islands
islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"                 # Northern Hawaiian Islands

select = dplyr::select

for (isl in 1:length(islands)) {
  
  # isl = 2
  
  load(paste0("data/gis_bathymetry/", islands[isl], ".RData"))
  
  if (file.exists(paste0("data/gis_bathymetry/", islands[isl], "_merged.RData"))) {
    load(paste0("data/gis_bathymetry/", islands[isl], "_merged.RData"))
  }
  
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
    
    if (file.exists(paste0("data/gis_bathymetry/", islands[isl], "_merged.RData"))) {
      load(paste0("data/gis_bathymetry/", islands[isl], "_merged.RData"))
    }
    
    sector = topo_i
    sector[sector <= 0] <- 1
    sector_name = data.frame(ID = 1L, nam = paste0(islands[isl], "_sector"))
    
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
    
    if (file.exists(paste0("data/gis_bathymetry/", islands[isl], "_merged.RData"))) {
      load(paste0("data/gis_bathymetry/", islands[isl], "_merged.RData"))
    }
    
    reef = topo_i
    reef[reef <= 0] <- 1
    reef_name = data.frame(ID = 1L, nam = "forereef")
    
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
    
    if (file.exists(paste0("data/gis_bathymetry/", islands[isl], "_merged.RData"))) {
      load(paste0("data/gis_bathymetry/", islands[isl], "_merged.RData"))
    }
    
    hardsoft = topo_i
    hardsoft[hardsoft <= 0] <- 1
    hardsoft_name = data.frame(ID = 1L, nam = "hard")
    
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
    
    if (file.exists(paste0("data/gis_bathymetry/", islands[isl], "_merged.RData"))) {
      load(paste0("data/gis_bathymetry/", islands[isl], "_merged.RData"))
    }
    
    buffer = topo_i
    buffer[buffer <= 0] <- 1
    buffer_name = data.frame(ID = 1L, nam = paste0(islands[isl], "_5km_buffer"))
    
  }
  
  ### survey boxes ###
  if (file.exists(paste0("data/gis_survey_boxes/", islands[isl], ".RData"))) {
    
    load(paste0("data/gis_survey_boxes/", islands[isl], ".RData"))
    boxes = raster_and_table[[1]]; boxes_name = raster_and_table[[2]]
    
  } else {
    
    # using bathymetry raster as a placeholder bc there is no reef data for this island
    load(paste0("data/gis_bathymetry/", islands[isl], ".RData"))
    
    if (file.exists(paste0("data/gis_bathymetry/", islands[isl], "_merged.RData"))) {
      load(paste0("data/gis_bathymetry/", islands[isl], "_merged.RData"))
    }
    
    boxes = topo_i
    boxes[boxes <= 0] <- 1
    boxes_name = data.frame(ID = 1L, nam = paste0(islands[isl], "_box"))
    
  }
  
  ### MHI restricted areas ###
  if (region == "MHI") {
    
    load("data/gis_sector/mhi_restricted_areas_haw.RData"); restricted_haw = raster_and_table
    load("data/gis_sector/mhi_restricted_areas_kau_oah_a.RData"); restricted_kau_oah_a = raster_and_table
    load("data/gis_sector/mhi_restricted_areas_kau_oah_b.RData"); restricted_kau_oah_b = raster_and_table
    load("data/gis_sector/mhi_restricted_areas_oah_lan_mai.RData"); restricted_oah_lan_mai = raster_and_table
    load("data/gis_sector/mhi_restricted_areas_oah_mokapu.RData"); restricted_oah_mokapu = raster_and_table
    
    restricted_haw_areas = restricted_haw[[1]]; restricted_haw_names = restricted_haw[[2]]
    restricted_kau_oah_a_areas = restricted_kau_oah_a[[1]]; restricted_kau_oah_a_names = restricted_kau_oah_a[[2]]
    restricted_kau_oah_b_areas = restricted_kau_oah_b[[1]]; restricted_kau_oah_b_names = restricted_kau_oah_b[[2]]
    restricted_oah_lan_mai_areas = restricted_oah_lan_mai[[1]]; restricted_oah_lan_mai_names = restricted_oah_lan_mai[[2]]
    restricted_oah_mokapu_areas = restricted_oah_mokapu[[1]]; restricted_oah_mokapu_names = restricted_oah_mokapu[[2]]
    
    restricted_haw_names$ID  = as.character(restricted_haw_names$ID)
    restricted_kau_oah_a_names$ID = as.character(restricted_kau_oah_a_names$ID)
    restricted_kau_oah_b_names$ID = as.character(restricted_kau_oah_b_names$ID)
    restricted_oah_lan_mai_names$ID = as.character(restricted_oah_lan_mai_names$ID)
    restricted_oah_lan_mai_names$ID = as.character(restricted_oah_lan_mai_names$ID)
    restricted_oah_mokapu_names$ID = as.character(restricted_oah_mokapu_names$ID)
    
    colnames(restricted_haw_names) = c("restricted_haw_areas", "restricted_haw_areas_id")
    colnames(restricted_kau_oah_a_names) = c("restricted_kau_oah_a_areas", "restricted_kau_oah_a_names_id")
    colnames(restricted_kau_oah_b_names) = c("restricted_kau_oah_b_areas", "restricted_kau_oah_b_names_id")
    colnames(restricted_oah_lan_mai_names) = c("restricted_oah_lan_mai_areas", "restricted_oah_lan_mai_names_id")
    colnames(restricted_oah_mokapu_names) = c("restricted_oah_mokapu_areas", "restricted_oah_mokapu_names_id")
    
    restricted_haw_names$restricted_haw_areas_id <- gsub(" ", "_", tolower(restricted_haw_names$restricted_haw_areas_id))
    restricted_kau_oah_a_names$restricted_kau_oah_a_names_id <- gsub(" ", "_", tolower(restricted_kau_oah_a_names$restricted_kau_oah_a_names_id))
    restricted_kau_oah_b_names$restricted_kau_oah_b_names_id <- gsub(" ", "_", tolower(restricted_kau_oah_b_names$restricted_kau_oah_b_names_id))
    restricted_oah_lan_mai_names$restricted_oah_lan_mai_names_id <- gsub(" ", "_", tolower(restricted_oah_lan_mai_names$restricted_oah_lan_mai_names_id))
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
    restricted_kau_oah_a_areas = resample(restricted_kau_oah_a_areas, topo_i, method = "ngb")
    restricted_kau_oah_b_areas = resample(restricted_kau_oah_b_areas, topo_i, method = "ngb")
    
  }
  
  if (islands[isl] %in% c("oah", "lan", "mai")) {
    
    restricted_oah_lan_mai_areas = resample(restricted_oah_lan_mai_areas, topo_i, method = "ngb")
    
  } 
  
  rm(topo_i); gc()
  
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
               restricted_oah_lan_mai_areas,
               restricted_oah_mokapu_areas)
    
    names(df) <- c("hardsoft", "sector", "reef", "depth", "buffer", 
                   "restricted_kau_oah_a_areas", 
                   "restricted_kau_oah_b_areas", 
                   "restricted_oah_lan_mai_areas",
                   "restricted_oah_mokapu_areas")
  } 
  
  # if (islands[isl] %in% c("kau")) {
  #   
  #   df = stack(hardsoft, sector, reef, bathymetry, buffer, 
  #              restricted_kau_oah_a_areas, 
  #              restricted_kau_oah_b_areas)
  #   
  #   names(df) <- c("hardsoft", "sector", "reef", "depth", "buffer", 
  #                  "restricted_kau_oah_a_areas", 
  #                  "restricted_kau_oah_b_areas")
  # } 
  
  if (islands[isl] %in% c("mai", "lan")) {
    
    df = stack(hardsoft, sector, reef, bathymetry, buffer, restricted_oah_lan_mai_areas)
    names(df) <- c("hardsoft", "sector", "reef", "depth", "buffer", "restricted_oah_lan_mai_areas")
    
  } 
  
  # Convert raster to data frame and add cell numbers
  # df <- as.data.frame(rasterToPoints(df))
  df <- terra::as.data.frame(df, xy = TRUE, na.rm = FALSE) 
  
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
      restricted_oah_lan_mai_names,
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
  
  load("data/spc/SURVEY MASTER.RData")
  utm_i = utm %>% filter(Island_Code == islands[isl])
  sv = SURVEY_MASTER %>% 
    mutate(ISLAND = gsub(" ", "_", ISLAND)) %>% 
    filter(ISLAND == utm_i$Island) %>% 
    mutate(YEAR = ifelse(is.na(ANALYSIS_YEAR), OBS_YEAR, ANALYSIS_YEAR)) %>% 
    dplyr::select(LONGITUDE_LOV, LATITUDE_LOV, YEAR) %>% 
    na.omit()
  
  get_utm <- function(x, y, zone, loc){
    points = SpatialPoints(cbind(x, y), proj4string = CRS("+proj=longlat +datum=WGS84"))
    points_utm = spTransform(points, CRS(paste0("+proj=utm +zone=", zone[1]," +ellps=WGS84 +north")))
    if (loc == "x") {
      return(coordinates(points_utm)[,1])
    } else if (loc == "y") {
      return(coordinates(points_utm)[,2])
    }
  }
  
  xy_utm = data.frame(x = sv$LONGITUDE_LOV, y = sv$LATITUDE_LOV)  %>% 
    # mutate(zone2 = (floor((x + 180)/6) %% 60) + 1, keep = "all") %>% 
    mutate(zone2 = utm_i$UTM_Zone) %>%
    group_by(zone2) %>% 
    mutate(utm_x = get_utm(x, y, zone2, loc = "x"),
           utm_y = get_utm(x, y, zone2, loc = "y")) %>% 
    ungroup() %>% 
    dplyr::select(utm_x, utm_y) 
  
  colnames(xy_utm) = c("X", "Y")
  sv = cbind(sv, xy_utm)
  
  p1 = ggplot() + 
    geom_raster(data = df, aes(x, y, fill = depth_bin)) +
    geom_point(data = sv, aes(X, Y), color = "yellow", alpha = 0.8) + 
    theme_map() +
    scale_fill_discrete("") + 
    ggtitle("depth_bins") + 
    theme(panel.background = element_rect(fill = "gray10"),
          panel.grid = element_line(color = "gray15"),
          legend.background = element_rect(fill = "transparent"), 
          legend.text = element_text(size = 15, face = "bold", color = "white"))
  
  p2 = ggplot() + 
    geom_raster(data = df, aes(x, y, fill = sector_id)) +
    geom_point(data = sv, aes(X, Y), color = "yellow", alpha = 0.8) + 
    theme_map() +
    scale_fill_discrete("") + 
    ggtitle("sector") + 
    theme(panel.background = element_rect(fill = "gray10"),
          panel.grid = element_line(color = "gray15"),
          legend.background = element_rect(fill = "transparent"), 
          legend.text = element_text(size = 15, face = "bold", color = "white"))
  
  p3 = ggplot() + 
    geom_raster(data = df, aes(x, y, fill = reef_id)) +
    geom_point(data = sv, aes(X, Y), color = "yellow", alpha = 0.8) + 
    theme_map() +
    scale_fill_discrete("") + 
    ggtitle("reef_type") + 
    theme(panel.background = element_rect(fill = "gray10"),
          panel.grid = element_line(color = "gray15"),
          legend.background = element_rect(fill = "transparent"), 
          legend.text = element_text(size = 15, face = "bold", color = "white"))
  
  p4 = ggplot() + 
    geom_raster(data = df, aes(x, y, fill = hardsoft_id)) +
    geom_point(data = sv, aes(X, Y), color = "yellow", alpha = 0.8) + 
    theme_map() +
    scale_fill_discrete("") + 
    ggtitle("benthic_type") + 
    theme(panel.background = element_rect(fill = "gray10"),
          panel.grid = element_line(color = "gray15"),
          legend.background = element_rect(fill = "transparent"), 
          legend.text = element_text(size = 15, face = "bold", color = "white"))
  
  png(paste0("outputs/maps/base_layers_", region, "_", islands[isl], ".png"), height = 15, width = 15, res = 500, units = "in")
  print((p1 + p2) / (p3 + p4))
  dev.off()
  
  if (islands[isl] %in% c("kin", "ros", "ffs", "mar")) {
    
    df = df %>%
      subset(reef_id %in% c( "forereef", "backreef", "lagoon", "protected slope")) %>% # filter land and Reef Crest/Reef Flat
      subset(hardsoft_id %in% c("hard", "unknown")) # filter for sector
    
  } else if (islands[isl] == "mid") {
    
    df = df %>%
      subset(reef_id %in% c( "forereef", "backreef", "lagoon", "protected slope")) %>% # filter land and Reef Crest/Reef Flat
      subset(hardsoft_id %in% c("hard", "unknown", "other delinations")) 
    
  } else if (islands[isl] == "swa") {
    
    df = df %>%
      subset(reef_id %in% c( "forereef", "reef crest/reef flat")) %>% # keep reef crest/reef flat to emphasize east side of swain
      subset(hardsoft_id %in% c("hard", "unknown")) 
    
  } else if (islands[isl] == "lis") {
    
    df = df %>%
      subset(hardsoft_id %in% c("hard", "unknown")) 
    
  } else if (islands[isl] %in% c("kur", "phr")) {
    
    df = df %>%
      mutate(reef_id = coalesce(reef_id, "forereef")) %>% 
      subset(reef_id %in% c( "forereef", 
                             "back reef",
                             "outer back reef", 
                             "inner back reef",
                             "outer lagoon", 
                             "inner lagoon")) %>% # keep backreef and lagoon
      mutate(reef_id = ifelse(reef_id == "outer back reef", "backreef", reef_id),
             reef_id = ifelse(reef_id == "inner back reef", "backreef", reef_id),
             reef_id = ifelse(reef_id == "back reef", "backreef", reef_id),
             reef_id = ifelse(reef_id == "outer lagoon", "lagoon", reef_id),
             reef_id = ifelse(reef_id == "inner lagoon", "lagoon", reef_id)) %>% 
      subset(hardsoft_id %in% c("hard", "unknown")) 
    
  } else if (islands[isl] %in% c("lay")) {
    
    df = df %>%
      subset(hardsoft_id %in% c("hard")) 
    
  } else {
    
    df = df %>%
      subset(sector_id != "GUA_LAND") %>% 
      subset(reef_id %in% c( "forereef")) %>% 
      subset(hardsoft_id %in% c("hard", "unknown", "other", "other delinations")) 
    
  }
  
  df$strat_nam = paste(df$depth_bin, 
                       df$sector_id,
                       df$reef_id,
                       sep = "_")
  
  df$strat = as.numeric(as.factor(df$strat_nam))
  
  ################################################################
  ### create table to match strata to numbers for output table ###
  ################################################################
  tab <- df 
  
  tab$depth_bin_value = ""
  tab$depth_bin_value = ifelse(tab$depth_bin == "shallow", "SHAL", tab$depth_bin_value) 
  tab$depth_bin_value = ifelse(tab$depth_bin == "mid", "MIDD", tab$depth_bin_value) 
  tab$depth_bin_value = ifelse(tab$depth_bin == "deep", "DEEP", tab$depth_bin_value) 
  
  tab <- tab %>% 
    select(strat, strat_nam, sector_id, reef_id, depth_bin_value) %>%
    distinct() %>% 
    mutate(across(where(is.character), toupper))
  
  colnames(tab)[5] <- "depth_bin"
  
  write_csv(tab, file = paste0("outputs/tables/strata_keys_", region, "_", islands[isl], ".csv"))
  
  png(paste0("outputs/maps/strata_", region, "_", islands[isl], ".png"), height = 8, width = 12, res = 500, units = "in")
  
  print(ggplot() + 
          geom_raster(data = df, aes(longitude, latitude, fill = factor(strat_nam))) +
          # geom_point(data = sv %>% subset(YEAR > 2009), aes(X*0.001, Y*0.001, color = factor(YEAR)), alpha = 0.9) +
          geom_point(data = sv %>% subset(YEAR > 2009), aes(X*0.001, Y*0.001), color = "yellow", alpha = 0.9, show.legend = F) +
          scale_fill_discrete("") + 
          scale_color_viridis_d("") +
          # coord_fixed() +
          # theme_map() +
          labs(x = "", y = "") + 
          ggtitle(paste0(region, "_", islands[isl])) + 
          theme(panel.background = element_rect(fill = "gray10"),
                panel.grid = element_line(color = "gray15")
                # ,
                # legend.background = element_rect(fill = "transparent"), 
                # legend.text = element_text(color = "white"),           
                # legend.title = element_text(color = "white")
          ))
  
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
  
  cells <- data.table(terra::as.data.frame(survey_grid_ncrmp, xy = T, cells = T, na.rm = T))

  strat_det <- cells[, list(strat_cells = .N), by = "strat"]; strat_det
  strat_det$cell_area <- prod(res(survey_grid_ncrmp)); strat_det
  strat_det$strat_area <- strat_det$strat_cells * prod(res(survey_grid_ncrmp)); strat_det
  
  cat(paste0("saving strata table for ", region, " ", islands[isl], " as CSV...\n"))
  readr::write_csv(strat_det, file = paste0("outputs/tables/strata_table_", region, "_", islands[isl], ".csv"))
  
  save(survey_grid_ncrmp, file = paste0("data/survey_grid_ncrmp/survey_grid_", islands[isl], ".RData"))
  cat(paste0("... ", islands[isl], " survey domain generated ...\n"))
  
}
