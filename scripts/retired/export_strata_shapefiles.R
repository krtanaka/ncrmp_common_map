########################################
### Exporting Strata as Shapefiles   ###
### Calculate the aggregation factor ###
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

utm = read_csv('data/misc/ncrmp_utm_zones.csv')

islands = c("gua", "rot", "sai", "tin", "agu"); region = "S.MARIAN"                           # South Mariana Islands
islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "pag", "sar"); region = "N.MARIAN"      # North Mariana Islands
islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                              # American Samoa
islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"                # Pacific Remote Island Areas
islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"    # Main Hawaiian Islands
islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"                 # Northern Hawaiian Islands

desired_resolution <- 0.01

for (isl in 4:length(islands)) {
  
  # isl = 6
  
  load(paste0("data/survey_grid_ncrmp/survey_grid_", islands[isl], ".RData"))
  
  cat(paste0("converting rasterized strata layer to shapefile for ", 
             islands[isl], " at ", 
             desired_resolution, " km resolution ...\n"))
  
  default_proj = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  crs(survey_grid_ncrmp) = default_proj
  
  strat <- survey_grid_ncrmp$strat

  # current_resolution <- res(strat)[1]
  # factor <- desired_resolution / current_resolution
  # strat <- aggregate(strat, fact = factor, fun = mean)
  strat <- round(strat$strat, 0)
  names(strat) = "strat"
  
  p <- raster::rasterToPolygons(strat, dissolve = TRUE)
  
  key = read_csv(paste0("outputs/tables/strata_keys_", region, "_", islands[isl], ".csv"))
  strat_nam_vector <- setNames(key$strat_nam, key$strat)
  p$strat_nam <- strat_nam_vector[as.character(p$strat)]
  
  sf_object <- st_as_sf(p)
  
  plot(sf_object["strat"])
  plot(sf_object["strat_nam"])
  
  # plot(sf_object["strat_nam"] %>% filter(strat_nam %in% c("deep_mai_hana_forereef", "mid_mai_hana_forereef", "shallow_mai_hana_forereef")))
  # plot(sf_object["strat_nam"] %>% filter(strat_nam %in% c("deep_mai_nw_forereef", "mid_mai_nw_forereef", "shallow_mai_nw_forereef")))
  
  shapefile_dir <- dirname(paste0("outputs/shapefiles/", islands[isl], "_strata.shp"))
  
  if (!file.exists(shapefile_dir)) dir.create(shapefile_dir)
  shapefile_dir <- paste0("outputs/shapefiles/", islands[isl], "_strata.shp")
  
  st_write(sf_object, dsn = shapefile_dir, 
           # layer = basename(shapefile_name),
           driver = "ESRI Shapefile", append = FALSE)
  
}
