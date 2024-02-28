rm(list=ls())
library(raster)
library(terra)
library(sf)
library(stars)

grids=list.files("./data/survey_grid_ncrmp/",full.names = T)
drop_grids=which(regexpr(pattern = "Oahu",text = grids)>0)
grids=grids[-drop_grids]

keys=list.files("./outputs/sector_key/",full.names = T)


for(i in 1:length(grids)){
#for(i in c(7,9,22)){
  #i=1
  #Get the right grid and sector key
  grid_file=grids[i]
  grid_isl=substr(grid_file,38,40)
  keys_file=keys[which(substr(keys,22,24)==grid_isl)]
  
  #load raster and convert into SpatRaster
  load(grid_file) #called survey_grid_ncrmp
  survey_grid_ncrmp.sr=as(survey_grid_ncrmp,"SpatRaster")
  #load keys and convert into str_name
  load(keys_file)
  #generate str_name in lookup table
  tab$strata_id=paste0(tab$sector_id,"_",substr(tab$depth_bin_value,1,1))
  tabLU=tab$strata_id
  names(tabLU)=tab$strat
  
  
  #convert SpatRaster to polygon data
  survey_grid_ncrmp.sh=as.polygons(survey_grid_ncrmp.sr$strat)
  survey_grid_ncrmp.sh$str_name=tabLU[survey_grid_ncrmp.sh$strat]
  
  #determine UTM 
  survey_grid_ncrmp.sf=st_set_crs(x = st_as_sf(survey_grid_ncrmp.sh),value=32655)
  
  st_write(obj = survey_grid_ncrmp.sf,
           paste0("./outputs/shapefiles/",grid_isl,"_strata_names.shp"),
           append=F)
  }
