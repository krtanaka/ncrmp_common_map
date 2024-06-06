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
library(terra)

utm = read_csv('data/misc/ncrmp_utm_zones.csv')
domain_sf_object=NULL
desired_resolution <- 0.01
PLOT=FALSE

for(reg in 1:6){
  if(reg ==1){
    islands = c("gua", "rot", "sai", "tin", "agu"); region = "S.MARIAN"                           # South Mariana Islands
  } else if(reg ==2){
    islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "pag", "sar"); region = "N.MARIAN"      # North Mariana Islands
  } else if(reg ==3){
    islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                              # American Samoa
  } else if(reg ==4){
    islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"                # Pacific Remote Island Areas
  } else if(reg ==5){
    islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"    # Main Hawaiian Islands
  } else{
    islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"                 # Northern Hawaiian Islands
  }  
  
  regional_sf_object=NULL
  for (isl in 1:length(islands)) {
    
    # isl = 1

    #Load huge 50m survey grid raster data
    load(paste0("data/survey_grid_ncrmp/survey_grid_", islands[isl], ".RData"))
    
    #Print Status
    cat(paste0("converting rasterized strata layer to shapefile for ", 
               islands[isl], " at ", 
               desired_resolution, " km resolution ...\n"))

    #terra obejct for fast rasterization
    survey_grid_ncrmp.t=as(survey_grid_ncrmp,"SpatRaster"); rm(list="survey_grid_ncrmp")
    
    #crs local utm setting
    utm_proj = paste0("epsg:",utm$epsg[match(islands[isl],utm$Island_Code)])
    terra::crs(survey_grid_ncrmp.t) = utm_proj
    
    #pull just the strata
    strat <- survey_grid_ncrmp.t$strat
    
    strat <- round(strat$strat, 0)
    names(strat) = "strat"
    
    #convert to polygons
    p <- terra::as.polygons(strat, aggregate = TRUE)
    
    #get names for strata
    key = read_csv(paste0("outputs/tables/strata_keys_", region, "_", islands[isl], ".csv"))
    strat_nam_vector <- setNames(key$strat_nam, key$strat)
    #assign them
    p$strat_nam <- strat_nam_vector[as.character(p$strat)]
    
    #work in sf polygon
    sf_object <- st_as_sf(p)
    
    if (PLOT){#check em
      plot(sf_object["strat"])
      plot(sf_object["strat_nam"])
    }
    
    #build and create output dir
    shapefile_dir <- dirname(paste0("outputs/shapefiles/", islands[isl], "_strata.shp"))
    if (!file.exists(shapefile_dir)) dir.create(shapefile_dir)

    #build output filename
    shapefile_file <- paste0("outputs/shapefiles/", islands[isl], "_strata.shp")
    #write island object
    st_write(sf_object, dsn = shapefile_file, 
             driver = "ESRI Shapefile", append = FALSE)
    
    #tranform to universal WGS84
    sf_object.84=st_transform(x = sf_object,crs = 4326)
    #bind into regional object
    regional_sf_object=rbind(regional_sf_object,sf_object.84)
    
    #notification
    print(paste0("Done with Island: ",islands[isl]))
  }

  #built regional file
  regional_shapefile_file <- paste0("outputs/shapefiles/", region, "_strata.shp")
  #Check em
  if(PLOT){
    plot(regional_sf_object["strat_nam"])
  }
  #write regional object
  st_write(regional_sf_object, dsn = regional_shapefile_file, 
           driver = "ESRI Shapefile", append = FALSE)
  #bind into domain object
  domain_sf_object=rbind(domain_sf_object,regional_sf_object)
  
  #notification
  print(paste0("Done with Region: ",region))
  
}
#built domain file
domain_shapefile_file <- paste0("outputs/shapefiles/", "NCRMP_Domain", "_strata.shp")
#write domain object
st_write(domain_sf_object, dsn = domain_shapefile_file, 
         driver = "ESRI Shapefile", append = FALSE)

#notification
print(paste0("Done and Done."))

