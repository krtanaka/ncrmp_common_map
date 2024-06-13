########################################
### Exporting Strata as Shapefiles   ###
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

utm <- read_csv('data/misc/ncrmp_utm_zones.csv')
domain_sf_object <- NULL
PLOT <- FALSE

for(reg in 5:6){
  
  # reg = 6
  
  if(reg == 1) islands = c("gua", "rot", "sai", "tin", "agu"); region = "S.MARIAN"                           # South Mariana Islands
  if(reg == 2) islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "pag", "sar"); region = "N.MARIAN"      # North Mariana Islands
  if(reg == 3) islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                              # American Samoa
  if(reg == 4) islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"                # Pacific Remote Island Areas
  if(reg == 5) islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"    # Main Hawaiian Islands
  if(reg == 6) islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"                 # Northern Hawaiian Islands
  
  regional_sf_object = NULL
  
  for (isl in 1:length(islands)) {
    
    gc()  # Garbage collection
    
    # isl = 1
    
    # Load survey grid raster data
    load(paste0("data/survey_grid_ncrmp/survey_grid_", islands[isl], ".RData"))
    
    # print Status
    cat(paste0("\n\n\n\nconverting rasterized strata layer to shapefile for ", islands[isl], " ...\n\n\n\n"))
    
    # Convert to Terra object for fast rasterization
    survey_grid_ncrmp.t = as(survey_grid_ncrmp, "SpatRaster"); rm(list = "survey_grid_ncrmp")
    
    gc()
    
    # Set CRS local UTM
    utm_proj = paste0("+proj=utm +zone=",
                      utm$UTM_Zone[match(islands[isl], utm$Island_Code)],
                      " +",
                      utm$Hemisphere[match(islands[isl], utm$Island_Code)],
                      " +datum=WGS84 +units=km +no_defs +type=crs")
    
    terra::crs(survey_grid_ncrmp.t) = utm_proj
    
    # Extract strata
    strat <- survey_grid_ncrmp.t$strat
    
    # Convert to polygons
    p <- terra::as.polygons(strat, aggregate = TRUE)
    
    gc()  # Garbage collection
    
    # Get strata names
    key = read_csv(paste0("outputs/tables/strata_keys_", region, "_", islands[isl], ".csv"))
    strat_nam_vector <- setNames(key$strat_nam, key$strat)
    
    # Assign strata names
    p$strat_nam <- strat_nam_vector[as.character(p$strat)]
    
    # Convert to sf polygon
    sf_object <- st_as_sf(p)
    
    # Optionally plot
    if (PLOT) {
      plot(sf_object["strat"])
      plot(sf_object["strat_nam"])
    }
    
    # Create output directory
    shapefile_dir <- dirname(paste0("outputs/shapefiles/", islands[isl], "_strata.shp"))
    if (!file.exists(shapefile_dir)) dir.create(shapefile_dir)
    
    # Define output filename
    shapefile_file <- paste0("outputs/shapefiles/", islands[isl], "_strata.shp")
    
    # Write shapefile
    st_write(sf_object, dsn = shapefile_file, driver = "ESRI Shapefile", append = FALSE)
    
    # Transform to universal WGS84
    sf_object.84 = st_transform(x = sf_object, crs = 4326)
    
    # Bind into regional object
    regional_sf_object = rbind(regional_sf_object, sf_object.84)
    
    # Print notification
    print(paste0("Done with Island: ", islands[isl]))
    
  }
  
  # Define regional shapefile filename
  regional_shapefile_file <- paste0("outputs/shapefiles/", region, "_strata.shp")
  
  # Optionally plot
  if (PLOT) plot(regional_sf_object["strat_nam"])
  
  # Write regional shapefile
  st_write(regional_sf_object, dsn = regional_shapefile_file, driver = "ESRI Shapefile", append = FALSE)
  
  # Bind into domain object
  domain_sf_object = rbind(domain_sf_object, regional_sf_object)
  
  # Print notification
  print(paste0("Done with Region: ", region))
  
}

# Define domain shapefile filename
domain_shapefile_file = paste0("outputs/shapefiles/", "NCRMP_Domain", "_strata.shp")

# Write domain shapefile
st_write(domain_sf_object, dsn = domain_shapefile_file, driver = "ESRI Shapefile", append = FALSE)

# Print final notification
print(paste0("Done and Done."))

