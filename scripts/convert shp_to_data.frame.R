# convert shapefiles to data.frame
library(sf)
library(tidyverse)

rm(list = ls())

#################
### hard/soft ###
#################
shp_list = list.files(path = "G:/GIS/hardsoft/MHI/", pattern = "shp.shp"); shp_list
for (ils in 1:length(shp_list)) {
  
  # ils = 1
  
  bottom_type = st_read(paste0("G:/GIS/hardsoft/MHI/", shp_list[ils]))
  
  # p = bottom_type %>% 
  #   st_as_sf() %>% 
  #   # filter(!HardSoft %in% c("Unknown")) %>%
  #   filter(!HardSoft %in% c("Unknown", "Land", "Other")) %>%
  #   ggplot() + 
  #   geom_sf(aes(group = HardSoft, fill = HardSoft), color = "NA", show.legend = T) + 
  #   ggdark::dark_theme_minimal() + 
  #   # scale_fill_viridis_d() + 
  #   ggtitle(paste0(shp_list[ils], "_Bottom_Substrate"))
  # 
  # pdf(paste0("outputs/", shp_list[ils], "_Bottom_Substrate.pdf"), height = 5, width = 6)
  # print(p)
  # dev.off()
  
  bottom_type = st_cast(bottom_type,"POINT")
  
  bottom_type <- bottom_type %>%
    mutate(lon = unlist(map(bottom_type$geometry,1)),
           lat = unlist(map(bottom_type$geometry,2))) %>% 
    as.data.frame()
  
  # st_write(bottom_type, paste0("data/", ils_lower, "_hs_biogeo_shp.csv"), layer_options = "GEOMETRY=AS_XY")
  # bottom_type = readr::read_csv(paste0("data/", ils_lower, "_hs_biogeo_shp.csv"))
  
  filename = substr(tolower(shp_list[ils]),1,nchar(tolower(shp_list[ils]))-4)
  
  save(bottom_type, file = paste0("data/", filename, ".RData"))
  
  print(paste0(islands[ils], "...done..."))
  
}

##############
### sector ###
##############
shp_list = list.files(path = "G:/GIS/sector/MHI/", pattern = ".shp"); shp_list
for (ils in 1:length(shp_list)) {
  
  # ils = 1
  
  sector = st_read(paste0("G:/GIS/sector/MHI/", shp_list[ils]))
  
  # p = sector %>% 
  #   st_as_sf() %>% 
  #   ggplot() + 
  #   geom_sf(aes(group = SEC_NAME, fill = SEC_NAME), color = "NA", show.legend = T) + 
  #   ggdark::dark_theme_minimal() + 
  #   # scale_fill_viridis_d() + 
  #   ggtitle(paste0(shp_list[ils], "_Sector"))
  # 
  # pdf(paste0("outputs/", shp_list[ils], "_Sector.pdf"), height = 5, width = 6)
  # print(p)
  # dev.off()
  
  sector = st_cast(sector,"POINT")
  
  sector <- sector %>%
    mutate(lon = unlist(map(sector$geometry,1)),
           lat = unlist(map(sector$geometry,2))) %>% 
    as.data.frame()
  
  filename = substr(tolower(shp_list[ils]),1,nchar(tolower(shp_list[ils]))-4)
  
  save(sector, file = paste0("data/", filename, "_shp.RData"))
  
  print(paste0(islands[ils], "...done..."))
  
}


################
### reefzone ###
################
shp_list = list.files(path = "G:/GIS/reef/MHI/", pattern = ".shp"); shp_list
for (ils in 1:length(shp_list)) {
  
  # ils = 4
  
  reef = st_read(paste0("G:/GIS/reef/MHI/", shp_list[ils]))
  
  # p = reef %>%
  #   st_as_sf() %>%
  #   filter(!REEF_ZONE %in% c("Unknown", "Land", "Other")) %>%
  #   ggplot() +
  #   geom_sf(aes(group = REEF_ZONE, fill = REEF_ZONE), color = "NA", show.legend = T) +
  #   ggdark::dark_theme_minimal() +
  #   # scale_fill_viridis_d() +
  #   ggtitle(paste0(islands[ils], "_reefzones"))
  # 
  # pdf(paste0("outputs/", shp_list[ils], "_Sector.pdf"), height = 5, width = 6)
  # print(p)
  # dev.off()
  
  reef = st_cast(reef,"POINT")
  
  reef <- reef %>%
    mutate(lon = unlist(map(reef$geometry,1)),
           lat = unlist(map(reef$geometry,2))) %>% 
    as.data.frame()
  
  filename = substr(tolower(shp_list[ils]),1,nchar(tolower(shp_list[ils]))-4)
  
  save(reef, file = paste0("data/", tolower(shp_list[ils]), ".RData"))
  
  print(paste0(islands[ils], "...done..."))
  
}
