#####################################################
### Add sector information to site-level data ###
#####################################################
# make sure you are connected to relevant PIFSC drives

rm(list = ls())

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)
library(sp)
library(sf)

get_utm <- function(x, y, zone, loc){
  points = SpatialPoints(cbind(x, y), proj4string = CRS("+proj=longlat +datum=WGS84"))
  points_utm = spTransform(points, CRS(paste0("+proj=utm +zone=", zone[1]," +ellps=WGS84 +north")))
  if (loc == "x") {
    return(coordinates(points_utm)[,1])
  } else if (loc == "y") {
    return(coordinates(points_utm)[,2])
  }
}

# READ IN DATA ------------------------
df = read.csv("data/spc/Cleaned_sitevisit_112322.csv")
df = df[,2:51]
plot(df$LONGITUDE_LOS, df$LATITUDE_LOS, pch = 20, col = 2); map(add = T)
points(df$LONGITUDE_SV, df$LATITUDE_SV, pch = 20, col = 4)

df_sec = NULL

for (isl in 1:length(unique(df$ISLAND))) {
  
  # isl = 1
  
  df_i = df %>% subset(ISLAND == unique(df$ISLAND)[isl])
  plot(df_i$LONGITUDE_SV, df_i$LATITUDE_SV); map(add = T)
  
  # re-project in correct utm zone 
  zone <- (floor((df_i$LONGITUDE_SV[1] + 180)/6) %% 60) + 1
  
  xy_utm = data.frame(x = df_i$LONGITUDE_SV, y = df_i$LATITUDE_SV)  %>% 
    mutate(zone2 = (floor((x + 180)/6) %% 60) + 1, keep = "all") %>% 
    group_by(zone2) %>% 
    mutate(utm_x = get_utm(x, y, zone2, loc = "x"),
           utm_y = get_utm(x, y, zone2, loc = "y")) %>% 
    ungroup() %>% 
    select(utm_x, utm_y) %>% 
    as.data.frame()
  
  colnames(xy_utm) = c("X", "Y")
  df_i = cbind(df_i, xy_utm)
  
  # create a spatial object
  latlon = df_i[,c("X", "Y")]
  coordinates(latlon) = ~X+Y
  
  # read shp file, assign same projection attribute
  if (unique(df$ISLAND)[isl] == "Guam") shp <- st_read(file.path("N:/GIS/Projects/CommonMaps/Sector/gua_base_land_openwater_mpa_finalize.shp")) %>% as("Spatial")
  if (unique(df$ISLAND)[isl] == "Tutuila") shp <- st_read(file.path("N:/GIS/Projects/CommonMaps/Sector/tut_sector.shp")) %>% as("Spatial")
  if (unique(df$ISLAND)[isl] == "Hawaii") shp <- st_read(file.path("N:/GIS/Projects/CommonMaps/Sector/HAW_sectors.shp")) %>% as("Spatial")
  
  if (median(df_i$LATITUDE_SV) > 0) CRS.new <- CRS(paste0("+proj=utm +zone=", zone, " +datum=WGS84 +units=m +no_defs +north"))
  if (median(df_i$LATITUDE_SV) < 0) CRS.new <- CRS(paste0("+proj=utm +zone=", zone, " +datum=WGS84 +units=m +no_defs +south"))
  
  proj4string(latlon) <- CRS.new
  proj4string(shp) <- CRS.new # WARNING MESSAGE OK
  area <- over(latlon, shp)
  
  # combine with site data
  df_i = cbind(df_i, as.data.frame(area))
  
  # if sites do not fall into a sector, create island name as the sector name
  df_i = df_i %>% mutate(SEC_NAME = ifelse(is.na(SEC_NAME), ISLAND, SEC_NAME))
  df_i$SEC_NAME = toupper(df_i$SEC_NAME)
  
  df_i = df_i %>% dplyr::select(names(df), SEC_NAME)
  
  df_sec = rbind(df_sec, df_i)
  
}

df_sec %>%
  subset(REGION == "MARIAN") %>% 
  subset(ISLAND == "Guam") %>% 
  ggplot(aes(LONGITUDE_SV , LATITUDE_SV, fill = SEC_NAME, color = SEC_NAME)) +
  geom_point(shape = 21, size = 2) + 
  coord_fixed()

write_csv(df_sec, "outputs/Cleaned_sitevisit_112322_SEC_NAME.csv")
