################################################
### Add sector information to site visit #######
################################################

rm(list = ls())

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)
library(sp)
library(sf)

# Ensure you are connected to the T Drive and X Drive

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
x <- read.csv("T:/DataManagement/NCEI Archive Packages/Fish REA nSPC (NCRMP)/Data/2021/V0_FISH_REA_OAHU 2021.csv")

# ADD SECTOR INFO ---------------------
survey <- x %>%
  select(OBS_YEAR, REGION, REGION_NAME, ISLAND, SITE, DATE_, REEF_ZONE, DEPTH_BIN, LATITUDE, LONGITUDE, SITEVISITID, METHOD) %>%
  distinct(SITEVISITID, .keep_all = TRUE)

df <- survey

zone <- (floor((df$LONGITUDE[1] + 180)/6) %% 60) + 1

xy_utm = data.frame(x = df$LONGITUDE, y = df$LATITUDE)  %>% 
  mutate(zone2 = (floor((x + 180)/6) %% 60) + 1, keep = "all") %>% 
  group_by(zone2) %>% 
  mutate(utm_x = get_utm(x, y, zone2, loc = "x"),
         utm_y = get_utm(x, y, zone2, loc = "y")) %>% 
  ungroup() %>% 
  select(utm_x, utm_y) %>% 
  as.data.frame()

colnames(xy_utm) = c("X", "Y")
df = cbind(df, xy_utm)

# create a spatial object
latlon = df[,c("X", "Y")]
coordinates(latlon) = ~X+Y

# read shp file, assign same projection attribute
shp <- st_read(file.path("X:/GIS/Projects/CommonMaps/01_Preprocess/MARI/GUA/sector/gua_base_land_openwater_mpa_finalize.shp")) %>% as("Spatial")

if (median(df$LATITUDE) > 0) CRS.new <- CRS(paste0("+proj=utm +zone=", zone, " +datum=WGS84 +units=m +no_defs +north"))
if (median(df$LATITUDE) < 0) CRS.new <- CRS(paste0("+proj=utm +zone=", zone, " +datum=WGS84 +units=m +no_defs +south"))

proj4string(latlon) <- CRS.new
proj4string(shp) <- CRS.new # WARNING MESSAGE OK
area <- over(latlon, shp)

# combine with site data
df = cbind(df, as.data.frame(area))

df %>%
  ggplot(aes(X, Y, color = SEC_NAME)) +
  geom_point()

# if sites do not fall into a sector, create island name as the sector name
df<-df %>% mutate(SEC_NAME = ifelse(is.na(SEC_NAME), ISLAND, SEC_NAME))
df$SEC_NAME<-toupper(df$SEC_NAME)

x<-merge(x, df[,c("SITEVISITID", "SEC_NAME")], by="SITEVISITID", all.x=TRUE) 
