################################################
### add sector information to site visit #######
################################################
# MUST CONNECT TO THE N DRIVE 

rm(list = ls())

library(readr)
library(rgdal) # needed for working with shapefiles
library(tidyr)
library(dplyr)
library(ggplot2)

# READ IN DATA ------------------------
df = read.csv("data/rea/Cleaned_sitevisit_112322.csv")
df = df[,2:51]
plot(df$LONGITUDE_LOS, df$LATITUDE_LOS, pch = 20, col = 2); map(add = T)
plot(df$LONGITUDE_SV, df$LATITUDE_SV, pch = 20, col = 2); map(add = T)

# re-project in correct utm zone 
# For Guam, UTM 55 
zone <- (floor((df$LONGITUDE_SV[1] + 180)/6) %% 60) + 1
xy_utm = as.data.frame(cbind(utm = project(as.matrix(df[, c("LONGITUDE_SV", "LATITUDE_SV")]), paste0("+proj=utm +units=m +zone=", zone))))
colnames(xy_utm) = c("X", "Y")
df = cbind(df, xy_utm)

# create a spatial object
latlon = df[,c("X", "Y")]
coordinates(latlon) = ~X+Y

# read shp file, assign same projection attribute
shp <- rgdal::readOGR("N:/GIS/Projects/CommonMaps/Sector/gua_base_land_openwater_mpa_finalize.shp")
CRS.new <- CRS("+proj=utm +zone=55 +datum=WGS84 +units=m +no_defs")
proj4string(latlon) <- CRS.new
proj4string(shp) <- CRS.new # WARNING MESSAGE OK
area <- over(latlon,shp)

# read shp file, assign same projection attribute
shp <- rgdal::readOGR("N:/GIS/Projects/CommonMaps/Sector/tut_sector.shp")
CRS.new <- CRS("+proj=utm +zone=55 +datum=WGS84 +units=m +no_defs")
proj4string(latlon) <- CRS.new
proj4string(shp) <- CRS.new # WARNING MESSAGE OK
area <- over(latlon,shp)

# combine with site data
df = cbind(df, as.data.frame(area))

# if sites do not fall into a sector, create island name as the sector name
df = df %>% mutate(SEC_NAME = ifelse(is.na(SEC_NAME), ISLAND, SEC_NAME))
df$SEC_NAME = toupper(df$SEC_NAME)

df %>%
  subset(REGION == "MARIAN") %>%
  ggplot(aes(LONGITUDE_SV , LATITUDE_SV, fill = SEC_NAME)) +
  annotation_map(map_data("world")) + 
  geom_point(shape = 21)
