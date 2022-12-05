################################################
### add sector information to site visit #######
################################################
# MUST CONNECT TO THE T DRIVE AND X DRIVE

rm(list=ls())
library(readr)
library(rgdal)    # needed for working with shapefiles
library(tidyr)

# READ IN DATA ------------------------
x<-read.csv("T:/DataManagement/NCEI Archive Packages/Fish REA nSPC (NCRMP)/Data/V0_FISH_REA_MARIAN_2022.csv")

# ADD SECTOR INFO ---------------------

survey<-x %>% select(OBS_YEAR, REGION, REGION_NAME, ISLAND, SITE, DATE_, REEF_ZONE, DEPTH_BIN, LATITUDE, LONGITUDE, SITEVISITID, METHOD)
survey<- distinct(survey, SITEVISITID, .keep_all = TRUE)
df = survey

# re-project in correct utm zone 
# For Guam, UTM 55 
zone <- (floor((df$LONGITUDE[1] + 180)/6) %% 60) + 1
xy_utm = as.data.frame(cbind(utm = project(as.matrix(df[, c("LONGITUDE", "LATITUDE")]), paste0("+proj=utm +units=m +zone=", zone))))
colnames(xy_utm) = c("X", "Y")
df = cbind(df, xy_utm)

# create a spatial object
latlon = df[,c("X", "Y")]
coordinates(latlon) = ~X+Y

# read shp file, assign same projection attribute
shp <- rgdal::readOGR("X:/GIS/Projects/CommonMaps/01_Preprocess/MARI/GUA/sector/gua_base_land_openwater_mpa_finalize.shp")
CRS.new <- CRS("+proj=utm +zone=55 +datum=WGS84 +units=m +no_defs")
proj4string(latlon) <- CRS.new
proj4string(shp) <- CRS.new # WARNING MESSAGE OK
area <- over(latlon,shp)

# combine with site data
df = cbind(df, as.data.frame(area))

df %>%
  ggplot(aes(X, Y, color = SEC_NAME)) +
  geom_point()

# if sites do not fall into a sector, create island name as the sector name
df<-df %>% mutate(SEC_NAME = ifelse(is.na(SEC_NAME), ISLAND, SEC_NAME))
df$SEC_NAME<-toupper(df$SEC_NAME)

x<-merge(x, df[,c("SITEVISITID", "SEC_NAME")], by="SITEVISITID", all.x=TRUE) 
