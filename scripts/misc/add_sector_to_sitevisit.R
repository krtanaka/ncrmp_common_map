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
library(maps)

# READ IN DATA ------------------------
df = read.csv("data/spc/Cleaned_sitevisit_112322.csv")
df = df[,2:51]
plot(df$LONGITUDE_LOS, df$LATITUDE_LOS, pch = 20, col = 2); map(add = T)
points(df$LONGITUDE_SV, df$LATITUDE_SV, pch = 20, col = 4)

df_sec = NULL

for (isl in 1:length(unique(df$ISLAND))) {
  
  # isl = 12
  
  df_i = df %>% subset(ISLAND == unique(df$ISLAND)[isl])
  plot(df_i$LONGITUDE_SV, df_i$LATITUDE_SV); map(add = T)
  
  # re-project in correct utm zone 
  zone <- (floor((df_i$LONGITUDE_SV[1] + 180)/6) %% 60) + 1
  
  if (median(df_i$LATITUDE_SV) > 0) xy_utm = as.data.frame(cbind(utm = project(as.matrix(df_i[, c("LONGITUDE_SV", "LATITUDE_SV")]), paste0("+proj=utm +units=m +zone=",  zone, " +north"))))
  if (median(df_i$LATITUDE_SV) < 0) xy_utm = as.data.frame(cbind(utm = project(as.matrix(df_i[, c("LONGITUDE_SV", "LATITUDE_SV")]), paste0("+proj=utm +units=m +zone=",  zone, " +south"))))
  
  colnames(xy_utm) = c("X", "Y")
  df_i = cbind(df_i, xy_utm)
  
  # create a spatial object
  latlon = df_i[,c("X", "Y")]
  coordinates(latlon) = ~X+Y
  
  # read shp file, assign same projection attribute
  if (unique(df$ISLAND)[isl] == "Guam") shp <- rgdal::readOGR("N:/GIS/Projects/CommonMaps/Sector/gua_base_land_openwater_mpa_finalize.shp")
  if (unique(df$ISLAND)[isl] == "Tutuila") shp <- rgdal::readOGR("N:/GIS/Projects/CommonMaps/Sector/tut_sector.shp")
  if (unique(df$ISLAND)[isl] == "Hawaii") shp <- rgdal::readOGR("N:/GIS/Projects/CommonMaps/Sector/HAW_sectors.shp")
  
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
  ggplot(aes(LONGITUDE_SV , LATITUDE_SV, fill = SEC_NAME, color = SEC_NAME)) +
  geom_point(shape = 21, size = 2) + 
  coord_fixed()

write_csv(df_sec, "outputs/Cleaned_sitevisit_112322_SEC_NAME.csv")
