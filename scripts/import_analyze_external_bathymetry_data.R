rm(list = ls())

library(spatial)
library(raster)
library(lubridate)
library(raster)
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(colorRamps)

GIS_Survey_Grid = read_csv("T:/Fish/GIS/Projects/Gridding/OAH/OAH_sites.csv")
GIS_Survey_Grid[GIS_Survey_Grid == -9999] <- NA

glimpse(GIS_Survey_Grid)

source("/Users/Kisei.Tanaka/env_data_summary/scripts/HelperCode/ExpandingExtract.R")

load("data/BenthicCover_2010-2020_Tier1_SITE.RData")

SM = as.data.frame(GIS_Survey_Grid)

SM = SM[complete.cases(SM[,c("X", "Y")]), ]
SM_sp = SM; SM_sp = as.data.frame(SM_sp)
coordinates(SM_sp) = ~X + Y

#strm15
this_r = raster("M:/Environmental Data Summary/DataDownload/Bathymetry_SRTM15/Bathymetry_SRTM15_Bathy_M_AllIslands.nc")

#crm, this is better
this_r = raster("G:/GIS/usgsCeCrm10.nc")

e <- extent(range(pretty(SM$X))[1] , 
            range(pretty(SM$X))[2], 
            range(pretty(SM$Y))[1],
            range(pretty(SM$Y))[2])

this_r <- crop(this_r, e)
this_r[this_r > 0] <- 0
plot(this_r, pch = ".")

crs(SM_sp) = crs(this_r)

this_Ex = ExpandingExtract(this_r, SM_sp, Dists = c(0, 1)); summary(this_Ex)

SM_sp$DEPTH_e = this_Ex$values

GIS_Survey_Grid = as.data.frame(SM_sp) %>% subset(DEPTH_e > -30)

g1 = GIS_Survey_Grid %>%
  group_by(X, Y) %>% 
  summarise(depth = mean(DEPTH_e)) %>% 
  ggplot(aes(X, Y, fill = depth)) + 
  # geom_raster(interpolate = T) +
  geom_tile(aes(height = 0.001, width = 0.001)) +
  scale_fill_viridis_c("data")+
  coord_fixed() +
  ggdark::dark_theme_void() 

g2 = GIS_Survey_Grid %>% 
  group_by(X, Y) %>% 
  summarise(depth = mean(DEPTH)) %>% 
  ggplot(aes(X, Y, fill = depth)) + 
  # geom_raster(interpolate = T) +
  geom_tile(aes(height = 0.001, width = 0.001)) +
  scale_fill_viridis_c("obs")+
  coord_fixed() +
  ggdark::dark_theme_void() 

g3 = GIS_Survey_Grid %>% 
  group_by(X, Y) %>% 
  mutate(error = DEPTH-DEPTH_e) %>% 
  summarise(error = abs(mean(error))) %>% 
  ggplot(aes(X, Y, fill = error)) + 
  # geom_raster(interpolate = T) +
  geom_tile(aes(height = 0.001, width = 0.001)) +
  scale_fill_gradientn(colours = matlab.like(10), "error") +
  coord_fixed() +
  ggdark::dark_theme_void() 

g1 + g2 + g3

save(GIS_Survey_Grid, file = "data/OAH_Grid.RData")
