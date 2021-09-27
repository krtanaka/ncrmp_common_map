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

source("/Users/Kisei.Tanaka/env_data_summary/scripts/HelperCode/ExpandingExtract.R")

load("data/rea/BenthicCover_2010-2020_Tier1_SITE.RData")
load("data/rea/BenthicREA_sitedata_TAXONCODE.RData")

SM = df %>% subset(REGION == "MHI") %>% as.data.frame()

SM$X = SM$LONGITUDE
SM$Y = SM$LATITUDE

SM$LONGITUDE = ifelse(SM$LONGITUDE < 0, SM$LONGITUDE + 180, SM$LONGITUDE)

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

df = as.data.frame(SM_sp)

df %>%
  group_by(X, Y) %>% 
  summarise(depth = mean(DEPTH_e)) %>% 
  ggplot(aes(X, Y, fill = depth)) + 
  # geom_raster(interpolate = T) +
  geom_tile(aes(height = 0.01, width = 0.01)) +
  scale_fill_viridis_c("data") +
  coord_fixed() +
  ggdark::dark_theme_void() 


save(df, file = "data/BenthicREA_sitedata_TAXONCODE.RData_MHI_w_CRM.RData")
