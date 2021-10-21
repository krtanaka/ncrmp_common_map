#####################################################################################################
### Mariana Trench 6 arc-second Bathymetric Digital Elevation Model                               ###
### https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ngdc.mgg.dem:4870  ###
#####################################################################################################

library(dplyr)
library(ggplot2)
library(raster)
library(tidyverse)

topo = raster("L:/ktanaka/GIS/bathymetry/mariana_trench_6_msl_2012.nc")
topo = as.data.frame(rasterToPoints(topo))
topo$Topography = ifelse(topo$GDAL.Band.Number.1 %in% c(-30:0), topo$GDAL.Band.Number.1, NA)
topo = topo %>% drop_na()

# Auijan
topo = topo %>% 
  subset(x > 145.5 & x < 145.6) %>% 
  subset(y > 14.8 & y < 14.9)

topo %>%
  ggplot(aes(x, y, fill = GDAL.Band.Number.1)) +
  # geom_tile(aes(width = 0.005, height = 0.005)) +
  geom_raster() + 
  scale_fill_viridis_c() +
  coord_fixed() +
  ggdark::dark_theme_minimal() +
  theme(axis.title = element_blank())

save(topo, file = 'data/gis_bathymetry/raster/mariana_0_30m.RData')
save(topo, file = 'data/gis_bathymetry/raster/agu.RData')
