rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(tidyr)
library(marmap)
library(lattice)

topo = raster("L:/ktanaka/GIS/bathymetry/mariana_trench_6_msl_2012.nc") # Mariana 
topo = raster("L:/ktanaka/GIS/bathymetry/pago_pago_3_mhw_2009.nc") # American Samoa
topo = raster("L:/ktanaka/GIS/bathymetry/usgsCeCrm10.nc") # Main Hawaiian Islands

topo = as.data.frame(rasterToPoints(topo))
colnames(topo)[3] = "depth"
topo$depth = as.numeric(as.character(topo$depth))

topo = topo %>% 
  subset(depth > -30 & depth < 0)

topo %>%
  ggplot(aes(x, y, fill = depth)) +
  # geom_tile(aes(width = 0.005, height = 0.005)) +
  geom_raster() + 
  scale_fill_viridis_c() +
  coord_fixed() +
  ggdark::dark_theme_minimal() +
  theme(axis.title = element_blank())

save(topo, file = 'data/gis_bathymetry/raster/mariana_0_30m.RData')
save(topo, file = 'data/gis_bathymetry/raster/american_samoa_0_30m.RData')
save(topo, file = 'data/gis_bathymetry/raster/agu.RData')

wireframe(unclass(as.bathy(topo)), 
          shade = T,
          aspect = c(length(unique(topo$y))/length(unique(topo$x)), 0.01),
          par.box = c(col = "transparent"),
          scales = list(arrows = FALSE, col = "transparent"), # col="black" is required
          par.settings = list(axis.line = list(col = 'transparent')),
          light.source = c(10,0,10),
          zlab = "", 
          xlab = "",
          ylab = "",
          perspective = T,
          screen = list(z = 10, x = -40, y = 10),
          zoom = 1.1)


