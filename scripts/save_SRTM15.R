################################################################################
### Dataset Title: 	SRTM15_PLUS Estimated Topography, 15 seconds, Global, v1 ###
### https://coastwatch.pfeg.noaa.gov/erddap/griddap/srtm15plus.html          ###
################################################################################

library(dplyr)
library(ggplot2)
library(raster)
library(tidyr)
library(marmap)
library(lattice)
library(rayshader)

topo = raster("data/gis_bathymetry/raster/srtm30.nc")
topo = raster("data/gis_bathymetry/raster/etopo.nc")
topo = raster("data/gis_bathymetry/raster/srtm15.nc")
topo = raster("data/gis_bathymetry/raster/gebco.nc")
topo = raster("L:/ktanaka/GIS/bathymetry/mariana_trench_6_msl_2012.nc")

topo = as.data.frame(rasterToPoints(topo))
names(topo)[3] = "Altitude"

# topo$Altitude = ifelse(topo$Altitude %in% c(-30:0), topo$Altitude, NA)
topo$Altitude = ifelse(topo$Altitude >= 0, topo$Altitude, NA)
# topo$Altitude = ifelse(topo$Altitude <= 0, topo$Altitude, 0)
topo = topo %>% drop_na() %>% dplyr::select(x, y, Altitude)
# topo = topo %>% subset(y < 14.9 & y > 14.8 & x > 145.5 & x < 145.62)

topo %>%
  ggplot(aes(x, y, fill = Altitude)) +
  # geom_tile(aes(width = 0.005, height = 0.005)) +
  geom_raster() + 
  scale_fill_viridis_c() +
  coord_fixed() +
  ggdark::dark_theme_minimal() +
  theme(axis.title = element_blank())

# save(topo, file = 'data/gis_bathymetry/raster/Topography_SRTM15.RData')

wireframe(unclass(as.bathy(topo)), 
          shade = T,
          aspect = c(length(unique(topo$y))/length(unique(topo$x)), 0.45),
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

topo = topo %>% 
  ggplot(aes(x=x,y=y,fill=Altitude)) + 
  geom_tile() +
  scale_fill_viridis() + 
  coord_fixed() + 
  theme_void()

plot_gg(topo, 
        multicore = T,
        height = 5,
        width = 6,
        scale = 30, 
        raytrace = TRUE,
        # windowsize = c(1400, 866), 
        zoom = 0.5, 
        phi = 30, 
        theta = 30)

Sys.sleep(0.2)

render_snapshot(clear = TRUE)
