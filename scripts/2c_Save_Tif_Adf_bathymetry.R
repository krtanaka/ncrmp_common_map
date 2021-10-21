rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(tidyr)
library(marmap)
library(lattice)
library(rayshader)

islands = c("gua", "rot", "sai", "tin")

for (isl in 1:length(islands)) {
  
  isl = 2
  
  if (islands[isl] == "gua")  topo = raster("L:/ktanaka/GIS/bathymetry/gua_nthmp_dem_10m_mosaic.tif") # Guam
  if (islands[isl] == "rot") topo = raster("L:/ktanaka/GIS/bathymetry/Rota_5m_bathymetry.asc") # Rota
  if (islands[isl] == "sai") topo = raster("L:/ktanaka/GIS/bathymetry/sai_mb_5m.tif") # Saipan
  if (islands[isl] == "tin") topo = raster("L:/ktanaka/GIS/bathymetry/tinian_5m.asc") # Tinian
  
  topo[topo <= -30] <- NA
  topo[topo >= 0] <- NA
  
  topo <- aggregate(topo, fact = 100/res(topo))
  res(topo)
  
  topo = as.data.frame(rasterToPoints(topo)) %>% drop_na()
  colnames(topo) = c("x", "y", "depth")
  
  topo %>%
    ggplot(aes(x, y, fill = depth)) +
    geom_raster() +
    scale_fill_viridis_c() +
    ggdark::dark_theme_minimal() +
    coord_fixed() +
    theme(axis.title = element_blank())
  
  save(topo, file = paste0('data/gis_bathymetry/raster/', islands[isl], '.RData'))
  
}

load('data/gis_bathymetry/raster/gua.RData')

wireframe(unclass(as.bathy(topo)), 
          shade = T, 
          aspect = c(1/2, 0.05),
          par.box = c(col = "gray"),
          scales = list(arrows = FALSE, col = "transparent"), # col="black" is required 
          zlab = "", 
          xlab = "",
          ylab = "")

topo = ggplot(topo) +
  geom_tile(aes(x=x,y=y,fill=depth)) +
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
