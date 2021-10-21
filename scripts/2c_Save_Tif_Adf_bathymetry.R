rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(tidyr)
library(marmap)
library(lattice)
library(rayshader)

# need to connect to pifsc VPN

islands = c("gua", "rot", "sai", "tin", 
            # "agu", 
            "agr", "ala", "asu", "gug", "fdp", "mau", "sar")

for (isl in 1:length(islands)) {
  
  # isl = 2
  
  if (islands[isl] == "gua")  topo = raster("L:/ktanaka/GIS/bathymetry/gua_nthmp_dem_10m_mosaic.tif") # Guam
  if (islands[isl] == "rot") topo = raster("L:/ktanaka/GIS/bathymetry/Rota_5m_bathymetry.asc") # Rota
  if (islands[isl] == "sai") topo = raster("L:/ktanaka/GIS/bathymetry/sai_mb_5m.tif") # Saipan
  if (islands[isl] == "tin") topo = raster("L:/ktanaka/GIS/bathymetry/tinian_5m.asc") # Tinian
  # if (islands[isl] == "agu") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/agr_inpoo_new/w001001.adf") # Aguijan
  if (islands[isl] == "agr") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/agr_inpoo_new/w001001.adf") # Tinian
  if (islands[isl] == "ala") topo = raster("N:/GIS/Projects/SeafloorCalc/Workspace/Alamagan/ala_inpo_mbik/w001001.adf") # Alamagan
  if (islands[isl] == "asu") topo = raster("N:/GIS/Projects/SeafloorCalc/Workspace/Asuncion/asc_inpo/w001001.adf") # 
  if (islands[isl] == "gug") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/gug_inpo/w001001.adf") # 
  if (islands[isl] == "fdp") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/fdp_inpo/w001001.adf") # 
  if (islands[isl] == "mau") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/mau_inpo/w001001.adf") # 
  if (islands[isl] == "sar") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/sar_inpo/w001001.adf") # 
  
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
          aspect = c(1/2, 0.1),
          par.box = c(col = "gray"),
          scales = list(arrows = FALSE, col = "transparent"), # col="black" is required 
          zlab = "", 
          xlab = "",
          ylab = "")
