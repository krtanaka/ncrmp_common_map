rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(tidyr)
library(marmap)
library(lattice)

# need to connect to pifsc VPN

islands = c("gua", "rot", "sai", "tin", "agu")                              # South Mariana Islands
islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "sar")                # North Mariana Islands
# islands = c("ofu", "ros", "swa", "tau", "tut")                              # American Samoa
# islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak")                # Pacific Remote Island Areas
# islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah")  # Main Hawaiian Islands
# islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr")                # Northern Hawaiian Islands

for (isl in 1:length(islands)) {
  
  # isl = 1
  
  if (islands[isl] == "gua") topo = raster("L:/ktanaka/GIS/bathymetry/gua_nthmp_dem_10m_mosaic.tif") # Guam
  if (islands[isl] == "rot") topo = raster("L:/ktanaka/GIS/bathymetry/Rota_5m_bathymetry.asc") # Rota
  if (islands[isl] == "sai") topo = raster("L:/ktanaka/GIS/bathymetry/sai_mb_5m.tif") # Saipan
  if (islands[isl] == "tin") topo = raster("L:/ktanaka/GIS/bathymetry/tinian_5m.asc") # Tinian
  # if (islands[isl] == "agu") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/agr_inpoo_new/w001001.adf") # Aguijan
  if (islands[isl] == "agr") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/agr_inpoo_new/w001001.adf") 
  if (islands[isl] == "ala") topo = raster("N:/GIS/Projects/SeafloorCalc/Workspace/Alamagan/ala_inpo_mbik/w001001.adf") # Alamagan
  if (islands[isl] == "asu") topo = raster("N:/GIS/Projects/SeafloorCalc/Workspace/Asuncion/asc_inpo/w001001.adf") # 
  if (islands[isl] == "gug") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/gug_inpo/w001001.adf") # 
  if (islands[isl] == "fdp") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/fdp_inpo/w001001.adf") # 
  if (islands[isl] == "mau") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/mau_inpo/w001001.adf") # 
  if (islands[isl] == "sar") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/sar_inpo/w001001.adf") # 
  
  if (islands[isl] == "jar") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/jar_inpo_land/w001001.adf") # 
  if (islands[isl] == "joh") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/JOH/joh_inpo/w001001.adf") # 
  if (islands[isl] == "kin") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/kingman3/w001001.adf") # 
  if (islands[isl] == "pal") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/pal_inpo/w001001.adf") # 
  
  topo[topo <= -30] <- NA
  topo[topo >= 0] <- NA
  
  topo <- aggregate(topo, fact = 100/res(topo))
  res(topo)
  
  # topo = as.data.frame(rasterToPoints(topo)) %>% drop_na()
  # colnames(topo) = c("x", "y", "depth")
  # 
  # topo %>%
  #   ggplot(aes(x, y, fill = depth)) +
  #   geom_raster() +
  #   scale_fill_viridis_c("") +
  #   ggdark::dark_theme_minimal() +
  #   coord_fixed() +
  #   theme(axis.title = element_blank())
  
  topo_i = readAll(topo)
  
  save(topo_i, file = paste0('data/gis_bathymetry/', islands[isl], '.RData'))
  
}

load('data/gis_bathymetry/raster/ala.RData')

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
