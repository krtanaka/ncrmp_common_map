###################################################################
### Prep island bathymetry data using in-house island GIS files ###
###################################################################

rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(tidyr)
library(marmap)
library(lattice)

# need to connect to pifsc VPN if you are not at IRC

# South Mariana Islands
islands = c(
  "gua",
  # "rot", # in-house bathymetry file cannot represent eastern side of the island. Use Mariana Trench 6 arc-second Bathymetric Digital Elevation Model. Run 2c_prep_bathymetry_by_region.
  "sai", 
  "tin"
  # "agu" # in-house bathymetry file cannot represent the island. Use Mariana Trench 6 arc-second Bathymetric Digital Elevation Model. Run 2c_prep_bathymetry_by_region.
)

# North Mariana Islands
islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "sar", "pag")

# American Samoa
islands = c("ofu", "ros", "swa", "tau", "tut")

# Pacific Remote Island Areas
islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak")

# Main Hawaiian Islands
islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah")

# Northern Hawaiian Islands
islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr")

for (isl in 1:length(islands)) {
  
  # isl = 6
  
  # South Mariana Islands
  if (islands[isl] == "gua") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/ngdc_bathy_10m_guam_3111_dbef_9895.nc")
  # if (islands[isl] == "rot") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/Rota_60m.asc")
  if (islands[isl] == "sai") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/sai_mb_li_db.asc")
  if (islands[isl] == "tin") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/tinmblidbmos.asc")
  # if (islands[isl] == "agu") topo = raster("FILE_NOT_AVAILAVLE") # file not available
  
  # North Mariana Islands
  if (islands[isl] == "agr") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/agr_inpoo_new/w001001.adf")
  if (islands[isl] == "ala") topo = raster("N:/GIS/Projects/SeafloorCalc/Workspace/Alamagan/ala_inpo_mbik/w001001.adf")
  if (islands[isl] == "asc") topo = raster("N:/GIS/Projects/SeafloorCalc/Workspace/Asuncion/asc_inpo/w001001.adf")
  if (islands[isl] == "gug") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/gug_inpo/w001001.adf")
  if (islands[isl] == "fdp") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/fdp_inpo/w001001.adf")
  if (islands[isl] == "mau") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/mau_inpo/w001001.adf")
  if (islands[isl] == "sar") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/sar_inpo/w001001.adf")
  if (islands[isl] == "pag") topo = raster("N:/GIS/Projects/SeafloorCalc/Final_Products/pag_inpo/w001001.adf")
  
  # American Samoa
  if (islands[isl] == "ofu") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/oo_dbmb_mos4.asc")
  if (islands[isl] == "ros") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/rose_5m_dbmb.asc")
  if (islands[isl] == "swa") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/swa_dball_2m.asc")
  if (islands[isl] == "tau") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/tau_dbmb_mos.asc")
  if (islands[isl] == "tut") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/tut_dball.asc")
  
  # Pacific Remote Island Areas
  if (islands[isl] == "bak") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/bak_dbmb_5m.asc") # 
  if (islands[isl] == "how") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/Howland_5m.asc") #
  if (islands[isl] == "jar") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/Jarvis_5m.asc") #
  if (islands[isl] == "joh") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/johdballblend.asc") #
  if (islands[isl] == "kin") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/kin_dbmb.asc") #
  if (islands[isl] == "pal") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/pal_dbmb.asc") # 
  if (islands[isl] == "wak") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/wake_10m.asc") # 
  
  # Northern Hawaiian Islands
  if (islands[isl] == "ffs") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/FFS-5m.grd.asc") # 
  if (islands[isl] == "kur") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/Kure_5m.asc") #
  if (islands[isl] == "lay") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/NHamLays_20m.asc") #
  if (islands[isl] == "lis") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/Lisianski_20m.asc") #
  if (islands[isl] == "mar") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/Maro_20m.asc") #
  if (islands[isl] == "mid") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/Midway_5m.asc") # 
  if (islands[isl] == "phr") topo = raster("N:/GIS/Projects/CommonMaps/Bathymetry/PH_5m.asc") # 
  
  # if depth raster files contains no below sea-level cells (e.g. Swa), don't subset them
  if(min(values(topo), na.rm = T) <= 0) {
    
    topo[topo <= -30] <- NA
    topo[topo >= 0] <- NA  
    
  }
  
  if (islands[isl] %in% c("rot", "sai", "tin")) {
    
    topo <- aggregate(topo, fact = 50/res(topo))
    res(topo)
    
  } else {
    
    topo <- aggregate(topo, fact = 100/res(topo))
    res(topo)
    
  }
  
  topo_i = readAll(topo)
  plot(topo_i)
  print( islands[isl])
  
  save(topo_i, file = paste0('data/gis_bathymetry/', islands[isl], '.RData'))
  
}

load('data/gis_bathymetry/phr.RData')

wireframe(unclass(as.bathy(topo_i)), 
          shade = T,
          aspect = c(dim(topo_i)[1]/dim(topo_i)[2], 0.001),
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
