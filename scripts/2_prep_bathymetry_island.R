###################################################################
### Prep island bathymetry data using in-house island GIS files ###
###################################################################

# Clear the workspace
rm(list = ls())

# Load required libraries
library(dplyr)       # For data manipulation
library(ggplot2)     # For creating plots
library(raster)      # For working with raster data
library(tidyr)       # For tidying data
library(marmap)      # For working with bathymetric data
library(lattice)     # For creating trellis plots
library(colorRamps)  # For color ramp functions

# Note: Connect to PIFSC VPN if not at IRC 

# South Mariana Islands
# Guam: Run 2b_prep_bathymetry_Guam to process ngdc_bathy_10m_guam file separately
# Rota: In-house bathymetry file cannot represent eastern side. Use Mariana Trench 6 arc-second Bathymetric Digital Elevation Model. Run 2c_prep_bathymetry_by_region.
# Aguijan: In-house bathymetry file cannot represent the island. Use Mariana Trench 6 arc-second Bathymetric Digital Elevation Model. Run 2c_prep_bathymetry_by_region.
islands = c(
  # "gua", 
  # "rot", 
  "sai",
  "tin"
  # "agu" 
)

# North Mariana Islands
islands = c("agr", "ala", "ana", "asc", "gug", "fdp", "mau", "sar", "sup", "pag")

# American Samoa
islands = c("ofu", "ros", "swa", "tau", "tut")

# Pacific Remote Island Areas
islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak")

# Main Hawaiian Islands
islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah")

# Northern Hawaiian Islands
islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr")

# specify alphabetical letter assigned to the target drive path
letter = "N"

for (isl in 1:length(islands)) {
  
  # isl = 9
  
  # South Mariana Islands
  if (islands[isl] == "gua") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/ngdc_bathy_10m_guam_049d_6725_8fc5.nc"))
  if (islands[isl] == "rot") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/Rota_60m.asc"))
  if (islands[isl] == "sai") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/sai_mb_li_db.asc"))
  if (islands[isl] == "tin") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/tinmblidbmos.asc"))
  if (islands[isl] == "agu") topo = raster("FILE_NOT_AVAILAVLE") # file not available
  
  # North Mariana Islands
  if (islands[isl] == "agr") topo = raster(paste0(letter, ":/GIS/Projects/SeafloorCalc/Final_Products/agr_inpoo_new/w001001.adf"))
  if (islands[isl] == "ala") topo = raster(paste0(letter, ":/GIS/Projects/SeafloorCalc/Workspace/Alamagan/ala_inpo_mbik/w001001.adf"))
  if (islands[isl] == "ana") topo = raster(paste0(letter, ":/GIS/Projects/SeafloorCalc/Workspace/Anatahan/ana_60m_bathy/w001001.adf"))
  if (islands[isl] == "asc") topo = raster(paste0(letter, ":/GIS/Projects/SeafloorCalc/Workspace/Asuncion/asc_inpo/w001001.adf"))
  if (islands[isl] == "gug") topo = raster(paste0(letter, ":/GIS/Projects/SeafloorCalc/Final_Products/gug_inpo/w001001.adf"))
  if (islands[isl] == "fdp") topo = raster(paste0(letter, ":/GIS/Projects/SeafloorCalc/Final_Products/fdp_inpo/w001001.adf"))
  if (islands[isl] == "mau") topo = raster(paste0(letter, ":/GIS/Projects/SeafloorCalc/Final_Products/mau_inpo/w001001.adf"))
  if (islands[isl] == "sar") topo = raster(paste0(letter, ":/GIS/Projects/SeafloorCalc/Final_Products/sar_inpo/w001001.adf"))
  if (islands[isl] == "sup") topo = raster(paste0(letter, ":/GIS/Projects/SeafloorCalc/Workspace/Supply/sup_10m/w001001.adf"))
  if (islands[isl] == "pag") topo = raster(paste0(letter, ":/GIS/Projects/SeafloorCalc/Final_Products/pag_inpo/w001001.adf"))
  
  # American Samoa
  if (islands[isl] == "ofu") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/oo_dbmb_mos4.asc"))
  if (islands[isl] == "ros") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/rose_5m_dbmb.asc"))
  if (islands[isl] == "swa") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/swa_dball_2m.asc"))
  if (islands[isl] == "tau") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/tau_dbmb_mos.asc"))
  if (islands[isl] == "tut") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/tut_dbmb.asc"))
  
  # Pacific Remote Island Areas
  if (islands[isl] == "bak") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/bak_dbmb_5m.asc")) # 
  if (islands[isl] == "how") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/how_merged_5m.tif")) # run misc/merge_two_Howland_bathy_files.R first
  if (islands[isl] == "jar") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/Jarvis_5m.asc")) #
  if (islands[isl] == "joh") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/johdballblend.asc")) #
  if (islands[isl] == "kin") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/kin_dbmb.asc")) #
  if (islands[isl] == "pal") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/pal_dbmb.asc")) # 
  if (islands[isl] == "wak") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/wake_10m.asc")) # 
  
  # Northern Hawaiian Islands
  if (islands[isl] == "ffs") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/FFS-5m.grd.asc")) # 
  if (islands[isl] == "kur") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/Kure_5m.asc")) #
  if (islands[isl] == "lay") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/NHamLays_20m.asc")) #
  if (islands[isl] == "lis") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/Lisianski_20m.asc")) #
  if (islands[isl] == "mar") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/Maro_20m.asc")) #
  if (islands[isl] == "mid") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/Midway_5m.asc")) # 
  if (islands[isl] == "phr") topo = raster(paste0(letter, ":/GIS/Projects/CommonMaps/Bathymetry/PH_5m.asc")) # 
  
  # if depth raster files contains no below sea-level cells (e.g. Swa), don't subset them
  if(min(values(topo), na.rm = T) <= 0) {
    
    topo[topo <= -30] <- NA
    topo[topo >= 0] <- NA  
    
  }
  
  # if (islands[isl] %in% c("tut")) {
  
  # topo <- aggregate(topo, fact = 51/res(topo)) # aggregate to 51m resolution
  # res(topo)
  
  # } 
  
  topo_i = readAll(topo)
  plot(topo_i)
  print( islands[isl])
  
  save(topo_i, file = paste0('data/gis_bathymetry/', islands[isl], '.RData'))
  
}

