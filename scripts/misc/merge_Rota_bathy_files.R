library(dplyr)
library(terra)
library(readr)

rm(list = ls())

# patch finer res bathymetry files first

# Rota Island: Bathymetry PIBHMC
# https://www.soest.hawaii.edu/pibhmc/cms/data-by-location/cnmi-guam/rota-island/rota-island-bathymetry/
b1 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/rot_dball.asc"); plot(b1); res(b1)
b2 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/rot_dbmb_5m.asc"); plot(b2); res(b2)
b3 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/Rota_5m_bathymetry.asc"); plot(b3); res(b3)
b4 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/Rota_60m.asc"); plot(b4); res(b4)
b5 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi2019_rota_dem/cnmi2019_islands_dem_J1079504.tif"); plot(b5); res(b5)
# b6 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cnmi2019_rota_dem_hydflt/cnmi2019_isl_dem_hydflt_J1079505.tif"); plot(b6); res(b6)
b7 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/cudem_9th_cnmi_rota/cudem_9th_cnmi_J1079506.tif"); plot(b7); res(b7)
# b8 = rast("N:/GIS/Projects/CommonMaps/Bathymetry/noaa_slr_dem_cnmi_rota/NOAA_SLR_DEM_CNMI_J1079507.tif"); plot(b8); res(b8)

base = b7

b1 = resample(b1, base, method = "near"); plot(b1)
b2 = resample(b2, base, method = "near"); plot(b2)
b3 = resample(b3, base, method = "near"); plot(b3) 
b4 = resample(b4, base, method = "near"); plot(b4) 
b5 = resample(b5, base, method = "near"); plot(b5)
# b6 = resample(b6, base, method = "near"); plot(b6) 
b7 = resample(b7, base, method = "near"); plot(b7) 
# b8 = resample(b8, base, method = "near"); plot(b8) 

fine_topo <- mean(c(b1, b2, b3, b4, b5, b7), na.rm = TRUE)

fine_topo[fine_topo >= 0] <- NA
fine_topo[fine_topo <= -30] <- NA

plot(fine_topo)

topo_i = readAll(raster(fine_topo))

plot(topo_i)

save(topo_i, file = 'data/gis_bathymetry/rot_merged.RData')

