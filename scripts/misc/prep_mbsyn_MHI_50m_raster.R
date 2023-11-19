library(terra)

topo = rast("N:/GIS/Projects/CommonMaps/Bathymetry/mhi_mbsyn_bathyonly_50m_v21.nc")
topo = terra::aggregate(topo, fact = 2, core = 8) # fact = 10 gives you 30m, fact = 17 gives you 51m
topo[topo >= 0] <- NA
topo[topo <= -30] <- NA
topo = project(topo, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(topo)
writeRaster(topo, "N:/GIS/Projects/CommonMaps/Bathymetry/mhi_mbsyn_bathyonly_50m_v21_0.001deg.tif", overwrite = TRUE)
plot(rast("N:/GIS/Projects/CommonMaps/Bathymetry/mhi_mbsyn_bathyonly_50m_v21_0.001deg.tif"))
