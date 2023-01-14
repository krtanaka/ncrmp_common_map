# merge two bathymetry files for Howland to keep enough shallow sites

library(raster)

# Howland Island: Bathymetry
# https://www.soest.hawaii.edu/pibhmc/cms/data-by-location/pacific-remote-island-area/howland-island/howland-island-bathymetry/

# NOAA 2m derived depth from WorldView-2 multispectral satellite imagery. WV-2 derived depths bathy data
how1 = raster("N:/GIS/Projects/CommonMaps/Bathymetry/how_dball.asc"); plot(how1)

# CRED 5 m Gridded bathymetry of Howland Island, Pacific Remote Island Areas, Central Pacific (Arc ASCII Format)
how2 = raster("N:/GIS/Projects/CommonMaps/Bathymetry/Howland_5m.asc"); plot(how2)

# resample 2m file at 5m
how1 = resample(how1, how2, method = "ngb") 

topo = mean(stack(how1, how2), na.rm = T)

plot(topo)

topo = readAll(topo)

save(topo, file = "N:/GIS/Projects/CommonMaps/Bathymetry/how_merged_5m.Rdata")
