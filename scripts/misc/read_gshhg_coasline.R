library(rgdal)
library(maptools)

# https://gis.stackexchange.com/questions/31743/projecting-sp-objects-in-r

# Shoreline data for us detail

P4S.latlon <- CRS("+proj=longlat +datum=WGS84")

gshhg <- readShapePoly("N:/GIS/Projects/CommonMaps/Coastline/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp", verbose = T, proj4string = P4S.latlon)

gshhg <- readOGR("N:/GIS/Projects/CommonMaps/Coastline/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp")

save(gshhg, file = "data/gis_island_boundaries/gshhg_shp.RData")

gshhg <- crop(gshhg, extent(-170.847, -170.5614, -14.37421, -14.22956))

plot(gshhg["id"])

gshhg2 <- spTransform(gshhg, CRS("+init=epsg:9122"))

plot(gshhg2["id"])
