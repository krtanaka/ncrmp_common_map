library(rgdal)

# Shoreline data for us detail
gshhg <- readOGR("N:/GIS/Projects/CommonMaps/Coastline/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp")

gshhg = readAll(gshhg)

save(gshhg, file = "data/gis_island_boundaries/gshhg_shp.RData")

gshhg <- crop(gshhg, extent(-170.847, -170.5614, -14.37421, -14.22956))

plot(gshhg["id"])
