library(ggplot2)
library(dplyr)
library(raster)
library(sp)

load('/Users/kisei.tanaka/Desktop/ncrmp_islands_shp.RData')
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
ISL_this = ISL_bounds[which(ISL_bounds$ISLAND %in% "MOLOKAI"),]

load("/Users/kisei.tanaka/Desktop/survey_grid_kalaupapa.RData")

df = survey_grid_kalaupapa

colnames(df)[4:5] = c("x", "y")

utmcoor <- SpatialPoints(cbind(df$x, df$y), proj4string = CRS("+proj=utm +units=km +zone=4 +north"))
longlatcoor <- spTransform(utmcoor,CRS("+proj=longlat"))
df$lon <- coordinates(longlatcoor)[,1]
df$lat <- coordinates(longlatcoor)[,2]

ISL_this = crop(ISL_this, extent(-157.0345, -156.9005, 21.16467, 21.22161)); plot(ISL_this)

(p1 = ggplot() + 
    geom_polygon(data = ISL_this, aes(long, lat, group = group), fill = "gray20", color = NA, alpha = 0.9) +
    # geom_point(data = df, aes(lon, lat, fill = depth), shape = 21, size = 2, alpha = 0.8) + 
    geom_tile(data = df, aes(lon, lat, fill = depth), width = 0.0006, height = 0.0006) + 
    scale_fill_viridis_c() +
    coord_fixed())

(p2 = ggplot() + 
    geom_polygon(data = ISL_this, aes(long, lat, group = group), fill = "gray20", color = NA, alpha = 0.9) +
    # geom_point(data = df, aes(lon, lat, fill = depth_bin), shape = 21, size = 2, alpha = 0.8) + 
    geom_tile(data = df, aes(lon, lat, fill = depth_bin), width = 0.0006, height = 0.0006) + 
    scale_fill_viridis_d() + 
    coord_fixed())

(p3 = ggplot() + 
    geom_polygon(data = ISL_this, aes(long, lat, group = group), fill = "gray20", color = NA, alpha = 0.9) +
    # geom_point(data = df, aes(lon, lat, fill = sector_id), shape = 21, size = 2, alpha = 0.8) + 
    geom_tile(data = df, aes(lon, lat, fill = sector_id), width = 0.0006, height = 0.0006) + 
    scale_fill_viridis_d() + 
    coord_fixed())

(p4 = ggplot() + 
    geom_polygon(data = ISL_this, aes(long, lat, group = group), fill = "gray20", color = NA, alpha = 0.9) +
    # geom_point(data = df, aes(lon, lat, fill = reef_id), shape = 21, size = 2, alpha = 0.8) +
    geom_tile(data = df, aes(lon, lat, fill = reef_id), width = 0.0006, height = 0.0006) + 
    scale_fill_viridis_d() + 
    coord_fixed())

(p5 = ggplot() + 
    geom_polygon(data = ISL_this, aes(long, lat, group = group), fill = "gray20", color = NA, alpha = 0.9) +
    # geom_point(data = df, aes(lon, lat, fill = hardsoft_id), shape = 21, size = 2, alpha = 0.8) + 
    geom_tile(data = df, aes(lon, lat, fill = hardsoft_id), width = 0.0006, height = 0.0006) + 
    scale_fill_viridis_d() + 
    coord_fixed())

(p6 = ggplot() + 
    geom_polygon(data = ISL_this, aes(long, lat, group = group), fill = "gray20", color = NA, alpha = 0.9) +
    # geom_point(data = df, aes(lon, lat, fill = hardsoft_id), shape = 21, size = 2, alpha = 0.8) + 
    geom_tile(data = df, aes(lon, lat, fill = strat), width = 0.0006, height = 0.0006) + 
    scale_fill_viridis_d() + 
    coord_fixed())

png("/Users/kisei.tanaka/Desktop/kalaupapa_benthic_strata.png", height = 10, width = 10, units = "in", res = 500)
print(p6)
dev.off()




