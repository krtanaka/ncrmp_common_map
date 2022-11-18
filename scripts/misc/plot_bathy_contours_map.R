library(sf)
library(ggplot2)
library(rnaturalearth)
library(marmap)
library(raster)
library(rmapshaper)
library(ggrepel)
library(dplyr)

rm(list = ls())

world <- ne_countries(scale = "large", returnclass = "sf")

b = marmap::getNOAA.bathy(lon1 = min(-160.5),
                          lon2 = max(-154.8),
                          lat1 = min(18.91),
                          lat2 = max(22.25),
                          resolution = 1)

b = marmap::fortify.bathy(b)

islands = c("Kauai", #1
            # "Lehua", #2
            "Niihau", #3
            # "Kaula", #4
            "Oahu", #5
            "Molokai", #6
            "Maui", #7
            "Lanai", #8
            # "Molokini", #9
            # "Kahoolawe", #10
            "Hawaii")

load("data/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands)),]
world = st_transform(st_as_sf(world))
world <- ms_simplify(world, keep = 0.1, keep_shapes = F)

load("data/rea/ALL_REA_FISH_RAW_SST.RData")
# df = df %>% 
#   subset(REGION == "MHI" & ISLAND %in% islands) %>% 
#   group_by(LONGITUDE, LATITUDE, OBS_YEAR) %>% 
#   summarise(n = n())

label = df %>% 
  subset(REGION == "MHI" & ISLAND %in% islands) %>% 
  group_by(ISLAND) %>% 
  summarise(lat = mean(LATITUDE),
            lon = mean(LONGITUDE))

map <- ggplot(data = world) +
  coord_sf(crs = st_crs(4135) # old hawaii projection code
           # xlim = c(-160.5, -154.8),
           # ylim = c(18.91, 22.25), expand = F
           ) +
  geom_sf() +
  # scale_x_continuous(breaks = seq(-160.5, -154.8, by = 0.5)) +
  # scale_y_continuous(breaks = seq(18.91, 22.25, by = 0.5)) +
  # geom_point(data = df, aes(LONGITUDE, LATITUDE, color = factor(OBS_YEAR))) + 
  geom_contour(data = b,
               aes(x = x, y = y, z = z),
               breaks = seq(-8000, 0, by = 500),
               size = c(0.05),
               alpha = 0.8,
               colour = grey.colors(17003, rev = T)) +
  scale_fill_discrete("") + 
  scale_color_discrete("") + 
  geom_text_repel(data = label, 
                  aes(x = lon, y = lat, label = ISLAND), 
                  fontface = "bold",   
                  nudge_x = c(0.5, 0.5, 0.5, 0.5, 0.5),
                  nudge_y = c(0.5, 0.5, 0.5, 0.5, 0.5)) +
  theme_minimal() + 
  theme(
    # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title = element_blank())
    # legend.position = c(0.1, 0.3))

# pdf('/Users/Kisei.Tanaka/Desktop/MHI_200m_Bathy_Countour.pdf', height = 5, width = 7)
png('/Users/Kisei.Tanaka/Desktop/MHI_200m_Bathy_Countour.png', height = 5, width = 7, res = 100, units = "in")
print(map)
dev.off()
