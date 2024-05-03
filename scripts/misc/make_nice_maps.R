library(sf)
library(ggplot2)
library(rnaturalearth)
library(marmap)
library(raster)
library(rmapshaper)
library(ggrepel)
library(dplyr)
library(ggOceanMaps)
library(metR)
library(colorRamps)
library(cowplot)
library(readr)

islands <- read_csv("data/misc/Island_Extents.csv") %>%
  filter(Region %in% c("PRIAs")) %>%
  mutate(
    mean_lat = (ymax + ymin) / 2,
    mean_lon = (xmax + xmin) / 2
  )

islands %>%
  summarise(
    mean_lat = mean(mean_lat),
    mean_lon = mean(mean_lon)
  )

world <- ne_countries(scale = "small", returnclass = "sf")

a <- st_as_sf(data.frame(plot_id = 1, lat = 6.99, long = -120), 
              coords = c("long", "lat"), crs = 4326)

(globe <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = a, shape = 0, size = 10, color = "red", stroke = 2) +
  coord_sf(crs = "+proj=laea +lat_0=35 +lon_0=-156.8 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white")))

islands %>%
  summarise(
    min_ymin = min(ymin),
    max_ymax = max(ymax),
    min_xmin = min(xmin),
    max_xmax = max(xmax)
  )

b = marmap::getNOAA.bathy(lon1 = -179.5,
                          lon2 = -150,
                          lat1 = 17,
                          lat2 = 30,
                          resolution = 3)

b = marmap::fortify.bathy(b)

island_names =  unique(islands$Island)

load("data/misc/MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(island_names)),]
world = st_transform(st_as_sf(world))
world <- ms_simplify(world, keep = 0.1, keep_shapes = F)

label = islands %>% 
  select(Island, mean_lat, mean_lon)

(map <- ggplot() +
    geom_sf(data = world) +
    coord_sf(crs = st_crs(4135), # old Hawaii projection code
             # xlim = c(-160.5, -154.7),
             # ylim = c(18.8, 22.5), 
             expand = F) +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-5000, 0, by = 500),
                 size = c(0.5)) +
    scale_colour_distiller(palette = "RdYlBu", direction = -1, "Depth (m)") +
    geom_label_repel(data = label, 
                     aes(x = mean_lon, y = mean_lat, label = Island), 
                     label.size = NA,
                     fontface = "bold",   
                     label.padding = 0.1, 
                     na.rm = T,
                     fill = alpha(c("gray100"), 0.9),
                     # nudge_x = c(0.5, 0.5, 0.5, 0.5, 0.5),
                     nudge_y = c(0.5, 0.5, 0.5, 0.5, 0.5)
    ) +
    theme_light() +
    theme(
      legend.position = c(1, 1),
      legend.justification = c(1.1, 1.1),
      axis.title = element_blank()))

pdf(file = "/Users/kisei.tanaka/Desktop/fig1.pdf", width = 10, height = 10)
map
dev.off()

pdf(file = "/Users/kisei.tanaka/Desktop/fig2.pdf", width = 5, height = 5)
globe
dev.off()
