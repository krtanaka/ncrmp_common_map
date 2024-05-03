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

rm(list = ls())

islands = read_csv("data/misc/Island_Extents.csv") %>% 
  filter(Region %in% c("MHI", "NWHI")) %>%
  mutate(
    mean_y = (ymax + ymin) / 2,
    mean_x = (xmax + xmin) / 2
  )

ll_means <- islands %>%
  mutate(
    mean_lat = (ymax + ymin) / 2,
    mean_lon = (xmax + xmin) / 2
  ) %>%
  summarise(
    overall_mean_lat = mean(mean_lat),
    overall_mean_lon = mean(mean_lon)
  ) %>% 
  as.data.frame()

world <- ne_countries(scale = "small", returnclass = "sf")

a <- st_as_sf(data.frame(plot_id = 1, lat = -162, long = 22.4), 
              coords = c("lat", "long"), crs = 4326)

(globe = ggplot(data = world) +
    geom_sf() +
    geom_sf(data = a, shape = 0, size = 10, color = "red", stroke = 2) +
    coord_sf(crs = "+proj=laea +lat_0=35 +lon_0=-156.8 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") + 
    theme_bw() +
    theme(panel.background = element_rect(fill = 'white')))#transparent plot bg)

ll_min_max <- islands %>%
  summarise(
    min_ymin = min(ymin),
    max_ymax = max(ymax),
    min_xmin = min(xmin),
    max_xmax = max(xmax)
  )

b = marmap::getNOAA.bathy(lon1 = -180,
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
  select(Island, mean_y, mean_x)

# (f1a = basemap(limits = c(-161, -154, 18, 23), 
#                land.col = "gray20", 
#                bathy.style = "poly_greys",
#                bathy.size = 0.5,
#                land.border.col = NA, 
#                bathymetry = TRUE) + 
#     geom_label_repel(data = label,
#                      aes(x = lon, y = lat, label = ISLAND),
#                      fontface = "bold",
#                      nudge_x = c(0.5, 0.5, 0.5, 0.5, 0.5),
#                      nudge_y = c(0.5, 0.5, 0.5, 0.5, 0.5),
#                      label.size = NA,
#                      label.padding=.1,
#                      na.rm=TRUE,
#                      fill = alpha(c("white"),0.8)) +
#     scale_fill_viridis_d("Depth(m)", direction = -1) +
#     scale_x_longitude() +
#     scale_y_latitude() +
#     labs(x = "", y = "") + 
#     theme_pubr(I(20)) + 
#     theme(legend.position = "right"))

(f1a <- ggplot() +
    geom_sf(data = world) +
    coord_sf(crs = st_crs(4135), # old hawaii projection code
             # xlim = c(-160.5, -154.7),
             # ylim = c(18.8, 22.5), 
             expand = F) +
    # geom_point(data = df, aes(LONGITUDE, LATITUDE, color = factor(OBS_YEAR)), alpha = 0.5, size = 0.1) +
    # scale_color_manual(values = matlab.like(9), "") + 
    # geom_contour(data = b,
    #              aes(x = x, y = y, z = z),
    #              breaks = seq(-10000, 0, by = 500),
    #              size = c(0.2),
    #              alpha = 0.8,
    #              colour = matlab.like(29771)) +
    geom_contour(data = b,
                 aes(x = x, y = y, z = z, colour = stat(level)),
                 breaks = seq(-3000, 0, by = 100),
                 size = c(0.5)) +
    scale_colour_distiller(palette = "RdYlBu", direction = -1, "Depth (m)") +
    # scale_colour_viridis_c("Depth (m)") +
    geom_label_repel(data = label, 
                     aes(x = mean_x, y = mean_y, label = Island), 
                     label.size = NA,
                     fontface = "bold",   
                     label.padding = 0.5, 
                     na.rm = T,
                     fill = alpha(c("white"), 0.9),
                     nudge_x = c(0.5, 0.5, 0.5, 0.5, 0.5),
                     nudge_y = c(0.5, 0.5, 0.5, 0.5, 0.5)) +
    # theme_minimal() +
    theme(
      #legend.position = c(1, 1),
      #legend.justification = c(1.1, 1.1),
      # legend.position = c(0.95, 1),
      # legend.justification = c(1.1, 1.1),
      # panel.background = element_rect(fill = "white"), # bg of the panel
      # plot.background = element_rect(fill = "white"), # bg of the plot
      axis.title = element_blank()))

library(cowplot)

f1 <-
  ggdraw() +
  draw_plot(f1a) +
  draw_plot(globe, x = 0.01, y = 0.15, width = 0.5, height = 0.4)

f1

ggsave(filename = "outputs/fig1.png", 
       plot = f1,
       width = 12, 
       height = 10,
       units = "in",
       dpi = 500)
