library(sf)
library(ggmap)

# run prep_survey_domain.R and get df @ line 339
utm_crs <- 32702

# Define WGS84 CRS
wgs84_crs <- 4326

# Convert data frame to sf object
df_sf <- st_as_sf(df, coords = c("x", "y"), crs = utm_crs)

# Transform to WGS84
df_wgs84 <- st_transform(df_sf, crs = wgs84_crs)

# Extract latitude and longitude as columns
df$longitude <- st_coordinates(df_wgs84)[, 1]
df$latitude <- st_coordinates(df_wgs84)[, 2]

# View updated data frame
head(df)

ggmap::register_google("AIzaSyDpirvA5gB7bmbEbwB1Pk__6jiV4SXAEcY")

map = ggmap::get_map(location = c(-170.705, -14.294),
                     maptype = "satellite",
                     zoom = 11,
                     color = "bw",
                     force = T)

# Create a rasterized map for the island points
ggmap(map) + 
  geom_bin2d(data = df, aes(x = longitude , y = latitude, fill = reef_id), fill = "yellow", bins = 500, alpha = 0.8, show.legend = F) +
  scale_y_continuous(limits = c(-14.38, -14.22), "") +
  scale_x_continuous(limits = c(-170.85, -170.53), "") +
  coord_sf(crs = 4326) + 
  theme_void()

ggsave(last_plot(),
       filename =  file.path("outputs/tutuila_outreach.jpg"), 
       width = 10, 
       height = 5.16,
       limitsize = F)
