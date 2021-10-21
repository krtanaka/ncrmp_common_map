df <- readOGR("L:/ktanaka/GIS/hardsoft/agu_digchart_habitat_a_dis_new_N55.shp")[1]
df <- fortify(df)

df = st_read("L:/ktanaka/GIS/hardsoft/agu_digchart_habitat_a_dis_new_N55.shp")

df %>%
  st_as_sf() %>%
  # filter(!HardSoft %in% c("Unknown")) %>%
  filter(!HardSoft %in% c("Land", "Other", "soft")) %>%
  ggplot() +
  geom_sf(aes(group = HardSoft, fill = HardSoft), color = "NA", show.legend = T) +
  ggdark::dark_theme_minimal()

df = st_cast(df,"POINT")
df <- df %>%
  mutate(lon = unlist(map(df$geometry,1)),
         lat = unlist(map(df$geometry,2))) %>% 
  as.data.frame()

df %>% 
  ggplot(aes(lon, lat, fill = HardSoft)) + 
  geom_raster()



df %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = group)) +
  geom_tile(height = 1000, width = 1000)
