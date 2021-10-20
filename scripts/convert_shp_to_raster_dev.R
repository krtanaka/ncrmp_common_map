df <- readOGR(shp_list[shp_i])
df <- fortify(df)

df %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = group)) +
  geom_tile(height = 1000, width = 1000)
