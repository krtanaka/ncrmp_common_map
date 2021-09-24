########################
### see raw GIS data ###
########################

load("data/misc/HAW_Grid.RData")

Hawaii_Survey_Grid %>%
  group_by(X, Y) %>%
  summarise(X = mean(X),
            Y = mean(Y),
            depth = mean(DEPTH, na.rm = T)) %>%
  na_if(-9999) %>%
  ggplot( aes(X, Y, fill = depth)) +
  geom_tile(aes(width = 0.005, height = 0.005)) +
  scale_fill_viridis_c("") +
  coord_fixed() +
  ggdark::dark_theme_minimal() +
  theme(axis.title = element_blank())

rm(Hawaii_Survey_Grid)