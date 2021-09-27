rm(list = ls())

library(readr)
library(dplyr)
library(rnaturalearth)
library(rgeos)
library(sf)
library(ggplot2)
library(patchwork)
library(stringr)

load("data/rea/SURVEY MASTER.RData"); df = SURVEY_MASTER

# look at > 2013 for representative survey efforts

df <- df %>% 
  subset(REGION == "MHI") %>% 
  subset(OBS_YEAR %in% c(2013, 2016, 2019)) %>% 
  subset(ISLAND != "Kahoolawe")

p1 = df %>% 
  group_by(
    ISLAND, 
    # DEPTH_BIN,
    # SEC_NAME, 
    # REEF_ZONE,
    OBS_YEAR
  ) %>% 
  summarise(n = n()) %>% 
  na.omit() %>% 
  ggplot(aes(OBS_YEAR, n, color = ISLAND)) + 
  geom_point(show.legend = F) +
  geom_line(show.legend = F) + 
  # facet_grid(SEC_NAME~DEPTH_BIN ) +
  facet_wrap(~ISLAND, nrow = 3) + 
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p2 = df %>% 
  group_by(ISLAND, OBS_YEAR) %>% 
  summarise(n = n()) %>% 
  na.omit() %>% 
  ggplot(aes(ISLAND, n, color = ISLAND, fill = ISLAND)) + 
  geom_boxplot(outlier.colour = NULL, show.legend = F) +
  stat_summary(geom = "crossbar", 
               width = 0.65, 
               fatten = 0, 
               color = "white", 
               show.legend = F, 
               fun.data = function(x){ return(c(y = median(x), ymin = median(x), ymax = median(x))) }) + 
  # geom_text(data = stat, aes(label = median, y = median, colour = "white"), show.legend = F) + 
  theme_minimal() + 
  theme(
    legend.title = element_blank(), 
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.line.x = element_blank(), 
    axis.title.x=element_blank(),
    axis.line.y = element_blank(),
    axis.title.y = element_blank())

p1 + p2

survey_effort_MHI = df %>% 
  subset(OBS_YEAR %in% c(2013, 2016, 2019)) %>%
  # group_by(ISLAND, OBS_YEAR) %>%
  group_by(OBS_YEAR) %>%
  summarise(n = n()) %>% 
  group_by(ISLAND) %>%
  summarise(high = quantile(n, probs = 0.75),
            median = median(n),
            low = quantile(n, probs = 0.25)) %>% 
  as.data.frame()

save(survey_effort_MHI, file = "data/survey_effort_MHI_2014-2019.RData")


df$LONGITUDE_LOV = ifelse(df$LONGITUDE_LOV < 0, 
                          df$LONGITUDE_LOV + 360, 
                          df$LONGITUDE_LOV)

time_stamp = str_split_fixed(df$DATE_, "/", 3)
colnames(time_stamp) = c("Month", "Day", "Year")

df = cbind(time_stamp, df)

df = df %>% 
  group_by(LATITUDE_LOV, LONGITUDE_LOV, Day, Month, Year) %>% 
  summarise(n = n())

df = df[complete.cases(df), ]

world <- rnaturalearth::ne_countries(scale = 'small', returnclass = "sp")
box_cut <- bbox2SP(n = 90, s = -90, w = -20, e = 20, proj4string = world@proj4string)
world_crop <- gDifference(world, box_cut)

pacific_crop <- world_crop %>% 
  st_as_sf() %>% 
  st_shift_longitude() %>% 
  st_crop(c(
    # xmin = st_bbox(.)[["xmin"]],
    # xmax = st_bbox(.)[["xmax"]],
    xmin = min(df$LONGITUDE_LOV),
    xmax = max(df$LONGITUDE_LOV),
    ymin = min(df$LATITUDE_LOV),
    ymax = max(df$LATITUDE_LOV)))

df$Month = ifelse(df$Month %in% c(1:9), sprintf("%02d", as.numeric(df$Month)), df$Month)

ggplot() +
  # geom_sf(data = pacific_crop, size = 0.01, fill = "gray", color = "gray") +
  geom_hex(data = df, aes(x = LONGITUDE_LOV, y = LATITUDE_LOV), bins = 10) +
  scale_fill_viridis_c("") + 
  # facet_grid(Month ~ Year) +
  facet_wrap(~Year) +
  # facet_wrap(~Month) +
  # scale_x_continuous(expand = c(0, 0), "") +
  # scale_y_continuous(expand = c(0, 0), "") + 
  ggdark::dark_theme_void() +
  coord_fixed()
