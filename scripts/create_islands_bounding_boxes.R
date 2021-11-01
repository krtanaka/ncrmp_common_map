################################################################
### Scripts to create bounding boxes around surveyed islands ###
### Originally developed & conceptualized by T.A.Oliver      ###
### Revised & Edited & Maintained by K.R.Tanaka & T.A.Oliver ###
################################################################

rm(list = ls())

detach("package:plyr", unload = TRUE)

library(dplyr)
library(readr)
library(ggplot2)

load('data/rea/SURVEY MASTER.RData'); df = SURVEY_MASTER; rm(SURVEY_MASTER)

df$lon = df$LONGITUDE_LOV
df$lat = df$LATITUDE_LOV

drop_LatLonNAs = unique(c(which(is.na(df$lon)), which(is.na(df$lat))))
if(length(drop_LatLonNAs) > 0) df = df[-drop_LatLonNAs,] 
dim(df)

# df$lon = ifelse(df$lon < 0, df$lon + 180, df$lon)

###############
### mapping ###
###############

unique(df$REGION)
region = unique(df$REGION)[4]

df %>%
  subset(REGION == region) %>%
  group_by(ISLAND) %>% 
  summarise(RIGHT_XMAX = max(lon)+0.05,
            LEFT_XMIN = min(lon)-0.05,
            TOP_YMAX = max(lat)+0.05,
            BOTTOM_YMIN = min(lat)-0.05) %>%
  ggplot() +
  annotation_map(map_data("world")) +
  geom_point(data = df %>% subset(REGION == region), aes(lon, lat, color = ISLAND), alpha = 0.5) +
  geom_rect(mapping = aes(
    xmin = LEFT_XMIN,
    xmax = RIGHT_XMAX,
    ymin = BOTTOM_YMIN,
    ymax = TOP_YMAX,
    fill = ISLAND,
    color = ISLAND), alpha = 0.2) + 
  facet_wrap(.~ISLAND, scales = "free") +
  theme(legend.position = "none") 

############################
### export as a csv file ###
############################

boxes = df %>%
  group_by(ISLAND) %>% 
  summarise(RIGHT_XMAX = max(lon)+0.05,
            LEFT_XMIN = min(lon)-0.05,
            TOP_YMAX = max(lat)+0.05,
            BOTTOM_YMIN = min(lat)-0.05)

df_frame = df[,c("REGION", "ISLAND")]

df = merge(boxes, df_frame)
df = df[!duplicated(df), ]

df = df[,c("REGION", "ISLAND", "TOP_YMAX", "BOTTOM_YMIN", "LEFT_XMIN", "RIGHT_XMAX")]
colnames(df) = c("Region", "Island", "ymax", "ymin", "xmin", "xmax")

df$Island = gsub(" ", "_", df$Island)

write_csv(df, 'data/misc/Island_Extents.csv')
