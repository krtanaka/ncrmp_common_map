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

(survey_effort = df %>% 
    # subset(OBS_YEAR >= 2013) %>%
    group_by(ISLAND, OBS_YEAR) %>%
    # group_by(OBS_YEAR) %>%
    # group_by(ISLAND) %>%
    summarise(n = n()) %>% 
    group_by(ISLAND) %>%
    summarise(low = round(quantile(n, probs = 0.25), 0),
              mid = round(median(n), 0),
              high = round(quantile(n, probs = 0.75), 0)) %>% 
    as.data.frame())

colnames(survey_effort)[1] = "Island"

save(survey_effort, file = "data/misc/survey_effort_ncrmp_2000-2020.RData")


# df <- df %>% 
# subset(REGION == "MHI") %>% 
# subset(ISLAND != "Kahoolawe") %>% 
# subset(OBS_YEAR %in% c(2013, 2016, 2019))  

(p1 = df %>% 
    group_by(ISLAND,OBS_YEAR) %>% 
    summarise(n = n()) %>% 
    na.omit() %>% 
    ggplot(aes(OBS_YEAR, n, color = ISLAND)) + 
    geom_point(show.legend = F) +
    geom_line(show.legend = F) + 
    # facet_grid(SEC_NAME~DEPTH_BIN ) +
    facet_grid(~ISLAND) + 
    theme_minimal() + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))

(p2 = df %>% 
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
)

p1 + p2


