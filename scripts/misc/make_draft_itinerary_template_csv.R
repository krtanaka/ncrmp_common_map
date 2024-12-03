##################################################################
### Exporting draft itinerary table as csv for selected regions ##
##################################################################

rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(sp)
library(tidyr)
library(sf)
library(readr)
library(concaveman)
library(patchwork)
library(ggthemes)

utm = read_csv('data/misc/ncrmp_utm_zones.csv')

regions = c("S.MARIAN",
            "N.MARIAN", 
            # "SAMOA",
            "PRIAs")
            # "MHI", 
            # "NWHI") 

df_all = NULL

for (r in 1:length(regions)) {
  
  # r = 7
  
  df = read_csv(paste0("outputs/tables/strata_table_", regions[r], ".csv")) %>%
    mutate(Island_Code = tolower(island))
  
  df <- left_join(df, utm)
  
  df = df %>% 
    dplyr::select(Island, Island_Code, sector_id, reef_id, depth_bin, area_km2)
  
  df_all = rbind(df_all, df)
  
}

df_all = df_all %>% 
  mutate(across(where(is.character), toupper)) %>%
  rename_with(toupper)

write_csv(df_all, file = "data/misc/draft_itinerary.csv")
