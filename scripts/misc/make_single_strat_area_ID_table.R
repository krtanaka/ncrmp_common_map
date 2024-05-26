###################################################
### Exporting Strata ID, Areas as single tables ###
###################################################

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

islands = c("gua", "rot", "sai", "tin", "agu"); region = "S.MARIAN"                           # South Mariana Islands
islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "pag", "sar"); region = "N.MARIAN"      # North Mariana Islands
islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                              # American Samoa
islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"                # Pacific Remote Island Areas
islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"    # Main Hawaiian Islands
islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"                 # Northern Hawaiian Islands

df_all = NULL

for (isl in 1:length(islands)) {
  
  # isl = 6
  
  key = read_csv(paste0("outputs/tables/strata_keys_", region, "_", islands[isl], ".csv"))
  table = read_csv(paste0("outputs/tables/strata_table_", region, "_", islands[isl], ".csv"))
  
  key_table = left_join(key, table)
  key_table$region = region
  key_table$island = islands[isl]
  
  key_table = key_table %>% dplyr::select(strat, strat_nam, sector_id, reef_id, depth_bin_value, strat_area, island, region)
  colnames(key_table)[1] = "strat_id"
  
  key_table$strat_area = round(key_table$strat_area, 5)
  
  df_all = rbind(df_all, key_table)
  
}

write_csv(df_all, file = paste0("outputs/tables/strata_table_", region, ".csv"))
