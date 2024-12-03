##################################################################
### Exporting Strata ID, Areas as single table for each region ###
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

islands = c("gua", "rot", "sai", "tin", "agu"); region = "S.MARIAN"                           # South Mariana Islands
islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "pag", "sar"); region = "N.MARIAN"      # North Mariana Islands
islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                              # American Samoa
islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"                # Pacific Remote Island Areas
islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"    # Main Hawaiian Islands
islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"                 # Northern Hawaiian Islands

df_all = NULL

for (isl in 1:length(islands)) {
  
  # isl = 7
  
  key = read_csv(paste0("outputs/keys/key_", region, "_", islands[isl], ".csv"))

  key$region = region
  key$island = islands[isl]
  
  key = key %>% dplyr::select(stratum, stratum_name, sector_id, reef_id, depth_bin, area_km2, island, region)
  colnames(key)[1] = "stratum_id"
  
  key$area_km2 = round(key$area_km2, 2)
  
  df_all = rbind(df_all, key)
  
}

df_all = df_all %>% 
  mutate(across(where(is.character), toupper))

write_csv(df_all, file = paste0("outputs/tables/strata_table_", region, ".csv"))
