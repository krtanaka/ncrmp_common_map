########################################
### prep survey_grid for simulations ###
########################################

rm(list = ls())

library(dplyr)
library(readr)

utm = read_csv('data/misc/ncrmp_utm_zones.csv')

islands = c("gua", "rot", "sai", "tin", "agu"); region = "S.MARIAN"                           # South Mariana Islands
islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "pag", "sar"); region = "N.MARIAN"      # North Mariana Islands
islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                              # American Samoa
islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"                # Pacific Remote Island Areas
islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"    # Main Hawaiian Islands
islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"                 # Northern Hawaiian Islands

itinerary = read_csv("data/misc/Draft Itinerary SE-24-06 - RANDOM_maptargets.csv")
names(itinerary) <- gsub(" ", "_", names(itinerary))

itinerary = left_join(itinerary, utm)

for (isl in 1:length(islands)) {
  
  # isl = 1
  
  tab = read_csv(paste0("outputs/tables/strata_keys_", region, "_", islands[isl], ".csv"))
  
  itinerary_i = itinerary %>% filter(Island_Code == islands[isl])
  
  if (nrow(itinerary_i) == 0) next
  
  tab$Island_Code = islands[isl]
  tab$depth_bin_short = substr(tab$depth_bin, 1, 1)
  tab$reef_short = ""
  tab$reef_short = ifelse(tab$reef_id == "forereef", "FRF", tab$reef_short)
  tab$reef_short = ifelse(tab$reef_id == "lagoon", "LAG", tab$reef_short)
  tab$reef_short = ifelse(tab$reef_id == "protected slope", "PRS", tab$reef_short)
  tab$STRATUM = paste0(tab$reef_short, "_", tab$depth_bin_short)
  
  print(table(itinerary_i$sector_id))
  print(table(tab$sector_id))
  
  tab = left_join(tab, itinerary_i) 
  
  save(tab, file = paste0("outputs/sector_keys/", islands[isl], "_itinerary.RData"))
  
}
