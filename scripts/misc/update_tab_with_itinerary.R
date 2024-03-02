########################################
### prep survey_grid for simulations ###
########################################

rm(list = ls())

library(dplyr)
library(readr)

utm = read_csv('data/misc/ncrmp_utm_zones.csv')

# islands = c("gua", "rot", "sai", "tin", "agu"); region = "S.MARIAN"                           # South Mariana Islands
# islands = c("agr", "ala", "asc", "gug", "fdp", "mau", "pag", "sar"); region = "N.MARIAN"      # North Mariana Islands
# islands = c("ofu", "ros", "swa", "tau", "tut"); region = "SAMOA"                              # American Samoa
# islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"); region = "PRIAs"                # Pacific Remote Island Areas
islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"); region = "MHI"    # Main Hawaiian Islands
islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"                 # Northern Hawaiian Islands

itinerary = read_csv("data/misc/Draft Itinerary SE-24-06 - RANDOM_maptargets.csv")

names(itinerary) <- gsub(" ", "_", names(itinerary))
colnames(itinerary)[1] = "Island"
itinerary$Island = ifelse(itinerary$Island == "Niihau and Lehua", "Niihau", itinerary$Island)
itinerary$Island = ifelse(itinerary$Island == "French Frigate Shoals", "French_Frigate", itinerary$Island)
itinerary$Island = ifelse(itinerary$Island == "Kure Atoll", "Kure", itinerary$Island)
itinerary$Island = ifelse(itinerary$Island == "Lisianski Island", "Lisianski", itinerary$Island)
itinerary$Island = ifelse(itinerary$Island == "Pearl and Hermes Atolls", "Pearl_&_Hermes", itinerary$Island)

itinerary = left_join(itinerary, utm)

for (isl in 1:length(islands)) {
  
  # isl = 3
  
  load(paste0("outputs/sector_keys/", islands[isl], ".RData"))
  
  itinerary_i = itinerary %>% filter(Island_Code == islands[isl])
  
  if (nrow(itinerary_i) == 0) next
  
  tab$Island_Code = islands[isl]
  tab$depth_bin_short = substr(tab$depth_bin_value, 1, 1)
  tab$reef_short = ""
  tab$reef_short = ifelse(tab$reef_id == "forereef", "FRF", tab$reef_short)
  tab$reef_short = ifelse(tab$reef_id == "lagoon", "LAG", tab$reef_short)
  tab$reef_short = ifelse(tab$reef_id == "protected slope", "PRS", tab$reef_short)
  tab$STRATUM = paste0(tab$reef_short, "_", tab$depth_bin_short)
  
  tab$sector_id <- gsub("south", "s", tab$sector_id)
  tab$sector_id <- gsub("east", "e", tab$sector_id)
  tab$sector_id <- gsub("north", "n", tab$sector_id)
  tab$sector_id <- gsub("west", "w", tab$sector_id)
  tab$sector_id <- gsub("_sector", "", tab$sector_id)
  
  itinerary_i$sector_id <- tolower(substr(itinerary_i$SECTOR, 1, 6))
  tab$sector_id <- tolower(substr(tab$sector_id, 1, 6))
  
  table(itinerary_i$sector_id)
  table(tab$sector_id)
  
  tab = left_join(tab, itinerary_i) 
  
  save(tab, file = paste0("outputs/sector_keys/", islands[isl], "_itinerary.RData"))

}
