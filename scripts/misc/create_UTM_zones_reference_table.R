library(dplyr)
library(readr)

load('data/spc/SURVEY MASTER.RData'); df = SURVEY_MASTER
dat = as.data.frame(do.call('rbind', strsplit(as.character(df$SITE),'-',fixed = TRUE)))[1]

colnames(dat) = "Island_Code"

df = cbind(df, dat)

ncrmp_utm_zones = df %>% 
  group_by(ISLAND, Island_Code) %>% 
  summarise(Lon = median(LONGITUDE_LOV, na.rm = T),
            Lat = median(LATITUDE_LOV, na.rm = T)) %>% 
  na.omit() %>% 
  mutate(UTM_Zone = (floor((Lon + 180)/6) %% 60) + 1,
         Hemisphere = ifelse(Lat > 0, "+north", "+south")) %>% 
  dplyr::select(ISLAND, Island_Code, UTM_Zone, Hemisphere) %>% 
  as.data.frame()

colnames(ncrmp_utm_zones) = c("Island", "Island_Code", "UTM_Zone", "Hemisphere")

ncrmp_utm_zones$Island_Code = tolower(as.character(ncrmp_utm_zones$Island_Code))
ncrmp_utm_zones$Island = gsub(" " , "_", ncrmp_utm_zones$Island)

ncrmp_utm_zones$UTM_Zone = ifelse(ncrmp_utm_zones$Island == "Lisianski", 1L, ncrmp_utm_zones$UTM_Zone)

ncrmp_utm_zones$Satellite = 13

ncrmp_utm_zones$Satellite = ifelse(ncrmp_utm_zones$Island %in% c("Guam", "Tutuila", "Jarvis", "Johnston", "Kingman", "Palmyra", "Wake", "Oahu", "Lisianski", "Maro"), 12, ncrmp_utm_zones$Satellite)
ncrmp_utm_zones$Satellite = ifelse(ncrmp_utm_zones$Island %in% c("Tutuila", "Kahoolawe", "Pearl_&_Hermes"), 11, ncrmp_utm_zones$Satellite)
ncrmp_utm_zones$Satellite = ifelse(ncrmp_utm_zones$Island %in% c("Hawaii"), 9, ncrmp_utm_zones$Satellite)

write_csv(ncrmp_utm_zones, file = 'data/misc/ncrmp_utm_zones.csv')
