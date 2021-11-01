library(dplyr)
library(readr)

load('data/rea/SURVEY MASTER.RData'); df = SURVEY_MASTER
dat = as.data.frame(do.call('rbind', strsplit(as.character(df$SITE),'-',fixed = TRUE)))[1]

colnames(dat) = "Island_Code"

df = cbind(df, dat)

ncrmp_utm_zones = df %>% 
  group_by(ISLAND, Island_Code) %>% 
  summarise(Lon = median(LONGITUDE_LOV, na.rm = T),
            Lat = median(LATITUDE_LOV, na.rm = T)) %>% 
  mutate(UTM_Zone = (floor((Lon + 180)/6) %% 60) + 1) %>% 
  dplyr::select(ISLAND, Island_Code, UTM_Zone) %>% 
  as.data.frame()

colnames(ncrmp_utm_zones) = c("Island", "Island_Code", "UTM_Zone")

ncrmp_utm_zones$Island_Code = tolower(as.character(ncrmp_utm_zones$Island_Code))
ncrmp_utm_zones$Island = gsub(" " , "_", ncrmp_utm_zones$Island)

write_csv(ncrmp_utm_zones, file = 'data/ncrmp_utm_zones.csv')
