###########################################################
### Merge updated strata areas with Fish relevant files ###
###########################################################

rm(list=ls())
library(tidyverse)
select <- dplyr::select
# CURRENT STRATUM FILE
sectors<-read_csv('data/misc/Sectors-Strata-Areas.csv')

# NCRMP VizTool FILE
vt_names<-read_csv('data/misc/VizTool Names.csv') %>% 
  unite("strata.ID", SectorCode, StrataCode, sep = "_", remove = FALSE) # CREATE UNIQUE STRATA IDENTIFIER

########################## Different code for each region ##########################

### N.MARIAN + S.MARIAN + WAK ####
## UPDATE AREAS
# strata areas --> AREA = KM^2 (MULTIPLY BY 100 TO CONVERT TO HECTARES)
area<-read_csv('outputs/keys/strata_table_PRIAs.csv') %>% filter(island == "WAK") %>% 
  bind_rows(read_csv('outputs/keys/strata_table_S.MARIAN.csv')) %>%
  bind_rows(read_csv('outputs/keys/strata_table_N.MARIAN.csv')) 

sectors %>% filter(REGION %in% c("MARIAN") | ISLAND == "Wake") #%>% distinct(SEC_NAME)

# UPDATE FORMATTING --> consistent with areas file in fish-paste
new.area <- area %>%   
  # attach island name from VizTool file
  left_join(vt_names %>% distinct(SubjurisdictionCode, SubjurisdictionName) %>% dplyr::rename(island = SubjurisdictionCode, ISLAND = SubjurisdictionName), by = c("island")) %>%
  #  filter(is.na(ISLAND)) # CHECK
  # manually add AGS
  mutate(ISLAND = ifelse(island == "ALA", "Alamagan", ifelse(island == "GUG", "Guguan", ifelse(island == "SAR", "Sarigan", ISLAND)))) %>%
  dplyr::rename(AREA_KM2 = area_km2) %>% 
  #  mutate_at(vars(sector_id), ~toupper(.)) %>% 
  #  mutate_at(vars(depth_bin), ~tolower(.)) %>% mutate_at(vars(reef_id, depth_bin), ~sub("(.)", "\\U\\1", ., perl=TRUE)) %>%
  mutate(depth_bin = ifelse(depth_bin == "MIDD", "Mid", ifelse(depth_bin == "SHAL", "Shallow", "Deep"))) %>% 
  mutate_at(vars(reef_id), ~tolower(.)) %>% mutate_at(vars(reef_id), ~sub("(.)", "\\U\\1", ., perl=TRUE)) %>%
  mutate(SEC_NAME = ifelse(str_detect(sector_id, "_SECTOR"), ISLAND, sector_id)) %>% 
  #  distinct(SEC_NAME) # CHECK
  # REMOVE extra sectors
  # manually fix sec names
  mutate(SEC_NAME = ifelse(SEC_NAME == "GUA_PATI_PT", "GUA_PATI_POINT", ifelse(SEC_NAME == "GUA_TUMON_BAY", "GUA_TUMON", SEC_NAME))) %>%
  left_join(sectors %>% filter(REGION %in% c("MARIAN") | ISLAND == "Wake") %>% distinct(SEC_NAME) %>% mutate(keep = "Y")) %>%
  #  filter(is.na(keep)) # CHECK --> manually fix sec names in code above!
  filter(!is.na(keep)) %>% # remove extra sectors (GUA_LAND)
  # reformat for merging
  dplyr::rename(REEF_ZONE = reef_id, DEPTH_BIN = depth_bin) 

# UPDATE STRATUM FILE
sec.new <- sectors %>% left_join(new.area %>% select(SEC_NAME, REEF_ZONE, DEPTH_BIN, AREA_KM2), by = c("SEC_NAME", "REEF_ZONE", "DEPTH_BIN")) %>%
  #  filter(REGION %in% c("MARIAN") | ISLAND == "Wake") %>% filter(is.na(AREA_KM2)) %>% distinct(SEC_NAME, REEF_ZONE, DEPTH_BIN) # CHECK --> *OK!*
  mutate(AREA_HA_2024 = ifelse(!is.na(AREA_KM2), AREA_KM2 * 100, AREA_HA_2024)) %>% # CONVERT KM2 --> HECTARE
  #  filter(REGION %in% c("MARIAN") | ISLAND == "Wake") %>% filter(is.na(AREA_HA_2024)) # CHECK --> *OK!*
  select(-AREA_KM2)

# CHECK EXTRA SECTORS --> GUA_SASA_BAY, Saipan Backreef & Lagoon, Wake Backreef & Lagoon **REMOVE ALL**
load("D:/CRED/GitHub/fish-paste/data/ALL_REA_FISH_RAW.Rdata")

df %>% filter(SEC_NAME %in% c("GUA_SASA_BAY")) # GUA_SASA_BAY not in dataset or SURVEY MASTER file
df %>% filter(ISLAND == "Saipan" & REEF_ZONE == "Backreef") %>% distinct(SITEVISITID) # Saipan Backreef Shallow --> 1 fish site in 2009
df %>% filter(ISLAND == "Wake" & REEF_ZONE != "Forereef") %>% distinct(METHOD, OBS_YEAR, ISLAND, REEF_ZONE, DEPTH_BIN, SITEVISITID) # 1 belt in 2005 in Lagoon Shallow


# SAVE
write.csv(sec.new %>% select(-...1), 'outputs/keys/Update_Sectors-Strata-Areas.csv')


### NWHI + MHI ####
# LOAD COMMON MAPS FILES
haw.area<-read.csv('outputs/tables/strata_table_MHI.csv') %>% # strata areas --> AREA = KM^2 (MULTIPLY BY 100 TO CONVERT TO HECTARES)
  bind_rows(read.csv('outputs/tables/strata_table_NWHI.csv'))

haw.new <- haw.area %>% 
  dplyr::rename(AREA_KM2 = strat_area) %>% 
  # UPDATE FORMATTING --> consistent with areas file in fish-paste
  mutate_at(vars(sector_id), ~toupper(.)) %>% mutate_at(vars(depth_bin, reef_id), ~tolower(.)) %>% 
  mutate_at(vars(reef_id, depth_bin), ~sub("(.)", "\\U\\1", ., perl=TRUE)) %>% mutate(reef_id = ifelse(reef_id == "Protected slope", "Protected Slope", reef_id)) %>% 
  mutate(depth_bin = ifelse(depth_bin == "Midd", "Mid", ifelse(depth_bin == "Shal", "Shallow", depth_bin))) %>% 
  dplyr::rename(SEC_NAME = sector_id, REEF_ZONE = reef_id, DEPTH_BIN = depth_bin) %>% 
  select(-strat_id, -strat_nam, -island, -region) %>% 
  # FIX SECTOR NAMES SO CONSISTENT WITH SURVEY MASTER
  mutate(SEC_NAME = ifelse(SEC_NAME == "HAW_HAMAK", "HAW_HAMAKUA", ifelse(SEC_NAME == "KAL", "KAL_KAULA", ifelse(SEC_NAME == "FFS_SECTOR", "French Frigate", ifelse(SEC_NAME == "KUR_SECTOR", "Kure", ifelse(SEC_NAME == "LAY_SECTOR", "Laysan", ifelse(SEC_NAME == "LIS_SECTOR", "Lisianski", ifelse(SEC_NAME == "MAR_SECTOR", "Maro", ifelse(SEC_NAME == "MID_SECTOR", "Midway", ifelse(SEC_NAME == "PHR_SECTOR", "Pearl & Hermes", SEC_NAME))))))))))

haw.new %>% distinct(SEC_NAME)

# UPDATE STRATUM FILE
sec.new <- sectors %>% select(-AREA_HA_2024) %>% left_join(haw.new, by = c("SEC_NAME", "REEF_ZONE", "DEPTH_BIN")) %>%
  mutate(AREA_NEW = AREA_KM2 * 100) %>% # CONVERT KM2 --> HECTARE
  dplyr::relocate(AREA_KM2, AREA_NEW, .after = AREA_HA_2023) %>%
  dplyr::rename(AREA_HA_2024 = AREA_NEW) %>% 
  select(-AREA_KM2)

sec.new %>% filter(!is.na(AREA_HA_2024))
sec.new %>% filter(REGION %in% c("MHI", "NHWI") & is.na(AREA_HA_2024)) # just no Molokini -- but that's ok!

# SAVE
write.csv(sec.new, 'outputs/keys/Update_Sectors-Strata-Areas.csv')
