library(dplyr)
library(mgcv)
library(sp)
library(rgdal)

rm(list = ls())

load("data/rea/ALL_REA_FISH_RAW.rdata")

#Function
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +units=km +zone=", zone, " ellps=WGS84", sep = '')))
  return(as.data.frame(res))
}

df[df == -9991] <- NA

select = dplyr::select

glimpse(df)

df %>% 
  subset(REGION == "MHI") %>% 
  select(LONGITUDE, LATITUDE, DEPTH_BIN, SECTOR, ISLAND, TROPHIC_MONREP, BIOMASS_G_M2) %>% 
  na.omit() %>% 
  # group_by(DEPTH_BIN, SECTOR, ISLAND, TROPHIC_MONREP) %>% 
  group_by(DEPTH_BIN, SECTOR) %>% 
  summarise(sd = sd(BIOMASS_G_M2, na.rm = T),
            var = var(BIOMASS_G_M2, na.rm = T))

gdf = df %>% 
  subset(REGION == "MHI") %>% 
  subset(BIOMASS_G_M2 > 0) %>%
  # subset(TROPHIC_MONREP == "PLANKTIVORE") %>%
  # subset(TROPHIC_MONREP == "PISCIVORE") %>%
  # subset(TROPHIC_MONREP == "PRIMARY") %>%
  # subset(TROPHIC_MONREP == "SECONDARY") %>%
  subset(TROPHIC_MONREP %in% c("PISCIVORE", "PLANKTIVORE", "PRIMARY", "SECONDARY")) %>%
  select(LONGITUDE, LATITUDE, DEPTH, TROPHIC_MONREP, OBS_YEAR, BIOMASS_G_M2) %>% 
  na.omit() %>% 
  group_by(LONGITUDE, LATITUDE) %>% 
  summarise(sd = sd(BIOMASS_G_M2, na.rm = T),
            var = var(BIOMASS_G_M2, na.rm = T)) %>% 
  as.data.frame()

hist(gdf$var)
hist(gdf$sd)

zone <- (floor((gdf$LONGITUDE[1] + 180)/6) %% 60) + 1

gdf = cbind(gdf, LongLatToUTM(gdf$LONGITUDE, gdf$LATITUDE, zone))
colnames(gdf)[6:7] = c("x", "y")

g = gam(sd ~ s(x, y),
        # + s(DEPTH) + OBS_YEAR, 
        family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)", 
        gamma = 1.4,
        data = gdf)

summary(g)
vis.gam(g, too.far = 0.005, n.grid = 100, plot.type = "contour", type = "response", axes = F, pch = ".")
axis(1); axis(2)
gam.check(g)

save(g, file = "data/modeled_survey_variability.RData")
