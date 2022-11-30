##################################################################################
### estimate functional fish group biomass variabilities w/ simple Tweedie GAM ###
##################################################################################

library(dplyr)
library(mgcv)
library(sp)
library(rgdal)
library(ggplot2)

rm(list = ls())

load("data/rea/fish_site_data.Rdata"); df = wsd; rm(wsd)

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

regions = as.character(unique(df$REGION))

par(mfrow = c(2,3))

for (r in 1:length(regions)) {
  
  # r = 1
  
  print(regions[r])

  df %>% 
    subset(REGION == regions[r]) %>% 
    select(LONGITUDE, LATITUDE, DEPTH_BIN, ISLAND, TotFishBio) %>% 
    na.omit() %>% 
    group_by(DEPTH_BIN) %>% 
    summarise(sd = sd(TotFishBio, na.rm = T),
              var = var(TotFishBio, na.rm = T))
  
  df_r = df %>% 
    subset(REGION == regions[r]) %>% 
    mutate(LATITUDE = round(LATITUDE, 2),
           LONGITUDE = round(LONGITUDE, 2)) %>% 
    # subset(TotFishBio > 0) %>%
    # select(LONGITUDE, LATITUDE, DEPTH, OBS_YEAR, TotFishBio) %>% 
    # na.omit() %>%
    group_by(LONGITUDE, LATITUDE) %>% 
    summarise(sd = sd(TotFishBio, na.rm = T),
              var = var(TotFishBio, na.rm = T)) %>% 
    as.data.frame()
  
  # qplot(df_r$LONGITUDE, df_r$LATITUDE, color = log10(df_r$var + 1)) + scale_color_viridis_c()
  
  # par(mfrow = c(1,2))
  # hist(df_r$var)
  # hist(df_r$sd)
  # dev.off()
  
  zone <- (floor((df_r$LONGITUDE[1] + 180)/6) %% 60) + 1
  
  df_r = cbind(df_r, LongLatToUTM(df_r$LONGITUDE, df_r$LATITUDE, zone))
  colnames(df_r)[6:7] = c("x", "y")
  
  g = gam(var ~ s(x, y),
          # + s(DEPTH) + OBS_YEAR, 
          family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)", 
          gamma = 1.4,
          data = df_r)
  
  summary(g)
  vis.gam(g, too.far = 0.005, n.grid = 100, plot.type = "contour", type = "response", axes = F, pch = ".")
  axis(1); axis(2)
  # gam.check(g)
  
  save(g, file = paste0("data/rea/modeled_survey_variability_", regions[r], ".RData"))
  
  print(regions[r])
  
}
