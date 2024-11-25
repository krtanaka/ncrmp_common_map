############################################################################
### estimate functional fish group biomass variabilities via Tweedie GAM ###
############################################################################

library(dplyr)
library(mgcv)
library(sp)
# library(rgdal)
library(ggplot2)

rm(list = ls())

load("data/spc/fish_site_data.Rdata"); df = wsd; rm(wsd)

# Function
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## example
  res <- spTransform(xy, CRS(paste("+proj=utm +units=km +zone=", zone, " ellps=WGS84", sep = '')))
  return(as.data.frame(res))
}

df[df == -9991] <- NA

select = dplyr::select

glimpse(df)

regions = as.character(unique(df$REGION))

for (r in 1:length(regions)) {
  
  # r = 1
  
  print(regions[r])
  
  df_r = df %>% 
    # subset(REGION == regions[r]) %>% 
    subset(ISLAND == "Guam") %>% 
    mutate(LATITUDE = round(LATITUDE, 2),
           LONGITUDE = round(LONGITUDE, 2)) %>% 
    group_by(LONGITUDE, LATITUDE) %>% 
    summarise(sd_piscivore = sd(PISCIVORE_BIO, na.rm = T),
              sd_planktivore = sd(PLANKTIVORE_BIO, na.rm = T),
              sd_primary = sd(PRIMARY_BIO, na.rm = T),
              sd_secondary = sd(SECONDARY_BIO, na.rm = T),
              sd_total = sd(TotFishBio, na.rm = T)) %>% 
    as.data.frame()
  
  # qplot(df_r$LONGITUDE, df_r$LATITUDE, color = log10(df_r$sd_piscivore + 1)) + scale_color_viridis_c()
  # qplot(df_r$LONGITUDE, df_r$LATITUDE, color = log10(df_r$sd_planktivore + 1)) + scale_color_viridis_c()
  # qplot(df_r$LONGITUDE, df_r$LATITUDE, color = log10(df_r$sd_primary + 1)) + scale_color_viridis_c()
  # qplot(df_r$LONGITUDE, df_r$LATITUDE, color = log10(df_r$sd_secondary + 1)) + scale_color_viridis_c()
  
  # par(mfrow = c(2,2))
  # hist(df_r$sd_piscivore)
  # hist(df_r$sd_planktivore)
  # hist(df_r$sd_primary)
  # hist(df_r$sd_secondary)
  # dev.off()
  
  zone <- (floor((df_r$LONGITUDE[1] + 180)/6) %% 60) + 1
  
  df_r = cbind(df_r, LongLatToUTM(df_r$LONGITUDE, df_r$LATITUDE, zone))
  colnames(df_r)[9:10] = c("x", "y")
  
  g_piscivore = gam(sd_piscivore ~ s(x, y),
                    family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)", 
                    gamma = 1.4,
                    data = df_r)
  
  g_planktivore = gam(sd_planktivore ~ s(x, y),
                      family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)", 
                      gamma = 1.4,
                      data = df_r)
  
  g_primary = gam(sd_primary ~ s(x, y),
                  family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)", 
                  gamma = 1.4,
                  data = df_r)
  
  g_secondary = gam(sd_secondary ~ s(x, y),
                    family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)", 
                    gamma = 1.4,
                    data = df_r)
  
  g_total = gam(sd_total ~ s(x, y),
                family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)", 
                gamma = 1.4,
                data = df_r)
  
  # summary(g_total)
  # vis.gam(g_total, too.far = 0.01, n.grid = 100, plot.type = "contour", type = "response")
  # gam.check(g_total)
  
  save(g_piscivore, file = paste0("data/spc/modeled_piscivore_variability_", regions[r], ".RData"))
  save(g_planktivore, file = paste0("data/spc/modeled_planktivore_variability_", regions[r], ".RData"))
  save(g_primary, file = paste0("data/spc/modeled_primary_variability_", regions[r], ".RData"))
  save(g_secondary, file = paste0("data/spc/modeled_secondary_variability_", regions[r], ".RData"))
  save(g_total, file = paste0("data/spc/modeled_total_variability_", regions[r], ".RData"))

  print(regions[r])
  
}
