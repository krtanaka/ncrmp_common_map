##########################################
### Simulate stratified-random survey  ###
##########################################

library(SimSurvey)
library(raster)
library(data.table)
library(ggplot2)
library(dplyr)
library(scales)

rm(list = ls())

# set.seed(10)

islands = c("Hawaii", "Kahoolawe", "Kauai", "Lanai", "Maui", "Molokai", "Niihau", "Oahu" )[sample(1:8, 1)]
load(paste0("data/survey_grid_w_sector_reef/survey_grid_", islands, ".RData"))

n_sims = 10
min_sets = 2
set_den = 2/1000
trawl_dim = c(0.01, 0.0353)
resample_cells = FALSE
total_sample = 30

# options(scipen = 999, digits = 2)

sim = sim_abundance(years = 2010:2020, ages = 1:5,
                    R = sim_R(log_mean = log(100),
                              log_sd = 0.9),
                    Z = sim_Z(log_mean = log(0.1))) %>% 
  sim_distribution(grid = survey_grid_kt) 

strat_sets <- cell_sets <- NULL

cells <- data.table(rasterToPoints(sim$grid)); cells

strat_det <- cells[, list(strat_cells = .N), by = "strat"]; strat_det
strat_det$tow_area <- prod(trawl_dim); strat_det
strat_det$cell_area <- prod(res(sim$grid)); strat_det
strat_det$strat_area <- strat_det$strat_cells * prod(res(sim$grid)); strat_det
strat_det$strat_sets <- round(strat_det$strat_area * set_den); strat_det
strat_det$strat_sets = round((total_sample * strat_det$strat_area) / sum(strat_det$strat_area), 0); strat_det
strat_det$strat_sets[strat_det$strat_sets < min_sets] <- min_sets; strat_det
# strat_det$area_prop = rescale(strat_det$strat_area, to = c(0.1, 0.9)); strat_det

cells <- merge(cells, strat_det, by = c("strat")); cells

i <- rep(seq(nrow(cells)), times = length(sim$years)); i
y <- rep(sim$years, each = nrow(cells)); y

cells <- cells[i, ]
cells$year <- y

i <- rep(seq(nrow(cells)), times = n_sims); i
s <- rep(seq(n_sims), each = nrow(cells)); s

cells <- cells[i, ]
cells$sim <- s

subsets <- cells[, .SD[sample(.N, strat_sets, replace = resample_cells)], 
                 by = c("sim", "year", "strat")]

subsets[, `:=`(cell_sets, .N), by = c("sim", "year", "cell")]; subsets
subsets$set <- seq(nrow(subsets)); subsets

subsets %>% 
  subset(strat == 4) %>%
  ggplot(aes(x, y, color = factor(year))) + 
  geom_point() + 
  coord_fixed() + 
  scale_color_discrete("") 

subsets %>% 
  # subset(sim == 10) %>%
  group_by(strat, year) %>% 
  summarise(n = n()) 

subsets %>% 
  group_by(strat, year, sim) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(strat, n, fill = factor(year))) + 
  geom_bar(position = "dodge", stat = "identity")

