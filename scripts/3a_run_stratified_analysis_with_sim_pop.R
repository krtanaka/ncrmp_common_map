###################################################################################################################
### Simulate stratified-random surveys in MHI region with simulated fish density (count (n) or biomass (g/sq.m) ###
### Simple Power analysis by comparing different survey "efforts"                                               ###
###################################################################################################################

library(SimSurvey)
library(raster)
library(data.table)
library(ggplot2)
library(dplyr)
library(ggdark)
library(patchwork)

rm(list = ls())

set.seed(42)
# options(scipen = 999, digits = 2)

# pick an island ----------------------------------------------------------
island = c("Hawaii", "Kauai", "Lanai", "Maui", "Molokai", "Niihau", "Oahu" )[sample(1:7, 1)]
load(paste0("data/survey_grid_w_sector_reef/survey_grid_", island, ".RData"))
print(island)

# simulate spatiotemporal dyamic LMR --------------------------------------
sim = sim_abundance(years = 2000:2020, 
                    ages = 1:2,
                    R = sim_R(log_mean = log(100), log_sd = 0.8),
                    Z = sim_Z(log_mean = log(0.5))) %>% 
  sim_distribution(grid = survey_grid_kt)

I <- sim$N
I

# sim <- round_sim(sim)

# simulate stratified random surveys --------------------------------------

n_sims = 100 # number of simulations
total_sample = 30 # total sample efforts you want to deploy
min_sets = 2 # minimum number of sets per strat
set_den = 2/1000 # number of sets per [grid unit = km] squared)
trawl_dim = c(0.01, 0.0353) # 0.000353 sq.km (353 sq.m) from two 15-m diameter survey cylinders
resample_cells = F

n <- id <- division <- strat <- N <- NULL

# sets <- sim_sets(sim,
#                  resample_cells = resample_cells,
#                  n_sims = n_sims,
#                  trawl_dim = trawl_dim,
#                  set_den = set_den,
#                  min_sets = min_sets)

strat_sets <- cell_sets <- NULL

cells <- data.table(rasterToPoints(sim$grid))
strat_det <- cells[, list(strat_cells = .N), by = "strat"]; strat_det
strat_det$tow_area <- prod(trawl_dim); strat_det
strat_det$cell_area <- prod(res(sim$grid)); strat_det
strat_det$strat_area <- strat_det$strat_cells * prod(res(sim$grid)); strat_det
strat_det$strat_sets <- round(strat_det$strat_area * set_den); strat_det
strat_det$strat_sets = round((total_sample * strat_det$strat_area) / sum(strat_det$strat_area), 0); strat_det
strat_det$strat_sets[strat_det$strat_sets < min_sets] <- min_sets; strat_det #make sure minimum number of sets per strat is not 0 or 1

cells <- merge(cells, strat_det, by = c("strat")) # add "strat" "strat_cells" "tow_area" ...

i <- rep(seq(nrow(cells)), times = length(sim$years)) # number of cells * number of years
y <- rep(sim$years, each = nrow(cells)) # number of years * number of cells

cells <- cells[i, ] # increase the number of rows by number or years
cells$year <- y

i <- rep(seq(nrow(cells)), times = n_sims) # number of cells * number of simulations
s <- rep(seq(n_sims), each = nrow(cells)) # number of simulations * number of cells

cells <- cells[i, ] # increase the number of rows by number or simulations
cells$sim <- s

# .SD = "Subset of Data.table"
# .N = number of instances
# strat_sets, see unique(cells$strat_sets)

sets <- cells[, .SD[sample(.N, strat_sets, replace = resample_cells, prob = strat_area)], 
              by = c("sim", "year", "strat")]

sets[, `:=`(cell_sets, .N), by = c("sim", "year", "cell")]
sets$set <- seq(nrow(sets))
sets

setkeyv(sets, c("sim", "year", "cell"))

sp_I <- data.table(sim$sp_N[, c("cell", "year", "N")])

i <- rep(seq(nrow(sp_I)), times = n_sims)
s <- rep(seq(n_sims), each = nrow(sp_I))

sp_I <- sp_I[i, ]
sp_I$sim <- s
setdet <- merge(sets, sp_I, by = c("sim", "year", "cell"))

setdet$n <- stats::rbinom(rep(1, nrow(setdet)), 
                          size = round(setdet$N/setdet$cell_sets), 
                          # prob = (setdet$tow_area/setdet$cell_area) * q(setdet$age))
                          prob = (setdet$tow_area/setdet$cell_area))

setkeyv(setdet, "set")
setkeyv(sets, "set")

sim$I <- I

sim$setdet <- setdet

setdet <- sim$setdet

data = list(setdet = setdet)

data$setdet <- data$setdet[, c("sim", 
                               "year", 
                               "division",
                               "strat",
                               "strat_area", 
                               "tow_area",
                               "set",
                               "n"), 
                           with = FALSE]
data = data$setdet
data

metric = "n"
strat_groups = c("sim", "year", "division", "strat", "strat_area", "tow_area")
survey_groups = c("sim", "year")

confidence = 95

Nh <- strat_area <- tow_area <- Wh <- total <- sumYh <- nh <- gh <- meanYh <- varYh <- meanYst_lcl <- meanYst <- varYst <- df <- meanYst_ucl <- sumYst <- N <- sumYst_lcl <- sumYst_ucl <- NULL

lc <- (100 - confidence)/200; lc
uc <- (100 - confidence)/200 + (confidence/100); uc

d <- copy(data)
d <- d[, c(strat_groups, metric), with = FALSE]

setnames(d, names(d), c(strat_groups, "metric"))
setkeyv(d, strat_groups)

strat_tab <- d[, list(sumYh = sum(metric), # sum of samples (n)
                      meanYh = mean(metric, na.rm = T), # mean of samples (n)
                      varYh = stats::var(metric, na.rm = T), # variance of samples (n)
                      nh = .N), # number of strata
               by = strat_groups]; strat_tab

strat_tab[, `:=`(Nh, strat_area/tow_area)]; strat_tab
strat_tab[, `:=`(Wh, Nh/sum(Nh)), by = survey_groups]; strat_tab
strat_tab[, `:=`(total, Nh * sumYh/nh)]; strat_tab
strat_tab[, `:=`(gh, Nh * (Nh - nh)/nh)]; strat_tab

survey_tab <- strat_tab[, list(n = sum(nh, na.rm = T),
                               N = sum(Nh, na.rm = T), 
                               meanYst = sum(Wh * meanYh), 
                               varYst = (1/((sum(Nh))^2)) * sum(gh * varYh), 
                               df = ((sum(gh * varYh))^2)/(sum((gh^2 * varYh^2)/(nh - 1)))), by = survey_groups]; survey_tab

survey_tab[, `:=`(meanYst_lcl, (meanYst - (sqrt(varYst)) * abs(stats::qt(lc, df))))]; survey_tab
survey_tab[, `:=`(meanYst_ucl, (meanYst + (sqrt(varYst)) * abs(stats::qt(lc, df))))]; survey_tab
survey_tab[, `:=`(sumYst, N * meanYst)]; survey_tab
survey_tab[, `:=`(sumYst_lcl, (sumYst - abs(stats::qt(lc, df)) * N * sqrt(varYst)))]; survey_tab
survey_tab[, `:=`(sumYst_ucl, (sumYst + abs(stats::qt(lc, df)) * N * sqrt(varYst)))]; survey_tab

survey_tab[sapply(survey_tab, is.nan)] <- NA

survey_tab <- survey_tab[, c(survey_groups,
                             "n", 
                             "N", 
                             "df",
                             "varYst", 
                             "meanYst",
                             "meanYst_lcl", 
                             "meanYst_ucl",
                             "sumYst",
                             "sumYst_lcl", 
                             "sumYst_ucl"), 
                         with = FALSE]

survey_tab$varYst <- sqrt(survey_tab$varYst)

setnames(survey_tab, names(survey_tab), c(survey_groups, 
                                          "sets", 
                                          "sampling_units", 
                                          "df", 
                                          "sd", 
                                          "mean",
                                          "mean_lcl",
                                          "mean_ucl",
                                          "total", 
                                          "total_lcl", 
                                          "total_ucl"))
survey_tab

sim$total_strat = survey_tab

total <- NULL

I_hat <- sim$total_strat[, list(sim, year, total)]
names(I_hat) <- c("sim", "year", "I_hat")

I <- data.frame(year = sim$years, I = colSums(sim$I))

comp <- merge(I_hat, I, by = "year")
comp$error <- comp$I_hat - comp$I
means <- error_stats(comp$error)
sim$total_strat_error <- comp
sim$total_strat_error_stats <- means
I_hat <- sim$length_strat[, list(sim, year, length, total)]

sim$total_strat_error_stats
sim$total_strat_error
df = sim$total_strat_error

me = formatC(sim$total_strat_error_stats[1], digits = 3)
mae = formatC(sim$total_strat_error_stats[2], digits = 3)
mse = formatC(sim$total_strat_error_stats[3], digits = 3)
rmse = formatC(sim$total_strat_error_stats[4], digits = 3)

label = paste0("ME = ", me, "\n", "MAE = ", mae, "\n", "MSE = ", mse, "\n", "RMSE = ", rmse)

strata = sim$grid_xy %>%
  mutate(x = round(x/0.5, digits = 0),
         y = round(y/0.5, digits = 0)) %>%
  group_by(x, y) %>% 
  summarise(strat = round(mean(strat), digits = 0),
            depth = mean(depth)) %>% 
  ggplot(aes(x, y)) +
  coord_fixed() + 
  scale_fill_discrete("Strata") + 
  geom_raster(aes(fill = factor(strat))) + 
  theme_minimal() + 
  ylab("Northing (km)") + xlab("Easting (km)") + 
  theme(legend.position = "bottom") + 
  ggtitle(islands)

sim_output = df %>% 
  ggplot() + 
  geom_point(aes(year, I_hat, color = factor(sim), alpha = 0.5), show.legend = F) +
  geom_line(aes(year, I_hat, color = factor(sim), alpha = 0.8), show.legend = F) +
  geom_point(aes(year, I), size = 1, color = "red") + 
  geom_line(aes(year, I), size = 1, color = "red") + 
  scale_color_viridis_d() + 
  theme_minimal() + 
  ylab("total_abundance (n)")+
  labs(
    title = "",
    subtitle = paste0("Total # of surveyed sites = ", total_sample, "\n",
                      "Number of simulations = ", n_sims, "\n",
                      "Min # of sets per strat = ", min_sets))+
  annotate(label = label,
           geom = "text",
           x = Inf,
           y = Inf, 
           size = 4, 
           hjust = 1,
           vjust = 1) 

strata + sim_output
