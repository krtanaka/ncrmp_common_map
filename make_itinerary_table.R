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
islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"); region = "NWHI"    

