#############################################################################################
# Plastic Bag Ban
# Distance to coast, rivers, and lakes 
# last modified: 01/03/23 (2023 update)
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, ggplot2, dplyr, readr, tidyr, broom, stargazer, stringr, lubridate, raster, sf, ggpubr)    

## directory 
directory <- "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/.shortcut-targets-by-id/19xFikvYAiHUYb-Az8rxT_I6rsaNv_57l/BagBan_BeachDebris/"
setwd(paste0(directory, "plasticbag/replication/"))

# Load clean-up data -----------------------------------------------------------------------

load("data/processed/00_data_cleanup.rda")

# Load distance to rivers and coasts  ------------------------------------------------------

## Note: these distances are calculated in Google Earth Engine 
## script 1: https://code.earthengine.google.com/1c7d8600e39ea4426ed228ec37b7d880
## script 2: https://code.earthengine.google.com/f76e17729aa4d81bf88f6f27902b8f14
## script 3: https://code.earthengine.google.com/04129098eec313af5444f2a417dd8209

coast <- read.csv("data/other/distanceCoast.csv")%>% dplyr::select(id, distanceCoast = distance) %>% mutate(id = as.character(id))
rivers <- read.csv("data/other/distanceRivers.csv") %>% dplyr::select(id, distanceRiver = distance) %>% mutate(id = as.character(id))
lakes <- read.csv("data/other/distanceLakes.csv") %>% dplyr::select(id, distanceLake = distance) %>% mutate(id = as.character(id))

# Merge   -----------------------------------------------------------------------------------

cleanup <- left_join(cleanup, coast)
cleanup <- left_join(cleanup, rivers)
cleanup <- left_join(cleanup, lakes)

# find closest waterbody 
cleanup <- cleanup %>% mutate(distanceLake = ifelse(is.na(distanceLake), 200000, distanceLake), 
                              distanceRiver = ifelse(is.na(distanceRiver),250000, distanceRiver))

cleanup <- cleanup %>% mutate(closestWater = ifelse(distanceCoast < distanceRiver & distanceCoast < distanceLake, 0, 
                                                    ifelse(distanceRiver < distanceCoast & distanceRiver < distanceLake, 1, 
                                                        ifelse(distanceLake < distanceCoast & distanceLake < distanceRiver, 2, NA))))

# create indicators for ocean, lake, and river cleanups 
cleanup <- cleanup %>% mutate(coastInd = ifelse(closestWater == 0 & distanceCoast < 1000, 1, 0), 
                              riverInd = ifelse(closestWater == 1 & distanceRiver < 1000, 1, 0), 
                              lakeInd = ifelse(closestWater == 2 & distanceLake < 1000, 1, 0))
cleanup <- cleanup %>% mutate(otherInd = ifelse(coastInd == 0 & riverInd == 0 & lakeInd == 0, 1, 0))
cleanup <- cleanup %>% filter(id != "171393")

# count various types 
sum(cleanup$coastInd)/97774
sum(cleanup$riverInd)/97774
sum(cleanup$lakeInd)/97774
sum(cleanup$otherInd)/97774

# save 
save(cleanup, file="data/processed/00_data_cleanup.rda")

