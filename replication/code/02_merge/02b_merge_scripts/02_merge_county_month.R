#############################################################################################
# Plastic Bag Ban
# Zip code, month level final data combination 
# last modified: 07/19/23
#############################################################################################
# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, ggplot2, dplyr, readr, tidyr, broom, stargazer, stringr, lubridate, raster, sf, ggpubr)    

## directory 
directory <- "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/.shortcut-targets-by-id/19xFikvYAiHUYb-Az8rxT_I6rsaNv_57l/BagBan_BeachDebris/"
setwd(paste0(directory, "replication/"))

# Load cleanups and policies  ---------------------------------------------------------------

# zip policy 
load("data/processed/01_county_policy.rda")

# cleanups by zip x month 
# want to run for: all, coast, river, lake, other 
load('data/processed/00_data_intermediate/00_data_county_month.rda')

# Merge --------------------------------------------------------------------------------------

data <- left_join(cleanupCountyMonth, county %>% dplyr::select(county, state, firstEffect, completeBagBan, partialBagBan, bagLimit, plasticBagCharge, paperBagCharge, paperBagBan, repealed, repealedDate))

# Process ------------------------------------------------------------------------------------

data <- data %>% mutate(type = ifelse(completeBagBan == 1 & paperBagCharge == 0, "completeNoCharge", 
                                                                 ifelse(completeBagBan == 1 & paperBagCharge == 1, "completeCharge", 
                                                                        ifelse(partialBagBan == 1 & paperBagCharge == 0, "partialNoCharge", 
                                                                               ifelse(partialBagBan == 1 & paperBagCharge == 1, "partialCharge", 
                                                                                      ifelse(plasticBagCharge == 1 & paperBagCharge == 0, "chargeNoCharge", 
                                                                                             ifelse(plasticBagCharge == 1 & paperBagCharge == 1, "chargeCharge", NA)))))))
data <- data %>% mutate(type = ifelse(is.na(type), "control", type))
data <- data %>% mutate(type = ifelse(is.na(repealed) | repealed == 0, type,  "repealed"))

nrow(data %>% filter(type == "completeNoCharge"))
nrow(data %>% filter(type == "completeCharge"))
nrow(data %>% filter(type == "partialNoCharge"))
nrow(data %>% filter(type == "partialCharge"))
nrow(data %>% filter(type == "chargeNoCharge"))
nrow(data %>% filter(type == "chargeCharge"))
nrow(data %>% filter(type == "control"))
nrow(data %>% filter(type == "repealed"))

# all pooled policies 
data <- data %>% mutate(treatedAny = ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > firstEffect, 1, 0), 
                        treatedComplete = ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > firstEffect & completeBagBan == 1, 1, 0),
                        treatedCompleteCharge = ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > firstEffect & completeBagBan == 1& paperBagCharge == 1, 1, 0), 
                        treatedCompleteNoCharge = ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > firstEffect & completeBagBan == 1& paperBagCharge == 0, 1, 0), 
                        treatedPartial = ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > firstEffect & partialBagBan == 1, 1, 0), 
                        treatedPartialCharge = ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > firstEffect & partialBagBan == 1 & paperBagCharge == 1, 1, 0),
                        treatedPartialNoCharge = ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > firstEffect & partialBagBan == 1 & paperBagCharge == 0, 1, 0),
                        treatedCharge = ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > firstEffect & plasticBagCharge == 1, 1, 0),
                        treatedChargeCharge = ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > firstEffect & plasticBagCharge == 1 & paperBagCharge == 1, 1, 0),
                        treatedChargeNoCharge = ifelse(as.Date(paste0(year, "-", month, "-01"), format="%Y-%m-%d") > firstEffect & plasticBagCharge == 1 & paperBagCharge == 0, 1, 0), 
                        )

data <- data %>% mutate(treatedAny = ifelse(is.na(treatedAny), 0, treatedAny), 
                        treatedComplete = ifelse(is.na(treatedComplete), 0, treatedComplete), 
                        treatedCompleteCharge = ifelse(is.na(treatedCompleteCharge), 0, treatedCompleteCharge), 
                        treatedCompleteNoCharge = ifelse(is.na(treatedCompleteNoCharge), 0, treatedCompleteNoCharge), 
                        treatedPartial = ifelse(is.na(treatedPartial), 0, treatedPartial),
                        treatedPartialCharge = ifelse(is.na(treatedPartialCharge), 0, treatedPartialCharge),
                        treatedPartialNoCharge = ifelse(is.na(treatedPartialNoCharge), 0, treatedPartialNoCharge),
                        treatedCharge = ifelse(is.na(treatedCharge), 0, treatedCharge), 
                        treatedChargeCharge = ifelse(is.na(treatedChargeCharge), 0, treatedChargeCharge),
                        treatedChargeNoCharge = ifelse(is.na(treatedChargeNoCharge), 0, treatedChargeNoCharge), 
                        )

# Keep only 2016 - 2022, and areas treated beginning in 2017 
data <- data %>% filter(year >= 2016 & year <= 2022) %>% filter(year(firstEffect) >= 2017 | is.na(firstEffect))
data <- data %>% filter(!is.na(percPlasticBag)) 

# Save --------------------------------------------------------------------------------------

save(data, file="data/processed/02_data_merged/02_merged_county_month.rda")





