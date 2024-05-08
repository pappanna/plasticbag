#############################################################################################
# Plastic Bag Ban
# Zip code, quarter level final data combination 
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
setwd(paste0(directory, "plasticbag/replication/"))

# Load cleanups and policies  ---------------------------------------------------------------

# zip policy 
load("data/processed/01_zip_policy_characteristics.rda")

# cleanups by zip x quarter
load('data/processed/00_data_intermediate/00_data_zip_quarter.rda')

# spillover zip codes 
load('data/processed/01_zip_neighbors_policy.rda')

# Merge --------------------------------------------------------------------------------------

data <- left_join(cleanupZipQuarter, merged %>% dplyr::select(zip, state, medianIncome, incQuartile, incQuintile, firstEffect, completeBagBan, partialBagBan, bagLimit, plasticBagCharge, paperBagCharge, paperBagBan, repealed, repealedDate, 
                                                              lastPolicyGeo, firstPolicyGeo, lastPolicyType, firstPolicyType, lastPolicyID , firstPolicyID, popTotal))
data <- data %>% mutate(zip = as.integer(zip))

# neighbor 
data <- left_join(data, neighborZipFinal)
data <- data %>% mutate(neighborInd = ifelse(is.na(neighborInd), 0, neighborInd), 
                        neighborRepealedInd = ifelse(is.na(neighborRepealedInd), 0, neighborRepealedInd))

data <- data %>% mutate(quarterStartDate = ifelse(quarter == 0, as.Date(paste0(year-1, "-12-01"), format="%Y-%m-%d"), 
                                                  ifelse(quarter == 1, as.Date(paste0(year, "-03-01"), format="%Y-%m-%d"),
                                                         ifelse(quarter == 2, as.Date(paste0(year, "-06-01"), format="%Y-%m-%d"),
                                                                as.Date(paste0(year, "-09-01"), format="%Y-%m-%d")))))
data <- data %>% mutate(quarterStartDate = as.Date(quarterStartDate, format="%Y-%m-%d", origin = "1970-01-01"))

# Process ------------------------------------------------------------------------------------

data <- data %>% mutate(type = ifelse(completeBagBan == 1 & paperBagCharge == 0, "completeNoCharge", 
                                      ifelse(completeBagBan == 1 & paperBagCharge == 1, "completeCharge", 
                                             ifelse(partialBagBan == 1 & paperBagCharge == 0, "partialNoCharge", 
                                                    ifelse(partialBagBan == 1 & paperBagCharge == 1, "partialCharge", 
                                                           ifelse(plasticBagCharge == 1 & paperBagCharge == 0, "chargeNoCharge", 
                                                                  ifelse(plasticBagCharge == 1 & paperBagCharge == 1, "chargeCharge", NA)))))))
data <- data %>% mutate(type = ifelse(is.na(type), "control", type))
data <- data %>% mutate(type = ifelse(is.na(repealed) | repealed == 0, type,  "repealed"))
data <- data %>% mutate(type = ifelse(neighborInd == 1, "controlSpillover1", type))
data <- data %>% mutate(type = ifelse(neighborRepealedInd == 1, "controlRepealSpillover1", type))
data <- data %>% mutate(type = ifelse(neighborInd == 2, "controlSpillover2", type))
data <- data %>% mutate(type = ifelse(neighborRepealedInd == 2, "controlRepealSpillover2", type))
data <- data %>% mutate(type = ifelse(neighborInd == 3, "controlSpillover3", type))
data <- data %>% mutate(type = ifelse(neighborRepealedInd == 3, "controlRepealSpillover3", type))

nrow(data %>% filter(type == "completeNoCharge"))
nrow(data %>% filter(type == "completeCharge"))
nrow(data %>% filter(type == "partialNoCharge"))
nrow(data %>% filter(type == "partialCharge"))
nrow(data %>% filter(type == "chargeNoCharge"))
nrow(data %>% filter(type == "chargeCharge"))
nrow(data %>% filter(type == "control"))
nrow(data %>% filter(type == "controlSpillover1"))
nrow(data %>% filter(type == "controlRepealSpillover1"))
nrow(data %>% filter(type == "controlSpillover2"))
nrow(data %>% filter(type == "controlRepealSpillover2"))
nrow(data %>% filter(type == "controlSpillover3"))
nrow(data %>% filter(type == "controlRepealSpillover3"))
nrow(data %>% filter(type == "repealed"))

# all pooled policies 
data <- data %>% mutate(treatedAny = ifelse(quarterStartDate > firstEffect, 1, 0), 
                        treatedComplete = ifelse(quarterStartDate > firstEffect & completeBagBan == 1, 1, 0),
                        treatedCompleteCharge = ifelse(quarterStartDate > firstEffect & completeBagBan == 1& paperBagCharge == 1, 1, 0), 
                        treatedCompleteNoCharge = ifelse(quarterStartDate > firstEffect & completeBagBan == 1& paperBagCharge == 0, 1, 0), 
                        treatedPartial = ifelse(quarterStartDate > firstEffect & partialBagBan == 1, 1, 0), 
                        treatedPartialCharge = ifelse(quarterStartDate > firstEffect & partialBagBan == 1 & paperBagCharge == 1, 1, 0),
                        treatedPartialNoCharge = ifelse(quarterStartDate > firstEffect & partialBagBan == 1 & paperBagCharge == 0, 1, 0),
                        treatedCharge = ifelse(quarterStartDate > firstEffect & plasticBagCharge == 1, 1, 0),
                        treatedChargeCharge = ifelse(quarterStartDate > firstEffect & plasticBagCharge == 1 & paperBagCharge == 1, 1, 0),
                        treatedChargeNoCharge = ifelse(quarterStartDate > firstEffect & plasticBagCharge == 1 & paperBagCharge == 0, 1, 0), 
                        treatedSpillover1 =  ifelse(quarterStartDate > neighborFirstEffect & neighborInd == 1, 1, 0), 
                        treatedSpillover2 =  ifelse(quarterStartDate > neighborFirstEffect & neighborInd == 2, 1, 0), 
                        treatedSpillover3 =  ifelse(quarterStartDate > neighborFirstEffect & neighborInd == 3, 1, 0))

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
                        treatedSpillover1 = ifelse(is.na(treatedSpillover1), 0, treatedSpillover1), 
                        treatedSpillover2 = ifelse(is.na(treatedSpillover2), 0, treatedSpillover2), 
                        treatedSpillover3 = ifelse(is.na(treatedSpillover3), 0, treatedSpillover3))

# Keep only 2016 - 2022, and areas treated beginning in 2017 
data <- data %>% filter(year >= 2016 & year <= 2023) %>% filter(year(firstEffect) >= 2017 | is.na(firstEffect))
data <- data %>% filter(!is.na(percPlasticBag)) 

# Save --------------------------------------------------------------------------------------

save(data, file="data/processed/02_data_merged/02_merged_zip_quarter.rda")



