#############################################################################################
# Plastic Bag Ban
# Map of policies
# last modified: 01/04/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, ggplot2, dplyr, readr, tidyr, broom, stargazer, stringr, lubridate, raster, sf, ggpubr, PNWColors, usmap)    

## directory 
directory <- "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/.shortcut-targets-by-id/19xFikvYAiHUYb-Az8rxT_I6rsaNv_57l/BagBan_BeachDebris/"
setwd(paste0(directory, "plasticbag/replication/"))

## figure parameters 
textSize <- 10

# Load data -------------------------------------------------------------------------------------

load(file="data/processed/01_county_policy.rda")

# Setup for maps --------------------------------------------------------------------------------

countyCurrent <- county %>% filter(repealed != 1)
countyCurrent <- countyCurrent %>% mutate(values = ifelse(completeBagBan == 1 & paperBagCharge == 0, "Complete Ban, No Paper Charge", 
                                                          ifelse(completeBagBan == 1 & paperBagCharge == 1, "Complete Ban, Paper Charge", 
                                                                 ifelse(partialBagBan == 1 & paperBagCharge == 0, "Partial Ban, No Paper Charge", 
                                                                        ifelse(partialBagBan == 1 & paperBagCharge == 1, "Partial Ban, Paper Charge", 
                                                                               ifelse(plasticBagCharge == 1 & paperBagCharge == 0, "Plastic Charge, No Paper Charge", "Plastic Charge, Paper Charge"))))))
countyCurrentChart <- countyCurrent %>% dplyr::select(fips, values)

countiesAll <- read.csv("data/other/uscounties.csv")
preemptive <- countiesAll %>% filter(state_id %in% c("AZ", "AR", "FL", "ID", "IN", "IA", "MI", "MO", "MT", "NE", "MS", "ND", "OK", "SD", "TN", "TX", "WI"))
preemptive <- preemptive %>% dplyr::select(fips = county_fips) %>% mutate(values = "Ban on Ban/Charge")
countiesAll <- countiesAll %>% dplyr::select(fips = county_fips)
countyCurrentChart <- rbind(countyCurrentChart, preemptive)
countyCurrentChart <- left_join(countiesAll, countyCurrentChart)
countyCurrentChart <- countyCurrentChart %>% mutate(values = ifelse(is.na(values), "No Ban", values))

plot_usmap(data = countyCurrentChart, regions = "counties", color="black", size=0.1) + 
  scale_fill_manual(values = c("Complete Ban, No Paper Charge" = "#882255","Complete Ban, Paper Charge" = "#CC6677",
                               "Partial Ban, No Paper Charge" = "#44AA99", "Partial Ban, Paper Charge" = "#117733", 
                               "Plastic Charge, No Paper Charge" = "#DDCC77", "Plastic Charge, Paper Charge" = "#88CCEE",
                               "Ban on Ban/Charge" = "lightgray", "No Ban" = "white"), na.value="lightgray", name="") + 
  theme(legend.position="bottom", legend.text=element_text(size=textSize))
ggsave("figures/appendix/figure_s03_policies_map.png", width=17.4)



