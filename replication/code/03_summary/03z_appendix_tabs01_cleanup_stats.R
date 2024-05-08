#############################################################################################
# Plastic Bag Ban
# Summary stats table (appendix) clean-up data 
# last modified: 04/13/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, ggplot2, dplyr, readr, tidyr, broom, stargazer, stringr, lubridate, raster, sf, ggpubr, PNWColors, lfe)    

## directory 
directory <- "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/.shortcut-targets-by-id/19xFikvYAiHUYb-Az8rxT_I6rsaNv_57l/BagBan_BeachDebris/"
setwd(paste0(directory, "replication/"))

# Load data -------------------------------------------------------------------------------

# clean-up 
load("data/processed/00_data_cleanup.rda")

# policy 
load("data/processed/01_zip_policy_characteristics.rda")
policies <- merged %>% mutate(zip = as.integer(zip))

# Summary stats ----------------------------------------------------------------------------

river <- cleanup %>% filter(riverInd == 1)
coast <- cleanup %>% filter(coastInd == 1)
lake <- cleanup %>% filter(lakeInd == 1)
other <- cleanup %>% filter(otherInd == 1)

# table 

mod1a <-felm(data=cleanup, totalItems ~ people |0|0|0)
mod2a <-felm(data=river, itemsPP ~ people |0|0|0)
mod3a <-felm(data=coast, plasticGroceryBag ~ people |0|0|0)
mod4a <-felm(data=lake, itemsPPPM ~ people |0|0|0)
mod5a <-felm(data=other, paperBags ~ people |0|0|0)
stargazer(mod1a, mod3a, mod2a, mod4a, mod5a,
          type="text", df=F,
          report=("vc*sp"),
          font.size="scriptsize",
          column.sep.width = c("10pt"),
          omit.stat=c("ser","adj.rsq", "rsq"),
          digits=3,
          title = "", 
          keep = c("miles"),
          covariate.labels = c(" "), 
          dep.var.labels = c("All Cleanups", "Coastal Cleanups", "River Cleanups" , "Lake Cleanups", "Other Cleanups") ,
          add.lines = list(c("Number of People", round(mean(cleanup$people), 2) , round(mean(coast$people), 2), round(mean(river$people), 2), round(mean(lake$people), 2), round(mean(other$people), 2)), 
                           c(" ", paste0("(", round(sd(cleanup$people), 2), ")") , paste0("(", round(sd(coast$people), 2), ")") ,paste0("(", round(sd(river$people), 2), ")"), paste0("(", round(sd(lake$people), 2), ")"), paste0("(", round(sd(other$people), 2), ")")),
                           c("Number of Miles", round(mean(cleanup$miles), 2), round(mean(coast$miles), 2), round(mean(river$miles), 2), round(mean(lake$miles), 2), round(mean(other$miles), 2)),
                           c(" ", paste0("(", round(sd(cleanup$miles), 2), ")") , paste0("(", round(sd(coast$miles), 2), ")") ,paste0("(", round(sd(river$miles), 2), ")"), paste0("(", round(sd(lake$miles), 2), ")"), paste0("(", round(sd(other$miles), 2), ")")),
                           c("Total Items", round(mean(cleanup$totalItems), 2), round(mean(coast$totalItems), 2), round(mean(river$totalItems), 2), round(mean(lake$totalItems), 2), round(mean(other$totalItems), 2)),
                           c(" ", paste0("(", round(sd(cleanup$totalItems), 2), ")") , paste0("(", round(sd(coast$totalItems), 2), ")") ,paste0("(", round(sd(river$totalItems), 2), ")"), paste0("(", round(sd(lake$totalItems), 2), ")"), paste0("(", round(sd(other$totalItems), 2), ")")),
                           c("Items per Person", round(mean(cleanup$itemsPP), 2), round(mean(coast$itemsPP), 2), round(mean(river$itemsPP), 2), round(mean(lake$itemsPP), 2), round(mean(other$itemsPP), 2)),
                           c(" ", paste0("(", round(sd(cleanup$itemsPP), 2), ")") , paste0("(", round(sd(coast$itemsPP), 2), ")") ,paste0("(", round(sd(river$itemsPP), 2), ")"), paste0("(", round(sd(lake$itemsPP), 2), ")"), paste0("(", round(sd(other$itemsPP), 2), ")")),
                           c("Items per Person per Mile", round(mean(cleanup$itemsPPPM, na.rm=TRUE), 2), round(mean(coast$itemsPPPM, na.rm=TRUE), 2), round(mean(river$itemsPPPM, na.rm=TRUE), 2), round(mean(lake$itemsPPPM, na.rm=TRUE), 2), round(mean(other$itemsPPPM, na.rm=TRUE), 2)),
                           c(" ", paste0("(", round(sd(cleanup$itemsPPPM, na.rm=TRUE), 2), ")") , paste0("(", round(sd(coast$itemsPPPM, na.rm=TRUE), 2), ")") ,paste0("(", round(sd(river$itemsPPPM, na.rm=TRUE), 2), ")"), paste0("(", round(sd(lake$itemsPPPM, na.rm=TRUE), 2), ")"), paste0("(", round(sd(other$itemsPPPM, na.rm=TRUE), 2), ")")),
                           c("Plastic Bags", round(mean(cleanup$plasticGroceryBag), 2), round(mean(coast$plasticGroceryBag), 2), round(mean(river$plasticGroceryBag), 2), round(mean(lake$plasticGroceryBag), 2), round(mean(other$plasticGroceryBag), 2)),
                           c(" ", paste0("(", round(sd(cleanup$plasticGroceryBag), 2), ")") , paste0("(", round(sd(coast$plasticGroceryBag), 2), ")") ,paste0("(", round(sd(river$plasticGroceryBag), 2), ")"), paste0("(", round(sd(lake$plasticGroceryBag), 2), ")"), paste0("(", round(sd(other$plasticGroceryBag), 2), ")") ),
                           c("Plastic Bag Share of Items", round(mean(cleanup$percPlasticBag, na.rm=TRUE)*100, 2), round(mean(coast$percPlasticBag, na.rm=TRUE)*100, 2), round(mean(river$percPlasticBag, na.rm=TRUE)*100, 2), round(mean(lake$percPlasticBag, na.rm=TRUE)*100, 2), round(mean(other$percPlasticBag, na.rm=TRUE)*100, 2)),
                           c(" ", paste0("(", round(sd(cleanup$percPlasticBag, na.rm=TRUE)*100, 2), ")") , paste0("(", round(sd(coast$percPlasticBag, na.rm=TRUE)*100, 2), ")") ,paste0("(", round(sd(river$percPlasticBag, na.rm=TRUE)*100, 2), ")"), paste0("(", round(sd(lake$percPlasticBag, na.rm=TRUE)*100, 2), ")"), paste0("(", round(sd(other$percPlasticBag, na.rm=TRUE)*100, 2), ")"))
          ),
          out="tables/appendix/table_s01_summary_stats.tex"
)

# average number of clean-ups per group ID 
countAll <- cleanup %>% mutate(c = 1) %>% group_by(groupID01) %>% summarise(sum = sum(c), zip = as.integer(first(zip)))
mean(countAll$sum)
sd(countAll$sum)
countAll <- left_join(countAll, policies)
sum(countAll$policy, na.rm=TRUE)

countCoast <- coast %>% mutate(c = 1) %>% group_by(groupID01) %>% summarise(sum = sum(c), zip = as.integer(first(zip)))
mean(countCoast$sum)
sd(countCoast$sum)
countCoast <- left_join(countCoast, policies)
sum(countCoast$policy, na.rm=TRUE)

countRiver <- river %>% mutate(c = 1) %>% group_by(groupID01) %>% summarise(sum = sum(c), zip = as.integer(first(zip)))
mean(countRiver$sum)
sd(countRiver$sum)
countRiver <- left_join(countRiver, policies)
sum(countRiver$policy, na.rm=TRUE)

countLake <- lake %>% mutate(c = 1) %>% group_by(groupID01) %>% summarise(sum = sum(c), zip = as.integer(first(zip)))
mean(countLake$sum)
sd(countLake$sum)
countLake <- left_join(countLake, policies)
sum(countLake$policy, na.rm=TRUE)

countOther <- other %>% mutate(c = 1) %>% group_by(groupID01) %>% summarise(sum = sum(c), zip = as.integer(first(zip)))
mean(countOther$sum)
sd(countOther$sum)
countOther <- left_join(countOther, policies)
sum(countOther$policy, na.rm=TRUE)