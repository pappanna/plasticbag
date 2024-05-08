#############################################################################################
# Plastic Bag Ban
# Final data files 
# last modified: 01/03/24 (2023 update)
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

# Load clean-up data -----------------------------------------------------------------------

load("data/processed/00_data_cleanup.rda")

# Process by different levels ---------------------------------------------------------------

# generate quarter info 
cleanup <- cleanup %>% mutate(quarter = ifelse(month %in% c(12, 1, 2), 0, 
                                               ifelse(month %in% c(3, 4, 5), 1, 
                                                      ifelse(month %in% c(6, 7, 8), 2, 3))))

cleanup <- cleanup %>% dplyr::select(groupIDCounty, groupIDZip, groupID1, groupID01, groupID001, id, zip, county, state, fips, lat, lon,  date, year, month, quarter,
                                     distanceCoast, distanceRiver, distanceLake, coastInd, riverInd, lakeInd, otherInd, iccd,
                                     people, miles, totalItems, itemsPP, itemsPPPM, plasticGroceryBag, plasticBevBottle, 
                                     paperBags, 
                                     percPlasticBag, percOtherBag, percBag,
                                     percPlasticBottle, percPlasticBottleCap, percPlasticStraws, percPlasticFoodCont,
                                     percPaperBag, plasticBagPP, plasticBagPPPM, plasticBottlePP, plasticBottlePPPM, 
                                     paperBagPP, paperBagPPPM)

# County  -----------------------------------------------------------------------

## All, Month ---- 
cleanupCountyMonth <- cleanup %>% group_by(county, year, month) %>% summarise(state = first(state),
                                                                                 county = first(county), 
                                                                                 people = mean(people, na.rm=TRUE), 
                                                                                 miles = mean(miles, na.rm=TRUE), 
                                                                                 totalItems = mean(totalItems, na.rm=TRUE), 
                                                                                 itemsPP = mean(itemsPP, na.rm=TRUE), 
                                                                                 itemsPPPM = mean(itemsPPPM, na.rm=TRUE), 
                                                                                 percPlasticBag = mean(percPlasticBag, na.rm=TRUE),                                                                                  
                                                                                 percOtherBag = mean(percOtherBag, na.rm=TRUE), 
                                                                                 percBag = mean(percBag, na.rm=TRUE),
                                                                                 percPlasticBottle = mean(percPlasticBottle, na.rm=TRUE), 
                                                                                 percPlasticBottleCap = mean(percPlasticBottleCap, na.rm=TRUE),
                                                                                 percPlasticStraws = mean(percPlasticStraws, na.rm=TRUE),
                                                                                 percPlasticFoodCont = mean(percPlasticFoodCont, na.rm=TRUE),
                                                                                 percPaperBag = mean(percPaperBag, na.rm=TRUE), 
                                                                                 plasticBagPP = mean(plasticBagPP, na.rm=TRUE), 
                                                                                 plasticBagPPPM = mean(plasticBagPPPM, na.rm=TRUE), 
                                                                                 plasticBottlePP = mean(plasticBottlePP, na.rm=TRUE), 
                                                                                 plasticBottlePPPM = mean(plasticBottlePPPM, na.rm=TRUE), 
                                                                                 paperBagPP = mean(paperBagPP, na.rm=TRUE), 
                                                                                 paperBagPPPM = mean(paperBagPPPM, na.rm=TRUE))
cleanupCountyMonth <- cleanupCountyMonth %>% ungroup() %>% mutate(people = ifelse(is.nan(people), NA, people), 
                                                                    miles = ifelse(is.nan(miles), NA, miles),
                                                                    totalItems = ifelse(is.nan(totalItems), NA, totalItems),
                                                                    itemsPP = ifelse(is.nan(itemsPP), NA, itemsPP),
                                                                    itemsPPPM = ifelse(is.nan(itemsPPPM), NA, itemsPPPM),
                                                                    percPlasticBag = ifelse(is.nan(percPlasticBag), NA, percPlasticBag),
                                                                    percOtherBag = ifelse(is.nan(percOtherBag), NA, percOtherBag),
                                                                    percBag = ifelse(is.nan(percBag), NA, percBag),
                                                                    percPlasticBottle = ifelse(is.nan(percPlasticBottle), NA, percPlasticBottle),
                                                                    percPlasticBottleCap = ifelse(is.nan(percPlasticBottleCap), NA, percPlasticBottleCap),
                                                                    percPlasticStraws = ifelse(is.nan(percPlasticStraws), NA, percPlasticStraws),
                                                                    percPlasticFoodCont = ifelse(is.nan(percPlasticFoodCont), NA, percPlasticFoodCont),
                                                                    percPaperBag = ifelse(is.nan(percPaperBag), NA, percPaperBag),
                                                                    plasticBagPP = ifelse(is.nan(plasticBagPP), NA, plasticBagPP),
                                                                    plasticBagPPPM = ifelse(is.nan(plasticBagPPPM), NA, plasticBagPPPM),
                                                                    plasticBottlePP = ifelse(is.nan(plasticBottlePP), NA, plasticBottlePP),
                                                                    plasticBottlePPPM = ifelse(is.nan(plasticBottlePPPM), NA, plasticBottlePPPM),
                                                                    paperBagPP = ifelse(is.nan(paperBagPP), NA, paperBagPP),
                                                                    paperBagPPPM = ifelse(is.nan(paperBagPPPM), NA, paperBagPPPM))

# Zip Code -------------------------------------------------------------------------------

## All, Month ---- 
cleanupZipMonth <- cleanup %>% group_by(zip, year, month) %>% summarise(county = first(county), 
                                                                        people = mean(people, na.rm=TRUE), 
                                                                        miles = mean(miles, na.rm=TRUE), 
                                                                        totalItems = mean(totalItems, na.rm=TRUE), 
                                                                        itemsPP = mean(itemsPP, na.rm=TRUE), 
                                                                        itemsPPPM = mean(itemsPPPM, na.rm=TRUE), 
                                                                        percPlasticBag = mean(percPlasticBag, na.rm=TRUE), 
                                                                        percOtherBag = mean(percOtherBag, na.rm=TRUE), 
                                                                        percBag = mean(percBag, na.rm=TRUE),
                                                                        percPlasticBottle = mean(percPlasticBottle, na.rm=TRUE), 
                                                                        percPlasticBottleCap = mean(percPlasticBottleCap, na.rm=TRUE),
                                                                        percPlasticStraws = mean(percPlasticStraws, na.rm=TRUE),
                                                                        percPlasticFoodCont = mean(percPlasticFoodCont, na.rm=TRUE),
                                                                        percPaperBag = mean(percPaperBag, na.rm=TRUE), 
                                                                        plasticBagPP = mean(plasticBagPP, na.rm=TRUE), 
                                                                        plasticBagPPPM = mean(plasticBagPPPM, na.rm=TRUE), 
                                                                        plasticBottlePP = mean(plasticBottlePP, na.rm=TRUE), 
                                                                        plasticBottlePPPM = mean(plasticBottlePPPM, na.rm=TRUE), 
                                                                        paperBagPP = mean(paperBagPP, na.rm=TRUE), 
                                                                        paperBagPPPM = mean(paperBagPPPM, na.rm=TRUE))
cleanupZipMonth <- cleanupZipMonth %>% ungroup() %>% mutate(people = ifelse(is.nan(people), NA, people), 
                                                            miles = ifelse(is.nan(miles), NA, miles),
                                                            totalItems = ifelse(is.nan(totalItems), NA, totalItems),
                                                            itemsPP = ifelse(is.nan(itemsPP), NA, itemsPP),
                                                            itemsPPPM = ifelse(is.nan(itemsPPPM), NA, itemsPPPM),
                                                            percPlasticBag = ifelse(is.nan(percPlasticBag), NA, percPlasticBag),
                                                            percOtherBag = ifelse(is.nan(percOtherBag), NA, percOtherBag),
                                                            percBag = ifelse(is.nan(percBag), NA, percBag),
                                                            percPlasticBottle = ifelse(is.nan(percPlasticBottle), NA, percPlasticBottle),
                                                            percPlasticBottleCap = ifelse(is.nan(percPlasticBottleCap), NA, percPlasticBottleCap),
                                                            percPlasticStraws = ifelse(is.nan(percPlasticStraws), NA, percPlasticStraws),
                                                            percPlasticFoodCont = ifelse(is.nan(percPlasticFoodCont), NA, percPlasticFoodCont),
                                                            percPaperBag = ifelse(is.nan(percPaperBag), NA, percPaperBag),
                                                            plasticBagPP = ifelse(is.nan(plasticBagPP), NA, plasticBagPP),
                                                            plasticBagPPPM = ifelse(is.nan(plasticBagPPPM), NA, plasticBagPPPM),
                                                            plasticBottlePP = ifelse(is.nan(plasticBottlePP), NA, plasticBottlePP),
                                                            plasticBottlePPPM = ifelse(is.nan(plasticBottlePPPM), NA, plasticBottlePPPM),
                                                            paperBagPP = ifelse(is.nan(paperBagPP), NA, paperBagPP),
                                                            paperBagPPPM = ifelse(is.nan(paperBagPPPM), NA, paperBagPPPM))

## All, Quarter ---- 
cleanupZipQuarter <- cleanup %>% group_by(zip, year, quarter) %>% summarise(county = first(county), 
                                                                            people = mean(people, na.rm=TRUE), 
                                                                            miles = mean(miles, na.rm=TRUE), 
                                                                            totalItems = mean(totalItems, na.rm=TRUE), 
                                                                            itemsPP = mean(itemsPP, na.rm=TRUE), 
                                                                            itemsPPPM = mean(itemsPPPM, na.rm=TRUE), 
                                                                            percPlasticBag = mean(percPlasticBag, na.rm=TRUE), 
                                                                            percOtherBag = mean(percOtherBag, na.rm=TRUE), 
                                                                            percBag = mean(percBag, na.rm=TRUE),
                                                                            percPlasticBottle = mean(percPlasticBottle, na.rm=TRUE), 
                                                                            percPlasticBottleCap = ifelse(is.nan(percPlasticBottleCap), NA, percPlasticBottleCap),
                                                                            percPlasticStraws = ifelse(is.nan(percPlasticStraws), NA, percPlasticStraws),
                                                                            percPlasticFoodCont = ifelse(is.nan(percPlasticFoodCont), NA, percPlasticFoodCont),
                                                                            percPaperBag = mean(percPaperBag, na.rm=TRUE), 
                                                                            plasticBagPP = mean(plasticBagPP, na.rm=TRUE), 
                                                                            plasticBagPPPM = mean(plasticBagPPPM, na.rm=TRUE), 
                                                                            plasticBottlePP = mean(plasticBottlePP, na.rm=TRUE), 
                                                                            plasticBottlePPPM = mean(plasticBottlePPPM, na.rm=TRUE), 
                                                                            paperBagPP = mean(paperBagPP, na.rm=TRUE), 
                                                                            paperBagPPPM = mean(paperBagPPPM, na.rm=TRUE))
cleanupZipQuarter <- cleanupZipQuarter %>% ungroup() %>% mutate(people = ifelse(is.nan(people), NA, people), 
                                                                miles = ifelse(is.nan(miles), NA, miles),
                                                                totalItems = ifelse(is.nan(totalItems), NA, totalItems),
                                                                itemsPP = ifelse(is.nan(itemsPP), NA, itemsPP),
                                                                itemsPPPM = ifelse(is.nan(itemsPPPM), NA, itemsPPPM),
                                                                percPlasticBag = ifelse(is.nan(percPlasticBag), NA, percPlasticBag),
                                                                percOtherBag = ifelse(is.nan(percOtherBag), NA, percOtherBag),
                                                                percBag = ifelse(is.nan(percBag), NA, percBag),
                                                                percPlasticBottle = ifelse(is.nan(percPlasticBottle), NA, percPlasticBottle),
                                                                percPlasticBottleCap = ifelse(is.nan(percPlasticBottleCap), NA, percPlasticBottleCap),
                                                                percPlasticStraws = ifelse(is.nan(percPlasticStraws), NA, percPlasticStraws),
                                                                percPlasticFoodCont = ifelse(is.nan(percPlasticFoodCont), NA, percPlasticFoodCont),
                                                                percPaperBag = ifelse(is.nan(percPaperBag), NA, percPaperBag),
                                                                plasticBagPP = ifelse(is.nan(plasticBagPP), NA, plasticBagPP),
                                                                plasticBagPPPM = ifelse(is.nan(plasticBagPPPM), NA, plasticBagPPPM),
                                                                plasticBottlePP = ifelse(is.nan(plasticBottlePP), NA, plasticBottlePP),
                                                                plasticBottlePPPM = ifelse(is.nan(plasticBottlePPPM), NA, plasticBottlePPPM),
                                                                paperBagPP = ifelse(is.nan(paperBagPP), NA, paperBagPP),
                                                                paperBagPPPM = ifelse(is.nan(paperBagPPPM), NA, paperBagPPPM))

## All, Year ---- 
cleanupZipYear <- cleanup %>% group_by(zip, year) %>% summarise(county = first(county), 
                                                                people = mean(people, na.rm=TRUE), 
                                                                miles = mean(miles, na.rm=TRUE), 
                                                                totalItems = mean(totalItems, na.rm=TRUE), 
                                                                itemsPP = mean(itemsPP, na.rm=TRUE), 
                                                                itemsPPPM = mean(itemsPPPM, na.rm=TRUE), 
                                                                percPlasticBag = mean(percPlasticBag, na.rm=TRUE), 
                                                                percOtherBag = mean(percOtherBag, na.rm=TRUE), 
                                                                percBag = mean(percBag, na.rm=TRUE),
                                                                percPlasticBottle = mean(percPlasticBottle, na.rm=TRUE), 
                                                                percPlasticBottleCap = ifelse(is.nan(percPlasticBottleCap), NA, percPlasticBottleCap),
                                                                percPlasticStraws = ifelse(is.nan(percPlasticStraws), NA, percPlasticStraws),
                                                                percPlasticFoodCont = ifelse(is.nan(percPlasticFoodCont), NA, percPlasticFoodCont),
                                                                percPaperBag = mean(percPaperBag, na.rm=TRUE), 
                                                                plasticBagPP = mean(plasticBagPP, na.rm=TRUE), 
                                                                plasticBagPPPM = mean(plasticBagPPPM, na.rm=TRUE), 
                                                                plasticBottlePP = mean(plasticBottlePP, na.rm=TRUE), 
                                                                plasticBottlePPPM = mean(plasticBottlePPPM, na.rm=TRUE), 
                                                                paperBagPP = mean(paperBagPP, na.rm=TRUE), 
                                                                paperBagPPPM = mean(paperBagPPPM, na.rm=TRUE))
cleanupZipYear <- cleanupZipYear %>% ungroup() %>% mutate(people = ifelse(is.nan(people), NA, people), 
                                                          miles = ifelse(is.nan(miles), NA, miles),
                                                          totalItems = ifelse(is.nan(totalItems), NA, totalItems),
                                                          itemsPP = ifelse(is.nan(itemsPP), NA, itemsPP),
                                                          itemsPPPM = ifelse(is.nan(itemsPPPM), NA, itemsPPPM),
                                                          percPlasticBag = ifelse(is.nan(percPlasticBag), NA, percPlasticBag),
                                                          percOtherBag = ifelse(is.nan(percOtherBag), NA, percOtherBag),
                                                          percBag = ifelse(is.nan(percBag), NA, percBag),
                                                          percPlasticBottle = ifelse(is.nan(percPlasticBottle), NA, percPlasticBottle),
                                                          percPlasticBottleCap = ifelse(is.nan(percPlasticBottleCap), NA, percPlasticBottleCap),
                                                          percPlasticStraws = ifelse(is.nan(percPlasticStraws), NA, percPlasticStraws),
                                                          percPlasticFoodCont = ifelse(is.nan(percPlasticFoodCont), NA, percPlasticFoodCont),
                                                          percPaperBag = ifelse(is.nan(percPaperBag), NA, percPaperBag),
                                                          plasticBagPP = ifelse(is.nan(plasticBagPP), NA, plasticBagPP),
                                                          plasticBagPPPM = ifelse(is.nan(plasticBagPPPM), NA, plasticBagPPPM),
                                                          plasticBottlePP = ifelse(is.nan(plasticBottlePP), NA, plasticBottlePP),
                                                          plasticBottlePPPM = ifelse(is.nan(plasticBottlePPPM), NA, plasticBottlePPPM),
                                                          paperBagPP = ifelse(is.nan(paperBagPP), NA, paperBagPP),
                                                          paperBagPPPM = ifelse(is.nan(paperBagPPPM), NA, paperBagPPPM))


# Big Cell (1km)  -----------------------------------------------------------------------

## All, Month ---- 
cleanupBigCellMonth <- cleanup %>% group_by(groupID1, year, month) %>% summarise(zip = first(zip),
                                                                               county = first(county), 
                                                                               people = mean(people, na.rm=TRUE), 
                                                                               miles = mean(miles, na.rm=TRUE), 
                                                                               totalItems = mean(totalItems, na.rm=TRUE), 
                                                                               itemsPP = mean(itemsPP, na.rm=TRUE), 
                                                                               itemsPPPM = mean(itemsPPPM, na.rm=TRUE), 
                                                                               percPlasticBag = mean(percPlasticBag, na.rm=TRUE), 
                                                                               percOtherBag = mean(percOtherBag, na.rm=TRUE), 
                                                                               percBag = mean(percBag, na.rm=TRUE),
                                                                               percPlasticBottle = mean(percPlasticBottle, na.rm=TRUE), 
                                                                               percPlasticBottleCap = mean(percPlasticBottleCap, na.rm=T),
                                                                               percPlasticStraws = mean(percPlasticStraws, na.rm=T),
                                                                               percPlasticFoodCont = mean(percPlasticFoodCont, na.rm=T),
                                                                               percPaperBag = mean(percPaperBag, na.rm=TRUE), 
                                                                               plasticBagPP = mean(plasticBagPP, na.rm=TRUE), 
                                                                               plasticBagPPPM = mean(plasticBagPPPM, na.rm=TRUE), 
                                                                               plasticBottlePP = mean(plasticBottlePP, na.rm=TRUE), 
                                                                               plasticBottlePPPM = mean(plasticBottlePPPM, na.rm=TRUE), 
                                                                               paperBagPP = mean(paperBagPP, na.rm=TRUE), 
                                                                               paperBagPPPM = mean(paperBagPPPM, na.rm=TRUE))
cleanupBigCellMonth <- cleanupBigCellMonth %>% ungroup() %>% mutate(people = ifelse(is.nan(people), NA, people), 
                                                              miles = ifelse(is.nan(miles), NA, miles),
                                                              totalItems = ifelse(is.nan(totalItems), NA, totalItems),
                                                              itemsPP = ifelse(is.nan(itemsPP), NA, itemsPP),
                                                              itemsPPPM = ifelse(is.nan(itemsPPPM), NA, itemsPPPM),
                                                              percPlasticBag = ifelse(is.nan(percPlasticBag), NA, percPlasticBag),
                                                              percOtherBag = ifelse(is.nan(percOtherBag), NA, percOtherBag),
                                                              percBag = ifelse(is.nan(percBag), NA, percBag),
                                                              percPlasticBottle = ifelse(is.nan(percPlasticBottle), NA, percPlasticBottle),
                                                              percPlasticBottleCap = ifelse(is.nan(percPlasticBottleCap), NA, percPlasticBottleCap),
                                                              percPlasticStraws = ifelse(is.nan(percPlasticStraws), NA, percPlasticStraws),
                                                              percPlasticFoodCont = ifelse(is.nan(percPlasticFoodCont), NA, percPlasticFoodCont),
                                                              percPaperBag = ifelse(is.nan(percPaperBag), NA, percPaperBag),
                                                              plasticBagPP = ifelse(is.nan(plasticBagPP), NA, plasticBagPP),
                                                              plasticBagPPPM = ifelse(is.nan(plasticBagPPPM), NA, plasticBagPPPM),
                                                              plasticBottlePP = ifelse(is.nan(plasticBottlePP), NA, plasticBottlePP),
                                                              plasticBottlePPPM = ifelse(is.nan(plasticBottlePPPM), NA, plasticBottlePPPM),
                                                              paperBagPP = ifelse(is.nan(paperBagPP), NA, paperBagPP),
                                                              paperBagPPPM = ifelse(is.nan(paperBagPPPM), NA, paperBagPPPM))

## All, Year ---- 
cleanupBigCellYear <- cleanup %>% group_by(groupID1, year) %>% summarise(zip = first(zip),
                                                                         county = first(county), 
                                                                         people = mean(people, na.rm=TRUE), 
                                                                         miles = mean(miles, na.rm=TRUE), 
                                                                         totalItems = mean(totalItems, na.rm=TRUE), 
                                                                         itemsPP = mean(itemsPP, na.rm=TRUE), 
                                                                         itemsPPPM = mean(itemsPPPM, na.rm=TRUE), 
                                                                         percPlasticBag = mean(percPlasticBag, na.rm=TRUE), 
                                                                         percOtherBag = mean(percOtherBag, na.rm=TRUE), 
                                                                         percBag = mean(percBag, na.rm=TRUE),
                                                                         percPlasticBottle = mean(percPlasticBottle, na.rm=TRUE), 
                                                                         percPlasticBottleCap = mean(percPlasticBottleCap, na.rm=T),
                                                                         percPlasticStraws = mean(percPlasticStraws, na.rm=T),
                                                                         percPlasticFoodCont = mean(percPlasticFoodCont, na.rm=T),
                                                                         percPaperBag = mean(percPaperBag, na.rm=TRUE), 
                                                                         plasticBagPP = mean(plasticBagPP, na.rm=TRUE), 
                                                                         plasticBagPPPM = mean(plasticBagPPPM, na.rm=TRUE), 
                                                                         plasticBottlePP = mean(plasticBottlePP, na.rm=TRUE), 
                                                                         plasticBottlePPPM = mean(plasticBottlePPPM, na.rm=TRUE), 
                                                                         paperBagPP = mean(paperBagPP, na.rm=TRUE), 
                                                                         paperBagPPPM = mean(paperBagPPPM, na.rm=TRUE))
cleanupBigCellYear <- cleanupBigCellYear %>% ungroup()%>% mutate(people = ifelse(is.nan(people), NA, people), 
                                                                 miles = ifelse(is.nan(miles), NA, miles),
                                                                 totalItems = ifelse(is.nan(totalItems), NA, totalItems),
                                                                 itemsPP = ifelse(is.nan(itemsPP), NA, itemsPP),
                                                                 itemsPPPM = ifelse(is.nan(itemsPPPM), NA, itemsPPPM),
                                                                 percPlasticBag = ifelse(is.nan(percPlasticBag), NA, percPlasticBag),
                                                                 percOtherBag = ifelse(is.nan(percOtherBag), NA, percOtherBag),
                                                                 percBag = ifelse(is.nan(percBag), NA, percBag),
                                                                 percPlasticBottle = ifelse(is.nan(percPlasticBottle), NA, percPlasticBottle),
                                                                 percPlasticBottleCap = ifelse(is.nan(percPlasticBottleCap), NA, percPlasticBottleCap),
                                                                 percPlasticStraws = ifelse(is.nan(percPlasticStraws), NA, percPlasticStraws),
                                                                 percPlasticFoodCont = ifelse(is.nan(percPlasticFoodCont), NA, percPlasticFoodCont),
                                                                 percPaperBag = ifelse(is.nan(percPaperBag), NA, percPaperBag),
                                                                 plasticBagPP = ifelse(is.nan(plasticBagPP), NA, plasticBagPP),
                                                                 plasticBagPPPM = ifelse(is.nan(plasticBagPPPM), NA, plasticBagPPPM),
                                                                 plasticBottlePP = ifelse(is.nan(plasticBottlePP), NA, plasticBottlePP),
                                                                 plasticBottlePPPM = ifelse(is.nan(plasticBottlePPPM), NA, plasticBottlePPPM),
                                                                 paperBagPP = ifelse(is.nan(paperBagPP), NA, paperBagPP),
                                                                 paperBagPPPM = ifelse(is.nan(paperBagPPPM), NA, paperBagPPPM))


# Regular Cell (1km)  --------------------------------------------------------------------

## All, Month ---- 
cleanupCellMonth <- cleanup %>% group_by(groupID01, year, month) %>% summarise(zip = first(zip),
                                                                                   county = first(county), 
                                                                                   people = mean(people, na.rm=TRUE), 
                                                                                   miles = mean(miles, na.rm=TRUE), 
                                                                                   totalItems = mean(totalItems, na.rm=TRUE), 
                                                                                   itemsPP = mean(itemsPP, na.rm=TRUE), 
                                                                                   itemsPPPM = mean(itemsPPPM, na.rm=TRUE), 
                                                                                   percPlasticBag = mean(percPlasticBag, na.rm=TRUE), 
                                                                               percOtherBag = mean(percOtherBag, na.rm=TRUE), 
                                                                               percBag = mean(percBag, na.rm=TRUE),
                                                                                   percPlasticBottle = mean(percPlasticBottle, na.rm=TRUE), 
                                                                                   percPlasticBottleCap = mean(percPlasticBottleCap, na.rm=T),
                                                                                   percPlasticStraws = mean(percPlasticStraws, na.rm=T),
                                                                                   percPlasticFoodCont = mean(percPlasticFoodCont, na.rm=T),
                                                                                   percPaperBag = mean(percPaperBag, na.rm=TRUE), 
                                                                                   plasticBagPP = mean(plasticBagPP, na.rm=TRUE), 
                                                                                   plasticBagPPPM = mean(plasticBagPPPM, na.rm=TRUE), 
                                                                                   plasticBottlePP = mean(plasticBottlePP, na.rm=TRUE), 
                                                                                   plasticBottlePPPM = mean(plasticBottlePPPM, na.rm=TRUE), 
                                                                                   paperBagPP = mean(paperBagPP, na.rm=TRUE), 
                                                                                   paperBagPPPM = mean(paperBagPPPM, na.rm=TRUE))
cleanupCellMonth <- cleanupCellMonth %>% ungroup() %>% mutate(people = ifelse(is.nan(people), NA, people), 
                                                                  miles = ifelse(is.nan(miles), NA, miles),
                                                                  totalItems = ifelse(is.nan(totalItems), NA, totalItems),
                                                                  itemsPP = ifelse(is.nan(itemsPP), NA, itemsPP),
                                                                  itemsPPPM = ifelse(is.nan(itemsPPPM), NA, itemsPPPM),
                                                                  percPlasticBag = ifelse(is.nan(percPlasticBag), NA, percPlasticBag),
                                                              percOtherBag = ifelse(is.nan(percOtherBag), NA, percOtherBag),
                                                              percBag = ifelse(is.nan(percBag), NA, percBag),
                                                                  percPlasticBottle = ifelse(is.nan(percPlasticBottle), NA, percPlasticBottle),
                                                                  percPlasticBottleCap = ifelse(is.nan(percPlasticBottleCap), NA, percPlasticBottleCap),
                                                                  percPlasticStraws = ifelse(is.nan(percPlasticStraws), NA, percPlasticStraws),
                                                                  percPlasticFoodCont = ifelse(is.nan(percPlasticFoodCont), NA, percPlasticFoodCont),
                                                                  percPaperBag = ifelse(is.nan(percPaperBag), NA, percPaperBag),
                                                                  plasticBagPP = ifelse(is.nan(plasticBagPP), NA, plasticBagPP),
                                                                  plasticBagPPPM = ifelse(is.nan(plasticBagPPPM), NA, plasticBagPPPM),
                                                                  plasticBottlePP = ifelse(is.nan(plasticBottlePP), NA, plasticBottlePP),
                                                                  plasticBottlePPPM = ifelse(is.nan(plasticBottlePPPM), NA, plasticBottlePPPM),
                                                                  paperBagPP = ifelse(is.nan(paperBagPP), NA, paperBagPP),
                                                                  paperBagPPPM = ifelse(is.nan(paperBagPPPM), NA, paperBagPPPM))

## All, Quarter ----
cleanupCellQuarter <- cleanup %>% group_by(groupID01, year, quarter) %>% summarise(zip = first(zip),
                                                                                   county = first(county), 
                                                                                   people = mean(people, na.rm=TRUE), 
                                                                                   miles = mean(miles, na.rm=TRUE), 
                                                                                   totalItems = mean(totalItems, na.rm=TRUE), 
                                                                                   itemsPP = mean(itemsPP, na.rm=TRUE), 
                                                                                   itemsPPPM = mean(itemsPPPM, na.rm=TRUE), 
                                                                                   percPlasticBag = mean(percPlasticBag, na.rm=TRUE), 
                                                                                   percOtherBag = mean(percOtherBag, na.rm=TRUE), 
                                                                                   percBag = mean(percBag, na.rm=TRUE),
                                                                                   percPlasticBottle = mean(percPlasticBottle, na.rm=TRUE), 
                                                                                   percPlasticBottleCap = mean(percPlasticBottleCap, na.rm=T),
                                                                                   percPlasticStraws = mean(percPlasticStraws, na.rm=T),
                                                                                   percPlasticFoodCont = mean(percPlasticFoodCont, na.rm=T),
                                                                                   percPaperBag = mean(percPaperBag, na.rm=TRUE), 
                                                                                   plasticBagPP = mean(plasticBagPP, na.rm=TRUE), 
                                                                                   plasticBagPPPM = mean(plasticBagPPPM, na.rm=TRUE), 
                                                                                   plasticBottlePP = mean(plasticBottlePP, na.rm=TRUE), 
                                                                                   plasticBottlePPPM = mean(plasticBottlePPPM, na.rm=TRUE), 
                                                                                   paperBagPP = mean(paperBagPP, na.rm=TRUE), 
                                                                                   paperBagPPPM = mean(paperBagPPPM, na.rm=TRUE))
cleanupCellQuarter <- cleanupCellQuarter %>% ungroup() %>% mutate(people = ifelse(is.nan(people), NA, people), 
                                                                  miles = ifelse(is.nan(miles), NA, miles),
                                                                  totalItems = ifelse(is.nan(totalItems), NA, totalItems),
                                                                  itemsPP = ifelse(is.nan(itemsPP), NA, itemsPP),
                                                                  itemsPPPM = ifelse(is.nan(itemsPPPM), NA, itemsPPPM),
                                                                  percPlasticBag = ifelse(is.nan(percPlasticBag), NA, percPlasticBag),
                                                                  percOtherBag = ifelse(is.nan(percOtherBag), NA, percOtherBag),
                                                                  percBag = ifelse(is.nan(percBag), NA, percBag),
                                                                  percPlasticBottle = ifelse(is.nan(percPlasticBottle), NA, percPlasticBottle),
                                                                  percPlasticBottleCap = ifelse(is.nan(percPlasticBottleCap), NA, percPlasticBottleCap),
                                                                  percPlasticStraws = ifelse(is.nan(percPlasticStraws), NA, percPlasticStraws),
                                                                  percPlasticFoodCont = ifelse(is.nan(percPlasticFoodCont), NA, percPlasticFoodCont),
                                                                  percPaperBag = ifelse(is.nan(percPaperBag), NA, percPaperBag),
                                                                  plasticBagPP = ifelse(is.nan(plasticBagPP), NA, plasticBagPP),
                                                                  plasticBagPPPM = ifelse(is.nan(plasticBagPPPM), NA, plasticBagPPPM),
                                                                  plasticBottlePP = ifelse(is.nan(plasticBottlePP), NA, plasticBottlePP),
                                                                  plasticBottlePPPM = ifelse(is.nan(plasticBottlePPPM), NA, plasticBottlePPPM),
                                                                  paperBagPP = ifelse(is.nan(paperBagPP), NA, paperBagPP),
                                                                  paperBagPPPM = ifelse(is.nan(paperBagPPPM), NA, paperBagPPPM))

## All, Year ---- 
cleanupCellYear <- cleanup %>% group_by(groupID01, year) %>% summarise(zip = first(zip),
                                                                       county = first(county), 
                                                                       people = mean(people, na.rm=TRUE), 
                                                                       miles = mean(miles, na.rm=TRUE), 
                                                                       totalItems = mean(totalItems, na.rm=TRUE), 
                                                                       itemsPP = mean(itemsPP, na.rm=TRUE), 
                                                                       itemsPPPM = mean(itemsPPPM, na.rm=TRUE), 
                                                                       percPlasticBag = mean(percPlasticBag, na.rm=TRUE), 
                                                                       percOtherBag = mean(percOtherBag, na.rm=TRUE), 
                                                                       percBag = mean(percBag, na.rm=TRUE),
                                                                       percPlasticBottle = mean(percPlasticBottle, na.rm=TRUE), 
                                                                       percPlasticBottleCap = mean(percPlasticBottleCap, na.rm=T),
                                                                       percPlasticStraws = mean(percPlasticStraws, na.rm=T),
                                                                       percPlasticFoodCont = mean(percPlasticFoodCont, na.rm=T),
                                                                       percPaperBag = mean(percPaperBag, na.rm=TRUE), 
                                                                       plasticBagPP = mean(plasticBagPP, na.rm=TRUE), 
                                                                       plasticBagPPPM = mean(plasticBagPPPM, na.rm=TRUE), 
                                                                       plasticBottlePP = mean(plasticBottlePP, na.rm=TRUE), 
                                                                       plasticBottlePPPM = mean(plasticBottlePPPM, na.rm=TRUE), 
                                                                       paperBagPP = mean(paperBagPP, na.rm=TRUE), 
                                                                       paperBagPPPM = mean(paperBagPPPM, na.rm=TRUE))
cleanupCellYear <- cleanupCellYear %>% ungroup() %>% mutate(people = ifelse(is.nan(people), NA, people), 
                                                            miles = ifelse(is.nan(miles), NA, miles),
                                                            totalItems = ifelse(is.nan(totalItems), NA, totalItems),
                                                            itemsPP = ifelse(is.nan(itemsPP), NA, itemsPP),
                                                            itemsPPPM = ifelse(is.nan(itemsPPPM), NA, itemsPPPM),
                                                            percPlasticBag = ifelse(is.nan(percPlasticBag), NA, percPlasticBag),
                                                            percOtherBag = ifelse(is.nan(percOtherBag), NA, percOtherBag),
                                                            percBag = ifelse(is.nan(percBag), NA, percBag),
                                                            percPlasticBottle = ifelse(is.nan(percPlasticBottle), NA, percPlasticBottle),
                                                            percPlasticBottleCap = ifelse(is.nan(percPlasticBottleCap), NA, percPlasticBottleCap),
                                                            percPlasticStraws = ifelse(is.nan(percPlasticStraws), NA, percPlasticStraws),
                                                            percPlasticFoodCont = ifelse(is.nan(percPlasticFoodCont), NA, percPlasticFoodCont),
                                                            percPaperBag = ifelse(is.nan(percPaperBag), NA, percPaperBag),
                                                            plasticBagPP = ifelse(is.nan(plasticBagPP), NA, plasticBagPP),
                                                            plasticBagPPPM = ifelse(is.nan(plasticBagPPPM), NA, plasticBagPPPM),
                                                            plasticBottlePP = ifelse(is.nan(plasticBottlePP), NA, plasticBottlePP),
                                                            plasticBottlePPPM = ifelse(is.nan(plasticBottlePPPM), NA, plasticBottlePPPM),
                                                            paperBagPP = ifelse(is.nan(paperBagPP), NA, paperBagPP),
                                                            paperBagPPPM = ifelse(is.nan(paperBagPPPM), NA, paperBagPPPM))

## Coast, Month ---- 
cleanupCellMonthCoast <- cleanup  %>% filter(coastInd == 1) %>% group_by(groupID01, year, month) %>% summarise(zip = first(zip),
                                                                                                               county = first(county), 
                                                                                                               people = mean(people, na.rm=TRUE), 
                                                                                                               miles = mean(miles, na.rm=TRUE), 
                                                                                                               totalItems = mean(totalItems, na.rm=TRUE), 
                                                                                                               itemsPP = mean(itemsPP, na.rm=TRUE), 
                                                                                                               itemsPPPM = mean(itemsPPPM, na.rm=TRUE), 
                                                                                                               percPlasticBag = mean(percPlasticBag, na.rm=TRUE), 
                                                                                                               percOtherBag = mean(percOtherBag, na.rm=TRUE), 
                                                                                                               percBag = mean(percBag, na.rm=TRUE),
                                                                                                               percPlasticBottle = mean(percPlasticBottle, na.rm=TRUE), 
                                                                                                               percPlasticBottleCap = mean(percPlasticBottleCap, na.rm=T),
                                                                                                               percPlasticStraws = mean(percPlasticStraws, na.rm=T),
                                                                                                               percPlasticFoodCont = mean(percPlasticFoodCont, na.rm=T),
                                                                                                               percPaperBag = mean(percPaperBag, na.rm=TRUE), 
                                                                                                               plasticBagPP = mean(plasticBagPP, na.rm=TRUE), 
                                                                                                               plasticBagPPPM = mean(plasticBagPPPM, na.rm=TRUE), 
                                                                                                               plasticBottlePP = mean(plasticBottlePP, na.rm=TRUE), 
                                                                                                               plasticBottlePPPM = mean(plasticBottlePPPM, na.rm=TRUE), 
                                                                                                               paperBagPP = mean(paperBagPP, na.rm=TRUE), 
                                                                                                               paperBagPPPM = mean(paperBagPPPM, na.rm=TRUE))
cleanupCellMonthCoast <- cleanupCellMonthCoast %>% ungroup() %>% mutate(people = ifelse(is.nan(people), NA, people), 
                                                                        miles = ifelse(is.nan(miles), NA, miles),
                                                                        totalItems = ifelse(is.nan(totalItems), NA, totalItems),
                                                                        itemsPP = ifelse(is.nan(itemsPP), NA, itemsPP),
                                                                        itemsPPPM = ifelse(is.nan(itemsPPPM), NA, itemsPPPM),
                                                                        percPlasticBag = ifelse(is.nan(percPlasticBag), NA, percPlasticBag),
                                                                        percOtherBag = ifelse(is.nan(percOtherBag), NA, percOtherBag),
                                                                        percBag = ifelse(is.nan(percBag), NA, percBag),
                                                                        percPlasticBottle = ifelse(is.nan(percPlasticBottle), NA, percPlasticBottle),
                                                                        percPlasticBottleCap = ifelse(is.nan(percPlasticBottleCap), NA, percPlasticBottleCap),
                                                                        percPlasticStraws = ifelse(is.nan(percPlasticStraws), NA, percPlasticStraws),
                                                                        percPlasticFoodCont = ifelse(is.nan(percPlasticFoodCont), NA, percPlasticFoodCont),
                                                                        percPaperBag = ifelse(is.nan(percPaperBag), NA, percPaperBag),
                                                                        plasticBagPP = ifelse(is.nan(plasticBagPP), NA, plasticBagPP),
                                                                        plasticBagPPPM = ifelse(is.nan(plasticBagPPPM), NA, plasticBagPPPM),
                                                                        plasticBottlePP = ifelse(is.nan(plasticBottlePP), NA, plasticBottlePP),
                                                                        plasticBottlePPPM = ifelse(is.nan(plasticBottlePPPM), NA, plasticBottlePPPM),
                                                                        paperBagPP = ifelse(is.nan(paperBagPP), NA, paperBagPP),
                                                                        paperBagPPPM = ifelse(is.nan(paperBagPPPM), NA, paperBagPPPM))

## River, Month ---- 
cleanupCellMonthRiver <- cleanup  %>% filter(riverInd == 1) %>% group_by(groupID01, year, month) %>% summarise(zip = first(zip),
                                                                                                               county = first(county), 
                                                                                                               people = mean(people, na.rm=TRUE), 
                                                                                                               miles = mean(miles, na.rm=TRUE), 
                                                                                                               totalItems = mean(totalItems, na.rm=TRUE), 
                                                                                                               itemsPP = mean(itemsPP, na.rm=TRUE), 
                                                                                                               itemsPPPM = mean(itemsPPPM, na.rm=TRUE), 
                                                                                                               percPlasticBag = mean(percPlasticBag, na.rm=TRUE), 
                                                                                                               percOtherBag = mean(percOtherBag, na.rm=TRUE), 
                                                                                                               percBag = mean(percBag, na.rm=TRUE),
                                                                                                               percPlasticBottle = mean(percPlasticBottle, na.rm=TRUE), 
                                                                                                               percPlasticBottleCap = mean(percPlasticBottleCap, na.rm=T),
                                                                                                               percPlasticStraws = mean(percPlasticStraws, na.rm=T),
                                                                                                               percPlasticFoodCont = mean(percPlasticFoodCont, na.rm=T),
                                                                                                               percPaperBag = mean(percPaperBag, na.rm=TRUE), 
                                                                                                               plasticBagPP = mean(plasticBagPP, na.rm=TRUE), 
                                                                                                               plasticBagPPPM = mean(plasticBagPPPM, na.rm=TRUE), 
                                                                                                               plasticBottlePP = mean(plasticBottlePP, na.rm=TRUE), 
                                                                                                               plasticBottlePPPM = mean(plasticBottlePPPM, na.rm=TRUE), 
                                                                                                               paperBagPP = mean(paperBagPP, na.rm=TRUE), 
                                                                                                               paperBagPPPM = mean(paperBagPPPM, na.rm=TRUE))
cleanupCellMonthRiver <- cleanupCellMonthRiver %>% ungroup() %>% mutate(people = ifelse(is.nan(people), NA, people), 
                                                                        miles = ifelse(is.nan(miles), NA, miles),
                                                                        totalItems = ifelse(is.nan(totalItems), NA, totalItems),
                                                                        itemsPP = ifelse(is.nan(itemsPP), NA, itemsPP),
                                                                        itemsPPPM = ifelse(is.nan(itemsPPPM), NA, itemsPPPM),
                                                                        percPlasticBag = ifelse(is.nan(percPlasticBag), NA, percPlasticBag),
                                                                        percOtherBag = ifelse(is.nan(percOtherBag), NA, percOtherBag),
                                                                        percBag = ifelse(is.nan(percBag), NA, percBag),
                                                                        percPlasticBottle = ifelse(is.nan(percPlasticBottle), NA, percPlasticBottle),
                                                                        percPlasticBottleCap = ifelse(is.nan(percPlasticBottleCap), NA, percPlasticBottleCap),
                                                                        percPlasticStraws = ifelse(is.nan(percPlasticStraws), NA, percPlasticStraws),
                                                                        percPlasticFoodCont = ifelse(is.nan(percPlasticFoodCont), NA, percPlasticFoodCont),
                                                                        percPaperBag = ifelse(is.nan(percPaperBag), NA, percPaperBag),
                                                                        plasticBagPP = ifelse(is.nan(plasticBagPP), NA, plasticBagPP),
                                                                        plasticBagPPPM = ifelse(is.nan(plasticBagPPPM), NA, plasticBagPPPM),
                                                                        plasticBottlePP = ifelse(is.nan(plasticBottlePP), NA, plasticBottlePP),
                                                                        plasticBottlePPPM = ifelse(is.nan(plasticBottlePPPM), NA, plasticBottlePPPM),
                                                                        paperBagPP = ifelse(is.nan(paperBagPP), NA, paperBagPP),
                                                                        paperBagPPPM = ifelse(is.nan(paperBagPPPM), NA, paperBagPPPM))

## Lake, Month ---- 
cleanupCellMonthLake<- cleanup  %>% filter(lakeInd == 1) %>% group_by(groupID01, year, month) %>% summarise(zip = first(zip),
                                                                                                            county = first(county), 
                                                                                                            people = mean(people, na.rm=TRUE), 
                                                                                                            miles = mean(miles, na.rm=TRUE), 
                                                                                                            totalItems = mean(totalItems, na.rm=TRUE), 
                                                                                                            itemsPP = mean(itemsPP, na.rm=TRUE), 
                                                                                                            itemsPPPM = mean(itemsPPPM, na.rm=TRUE), 
                                                                                                            percPlasticBag = mean(percPlasticBag, na.rm=TRUE), 
                                                                                                            percOtherBag = mean(percOtherBag, na.rm=TRUE), 
                                                                                                            percBag = mean(percBag, na.rm=TRUE),
                                                                                                            percPlasticBottle = mean(percPlasticBottle, na.rm=TRUE), 
                                                                                                            percPlasticBottleCap = mean(percPlasticBottleCap, na.rm=T),
                                                                                                            percPlasticStraws = mean(percPlasticStraws, na.rm=T),
                                                                                                            percPlasticFoodCont = mean(percPlasticFoodCont, na.rm=T),
                                                                                                            percPaperBag = mean(percPaperBag, na.rm=TRUE), 
                                                                                                            plasticBagPP = mean(plasticBagPP, na.rm=TRUE), 
                                                                                                            plasticBagPPPM = mean(plasticBagPPPM, na.rm=TRUE), 
                                                                                                            plasticBottlePP = mean(plasticBottlePP, na.rm=TRUE), 
                                                                                                            plasticBottlePPPM = mean(plasticBottlePPPM, na.rm=TRUE), 
                                                                                                            paperBagPP = mean(paperBagPP, na.rm=TRUE), 
                                                                                                            paperBagPPPM = mean(paperBagPPPM, na.rm=TRUE))
cleanupCellMonthLake <- cleanupCellMonthLake %>% ungroup() %>% mutate(people = ifelse(is.nan(people), NA, people), 
                                                                      miles = ifelse(is.nan(miles), NA, miles),
                                                                      totalItems = ifelse(is.nan(totalItems), NA, totalItems),
                                                                      itemsPP = ifelse(is.nan(itemsPP), NA, itemsPP),
                                                                      itemsPPPM = ifelse(is.nan(itemsPPPM), NA, itemsPPPM),
                                                                      percPlasticBag = ifelse(is.nan(percPlasticBag), NA, percPlasticBag),
                                                                      percOtherBag = ifelse(is.nan(percOtherBag), NA, percOtherBag),
                                                                      percBag = ifelse(is.nan(percBag), NA, percBag),
                                                                      percPlasticBottle = ifelse(is.nan(percPlasticBottle), NA, percPlasticBottle),
                                                                      percPlasticBottleCap = ifelse(is.nan(percPlasticBottleCap), NA, percPlasticBottleCap),
                                                                      percPlasticStraws = ifelse(is.nan(percPlasticStraws), NA, percPlasticStraws),
                                                                      percPlasticFoodCont = ifelse(is.nan(percPlasticFoodCont), NA, percPlasticFoodCont),
                                                                      percPaperBag = ifelse(is.nan(percPaperBag), NA, percPaperBag),
                                                                      plasticBagPP = ifelse(is.nan(plasticBagPP), NA, plasticBagPP),
                                                                      plasticBagPPPM = ifelse(is.nan(plasticBagPPPM), NA, plasticBagPPPM),
                                                                      plasticBottlePP = ifelse(is.nan(plasticBottlePP), NA, plasticBottlePP),
                                                                      plasticBottlePPPM = ifelse(is.nan(plasticBottlePPPM), NA, plasticBottlePPPM),
                                                                      paperBagPP = ifelse(is.nan(paperBagPP), NA, paperBagPP),
                                                                      paperBagPPPM = ifelse(is.nan(paperBagPPPM), NA, paperBagPPPM))

## Other, Month ---- 
cleanupCellMonthOther <- cleanup  %>% filter(otherInd == 1) %>% group_by(groupID01, year, month) %>% summarise(zip = first(zip),
                                                                                                               county = first(county), 
                                                                                                               people = mean(people, na.rm=TRUE), 
                                                                                                               miles = mean(miles, na.rm=TRUE), 
                                                                                                               totalItems = mean(totalItems, na.rm=TRUE), 
                                                                                                               itemsPP = mean(itemsPP, na.rm=TRUE), 
                                                                                                               itemsPPPM = mean(itemsPPPM, na.rm=TRUE), 
                                                                                                               percPlasticBag = mean(percPlasticBag, na.rm=TRUE), 
                                                                                                               percOtherBag = mean(percOtherBag, na.rm=TRUE), 
                                                                                                               percBag = mean(percBag, na.rm=TRUE),
                                                                                                               percPlasticBottle = mean(percPlasticBottle, na.rm=TRUE), 
                                                                                                               percPlasticBottleCap = mean(percPlasticBottleCap, na.rm=T),
                                                                                                               percPlasticStraws = mean(percPlasticStraws, na.rm=T),
                                                                                                               percPlasticFoodCont = mean(percPlasticFoodCont, na.rm=T),
                                                                                                               percPaperBag = mean(percPaperBag, na.rm=TRUE), 
                                                                                                               plasticBagPP = mean(plasticBagPP, na.rm=TRUE), 
                                                                                                               plasticBagPPPM = mean(plasticBagPPPM, na.rm=TRUE), 
                                                                                                               plasticBottlePP = mean(plasticBottlePP, na.rm=TRUE), 
                                                                                                               plasticBottlePPPM = mean(plasticBottlePPPM, na.rm=TRUE), 
                                                                                                               paperBagPP = mean(paperBagPP, na.rm=TRUE), 
                                                                                                               paperBagPPPM = mean(paperBagPPPM, na.rm=TRUE))
cleanupCellMonthOther <- cleanupCellMonthOther %>% ungroup() %>% mutate(people = ifelse(is.nan(people), NA, people), 
                                                                        miles = ifelse(is.nan(miles), NA, miles),
                                                                        totalItems = ifelse(is.nan(totalItems), NA, totalItems),
                                                                        itemsPP = ifelse(is.nan(itemsPP), NA, itemsPP),
                                                                        itemsPPPM = ifelse(is.nan(itemsPPPM), NA, itemsPPPM),
                                                                        percPlasticBag = ifelse(is.nan(percPlasticBag), NA, percPlasticBag),
                                                                        percOtherBag = ifelse(is.nan(percOtherBag), NA, percOtherBag),
                                                                        percBag = ifelse(is.nan(percBag), NA, percBag),
                                                                        percPlasticBottle = ifelse(is.nan(percPlasticBottle), NA, percPlasticBottle),
                                                                        percPlasticBottleCap = ifelse(is.nan(percPlasticBottleCap), NA, percPlasticBottleCap),
                                                                        percPlasticStraws = ifelse(is.nan(percPlasticStraws), NA, percPlasticStraws),
                                                                        percPlasticFoodCont = ifelse(is.nan(percPlasticFoodCont), NA, percPlasticFoodCont),
                                                                        percPaperBag = ifelse(is.nan(percPaperBag), NA, percPaperBag),
                                                                        plasticBagPP = ifelse(is.nan(plasticBagPP), NA, plasticBagPP),
                                                                        plasticBagPPPM = ifelse(is.nan(plasticBagPPPM), NA, plasticBagPPPM),
                                                                        plasticBottlePP = ifelse(is.nan(plasticBottlePP), NA, plasticBottlePP),
                                                                        plasticBottlePPPM = ifelse(is.nan(plasticBottlePPPM), NA, plasticBottlePPPM),
                                                                        paperBagPP = ifelse(is.nan(paperBagPP), NA, paperBagPP),
                                                                        paperBagPPPM = ifelse(is.nan(paperBagPPPM), NA, paperBagPPPM))




# Small Cell (1km)  ----------------------------------------------------------------------

## All, Month ---- 
cleanupSmallCellMonth <- cleanup %>% group_by(groupID001, year, month) %>% summarise(zip = first(zip),
                                                                                 county = first(county), 
                                                                                 people = mean(people, na.rm=TRUE), 
                                                                                 miles = mean(miles, na.rm=TRUE), 
                                                                                 totalItems = mean(totalItems, na.rm=TRUE), 
                                                                                 itemsPP = mean(itemsPP, na.rm=TRUE), 
                                                                                 itemsPPPM = mean(itemsPPPM, na.rm=TRUE), 
                                                                                 percPlasticBag = mean(percPlasticBag, na.rm=TRUE), 
                                                                                 percOtherBag = mean(percOtherBag, na.rm=TRUE), 
                                                                                 percBag = mean(percBag, na.rm=TRUE),
                                                                                 percPlasticBottle = mean(percPlasticBottle, na.rm=TRUE), 
                                                                                 percPlasticBottleCap = mean(percPlasticBottleCap, na.rm=T),
                                                                                 percPlasticStraws = mean(percPlasticStraws, na.rm=T),
                                                                                 percPlasticFoodCont = mean(percPlasticFoodCont, na.rm=T),
                                                                                 percPaperBag = mean(percPaperBag, na.rm=TRUE), 
                                                                                 plasticBagPP = mean(plasticBagPP, na.rm=TRUE), 
                                                                                 plasticBagPPPM = mean(plasticBagPPPM, na.rm=TRUE), 
                                                                                 plasticBottlePP = mean(plasticBottlePP, na.rm=TRUE), 
                                                                                 plasticBottlePPPM = mean(plasticBottlePPPM, na.rm=TRUE), 
                                                                                 paperBagPP = mean(paperBagPP, na.rm=TRUE), 
                                                                                 paperBagPPPM = mean(paperBagPPPM, na.rm=TRUE))
cleanupSmallCellMonth <- cleanupSmallCellMonth %>% ungroup() %>% mutate(people = ifelse(is.nan(people), NA, people), 
                                                                    miles = ifelse(is.nan(miles), NA, miles),
                                                                    totalItems = ifelse(is.nan(totalItems), NA, totalItems),
                                                                    itemsPP = ifelse(is.nan(itemsPP), NA, itemsPP),
                                                                    itemsPPPM = ifelse(is.nan(itemsPPPM), NA, itemsPPPM),
                                                                    percPlasticBag = ifelse(is.nan(percPlasticBag), NA, percPlasticBag),
                                                                    percOtherBag = ifelse(is.nan(percOtherBag), NA, percOtherBag),
                                                                    percBag = ifelse(is.nan(percBag), NA, percBag),
                                                                    percPlasticBottle = ifelse(is.nan(percPlasticBottle), NA, percPlasticBottle),
                                                                    percPlasticBottleCap = ifelse(is.nan(percPlasticBottleCap), NA, percPlasticBottleCap),
                                                                    percPlasticStraws = ifelse(is.nan(percPlasticStraws), NA, percPlasticStraws),
                                                                    percPlasticFoodCont = ifelse(is.nan(percPlasticFoodCont), NA, percPlasticFoodCont),
                                                                    percPaperBag = ifelse(is.nan(percPaperBag), NA, percPaperBag),
                                                                    plasticBagPP = ifelse(is.nan(plasticBagPP), NA, plasticBagPP),
                                                                    plasticBagPPPM = ifelse(is.nan(plasticBagPPPM), NA, plasticBagPPPM),
                                                                    plasticBottlePP = ifelse(is.nan(plasticBottlePP), NA, plasticBottlePP),
                                                                    plasticBottlePPPM = ifelse(is.nan(plasticBottlePPPM), NA, plasticBottlePPPM),
                                                                    paperBagPP = ifelse(is.nan(paperBagPP), NA, paperBagPP),
                                                                    paperBagPPPM = ifelse(is.nan(paperBagPPPM), NA, paperBagPPPM))


# Save -------------------------------------------------------------------------------

# county files 
save(cleanupCountyMonth, file="data/processed/00_data_intermediate/00_data_county_month.rda")

# zip files 
save(cleanupZipMonth, file="data/processed/00_data_intermediate/00_data_zip_month.rda")
save(cleanupZipQuarter, file="data/processed/00_data_intermediate/00_data_zip_quarter.rda")
save(cleanupZipYear, file="data/processed/00_data_intermediate/00_data_zip_year.rda")

# big cell files 
save(cleanupBigCellMonth, file="data/processed/00_data_intermediate/00_data_bigcell_month.rda")
save(cleanupBigCellYear, file="data/processed/00_data_intermediate/00_data_bigcell_year.rda")

# cell files 
save(cleanupCellMonth, file="data/processed/00_data_intermediate/00_data_cell_month.rda")
save(cleanupCellMonthCoast, file="data/processed/00_data_intermediate/00_data_cell_month_coast.rda")
save(cleanupCellMonthRiver, file="data/processed/00_data_intermediate/00_data_cell_month_river.rda")
save(cleanupCellMonthLake, file="data/processed/00_data_intermediate/00_data_cell_month_lake.rda")
save(cleanupCellMonthOther, file="data/processed/00_data_intermediate/00_data_cell_month_other.rda")

save(cleanupCellQuarter, file="data/processed/00_data_intermediate/00_data_cell_quarter.rda")
save(cleanupCellYear, file="data/processed/00_data_intermediate/00_data_cell_year.rda")

# small cell files 
save(cleanupSmallCellMonth, file="data/processed/00_data_intermediate/00_data_smallcell_month.rda")


  
  
  
  
