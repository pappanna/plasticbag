#############################################################################################
# Plastic Bag Ban
# Linking clean up data to zip codes 
# last modified: 01/03/23 (update with 2023 data)
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

# spherical geometry 
sf_use_s2(FALSE)

# Load clean-up data -----------------------------------------------------------------------

load("data/processed/00_data_cleanup_county.rda")
states <- as.list(cleanup %>% filter(!is.na(state)) %>% distinct(state))
states <- states$state

# Load zip code data -----------------------------------------------------------------------

# zip code shapefile (Tiger 2019)
zipSf <- st_read("data/shapefiles/tl_2019_us_zcta510/tl_2019_us_zcta510.shp") 
zipSf <- zipSf %>% st_transform(4326)

# Intersection -----------------------------------------------------------------------------

df <- data.frame(matrix(ncol = 84, nrow = 0))

for(stateName in states){
  
  print(stateName)
  
  # get relevant cleanup data for state
  data <- cleanup %>% filter(state == stateName)
  dataSf <- st_as_sf(data, coords = c("lon", "lat"), crs=4326)
  
  # get relevant shapefile for state 
  zip <- zipSf 
  
  # get intersection 
  dataInt <- st_intersection(dataSf, zip)
  dataInt <- data.frame(dataInt) %>% dplyr::select(id, zip = ZCTA5CE10)
  dataInt <- dataInt %>% distinct(id, zip)
  data <- left_join(data, dataInt)
  
  # add to main dataframe 
  df <- rbind(df, data)
  
}

# reorder 
cleanup <- df %>% dplyr::select(id, zip, county, state, fips, lat, lon, date, year, month, iccd, adults, children, people, 
                                pounds, miles, bags, totalItems, 
                                plasticGroceryBag, plasticOtherBag, plasticBevBottle, plasticBottleOther, plasticBottleCap, plasticCupPlates, plasticFoodCont, plasticLids, plasticStraws, plasticUtensils, plastic6packHolders, plasticPiece, plasticOtherWaste, plasticFoamOtherPackaging, plasticFoamPiece, 
                                foamCupsPlates, foamFoodCont, foamDockPieces, foamPackaging, foamPiece,
                                glassBevBottle, glassPiece, metalCanbev, pouchBeverage, metalBottleCap, paperCupsPlates,  paperBags, 
                                foodWrappers, appliances, constructionMat, tires, ewaste, clothes, footwear, balloons, toys, fireworks, strappingBands, 
                                fishingBuoysPotsTraps, fishingNet, fishingLine, rope, fishingGear, 
                                linesRopes, tobaccoCigbutt, tobaccoCigartips, tobaccoCigLighters, tobaccoEcig, tobaccoWrap, tobaccoOtherPackaging, tobaccoOtherProducts, 
                                personalHygiene, condoms, tampons, cottonBud, diaper, syringe, ppe, nonplasticOtherWaste, otherPackaging,otherTrash)

# missing 
zipMissing <- cleanup %>% filter(is.na(zip))

# get closest zip code to missing 
zipMissingClosest <- st_join(st_as_sf(zipMissing, coords = c("lon", "lat"), crs=4326), zipSf, st_nearest_feature)
zipMissingClosest <- data.frame(zipMissingClosest) %>% dplyr::select(id, zip = ZCTA5CE10)
zipMissing <- left_join(zipMissing %>% dplyr::select(-c(zip)), zipMissingClosest)

# add back 
cleanup <- rbind(cleanup %>% filter(!is.na(zip)), zipMissing)

# reorder 
cleanup <- cleanup %>% dplyr::select(id, zip, county, state, fips, lat, lon, date, year, month, iccd, adults, children, people, 
                                pounds, miles, bags, totalItems, 
                                plasticGroceryBag, plasticOtherBag, plasticBevBottle, plasticBottleOther, plasticBottleCap, plasticCupPlates, plasticFoodCont, plasticLids, plasticStraws, plasticUtensils, plastic6packHolders, plasticPiece, plasticOtherWaste, plasticFoamOtherPackaging, plasticFoamPiece, 
                                foamCupsPlates, foamFoodCont, foamDockPieces, foamPackaging, foamPiece,
                                glassBevBottle, glassPiece, metalCanbev, pouchBeverage, metalBottleCap, paperCupsPlates,  paperBags, 
                                foodWrappers, appliances, constructionMat, tires, ewaste, clothes, footwear, balloons, toys, fireworks, strappingBands, 
                                fishingBuoysPotsTraps, fishingNet, fishingLine, rope, fishingGear, 
                                linesRopes, tobaccoCigbutt, tobaccoCigartips, tobaccoCigLighters, tobaccoEcig, tobaccoWrap, tobaccoOtherPackaging, tobaccoOtherProducts, 
                                personalHygiene, condoms, tampons, cottonBud, diaper, syringe, ppe, nonplasticOtherWaste, otherPackaging,otherTrash)
cleanup <- cleanup %>% arrange(state, county, zip, date)

# save data 
save(cleanup, file="data/processed/00_data_cleanup_county_zip.rda")


