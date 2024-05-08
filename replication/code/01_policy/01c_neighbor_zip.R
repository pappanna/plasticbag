#############################################################################################
# Plastic Bag Ban
# Linking policies to zip codes and counties 
# last modified: 06/08/23
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, ggplot2, dplyr, readr, tidyr, broom, stargazer, stringr, lubridate, raster, sf, ggpubr, spdep)    

## directory 
directory <- "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/.shortcut-targets-by-id/19xFikvYAiHUYb-Az8rxT_I6rsaNv_57l/BagBan_BeachDebris/"
setwd(paste0(directory, "plasticbag/replication/"))

## spherical geometry 
sf_use_s2(FALSE)

# Load Data ---------------------------------------------------------------------------------

# get shapefile
zipSf <- st_read("data/shapefiles/tl_2019_us_zcta510/tl_2019_us_zcta510.shp") 

# Get Neighbors for Each -------------------------------------------------------------------- 

# identify neighbours for each poly
neighbor <- setNames(poly2nb(zipSf), zipSf$ZCTA5CE10)

# convert to a binary neighbour matrix
neighborMatrix <- nb2mat(neighbor, zero.policy=TRUE, style='B')

# assign zip codes as dimension names
dimnames(neighborMatrix) <- list(zipSf$ZCTA5CE10, zipSf$ZCTA5CE10)

# check for a small subset 
neighborMatrix[1:10, 1:10]

# convert to dataframe 
neighborList <- sapply(row.names(neighborMatrix), function(x) names(which(neighborMatrix[x, ] == 1)))
neighborPairs <- data.frame(zip=rep(names(neighborList), sapply(neighborList, length)), 
                            neighbor=unlist(neighborList))

# check for a small subset 
head(neighborPairs)

# Save --------------------------------------------------------------------------------------

# save 
save(neighborPairs, file="data/processed/01_zip_neighbors_list.rda")


