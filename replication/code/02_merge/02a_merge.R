#############################################################################################
# Plastic Bag Ban
# Script for running all spatiotemporal combinations 
# last modified: 05/07/24
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


# Run all codes ---------------------------------------------------------------------------------------

# county 
source("code/02_merge/02b_merge_scripts/02_merge_county_month.R",local = knitr::knit_global())

# zip 
source("code/02_merge/02b_merge_scripts/02_merge_zip_month.R",local = knitr::knit_global())
source("code/02_merge/02b_merge_scripts/02_merge_zip_quarter.R",local = knitr::knit_global())
source("code/02_merge/02b_merge_scripts/02_merge_zip_year.R",local = knitr::knit_global())

# big cell 
source("code/02_merge/02b_merge_scripts/02_merge_bigcell_month.R",local = knitr::knit_global())
source("code/02_merge/02b_merge_scripts/02_merge_bigcell_year.R",local = knitr::knit_global())

# cell 
source("code/02_merge/02b_merge_scripts/02_merge_cell_month.R",local = knitr::knit_global())
source("code/02_merge/02b_merge_scripts/02_merge_cell_quarter.R",local = knitr::knit_global())
source("code/02_merge/02b_merge_scripts/02_merge_cell_year.R",local = knitr::knit_global())
source("code/02_merge/02b_merge_scripts/02_merge_cell_month_river.R",local = knitr::knit_global())
source("code/02_merge/02b_merge_scripts/02_merge_cell_month_lake.R",local = knitr::knit_global())
source("code/02_merge/02b_merge_scripts/02_merge_cell_month_coast.R",local = knitr::knit_global())
source("code/02_merge/02b_merge_scripts/02_merge_cell_month_other.R",local = knitr::knit_global())

# small cell 
source("code/02_merge/02b_merge_scripts/02_merge_smallcell_month.R",local = knitr::knit_global())

