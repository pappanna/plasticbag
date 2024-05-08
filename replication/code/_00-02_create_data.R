#############################################################################################
# Plastic Bag Ban
# Script for creating data 
# last modified: 05/07/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## directory 
directory <- "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/.shortcut-targets-by-id/19xFikvYAiHUYb-Az8rxT_I6rsaNv_57l/BagBan_BeachDebris/"
setwd(paste0(directory, "plasticbag/replication/"))


# Step 0: Clean-Up Data Processing -----------------------------------------------------------

# a
source("code/00_cleanup/00a_cleanup_data_county.R",local = knitr::knit_global())

# b
source("code/00_cleanup/00b_cleanup_data_zip.R",local = knitr::knit_global())

# c
source("code/00_cleanup/00c_cleanup_data_cell.R",local = knitr::knit_global())

# d
source("code/00_cleanup/00d_cleanup_data_clean.R",local = knitr::knit_global())

# e
source("code/00_cleanup/00e_cleanup_distance.R",local = knitr::knit_global())

# f
source("code/00_cleanup/00f_cleanup_final.R",local = knitr::knit_global())

# Step 1: Policy Data Processing ------------------------------------------------------------- 

# a
source("code/01_policy/01a_policy_county_zip.R",local = knitr::knit_global())

# b
source("code/01_policy/01b_zip_characteristics.R",local = knitr::knit_global())

# c
source("code/01_policy/01c_neighbor_zip.R",local = knitr::knit_global())

# d
source("code/01_policy/01d_neighbor_zip_policy.R",local = knitr::knit_global())

# Step 2: Merge Data ------------------------------------------------------------------------- 

# a
source("code/02_merge/02a_merge.R",local = knitr::knit_global())
