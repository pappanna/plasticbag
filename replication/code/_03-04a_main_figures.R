#############################################################################################
# Plastic Bag Ban
# Script for main figures 
# last modified: 05/07/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## directory 
directory <- "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/.shortcut-targets-by-id/19xFikvYAiHUYb-Az8rxT_I6rsaNv_57l/BagBan_BeachDebris/"
setwd(paste0(directory, "replication/"))


# Run Files for Main Figures ----------------------------------------------------------------

# figure 1 
source("code/03_summary/03a_figure1_data_summary.R",local = knitr::knit_global())

# figure 2 
source("code/04_analysis/04a_figure2_main_results.R",local = knitr::knit_global())

# figure 3 
source("code/03_summary/03b_figure3_spillover_demo.R",local = knitr::knit_global())
source("code/04_analysis/04b_figure3_spillover.R",local = knitr::knit_global())

# figure 4 
source("code/04_analysis/04c_figure4_policy_type.R",local = knitr::knit_global())
