#############################################################################################
# Plastic Bag Ban
# Script for appendix figures and tables 
# last modified: 05/07/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## directory 
directory <- "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/.shortcut-targets-by-id/19xFikvYAiHUYb-Az8rxT_I6rsaNv_57l/BagBan_BeachDebris/"
setwd(paste0(directory, "plasticbag/replication/"))


# Run Files for Main Figures ----------------------------------------------------------------

source("code/03_summary/03z_appendix_tabs01_cleanup_stats.R",local = knitr::knit_global())

source("code/03_summary/03z_appendix_figs02_tabs02_repeat_cleanup.R",local = knitr::knit_global())

source("code/03_summary/03z_appendix_figs03_policy_map.R",local = knitr::knit_global())

source("code/03_summary/03z_appendix_figs04_cleanup_objects_summary.R",local = knitr::knit_global())

source("code/03_summary/03z_appendix_figs05_cleanup_year.R",local = knitr::knit_global())

source("code/04_analysis/04z_appendix_figs06_tabs04_other.R",local = knitr::knit_global())

source("code/04_analysis/04z_appendix_figs07_time_aggregation.R",local = knitr::knit_global())

source("code/04_analysis/04z_appendix_figs08_spatial_aggregation.R",local = knitr::knit_global())

source("code/04_analysis/04z_appendix_figs09_figs10_balanced.R",local = knitr::knit_global())

source("code/04_analysis/04z_appendix_figs11_pandemic.R",local = knitr::knit_global())

source("code/04_analysis/04z_appendix_figs12_cleanuplocation.R",local = knitr::knit_global())

source("code/04_analysis/04z_appendix_figs13_state_details.R",local = knitr::knit_global())