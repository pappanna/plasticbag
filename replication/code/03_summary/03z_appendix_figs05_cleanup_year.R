#############################################################################################
# Plastic Bag Ban
# Summary stats figure, cleanups by year 
# last modified: 02/27/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, ggplot2, dplyr, readr, tidyr, broom, stargazer, stringr, lubridate, raster, sf, ggpubr, PNWColors)    

## directory 
directory <- "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/.shortcut-targets-by-id/19xFikvYAiHUYb-Az8rxT_I6rsaNv_57l/BagBan_BeachDebris/"
setwd(paste0(directory, "replication/"))

## figure parameters 
textSize <- 8

# Load data -------------------------------------------------------------------------------

# clean-up 
load("data/processed/00_data_cleanup.rda")

# Summary by Year -------------------------------------------------------------------------

cleanupByYear <- cleanup %>% mutate(ind = 1) %>% group_by(year) %>% summarise(sum = sum(ind))

# Plot ------------------------------------------------------------------------------------

textSize <- 16
ggplot(data = cleanupByYear, aes(x = year, y = sum)) + 
  geom_line(color="#332288", size = 1) + 
  geom_point(color="#332288", size = 2)+ 
  xlab("Year") + ylab("Clean-Up Count") + 
  theme_bw()+ 
  theme(plot.title = element_text(face="bold", size = rel(1), hjust = 0.5),
        axis.line = element_line(color = "darkgray"),
        axis.ticks = element_line(color = "darkgray"),
        axis.title.x = element_text(vjust= 0, size=rel(0.9)),
        axis.title.y = element_text(vjust= 1.1, size=rel(0.9)),
        axis.text.x = element_text(margin=margin(5,5,0,0,"pt")),
        axis.text.y = element_text(margin=margin(3,5,0,3,"pt")), 
        legend.position = 'bottom')+ 
  theme(text = element_text(size = textSize), 
        axis.text.x = element_text(size = textSize), 
        axis.text.y = element_text(size = textSize), 
        axis.title.x = element_text(size = textSize), 
        axis.title.y = element_text(size = textSize), 
        legend.text = element_text(size = 14), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks=element_line(size=1), 
        axis.ticks.length = unit(0.15, "cm"))
ggsave(file=paste0("figures/appendix/figure_s05_cleanups.png"), height = 5, width=8)


