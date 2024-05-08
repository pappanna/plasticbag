#############################################################################################
# Plastic Bag Ban
# Demonstrating spillover counties 
# last modified: 03/27/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, ggplot2, dplyr, readr, tidyr, broom, stargazer, stringr, lubridate, raster, sf, ggpubr, PNWColors, tigris)    

## directory 
directory <- "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/.shortcut-targets-by-id/19xFikvYAiHUYb-Az8rxT_I6rsaNv_57l/BagBan_BeachDebris/"
setwd(paste0(directory, "plasticbag/replication/"))

## spherical geo 
sf_use_s2(FALSE)

## figure parameters
textSize <- 10

# Load zip code data -----------------------------------------------------------------------
# cleanup data 
load("data/processed/00_data_cleanup.rda")
cleanup <- cleanup %>% filter(state == "SC" | state == "NC")
cleanup <- cleanup %>% filter(zip != 97365) %>% filter(zip != 23666) 
cleanupCell <- cleanup %>% mutate(latFloor01 = floor(lat * 100) / 100, 
                                  lonFloor01 = floor(lon * 100) / 100)
cleanupCell <- cleanupCell %>% distinct(zip, latFloor01, lonFloor01)
cleanupCell <- cleanupCell %>% mutate(xmin = lonFloor01, xmax = lonFloor01 + 0.01, 
                                      ymin = latFloor01, ymax = latFloor01 + 0.01)

# zip code data 
load("data/processed/01_zip_policy.rda")
zipSC <- zip %>% filter(state == "SC" | state == "NC")
zipSC <- zipSC %>% distinct(zip) %>% mutate(policy = 1)
  
# zip code neighbors 
load("data/processed/01_zip_neighbors_policy.rda")
zipNeighbor <- neighborZipFinal %>% distinct(zip, neighborInd, neighborRepealedInd)
rm(neighborZipFinal)

# zip code shapefile (Tiger 2019)
zipSf <- st_read("data/shapefiles/tl_2019_us_zcta510/tl_2019_us_zcta510.shp") 
zipSf <- zipSf %>% st_transform(4326)

# rivers shapefile 
riversSf <- st_read("data/shapefiles/rivers/hydrography_l_rivers_v2.shp") 
riversSf <- riversSf %>% st_transform(4326)

# lakes shapefile 
lakesSf <- st_read("data/shapefiles/lakes/ne_10m_lakes_north_america.shp") 
lakesSf <- lakesSf %>% st_transform(4326)

# states 
states <- states(cb = TRUE, class="sf") 
states <- states %>% filter(STUSPS == "NC" | STUSPS == "SC")
states <- states %>% st_transform(4326)

# intersect 
riversC <- st_intersection(riversSf, states)
lakesC <- st_intersection(lakesSf, states)

# Carolinas Example --------------------------------------------------------------------
zipSfSC <- zipSf %>% filter(substr(ZCTA5CE10, 1, 2) == "29" | substr(ZCTA5CE10, 1, 2) == "28" | substr(ZCTA5CE10, 1, 2) == "27") 
zipSfSC <- zipSfSC %>% mutate(zip = as.integer(ZCTA5CE10))
zipSfSC <- left_join(zipSfSC, zipSC)
zipSfSC <- left_join(zipSfSC, zipNeighbor)

# color palette
pal <- pnw_palette("Sailboat", 7) 

# map for Carolinas 
ggplot() + 
  geom_sf(data = zipSfSC, color=NA, linewidth=0.05, aes(fill = "Group 5")) +  
  geom_sf(data = zipSfSC %>% filter(policy == 1),  linewidth=0.05,color=NA, aes(fill = "Group 1"))  + 
  geom_sf(data = zipSfSC %>% filter(neighborInd == 1 | neighborRepealedInd == 1), linewidth=0.05,color=NA, aes(fill = "Group 2"))+ 
  geom_sf(data = zipSfSC %>% filter(neighborInd == 2 | neighborRepealedInd == 2), linewidth=0.05,color=NA, aes(fill = "Group 3"))+ 
  geom_sf(data = zipSfSC %>% filter(neighborInd == 3 | neighborRepealedInd == 3), linewidth=0.05,color=NA, aes(fill = "Group 4"))+ 
  geom_sf(data = riversC, color="blue", linewidth=0.2, alpha=0.6)+
  geom_sf(data = lakesC, color="blue", linewidth=0.1, alpha=0.6)+
  geom_sf(data = states, color="darkgray", linewidth=0.5, fill=NA) + 
  geom_rect(data = cleanupCell, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill="black", color="black") + 
  xlab(" ") + ylab(" ") + 
  theme_void() +
  scale_fill_manual(values = c("Group 5" = pal[3], "Group 4" = pal[4], "Group 3" = pal[5], "Group 2" = pal[6], "Group 1" = pal[7]), 
                    name = "Zip Codes",
                    labels = c("Treated", "Neighbor", "N. of N.", "N. of N. of N.", "Control"))+ 
  theme(legend.text = element_text(size = textSize),
        legend.title = element_text(size = textSize), 
        legend.key.size = unit(0.5, "lines"), 
        legend.position = c(0.85, 0.175))
ggsave("figures/figure_3_parta.png", width = 8.7, height = 5.8, units = "cm")


