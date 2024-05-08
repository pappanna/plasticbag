#############################################################################################
# Plastic Bag Ban
# More cleanup bag statistics
# last modified: 01/05/24 (2023 update)
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, ggplot2, dplyr, readr, tidyr, broom, stargazer, stringr, lubridate, raster, sf, ggpubr, PNWColors)    

## directory 
directory <- "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/.shortcut-targets-by-id/19xFikvYAiHUYb-Az8rxT_I6rsaNv_57l/BagBan_BeachDebris/"
setwd(paste0(directory, "plasticbag/replication/"))

## figure parameters 
textSize <- 16

# Load clean-up data -----------------------------------------------------------------------

load("data/processed/00_data_cleanup_county_zip_cell.rda")
cleanupAll <- cleanup 

load("data/processed/00_data_cleanup.rda")
cleanup <- cleanup %>% dplyr::select(id) %>% mutate(keep = 1)

# Merge and keep final cleanup --------------------------------------------------------------

cleanupAll <- left_join(cleanupAll, cleanup)
cleanupAll <- cleanupAll %>% filter(keep == 1) %>% dplyr::select(-c(keep))
cleanup <- cleanupAll
rm(cleanupAll)

# keep non-zero total items 
cleanup <- cleanup %>% filter(totalItems != 0)
cleanup <- cleanup %>% mutate_all(~ifelse(is.na(.), 0, .))

# Calculate shares --------------------------------------------------------------------------

# calculate the share of each item 
cleanupSummary <- cleanup %>% mutate_at(vars(plasticGroceryBag:otherTrash), ~ ./totalItems)

# calculate the averages across all cleanups 
cleanupSummary <- cleanupSummary %>% summarise_at(vars(plasticGroceryBag:otherTrash), ~ mean(.))

# check total items 
cleanupSummary <- cleanupSummary %>% mutate(totalItemsCheck = rowSums(dplyr::select(cleanupSummary, plasticGroceryBag:otherTrash)))
cleanupSummary <- cleanupSummary %>% dplyr::select(-c(totalItemsCheck))

# make long 
cleanupSummary <- cleanupSummary %>% pivot_longer(everything(), names_to = "item", values_to = "share")

# categorize 
cleanupSummary <- cleanupSummary %>% mutate(category = ifelse(item == "tobaccoCigbutt", "Cigarette Butt", 
                                                              ifelse(item == "foodWrappers", "Food Wrapper", 
                                                                     ifelse(item == "plasticBottleCap", "Plastic Bottle Cap", 
                                                                            ifelse(item == "plasticBevBottle", "Plastic Beverage Bottle", 
                                                                                   ifelse(item == "metalCanbev", "Metal Beverage Can", 
                                                                                          ifelse(item == "plasticGroceryBag", "Plastic Grocery Bag", 
                                                                                                 ifelse(item == "plasticStraws", "Plastic Straw", 
                                                                                                        ifelse(item == "glassBevBottle", "Glass Beverage Bottle", 
                                                                                                               ifelse(item %in% c("plasticPiece", "plasticFoamPiece", "plasticLids", "plasticCupPlates", "plasticFoodCont", "plasticUtensils", "plasticOtherBag", "plasticOtherWaste", "plasticFoamOtherPackaging", "plasticBottleOther", "plastic6packHolders"), "Other Plastic", 
                                                                                                                      ifelse(item %in% c("foamPiece", "foamFoodCont", "foamCupsPlates", "foamPackaging", "foamDockPieces"), "Other Foam", 
                                                                                                                             ifelse(item %in% c("fishingGear", "fishingLine", "fishingNet", "fishingBuoysPotsTraps"), "Fishing Gear", "Other"))))))))))))

cleanupSummary <- cleanupSummary %>% group_by(category) %>% summarise(share = sum(share)) %>% ungroup()

# Make a chart --------------------------------------------------------------------------

cleanupSummary <- cleanupSummary %>% arrange(desc(share))
cleanupSummary1 <- cleanupSummary %>% slice(1:2)  # Select the first two rows
cleanupSummary2 <- cleanupSummary%>% slice(-(1:2))  # Exclude the first two rows
cleanupSummary <- bind_rows(cleanupSummary2, cleanupSummary1)
rm(cleanupSummary1, cleanupSummary2)
cleanupSummary <- cleanupSummary %>% mutate(category = factor(category, levels = category))
cleanupSummary <- cleanupSummary %>% mutate(label = paste0(round(share * 100, 1), "%"))

# Create a bar plot
ggplot(cleanupSummary, aes(x = category, y = share)) +
  geom_bar(stat = "identity", fill = "#efbc82", alpha = 0.7) +
  geom_bar(data = cleanupSummary %>% filter(category == "Plastic Grocery Bag"), aes(x = category, y = share), stat = "identity", fill = "#675478", alpha = 1.0) +
  geom_bar(data = cleanupSummary %>% filter(category == "Other Plastic" | category == "Other"), aes(x = category, y = share), stat = "identity", fill = "#999999", alpha = 1.0) +
  geom_text(aes(label = label), vjust = -0.5, size = 5) + 
  labs(x = "Category", y = "Share of Total Objects Found at Cleanups") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x=element_text(size = textSize, angle=45, hjust=1), 
        axis.text.y=element_text(size = textSize), 
        axis.title.x=element_text(size = textSize), 
        axis.title.y=element_text(size = textSize, margin = margin(t = 0, r = 6, b = 0, l = 0)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks=element_line(size=1), 
        axis.ticks.length = unit(0.15, "cm"))
ggsave("figures/appendix/figure_s04_cleanup_objects.png", width=17.4)


