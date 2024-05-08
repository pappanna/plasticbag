#############################################################################################
# Plastic Bag Ban
# Summary stats on policy and clean-up data 
# last modified: 04/14/24
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
textSize <- 10

# Load data -------------------------------------------------------------------------------

# clean-up 
load("data/processed/00_data_cleanup.rda")

# policy 
load("data/processed/01_zip_policy_characteristics.rda")
zipChars <- merged %>% dplyr::select(zip, popTotal)

# all distinct policies 
load("data/processed/01_zip_policy_distinct.rda")

# Merge data ------------------------------------------------------------------------------

merged <- left_join(cleanup, merged %>% dplyr::select(-c(state, city, county, lat, lon)))
merged <- merged %>% mutate_all(~ replace(., is.nan(.), NA))
merged <- merged %>% mutate(policyType = firstPolicyType)
merged <- merged %>% mutate(policyInEffect = ifelse(policy == 1 & date > firstEffect, 1, 0))

# Policies over time, geographic scope + type ---------------------------------------------

# by type of policy
policy <- zipPoliciesYear
rm(zipPoliciesYear)
policy <- policy %>% rename(policyGeo = policyType)
policy <- policy %>% mutate(policyType = ifelse(complete_bag_ban == 1, "ban", 
                                                ifelse(partial_bag_ban == 1, "partial ban", 
                                                    ifelse(plastic_bag_charge ==1, "tax", ""))))
policy <- policy %>% dplyr::select(-c(complete_bag_ban, partial_bag_ban, plastic_bag_charge))
policy <- policy %>% mutate(year = year(effect))
policy <- policy %>% dplyr::select(zip, year, policyGeo, policyType, popIRS)
policy <- left_join(policy, zipChars %>% mutate(zip = as.numeric(zip)))
policy <- policy %>% mutate(pop = ifelse(is.na(popTotal), popIRS, popTotal))

# create each zip code x year combination 
years <- data.frame(seq(2007, 2024, by = 1))
zipCodes <- policy %>% distinct(zip)
zipYears <- crossing(zipCodes, years)
zipYears <- zipYears %>% dplyr::select(zip = zip, year = seq.2007..2024..by...1.)
rm(years, zipCodes)

# now merge with policy data and fill down 
policy <- left_join(zipYears, policy)
rm(zipYears)
policy <- policy %>% filter(!is.na(zip))
policy <- policy %>%  group_by(zip) %>% mutate_all(~zoo::na.locf(., na.rm = FALSE)) %>% fill(everything(), .direction = "down") %>% ungroup()

# now calculate for each year, geo level, and policy type 
policyGroup <- policy %>% filter(!is.na(policyGeo)) %>% group_by(year, policyGeo, policyType) %>% summarise(popSum = sum(pop, na.rm=T)) %>% ungroup()

# create combinations of quartile / policy type and every year 
years <- data.frame(seq(2007, 2024, by = 1))
policies <- data.frame(c("town", "county", "state"))
policiesYears <- crossing(policies, years)
policiesYears <- policiesYears %>% dplyr::select(policyGeo = c..town....county....state.., yearEffect = seq.2007..2024..by...1.)
rm(policies, years)
geos <- data.frame(c("ban", "partial ban", "tax"))
policiesYearsGeos <- crossing(policiesYears, geos) %>% dplyr::select(policyGeo, year = yearEffect, policyType = c..ban....partial.ban....tax..)
rm(geos, policiesYears)

# now merge back 
policyGroup <- left_join(policiesYearsGeos, policyGroup)
policyGroup <- policyGroup %>% mutate(popSum = ifelse(year== 2007 & is.na(popSum), 0, popSum))
policyGroup <- policyGroup %>% arrange(policyGeo, policyType, year)
policyGroup <- policyGroup %>% mutate_all(~zoo::na.locf(.)) %>% tidyr::fill(everything(), .direction = "down")
rm(policiesYearsGeos)

# policy combination 
policyGroup <- policyGroup %>% mutate(policy = paste0(policyGeo, " ", policyType))
policyGroup <- policyGroup %>% mutate(policy = factor(policy, levels = c("state ban", "state partial ban", "state tax",
                                                               "county ban", "county partial ban", "county tax", 
                                                               "town ban", "town partial ban", "town tax"
                                                               )))

# geographic 
policyGeo <- policyGroup %>% group_by(policyGeo, year) %>% summarise(popSum = sum(popSum, na.rm=T)) %>% ungroup()
policyGeo <- policyGeo %>% mutate(policyGeo = factor(policyGeo, levels = c("state", "county", "town")))

# scale by millions 
policyGroup <- policyGroup %>% mutate(popSum = popSum/1000000)
policyGeo <- policyGeo %>% mutate(popSum = popSum/1000000)

# Plot ---- 

# Stacked area graph
# type of policy 
palColor <- pnw_palette("Shuksan", 3)
palFill <- c(pnw_palette("Shuksan", 12)[1], pnw_palette("Shuksan", 12)[2], pnw_palette("Shuksan", 12)[3], pnw_palette("Shuksan", 12)[5], pnw_palette("Shuksan", 12)[6], pnw_palette("Shuksan", 12)[7], pnw_palette("Shuksan", 12)[10], pnw_palette("Shuksan", 12)[11], pnw_palette("Shuksan", 12)[12])
text <- data.frame(x = c(2026, 2026, 2026, 2026, 2026, 2026, 2026, 2026, 2026), 
                   y = c(2.82, 7.65, 12.6, 
                         17.02, 20.71, 24.22, 
                         28.42, 36.87, 79.23), 
                   label = c("Taxes", "Partial Bans", "Bans", "Taxes", "Partial Bans", "Bans", "Taxes", "Partial Bans", "Bans"))
textGeo <- data.frame(x = c(2025.25, 2025.25, 2025.25), 
                      y = c(7.76, 20.54, 70.77), 
                      levels = c("town", "county", "state"),
                      label = c("Town", "County", "State"))
textGeo <- textGeo %>% mutate(levels = factor(levels, levels = c("state", "county", "town")))

plot1 <- ggplot() + 
  geom_area(data=policyGroup, aes(x=year, y=popSum, fill=factor(policy)), alpha=0.6, color = NA, size=.5)+ 
  geom_text(data = text, aes(x = x, y = y, label = label), size = 2.125, hjust=0, color=rev(c(pnw_palette("Bay", 5)[1], pnw_palette("Bay", 5)[2], pnw_palette("Bay", 5)[5], pnw_palette("Bay", 5)[1], pnw_palette("Bay", 5)[2], pnw_palette("Bay", 5)[5], pnw_palette("Bay", 5)[1], pnw_palette("Bay", 5)[2], pnw_palette("Bay", 5)[5] )))+
  geom_text(data = textGeo, aes(x = x, y = y, label = label, color=levels), size = 2.5, angle=90)+
  scale_color_manual(values=palColor) + 
  scale_fill_manual(values=palFill) + 
  geom_segment(data = data.frame(x = c(2024.5), ymin = 0, ymax = 15.3),aes(x = x, xend = x, y = ymin, yend = ymax),inherit.aes = FALSE,color = pnw_palette("Shuksan", 5)[5])+
  geom_segment(data = data.frame(y = c(0.1), xmin = 2024.25, ymax = 2024.5),aes(x = xmin, xend = ymax, y = y, yend = y),inherit.aes = FALSE,color = pnw_palette("Shuksan", 5)[5])+
  geom_segment(data = data.frame(y = c(15.2), xmin = 2024.25, ymax = 2024.5),aes(x = xmin, xend = ymax, y = y, yend = y),inherit.aes = FALSE,color = pnw_palette("Shuksan", 5)[5])+
  geom_segment(data = data.frame(x = c(2024.5), ymin = 15.7, ymax = 25.3),aes(x = x, xend = x, y = ymin, yend = ymax),inherit.aes = FALSE,color = pnw_palette("Shuksan", 5)[3])+
  geom_segment(data = data.frame(y = c(15.8), xmin = 2024.25, ymax = 2024.5),aes(x = xmin, xend = ymax, y = y, yend = y),inherit.aes = FALSE,color = pnw_palette("Shuksan", 5)[3])+
  geom_segment(data = data.frame(y = c(25.2), xmin = 2024.25, ymax = 2024.5),aes(x = xmin, xend = ymax, y = y, yend = y),inherit.aes = FALSE,color = pnw_palette("Shuksan", 5)[3])+
  geom_segment(data = data.frame(x = c(2024.5), ymin = 25.7, ymax = 116),aes(x = x, xend = x, y = ymin, yend = ymax),inherit.aes = FALSE,color = pnw_palette("Shuksan", 5)[1])+
  geom_segment(data = data.frame(y = c(25.8), xmin = 2024.25, ymax = 2024.5),aes(x = xmin, xend = ymax, y = y, yend = y),inherit.aes = FALSE,color = pnw_palette("Shuksan", 5)[1])+
  geom_segment(data = data.frame(y = c(115.9), xmin = 2024.25, ymax = 2024.5),aes(x = xmin, xend = ymax, y = y, yend = y),inherit.aes = FALSE,color = pnw_palette("Shuksan", 5)[1])+
  ylab("Cumulative Population Subject to\nPlastic Bag Policy (Millions)") + xlab("Year")+
  theme_bw() + ylim(0, 120)+ 
  scale_x_continuous(breaks = c(2008, 2012, 2016, 2020, 2024), lim=c(2008, 2029))+
  theme(legend.position = "none", 
        axis.text.x=element_text(size = textSize), 
        axis.text.y=element_text(size = textSize), 
        axis.title.x=element_text(size = textSize), 
        axis.title.y=element_text(size = textSize, margin = margin(t = 0, r = 6, b = 0, l = 0)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks=element_line(size=1), 
        axis.ticks.length = unit(0.15, "cm"))

# Cleanups over time -----------------------------------------------------------------------

# average plastic bag as fraction of cleanup items 
cleanupOverTime <- merged %>% filter(!is.na(policy)) %>% group_by(groupID01, year) %>% summarise(percPlasticBag = mean(percPlasticBag, na.rm=T), 
                                                                                            policy = max(policy, na.rm=T), 
                                                                                            policyType = first(policyType), 
                                                                                            incQuintile = max(incQuintile, na.rm=T)) %>% ungroup()
cleanupOverTime <- cleanupOverTime %>% mutate_all(~ replace(., is.nan(.), NA))
cleanupOverTimeSum <- cleanupOverTime %>% group_by(year, policy) %>% summarise(meanPercPlasticBag = mean(percPlasticBag, na.rm=T), 
                                                                               sdPercPlasticBag = sd(percPlasticBag, na.rm=T)) %>% ungroup()

#cleanupOverTimeSum <- cleanupOverTimeSum %>% mutate(date = as.Date(paste0(year, "-", month, "-01")))
cleanupOverTimeSum <- cleanupOverTimeSum %>% mutate(meanPercPlasticBag = meanPercPlasticBag * 100, 
                                                    sdPercPlasticBag = sdPercPlasticBag * 100)

# % of cleanups in areas with and without bans 
mergedYear <- merged %>% mutate(ind = 1) %>% group_by(year, policy) %>% summarise(meanPolicy = mean(policy, na.rm=T), 
                                                                          countCleanup = sum(ind, na.rm=T)) %>% ungroup()
mergedYear <- mergedYear %>% filter(!is.na(meanPolicy))
mergedYear <- mergedYear %>% mutate(countCleanup = countCleanup / 1000)

# plot 
pal <- pnw_palette("Sailboat", 5)

# % of plastic
a <- ggplot() +
  geom_line(data=cleanupOverTimeSum %>% filter(policy == 0), aes(x = year, y = meanPercPlasticBag), size = 0.85, color=pal[1]) +
  geom_line(data=cleanupOverTimeSum %>% filter(policy == 1), aes(x = year, y = meanPercPlasticBag), size = 0.85, color=pal[5]) +
  scale_color_manual(values=rev(pal)) + 
  ylim(2, 5) +  scale_x_continuous(breaks = c(2016, 2018, 2020, 2022), lim=c(2016, 2023))+
  labs(x = "", y = "Plastic Bag as % of Items") +
  theme_bw() + 
  theme(legend.position = "none", 
        axis.text.x=element_text(size = textSize), 
        axis.text.y=element_text(size = textSize), 
        axis.title.x=element_text(size = textSize), 
        axis.title.y=element_text(size = textSize),
        legend.text=element_text(size=textSize), 
        legend.title=element_text(size=textSize), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks=element_line(size=1), 
        axis.ticks.length = unit(0.15, "cm"))

b <- ggplot() + 
  geom_line(data=mergedYear %>% filter(policy == 0), aes(x=year, y = countCleanup), color=pal[1], size = 0.5)+
  geom_line(data=mergedYear %>% filter(policy == 1), aes(x=year, y = countCleanup), color=pal[5], size = 0.5)+ 
  theme_bw() + xlab("Year")+ylab("Cleanup Count\n(Thousands)")+
  scale_y_continuous(breaks = c(5, 10, 15), lim=c(0, 15))+
  theme(legend.position = "none", 
        axis.text.x=element_text(size = textSize), 
        axis.text.y=element_text(size = textSize), 
        axis.title.x=element_text(size = textSize), 
        axis.title.y=element_text(size = textSize),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks.x =element_line(size=1), 
        axis.ticks.length.x = unit(0.15, "cm"), 
        axis.ticks.y =element_line(size=0), 
        axis.ticks.length.y = unit(0., "cm"), 
        panel.background = element_rect(fill='grey89', colour='grey89'))

plot2 <- ggarrange(a, NULL, b, ncol = 1, align = "v", heights = c(2, -0.30, .75))

plot <- ggarrange(plot1, plot2, ncol = 2)

ggsave("figures/figure_1.png", width = 8, height = 4, units = "in")



