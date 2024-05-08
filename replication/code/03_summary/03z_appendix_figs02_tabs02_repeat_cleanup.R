#############################################################################################
# Plastic Bag Ban
# Repeated clean-up analysis 
# last modified: 03/04/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------
## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, ggplot2, dplyr, readr, tidyr, broom, stargazer, stringr, lubridate, raster, sf, ggpubr, PNWColors, lfe)    

## directory 
directory <- "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/.shortcut-targets-by-id/19xFikvYAiHUYb-Az8rxT_I6rsaNv_57l/BagBan_BeachDebris/"
setwd(paste0(directory, "plasticbag/replication/"))

## figure parameters 
textSize <- 10

# Load clean-up data -----------------------------------------------------------------------

load("data/processed/00_data_cleanup.rda")

# Flag nearby  -----------------------------------------------------------------------------

# by 0.01 lat/lon
cleanup <- cleanup %>% arrange(groupID01, date)
cleanup <- cleanup %>% group_by(groupID01) %>% mutate(dayDiff01 = date - dplyr::lag(date), 
                                                       itemsDiff01 = totalItems - dplyr::lag(totalItems),
                                                       itemsPPDiff01 = itemsPP - dplyr::lag(itemsPP), 
                                                       itemsPPPMDiff01 = itemsPPPM - dplyr::lag(itemsPPPM), 
                                                       percPlasticBagDiff01 = percPlasticBag - dplyr::lag(percPlasticBag)) %>% ungroup()

# Calculate average frequency within cell  -------------------------------------------------

# average revisit frequency within cell 
cleanup <- cleanup %>% mutate(sum = 1)
cleanupMean <- cleanup %>% group_by(groupID01) %>% summarise(meanDayDiff01 = mean(dayDiff01, na.rm=T), 
                                                             count = sum(sum)) %>% ungroup()
cleanupMean <- cleanupMean %>% filter(!is.na(meanDayDiff01))
cleanupMean <- cleanupMean %>% filter(meanDayDiff01 > 0)
cleanupMean <- cleanupMean %>% mutate(diffMonths = as.numeric(meanDayDiff01) / 30)

median <- median(cleanupMean$diffMonths)
mean <- mean(cleanupMean$diffMonths)

# average time between cleanups chart 
pal <- pnw_palette("Sailboat", 5)
ggplot(cleanupMean, aes(x=diffMonths)) + geom_histogram(binwidth=1, fill = pal[1]) + 
  scale_x_continuous(breaks=seq(0, 96, by = 12))+
  theme_bw() + ylab("Gridcells") + xlab("Average Time Between Cleanups (Months)") + 
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
ggsave("figures/appendix/figure_s02_cleanup_freq_histogram.png", width = 17.4, height = 5.8, units = "cm")

# Repeated cleanups ------------------------------------------------------------------------

# repeated counts 
repeatCleanup01 <- cleanup %>% filter(dayDiff01 > 0)

# 0.01 lat/lon cells 
ggplot(data = repeatCleanup01, aes(x = as.integer(dayDiff01), y = itemsPPDiff01)) + 
  geom_point( size = 0.1) + 
  geom_smooth() + 
  xlab("Days between clean-ups") + ylab("Difference in Items per Person")

ggplot(data = repeatCleanup01 %>% filter(dayDiff01 < 31), aes(x = as.integer(dayDiff01), y = itemsPPDiff01)) + 
  geom_point( size = 0.1) + 
  geom_smooth() + 
  xlab("Days between clean-ups") + ylab("Difference in Items per Person")

ggplot(data = repeatCleanup01, aes(x = as.integer(dayDiff01), y = itemsPPPMDiff01)) + 
  geom_point( size = 0.1) + 
  geom_smooth() + 
  xlab("Days between clean-ups") + ylab("Difference in Items per Person per Mile")

ggplot(data = repeatCleanup01 %>% filter(dayDiff01 < 31), aes(x = as.integer(dayDiff01), y = itemsPPPMDiff01)) + 
  geom_point( size = 0.1) + 
  geom_smooth() + 
  xlab("Days between clean-ups") + ylab("Difference in Items per Person per Mile")

ggplot(data = repeatCleanup01, aes(x = as.integer(dayDiff01), y = percPlasticBagDiff01)) + 
  geom_point( size = 0.1) + 
  geom_smooth() + 
  xlab("Days between clean-ups") + ylab("Difference in Plastic Bag Share of Total Items")

ggplot(data = repeatCleanup01 %>% filter(dayDiff01 < 31), aes(x = as.integer(dayDiff01), y = percPlasticBagDiff01)) + 
  geom_point( size = 0.1) + 
  geom_smooth() + 
  xlab("Days between clean-ups") + ylab("Difference in Plastic Bag Share of Total Items")

#### REGRESSIONS ---- 
repeatCleanup01 <- repeatCleanup01 %>% mutate(percPlasticBagDiff01 = percPlasticBagDiff01 * 100, 
                                              percPlasticBag = percPlasticBag * 100)

# 0.01 lat/lon 
mod1a <-felm(data=repeatCleanup01, itemsPPDiff01 ~ dayDiff01 |0|0|groupID01)
mod1b <-felm(data=repeatCleanup01, itemsPPDiff01 ~ dayDiff01 |factor(groupID01)|0|groupID01)
mean1 <- repeatCleanup01 %>% summarise(mean=mean(itemsPPDiff01))
mean1b <- repeatCleanup01 %>% summarise(mean=mean(itemsPP))
mean1c <- repeatCleanup01 %>% filter(dayDiff01 < 30) %>% summarise(mean=mean(itemsPPDiff01, na.rm=TRUE))
mean1d <- repeatCleanup01 %>% filter(dayDiff01 > 30) %>% summarise(mean=mean(itemsPPDiff01, na.rm=TRUE))
mean1e <- repeatCleanup01 %>% filter(dayDiff01 > 60) %>% summarise(mean=mean(itemsPPDiff01, na.rm=TRUE))

mod2a <-felm(data=repeatCleanup01, itemsPPPMDiff01 ~ dayDiff01 |0|0|groupID01)
mod2b <-felm(data=repeatCleanup01, itemsPPPMDiff01 ~ dayDiff01 |factor(groupID01)|0|groupID01)
mean2 <- repeatCleanup01 %>% summarise(mean=mean(itemsPPPMDiff01, na.rm=TRUE))
mean2b <- repeatCleanup01 %>% summarise(mean=mean(itemsPPPM, na.rm=TRUE))
mean2c <- repeatCleanup01 %>% filter(dayDiff01 < 30) %>% summarise(mean=mean(itemsPPPMDiff01, na.rm=TRUE))
mean2d <- repeatCleanup01 %>% filter(dayDiff01 > 30) %>% summarise(mean=mean(itemsPPPMDiff01, na.rm=TRUE))
mean2e <- repeatCleanup01 %>% filter(dayDiff01 > 60) %>% summarise(mean=mean(itemsPPPMDiff01, na.rm=TRUE))

mod3a <-felm(data=repeatCleanup01, percPlasticBagDiff01 ~ dayDiff01 |0|0|groupID01)
mod3b <-felm(data=repeatCleanup01, percPlasticBagDiff01 ~ dayDiff01 |factor(groupID01)|0|groupID01)
mean3 <- repeatCleanup01 %>% summarise(mean=mean(percPlasticBagDiff01, na.rm=TRUE))
mean3b <- repeatCleanup01 %>% summarise(mean=mean(percPlasticBag, na.rm=TRUE))
mean3c <- repeatCleanup01 %>% filter(dayDiff01 < 30) %>% summarise(mean=mean(percPlasticBagDiff01, na.rm=TRUE))
mean3d <- repeatCleanup01 %>% filter(dayDiff01 > 30) %>% summarise(mean=mean(percPlasticBagDiff01, na.rm=TRUE))
mean3e <- repeatCleanup01 %>% filter(dayDiff01 > 60) %>% summarise(mean=mean(percPlasticBagDiff01, na.rm=TRUE))

# table 
stargazer(mod1b, mod2b,  mod3b,
          type="text", df=F,
          report=("vc*sp"),
          font.size="scriptsize",
          column.sep.width = c("10pt"),
          omit.stat=c("ser","adj.rsq", "rsq"),
          digits=3,
          title = "Repeated Clean-Ups", 
          keep = c("dayDiff01"),
          covariate.labels = c("Days b/w Cleanups"), 
          dep.var.labels = c("Items per Person", "Items Per Person Per Mile", "Plastic Bag % of Items"),
          add.lines = list(c("Mean Diff", round(mean1$mean, 2), round(mean2$mean, 2), round(mean3$mean, 3)), 
                           c("Mean Diff $<$ 30 Days", round(mean1c$mean, 2), round(mean2c$mean, 2), round(mean3c$mean, 3)), 
                           c("Mean Diff $>$ 30 Days", round(mean1d$mean, 2), round(mean2d$mean, 2), round(mean3d$mean, 3)), 
                           c("Mean Diff $>$ 60 Days", round(mean1e$mean, 2), round(mean2e$mean, 2), round(mean3e$mean, 3)), 
                           c("Mean", round(mean1b$mean, 2), round(mean2b$mean, 2), round(mean3b$mean, 3))),
          out="tables/appendix/table_s02_repeat_cleanups.tex")
