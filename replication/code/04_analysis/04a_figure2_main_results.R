#############################################################################################
# Plastic Bag Ban
# Main TWFE analysis of bag bans 
# last modified: 03/29/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, ggplot2, dplyr, readr, tidyr, broom, stargazer, stringr, lubridate, ggpubr, 
               didimputation, fixest, lfe, PNWColors, dotwhisker)    

## directory 
directory <- "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/.shortcut-targets-by-id/19xFikvYAiHUYb-Az8rxT_I6rsaNv_57l/BagBan_BeachDebris/"
setwd(paste0(directory, "plasticbag/replication/"))

## figure parameters 
textSize <- 10

# Import Data -------------------------------------------------------------------------------

# all
load("data/processed/02_data_merged/02_merged_cell_month.rda")
data <- data %>% mutate(treat = treatedAny, 
                        ym = month + (year - 2016)*12, 
                        firstYM = month(firstEffect) + (year(firstEffect)-2016)*12 )

controlMean <- data %>% filter(is.na(firstYM)) 
mean <- mean(controlMean$percPlasticBag, na.rm=TRUE)
rm(controlMean)

# keep relevant variables 
dataMain <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))

# keep final needed variables 
dataMain <- dataMain %>% dplyr::select(type, groupID01, zip, county, firstYM, treat, ym, percPlasticBag, plasticBagPP, percOtherBag, percPlasticBottle, percPlasticBottleCap, percPlasticStraws, percPlasticFoodCont, lastPolicyID, firstPolicyID)

# add plastic bottles and bottle caps 
dataMain <- dataMain %>% mutate(percPlasticBottleAll = percPlasticBottle + percPlasticBottleCap)


# Main Headline Results -----------------------------------------------------------------------

# TWFE 
model <- felm(data = dataMain, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model)
control_mean <- dataMain %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean <- control_mean$mean
results_model <- tidy(model, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Grocery Bags,\n% of Items") %>% mutate(estimate = estimate / control_mean, std.error = std.error / control_mean) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak <- did_imputation(data = dataMain, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results <- c('Borusyak et al. (2021)', "Grocery Bags,\n% of Items", borusyak$estimate/control_mean, borusyak$std.error/control_mean)
results_model <- rbind(results_model, borusyak_results)

# Only One Policy ------------------------------------------------------------------------------

# TWFE 
model_onepolicy <- felm(data = dataMain %>% filter((lastPolicyID == firstPolicyID) | (is.na(lastPolicyID) | is.na(firstPolicyID))), percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_onepolicy)
results_onepolicy <- tidy(model_onepolicy, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Grocery Bags,\n% of Items")%>% mutate(estimate = estimate / control_mean, std.error = std.error / control_mean) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak <- did_imputation(data = dataMain %>% filter((lastPolicyID == firstPolicyID) | (is.na(lastPolicyID) | is.na(firstPolicyID))), yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results <- c('Borusyak et al. (2021)', "Grocery Bags,\n% of Items", borusyak$estimate/control_mean, borusyak$std.error/control_mean)
results_onepolicy <- rbind(results_onepolicy, borusyak_results)

# Plastic Bags per Person ---------------------------------------------------------------------

# TWFE 
model_PP <- felm(data = dataMain, plasticBagPP ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_PP)
control_mean_PP <- dataMain %>% filter(type == "control") %>% summarise(mean = mean(plasticBagPP, na.rm=T))
control_mean_PP <- control_mean_PP$mean
results_model_PP <- tidy(model_PP, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Grocery Bags,\nper Person") %>% mutate(estimate = estimate / control_mean_PP, std.error = std.error / control_mean_PP) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_PP <- did_imputation(data = dataMain, yname = "plasticBagPP", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_PP <- c('Borusyak et al. (2021)', "Grocery Bags,\nper Person", borusyak_PP$estimate/control_mean_PP, borusyak_PP$std.error/control_mean_PP)
results_model_PP <- rbind(results_model_PP, borusyak_results_PP)

# Non-Grocery Plastic Bags --------------------------------------------------------------------

model_otherbag <- felm(data = dataMain, percOtherBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_otherbag)
control_mean_otherbag <- dataMain %>% filter(type == "control") %>% summarise(mean = mean(percOtherBag, na.rm=T))
control_mean_otherbag <- control_mean_otherbag$mean
results_model_otherbag <- tidy(model_otherbag, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Non-Grocery Bags,\n% of Items") %>% mutate(estimate = estimate / control_mean_otherbag, std.error = std.error / control_mean_otherbag) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_otherbag <- did_imputation(data = dataMain, yname = "percOtherBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_otherbag <- c('Borusyak et al. (2021)', "Non-Grocery Bags,\n% of Items", borusyak_otherbag$estimate/control_mean_otherbag, borusyak_otherbag$std.error/control_mean_otherbag)
results_model_otherbag <- rbind(results_model_otherbag, borusyak_results_otherbag)

# Plastic Bottles + Bottle Caps ----------------------------------------------------------------

# TWFE 
model_plasticbottle <- felm(data = dataMain, percPlasticBottleAll ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_plasticbottle)
control_mean_plasticbottle <- dataMain %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBottleAll, na.rm=T))
control_mean_plasticbottle <- control_mean_plasticbottle$mean
results_model_plasticbottle <- tidy(model_plasticbottle, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Bottles and Caps,\n% of Items") %>% mutate(estimate = estimate / control_mean_plasticbottle, std.error = std.error / control_mean_plasticbottle) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_plasticbottle <- did_imputation(data = dataMain, yname = "percPlasticBottleAll", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_plasticbottle <- c('Borusyak et al. (2021)', "Bottles and Caps,\n% of Items", borusyak_plasticbottle$estimate/control_mean_plasticbottle, borusyak_plasticbottle$std.error/control_mean_plasticbottle)
results_model_plasticbottle <- rbind(results_model_plasticbottle, borusyak_results_plasticbottle)

# Plastic Straws ------------------------------------------------------------------------------

model_plasticstraw <- felm(data = dataMain, percPlasticStraws ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_plasticstraw)
control_mean_plasticstraw <- dataMain %>% filter(type == "control") %>% summarise(mean = mean(percPlasticStraws, na.rm=T))
control_mean_plasticstraw <- control_mean_plasticstraw$mean
results_model_plasticstraw <- tidy(model_plasticstraw, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Straws,\n% of Items") %>% mutate(estimate = estimate / control_mean_plasticstraw, std.error = std.error / control_mean_plasticstraw) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_plasticstraw <- did_imputation(data = dataMain, yname = "percPlasticStraws", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_plasticstraw <- c('Borusyak et al. (2021)', "Straws,\n% of Items", borusyak_plasticstraw$estimate/control_mean_plasticstraw, borusyak_plasticstraw$std.error/control_mean_plasticstraw)
results_model_plasticstraw <- rbind(results_model_plasticstraw, borusyak_results_plasticstraw)

# Plastic Food Containers  --------------------------------------------------------------------

# TWFE
model_plasticcont <- felm(data = dataMain, percPlasticFoodCont ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_plasticcont)
control_mean_plasticcont <- dataMain %>% filter(type == "control") %>% summarise(mean = mean(percPlasticFoodCont, na.rm=T))
control_mean_plasticcont <- control_mean_plasticcont$mean
results_model_plasticcont <- tidy(model_plasticcont, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Food Containers,\n% of Items") %>% mutate(estimate = estimate / control_mean_plasticcont, std.error = std.error / control_mean_plasticcont) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_plasticcont <- did_imputation(data = dataMain, yname = "percPlasticFoodCont", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_plasticcont <- c('Borusyak et al. (2021)', "Food Containers,\n% of Items", borusyak_plasticcont$estimate/control_mean_plasticcont, borusyak_plasticcont$std.error/control_mean_plasticcont)
results_model_plasticcont <- rbind(results_model_plasticcont, borusyak_results_plasticcont)

# Main Plot ------------------------------------------------------------------------------------

# COEFFICIENT PLOT
pal <- pnw_palette("Shuksan2", 5)
palette <- c(pal[1], pal[5])
  
# combine 
results <- rbind(results_model, results_model_PP, results_model_otherbag, results_model_plasticbottle, results_model_plasticstraw, results_model_plasticcont) 

# add 90th and 99th percentile conf interval 
results <- results %>% mutate(estimate = as.numeric(estimate), std.error = as.numeric(std.error))
results <- results %>% mutate(estimate = estimate * 100, std.error = std.error * 100)
results <- results %>% mutate(conf.low = estimate - 1.96 * std.error, conf.high = estimate + 1.96 * std.error, 
                              conf.low.90 = estimate - 1.65 * std.error,  conf.high.90 = estimate + 1.65 * std.error, 
                              conf.low.99 = estimate - 2.58 * std.error,  conf.high.99 = estimate + 2.58 * std.error)
results <- results %>% rename(term = item)

# observations 
nobs <- nobs(model)

# rename 
results <- results %>% mutate(term = ifelse(term == "Grocery Bags,\n% of Items", "Grocery Bags", term))
results <- results %>% mutate(term = ifelse(term == "Bottles and Caps,\n% of Items", "Bottles and Caps", term))
results <- results %>% mutate(term = ifelse(term == "Straws,\n% of Items", "Straws", term))
results <- results %>% mutate(term = ifelse(term == "Food Containers,\n% of Items", "Food Containers", term))

# create plot 
a <- dwplot(results %>% filter(term %in% c("Grocery Bags")), dot_args = list(size = 3, aes(shape = model, color = model)), whisker_args = list(size = 0.75), dodge_size = 0.4, model_order = c("TWFE", "Borusyak et al. (2021)"), vars_order = rev(c("Grocery Bags"))) + 
  scale_shape_discrete(name = "Estimator") +
  scale_color_manual(values = palette, name = "Estimator") +
  theme_bw() + coord_flip() + xlab("Share of Items\n∆ Relative to Control Mean (%)") + ylab(paste0("\nPlastic Grocery Bags\n(Subject to Policy)")) +
  geom_vline(xintercept = 0,colour = "grey60", size=0.2) + 
  xlim(-50, 35)+
  # 90th perc confidence interval 
  geom_segment(aes(y=0.90, yend=0.90, x=as.numeric(results[2,7]), xend = as.numeric(results[2,8])), alpha=0.1, color = palette[1], size = 2)+  
  geom_segment(aes(y=1.1, yend=1.1, x=as.numeric(results[1,7]), xend = as.numeric(results[1,8])), alpha=0.1, color = palette[2], size = 2)+ 
  # 99th perc confidence interval 
  geom_segment(aes(y=0.90, yend=0.90, x=as.numeric(results[2,9]), xend = as.numeric(results[2,10])), alpha=0.5, color = palette[1], size = 0.05)+  
  geom_segment(aes(y=1.1, yend=1.1, x=as.numeric(results[1,9]), xend = as.numeric(results[1,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
  theme(legend.position = c(0.25, 0.90), 
        axis.text.x=element_text(size = textSize-2), 
        axis.text.y=element_text(size = textSize), 
        axis.title.x=element_text(size = textSize), 
        axis.title.y=element_text(size = textSize, margin = margin(t = 0, r = 0, b = 0, l = -4)),
        legend.text=element_text(size=textSize), 
        legend.title=element_text(size=textSize), 
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks=element_line(size=1), 
        axis.ticks.length = unit(0.15, "cm")) 
a <- a+guides(shape = guide_legend(override.aes = list(color = c(palette[1], palette[2]))))+guides(color = 'none')

# create plot 
b <- dwplot(results, dot_args = list(size = 3, aes(shape = model, color = model)), whisker_args = list(size = 0.75), dodge_size = 0.4, model_order = c("TWFE", "Borusyak et al. (2021)"), vars_order = rev(c("Bottles and Caps", "Straws", "Food Containers"))) + 
  scale_shape_discrete(name = "Estimator") +
  scale_color_manual(values = palette, name = "Estimator") +
  theme_bw() + coord_flip() + xlab("Share of Items\n∆ Relative to Control Mean (%)") + ylab(paste0("\nOther Plastic Items\n(Not Subject to Policy)")) +
  geom_vline(xintercept = 0,colour = "grey60", size=0.2) + 
  xlim(-50, 35)+
  # 90th perc confidence interval 
  geom_segment(aes(y=0.90, yend=0.90, x=as.numeric(results[8,7]), xend = as.numeric(results[8,8])), alpha=0.04, color = palette[1], size = 2)+ 
  geom_segment(aes(y=1.1, yend=1.1, x=as.numeric(results[7,7]), xend = as.numeric(results[7,8])), alpha=0.04, color = palette[2], size = 2)+ 
  geom_segment(aes(y=1.90, yend=1.90, x=as.numeric(results[10,7]), xend = as.numeric(results[10,8])), alpha=0.04, color = palette[1], size = 2)+ 
  geom_segment(aes(y=2.1, yend=2.1, x=as.numeric(results[9,7]), xend = as.numeric(results[9,8])), alpha=0.04, color = palette[2], size = 2)+ 
  geom_segment(aes(y=2.90, yend=2.90, x=as.numeric(results[12,7]), xend = as.numeric(results[12,8])), alpha=0.04, color = palette[1], size = 2)+ 
  geom_segment(aes(y=3.1, yend=3.1, x=as.numeric(results[11,7]), xend = as.numeric(results[11,8])), alpha=0.04, color = palette[2], size = 2)+ 
  # 99th perc confidence interval 
  geom_segment(aes(y=0.90, yend=0.90, x=as.numeric(results[8,9]), xend = as.numeric(results[8,10])), alpha=0.5, color = palette[1], size = 0.05)+ 
  geom_segment(aes(y=1.1, yend=1.1, x=as.numeric(results[7,9]), xend = as.numeric(results[7,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
  geom_segment(aes(y=1.90, yend=1.90, x=as.numeric(results[10,9]), xend = as.numeric(results[10,10])), alpha=0.5, color = palette[1], size = 0.05)+ 
  geom_segment(aes(y=2.1, yend=2.1, x=as.numeric(results[9,9]), xend = as.numeric(results[9,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
  geom_segment(aes(y=2.90, yend=2.90, x=as.numeric(results[12,9]), xend = as.numeric(results[12,10])), alpha=0.5, color = palette[1], size = 0.05)+ 
  geom_segment(aes(y=3.1, yend=3.1, x=as.numeric(results[11,9]), xend = as.numeric(results[11,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
  theme(legend.position = "none", 
        axis.text.x=element_text(size = textSize-2), 
        axis.text.y=element_text(size = textSize), 
        axis.title.x=element_text(size = textSize), 
        axis.title.y=element_text(size = textSize, margin = margin(t = 0, r = 0, b = 0, l = -4)),
        legend.text=element_text(size=textSize-3), 
        legend.title=element_text(size=textSize-3), 
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks=element_line(size=1), 
        axis.ticks.length = unit(0.15, "cm")) 
b <- b+guides(shape = guide_legend(override.aes = list(color = c(palette[1], palette[2]))))+guides(color = 'none')

ggarrange(a, b, ncol = 2, nrow = 1, heights = c(1, 1))
ggsave("figures/figure_2.png", width = 8, height = 4, unit = 'in')
