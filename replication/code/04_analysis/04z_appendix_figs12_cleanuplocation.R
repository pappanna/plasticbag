#############################################################################################
# Plastic Bag Ban
# Robustness checks of main TWFE analysis, different type of cleanups 
# last modified: 04/13/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, ggplot2, dplyr, readr, tidyr, broom, stargazer, stringr, lubridate, ggpubr, 
               didimputation, did, fixest, lfe, PNWColors, dotwhisker)    

## directory 
directory <- "/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/.shortcut-targets-by-id/19xFikvYAiHUYb-Az8rxT_I6rsaNv_57l/BagBan_BeachDebris/"
setwd(paste0(directory, "replication/"))

## figure parameters 
textSize <- 10

# Import Data -------------------------------------------------------------------------------

# cell x month
load("data/processed/02_data_merged/02_merged_cell_month.rda")
data <- data %>% mutate(treat = treatedAny, 
                        ym = month + (year - 2016)*12, 
                        firstYM = month(firstEffect) + (year(firstEffect)-2016)*12 )

dataMain <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataMain <- dataMain %>% dplyr::select(type, groupID01, zip, firstYM, treat, ym, percPlasticBag)

# cell x month, coastal
load("data/processed/02_data_merged/02_merged_cell_month_coast.rda")
data <- data %>% mutate(treat = treatedAny, 
                        ym = month + (year - 2016)*12, 
                        firstYM = month(firstEffect) + (year(firstEffect)-2016)*12 )

dataCoast <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataCoast <- dataCoast %>% dplyr::select(type, groupID01, zip, firstYM, treat, ym, percPlasticBag)

# cell x month, river
load("data/processed/02_data_merged/02_merged_cell_month_river.rda")
data <- data %>% mutate(treat = treatedAny, 
                        ym = month + (year - 2016)*12, 
                        firstYM = month(firstEffect) + (year(firstEffect)-2016)*12 )

dataRiver <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataRiver <- dataRiver %>% dplyr::select(type, groupID01, zip, firstYM, treat, ym, percPlasticBag)

# cell x month, lake
load("data/processed/02_data_merged/02_merged_cell_month_lake.rda")
data <- data %>% mutate(treat = treatedAny, 
                        ym = month + (year - 2016)*12, 
                        firstYM = month(firstEffect) + (year(firstEffect)-2016)*12 )

dataLake <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataLake <- dataLake %>% dplyr::select(type, groupID01, zip, firstYM, treat, ym, percPlasticBag)

dataRiverLake <- rbind(dataRiver, dataLake)

# cell x month, other
load("data/processed/02_data_merged/02_merged_cell_month_other.rda")
data <- data %>% mutate(treat = treatedAny, 
                        ym = month + (year - 2016)*12, 
                        firstYM = month(firstEffect) + (year(firstEffect)-2016)*12 )

dataOther <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataOther <- dataOther %>% dplyr::select(type, groupID01, zip, firstYM, treat, ym, percPlasticBag)

# Main Headline Results: All -----------------------------------------------------------------------

# TWFE 
model <- felm(data = dataMain, percPlasticBag ~ treat | factor(ym) + groupID01 | 0 | zip)
summary(model)
nobs <- nobs(model)
control_mean <- dataMain %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean <- control_mean$mean
results_model <- tidy(model, conf.int=TRUE) %>% mutate(model = "TWFE", location= paste0("All\nn = ", nobs)) %>% mutate(estimate = estimate / control_mean, std.error = std.error / control_mean) %>% dplyr::select(model, location, estimate, std.error)

# Borusyak et al. 2021
borusyak <- did_imputation(data = dataMain, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results <- c('Borusyak et al. (2021)', paste0("All\nn = ", nobs), borusyak$estimate/control_mean, borusyak$std.error/control_mean)
results_model <- rbind(results_model, borusyak_results)

# Main Headline Results: Coast -----------------------------------------------------------------------

# TWFE 
modelCoast <- felm(data = dataCoast, percPlasticBag ~ treat | factor(ym) + groupID01| 0 | zip)
summary(modelCoast)
nobsCoast <- nobs(modelCoast)
control_meanCoast <- dataCoast %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_meanCoast <- control_meanCoast$mean
results_modelCoast <- tidy(modelCoast, conf.int=TRUE) %>% mutate(model = "TWFE", location= paste0("Coast\nn = ", nobsCoast)) %>% mutate(estimate = estimate / control_meanCoast, std.error = std.error / control_meanCoast) %>% dplyr::select(model, location, estimate, std.error)

# Borusyak et al. 2021
borusyakCoast <- did_imputation(data = dataCoast, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_resultsCoast <- c('Borusyak et al. (2021)', paste0("Coast\nn = ", nobsCoast), borusyakCoast$estimate/control_meanCoast, borusyakCoast$std.error/control_meanCoast)
results_modelCoast <- rbind(results_modelCoast, borusyak_resultsCoast)

# Main Headline Results: River + Lake -----------------------------------------------------------------------

# TWFE 
modelRiver <- felm(data = dataRiverLake, percPlasticBag ~ treat | factor(ym) + groupID01| 0 | zip)
summary(modelRiver)
nobsRiver <- nobs(modelRiver)
control_meanRiver <- dataRiver %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_meanRiver <- control_meanRiver$mean
results_modelRiver <- tidy(modelRiver, conf.int=TRUE) %>% mutate(model = "TWFE", location= paste0("River + Lake\nn = ", nobsRiver)) %>% mutate(estimate = estimate / control_meanRiver, std.error = std.error / control_meanRiver) %>% dplyr::select(model, location, estimate, std.error)

# Borusyak et al. 2021
borusyakRiver <- did_imputation(data = dataRiverLake, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_resultsRiver <- c('Borusyak et al. (2021)', paste0("River + Lake\nn = ", nobsRiver), borusyakRiver$estimate/control_meanRiver, borusyakRiver$std.error/control_meanRiver)
results_modelRiver <- rbind(results_modelRiver, borusyak_resultsRiver)

# Main Headline Results: Lake -----------------------------------------------------------------------

# TWFE 
modelLake <- felm(data = dataLake, percPlasticBag ~ treat | factor(ym) + groupID01| 0 | zip)
summary(modelLake)
nobsLake <- nobs(modelLake)
control_meanLake <- dataLake %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_meanLake <- control_meanLake$mean
results_modelLake <- tidy(modelLake, conf.int=TRUE) %>% mutate(model = "TWFE", location= paste0("Lake\nn = ", nobsLake)) %>% mutate(estimate = estimate / control_meanLake, std.error = std.error / control_meanLake) %>% dplyr::select(model, location, estimate, std.error)

# Borusyak et al. 2021
borusyakLake <- did_imputation(data = dataLake, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_resultsLake <- c('Borusyak et al. (2021)', paste0("Lake\nn = ", nobsLake), borusyakLake$estimate/control_meanLake, borusyakLake$std.error/control_meanLake)
results_modelLake <- rbind(results_modelLake, borusyak_resultsLake)

# Main Headline Results: Other -----------------------------------------------------------------------

# TWFE 
modelOther <- felm(data = dataOther, percPlasticBag ~ treat | factor(ym) + groupID01| 0 | zip)
summary(modelOther)
nobsOther <- nobs(modelOther)
control_meanOther <- dataOther %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_meanOther <- control_meanOther$mean
results_modelOther <- tidy(modelOther, conf.int=TRUE) %>% mutate(model = "TWFE", location= paste0("Other\nn = ", nobsOther)) %>% mutate(estimate = estimate / control_meanOther, std.error = std.error / control_meanOther) %>% dplyr::select(model, location, estimate, std.error)

# Borusyak et al. 2021
borusyakOther <- did_imputation(data = dataOther, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_resultsOther <- c('Borusyak et al. (2021)', paste0("Other\nn = ", nobsOther), borusyakOther$estimate/control_meanOther, borusyakOther$std.error/control_meanOther)
results_modelOther <- rbind(results_modelOther, borusyak_resultsOther)

# Robustness to Time Aggregation -------------------------------------------------------------------

# palette
pal <- pnw_palette("Shuksan2", 5)
palette <- c(pal[1], pal[5])

# combine 
results <- rbind(results_model, results_modelCoast, results_modelRiver, results_modelOther) 

# add 90th and 99th percentile conf interval 
results <- results %>% mutate(estimate = as.numeric(estimate), std.error = as.numeric(std.error))
results <- results %>% mutate(estimate = estimate * 100, std.error = std.error * 100)
results <- results %>% mutate(conf.low = estimate - 1.96 * std.error, conf.high = estimate + 1.96 * std.error, 
                              conf.low.90 = estimate - 1.65 * std.error,  conf.high.90 = estimate + 1.65 * std.error, 
                              conf.low.99 = estimate - 2.58 * std.error,  conf.high.99 = estimate + 2.58 * std.error)
results <- results %>% rename(term = location)

# create plot 
a <- dwplot(results, dot_args = list(size = 3, aes(shape = model, color = model)), whisker_args = list(size = 0.75), dodge_size = 0.4, model_order = c("TWFE", "Borusyak et al. (2021)"), vars_order = rev(c(paste0("All\nn = ", nobs), paste0("Coast\nn = ", nobsCoast), paste0("River + Lake\nn = ", nobsRiver), paste0("Other\nn = ", nobsOther)))) + 
  scale_shape_discrete(name = "Estimator") +
  scale_color_manual(values = palette, name = "Estimator") +
  theme_bw() + coord_flip() + xlab("∆ Relative to Control Mean (%)") + ylab(paste0("\nPlastic Bags")) +
  geom_vline(xintercept = 0,colour = "grey60", size=0.2) + 
  # 90th perc confidence interval 
  geom_segment(aes(y=0.90, yend=0.90, x=as.numeric(results[2,7]), xend = as.numeric(results[2,8])), alpha=0.04, color = palette[1], size = 2)+  
  geom_segment(aes(y=1.1, yend=1.1, x=as.numeric(results[1,7]), xend = as.numeric(results[1,8])), alpha=0.04, color = palette[2], size = 2)+ 
  geom_segment(aes(y=1.90, yend=1.90, x=as.numeric(results[4,7]), xend = as.numeric(results[4,8])), alpha=0.04, color = palette[1], size = 2)+  
  geom_segment(aes(y=2.1, yend=2.1, x=as.numeric(results[3,7]), xend = as.numeric(results[3,8])), alpha=0.04, color = palette[2], size = 2)+ 
  geom_segment(aes(y=2.90, yend=2.90, x=as.numeric(results[6,7]), xend = as.numeric(results[6,8])), alpha=0.04, color = palette[1], size = 2)+  
  geom_segment(aes(y=3.1, yend=3.1, x=as.numeric(results[5,7]), xend = as.numeric(results[5,8])), alpha=0.04, color = palette[2], size = 2)+
  geom_segment(aes(y=3.90, yend=3.90, x=as.numeric(results[8,7]), xend = as.numeric(results[8,8])), alpha=0.04, color = palette[1], size = 2)+  
  geom_segment(aes(y=4.1, yend=4.1, x=as.numeric(results[7,7]), xend = as.numeric(results[7,8])), alpha=0.04, color = palette[2], size = 2)+
  # 99th perc confidence interval 
  geom_segment(aes(y=0.90, yend=0.90, x=as.numeric(results[2,9]), xend = as.numeric(results[2,10])), alpha=0.5, color = palette[1], size = 0.05)+  
  geom_segment(aes(y=1.1, yend=1.1, x=as.numeric(results[1,9]), xend = as.numeric(results[1,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
  geom_segment(aes(y=1.90, yend=1.90, x=as.numeric(results[4,9]), xend = as.numeric(results[4,10])), alpha=0.5, color = palette[1], size = 0.05)+  
  geom_segment(aes(y=2.1, yend=2.1, x=as.numeric(results[3,9]), xend = as.numeric(results[3,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
  geom_segment(aes(y=2.90, yend=2.90, x=as.numeric(results[6,9]), xend = as.numeric(results[6,10])), alpha=0.5, color = palette[1], size = 0.05)+  
  geom_segment(aes(y=3.1, yend=3.1, x=as.numeric(results[5,9]), xend = as.numeric(results[5,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
  geom_segment(aes(y=3.90, yend=3.90, x=as.numeric(results[8,9]), xend = as.numeric(results[8,10])), alpha=0.5, color = palette[1], size = 0.05)+  
  geom_segment(aes(y=4.1, yend=4.1, x=as.numeric(results[7,9]), xend = as.numeric(results[7,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
  theme(legend.position = c(0.10, 0.90), 
        axis.text.x=element_text(size = textSize), 
        axis.text.y=element_text(size = textSize), 
        axis.title.x=element_text(size = textSize), 
        axis.title.y=element_text(size = textSize, margin = margin(t = 0, r = 0, b = 0, l = -4)),
        legend.text=element_text(size=textSize-2), 
        legend.title=element_text(size=textSize-2), 
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
ggsave("figures/appendix/figure_s12_cleanup_location.png", width = 8, height = 4.25)







