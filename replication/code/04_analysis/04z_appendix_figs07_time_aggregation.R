#############################################################################################
# Plastic Bag Ban
# Robustness checks of main TWFE analysis, time aggregation
# last modified: 04/13/24
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

# cell x month
load("data/processed/02_data_merged/02_merged_cell_month.rda")
data <- data %>% mutate(treat = treatedAny, 
                        ym = month + (year - 2016)*12, 
                        firstYM = month(firstEffect) + (year(firstEffect)-2016)*12 )

dataMain <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataMain <- dataMain %>% dplyr::select(type, groupID01, zip, firstYM, treat, ym, percPlasticBag)

# cell x quarter
load("data/processed/02_data_merged/02_merged_cell_quarter.rda")
data <- data %>% mutate(treat = treatedAny, 
                        yq = quarter + (year - 2016)*4,
                        firstQ =  ifelse(month(firstEffect) %in% c(12, 1, 2), 0, 
                                         ifelse(month(firstEffect) %in% c(3, 4, 5), 1, 
                                                ifelse(month(firstEffect) %in% c(6, 7, 8), 2, 
                                                       ifelse(month(firstEffect) %in% c(9, 10, 11), 3, NA)))), 
                        firstY = ifelse(month(firstEffect) == 12, year(firstEffect) + 1, year(firstEffect)))
data <- data %>% mutate(firstYQ = firstQ + (firstY - 2016)*4)

dataMainQuarter <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataMainQuarter <- dataMainQuarter %>% dplyr::select(type, groupID01, zip, firstYQ, treat, yq, percPlasticBag)

# cell x year
load("data/processed/02_data_merged/02_merged_cell_year.rda")
data <- data %>% mutate(treat = treatedAny, 
                        firstY = year(firstEffect) )

dataMainYear <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataMainYear <- dataMainYear %>% dplyr::select(type, groupID01, zip, firstY, treat, year, percPlasticBag)

# Main Headline Results: Cell x Month -----------------------------------------------------------------------

# TWFE 
model <- felm(data = dataMain, percPlasticBag ~ treat | factor(ym) + groupID01 | 0 | zip)
summary(model)
nobs <- nobs(model)
control_mean <- dataMain %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean <- control_mean$mean
results_model <- tidy(model, conf.int=TRUE) %>% mutate(model = "TWFE", time = paste0("Month\nn = ", nobs)) %>% mutate(estimate = estimate / control_mean, std.error = std.error / control_mean) %>% dplyr::select(model, time, estimate, std.error)

# Borusyak et al. 2021
borusyak <- did_imputation(data = dataMain, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results <- c('Borusyak et al. (2021)', paste0("Month\nn = ", nobs), borusyak$estimate/control_mean, borusyak$std.error/control_mean)
results_model <- rbind(results_model, borusyak_results)


# Main Headline Results: Cell x Quarter -----------------------------------------------------------------------

# TWFE 
modelQuarter <- felm(data = dataMainQuarter, percPlasticBag ~ treat | factor(yq) + groupID01 | 0 | zip)
summary(modelQuarter)
nobsQuarter <- nobs(modelQuarter)
control_meanQuarter <- dataMain %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_meanQuarter <- control_meanQuarter$mean
results_modelQuarter <- tidy(modelQuarter, conf.int=TRUE) %>% mutate(model = "TWFE", time = paste0("Quarter\nn = ", nobsQuarter)) %>% mutate(estimate = estimate / control_meanQuarter, std.error = std.error / control_meanQuarter) %>% dplyr::select(model, time, estimate, std.error)

# Borusyak et al. 2021
borusyakQuarter <- did_imputation(data = dataMainQuarter, yname = "percPlasticBag", gname = "firstYQ", tname = "yq", idname = "groupID01", cluster_var = "zip")
borusyak_resultsQuarter <- c('Borusyak et al. (2021)', paste0("Quarter\nn = ", nobsQuarter), borusyakQuarter$estimate/control_meanQuarter, borusyakQuarter$std.error/control_meanQuarter)
results_modelQuarter <- rbind(results_modelQuarter, borusyak_resultsQuarter)

# Main Headline Results: Cell x Year -----------------------------------------------------------------------

# TWFE 
modelYear <- felm(data = dataMainYear, percPlasticBag ~ treat | factor(year) + groupID01 | 0 | zip)
summary(modelYear)
nobsYear <- nobs(modelYear)
control_meanYear <- dataMain %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_meanYear <- control_meanYear$mean
results_modelYear <- tidy(modelYear, conf.int=TRUE) %>% mutate(model = "TWFE", time = paste0("Year\nn = ", nobsYear)) %>% mutate(estimate = estimate / control_meanYear, std.error = std.error / control_meanYear) %>% dplyr::select(model, time, estimate, std.error)

# Borusyak et al. 2021
borusyakYear <- did_imputation(data = dataMainYear, yname = "percPlasticBag", gname = "firstY", tname = "year", idname = "groupID01", cluster_var = "zip")
borusyak_resultsYear <- c('Borusyak et al. (2021)', paste0("Year\nn = ", nobsYear), borusyakYear$estimate/control_meanYear, borusyakYear$std.error/control_meanYear)
results_modelYear <- rbind(results_modelYear, borusyak_resultsYear)

# Robustness to Time Aggregation -------------------------------------------------------------------

# palette
pal <- pnw_palette("Shuksan2", 5)
palette <- c(pal[1], pal[5])

# combine 
results <- rbind(results_model, results_modelQuarter, results_modelYear) 

# add 90th and 99th percentile conf interval 
results <- results %>% mutate(estimate = as.numeric(estimate), std.error = as.numeric(std.error))
results <- results %>% mutate(estimate = estimate * 100, std.error = std.error * 100)
results <- results %>% mutate(conf.low = estimate - 1.96 * std.error, conf.high = estimate + 1.96 * std.error, 
                              conf.low.90 = estimate - 1.65 * std.error,  conf.high.90 = estimate + 1.65 * std.error, 
                              conf.low.99 = estimate - 2.58 * std.error,  conf.high.99 = estimate + 2.58 * std.error)
results <- results %>% rename(term = time)

# create plot 
a <- dwplot(results %>% filter(term %in% c(paste0("Year\nn = ", nobsYear), paste0("Quarter\nn = ", nobsQuarter), paste0("Month\nn = ", nobs))), dot_args = list(size = 3, aes(shape = model, color = model)), whisker_args = list(size = 0.75), dodge_size = 0.4, model_order = c("TWFE", "Borusyak et al. (2021)")) + 
  scale_shape_discrete(name = "Estimator") +
  scale_color_manual(values = palette, name = "Estimator") +
  theme_bw() + coord_flip() + xlab("∆ Relative to Control Mean (%)") + ylab(paste0("\nPlastic Bags")) +
  geom_vline(xintercept = 0,colour = "grey60", size=0.2) + 
  xlim(-65, 25)+
  # 90th perc confidence interval 
  geom_segment(aes(y=0.90, yend=0.90, x=as.numeric(results[6,7]), xend = as.numeric(results[6,8])), alpha=0.04, color = palette[1], size = 2)+  
  geom_segment(aes(y=1.1, yend=1.1, x=as.numeric(results[5,7]), xend = as.numeric(results[5,8])), alpha=0.04, color = palette[2], size = 2)+ 
  geom_segment(aes(y=1.90, yend=1.90, x=as.numeric(results[4,7]), xend = as.numeric(results[4,8])), alpha=0.04, color = palette[1], size = 2)+  
  geom_segment(aes(y=2.1, yend=2.1, x=as.numeric(results[3,7]), xend = as.numeric(results[3,8])), alpha=0.04, color = palette[2], size = 2)+ 
  geom_segment(aes(y=2.90, yend=2.90, x=as.numeric(results[2,7]), xend = as.numeric(results[2,8])), alpha=0.04, color = palette[1], size = 2)+  
  geom_segment(aes(y=3.1, yend=3.1, x=as.numeric(results[1,7]), xend = as.numeric(results[1,8])), alpha=0.04, color = palette[2], size = 2)+
  # 99th perc confidence interval 
  geom_segment(aes(y=0.90, yend=0.90, x=as.numeric(results[6,9]), xend = as.numeric(results[6,10])), alpha=0.5, color = palette[1], size = 0.05)+  
  geom_segment(aes(y=1.1, yend=1.1, x=as.numeric(results[5,9]), xend = as.numeric(results[5,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
  geom_segment(aes(y=1.90, yend=1.90, x=as.numeric(results[4,9]), xend = as.numeric(results[4,10])), alpha=0.5, color = palette[1], size = 0.05)+  
  geom_segment(aes(y=2.1, yend=2.1, x=as.numeric(results[3,9]), xend = as.numeric(results[3,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
  geom_segment(aes(y=2.90, yend=2.90, x=as.numeric(results[2,9]), xend = as.numeric(results[2,10])), alpha=0.5, color = palette[1], size = 0.05)+  
  geom_segment(aes(y=3.1, yend=3.1, x=as.numeric(results[1,9]), xend = as.numeric(results[1,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
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

ggsave("figures/appendix/figure_s07_time_aggregation.png", width = 8, height = 4.25)







