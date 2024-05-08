#############################################################################################
# Plastic Bag Ban
# Spillover analyses 
# last modified: 03/27/24
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
setwd(paste0(directory, "replication/"))

## figure parameters 
textSize <- 10

# Import Data -------------------------------------------------------------------------------

# all
load("data/processed/02_data_merged/02_merged_cell_month.rda")
data <- data %>% mutate(ym = month + (year - 2016)*12 )

dataMain <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataMain <- dataMain %>% mutate(firstYM = month(firstEffect) + (year(firstEffect)-2016)*12, treat = treatedAny)
dataMain <- dataMain %>% dplyr::select(type, groupID01, zip, county, firstYM, treat, ym,  percPlasticBag)

dataSpillover1 <- data %>% filter(type %in% c("controlSpillover1", "control"))
dataSpillover1 <- dataSpillover1 %>% mutate(firstYM = month(neighborFirstEffect) + (year(neighborFirstEffect)-2016)*12)
dataSpillover1 <- dataSpillover1 %>% dplyr::select(type, groupID01, zip, county, firstYM, treat = treatedSpillover1, ym, percPlasticBag)

dataSpillover2 <- data %>% filter(type %in% c("controlSpillover2", "control"))
dataSpillover2 <- dataSpillover2 %>% mutate(firstYM = month(neighborFirstEffect) + (year(neighborFirstEffect)-2016)*12)
dataSpillover2 <- dataSpillover2 %>% dplyr::select(type, groupID01, zip, county, firstYM, treat = treatedSpillover2, ym, percPlasticBag)

dataSpillover3 <- data %>% filter(type %in% c("controlSpillover3", "control"))
dataSpillover3 <- dataSpillover3 %>% mutate(firstYM = month(neighborFirstEffect) + (year(neighborFirstEffect)-2016)*12)
dataSpillover3 <- dataSpillover3 %>% dplyr::select(type, groupID01, zip, county, firstYM, treat = treatedSpillover3, ym, percPlasticBag)

controlMean <- dataSpillover3 %>% filter(is.na(firstYM)) 
mean <- mean(controlMean$percPlasticBag, na.rm=TRUE)
rm(controlMean)

# Main Results ---------------------------------------------------------------------------------

# TWFE 
model <- felm(data = dataMain, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model)
control_mean <- dataMain %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean <- control_mean$mean
results_model <- tidy(model, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Treated") %>% mutate(estimate = estimate / control_mean, std.error = std.error / control_mean) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak <- did_imputation(data = dataMain, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results <- c('Borusyak et al. (2021)', "Treated", borusyak$estimate/control_mean, borusyak$std.error/control_mean)
results_model <- rbind(results_model, borusyak_results)

# Main Spillover Results -----------------------------------------------------------------------

# SPILLOVER 1
# TWFE 
model_spillover1 <- felm(data = dataSpillover1, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model)
control_mean_spillover1 <- dataSpillover1 %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean_spillover1 <- control_mean_spillover1$mean
results_model_spillover1 <- tidy(model_spillover1, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Neighbors") %>% mutate(estimate = estimate / control_mean_spillover1, std.error = std.error / control_mean_spillover1) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_spillover1 <- did_imputation(data = dataSpillover1, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_spillover1 <- c('Borusyak et al. (2021)', "Neighbors", borusyak_spillover1$estimate/control_mean_spillover1, borusyak_spillover1$std.error/control_mean_spillover1)
results_model_spillover1 <- rbind(results_model_spillover1, borusyak_results_spillover1)

# SPILLOVER 2
# TWFE 
model_spillover2 <- felm(data = dataSpillover2, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model)
control_mean_spillover2 <- dataSpillover1 %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean_spillover2 <- control_mean_spillover2$mean
results_model_spillover2 <- tidy(model_spillover2, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Neighbors\nof Neighbors") %>% mutate(estimate = estimate / control_mean_spillover2, std.error = std.error / control_mean_spillover2) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_spillover2 <- did_imputation(data = dataSpillover2, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_spillover2 <- c('Borusyak et al. (2021)', "Neighbors\nof Neighbors", borusyak_spillover2$estimate/control_mean_spillover2, borusyak_spillover2$std.error/control_mean_spillover2)
results_model_spillover2 <- rbind(results_model_spillover2, borusyak_results_spillover2)

# SPILLOVER 3
# TWFE 
model_spillover3 <- felm(data = dataSpillover3, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model)
control_mean_spillover3 <- dataSpillover1 %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean_spillover3 <- control_mean_spillover3$mean
results_model_spillover3 <- tidy(model_spillover3, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Neighbors\nof Neighbors\nof Neighbors") %>% mutate(estimate = estimate / control_mean_spillover3, std.error = std.error / control_mean_spillover3) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_spillover3 <- did_imputation(data = dataSpillover3, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_spillover3 <- c('Borusyak et al. (2021)', "Neighbors\nof Neighbors\nof Neighbors", borusyak_spillover3$estimate/control_mean_spillover3, borusyak_spillover3$std.error/control_mean_spillover3)
results_model_spillover3 <- rbind(results_model_spillover3, borusyak_results_spillover3)

# Plot -----------------------------------------------------------------------------------------

# COEFFICIENT PLOT
pal <- pnw_palette("Sailboat", 7)
results <- rbind(results_model, results_model_spillover1, results_model_spillover2, results_model_spillover3) 

# add 90th and 99th percentile conf interval 
results <- results %>% mutate(estimate = as.numeric(estimate), std.error = as.numeric(std.error))
results <- results %>% mutate(estimate = estimate * 100, std.error = std.error * 100)
results <- results %>% mutate(conf.low = estimate - 1.96 * std.error, conf.high = estimate + 1.96 * std.error, 
                              conf.low.90 = estimate - 1.65 * std.error,  conf.high.90 = estimate + 1.65 * std.error, 
                              conf.low.99 = estimate - 2.58 * std.error,  conf.high.99 = estimate + 2.58 * std.error)
results <- results %>% rename(term = item)

# keep only borusyak 
results <- results %>% filter(model == "Borusyak et al. (2021)")

# create plot 
a <- dwplot(results, dot_args = list(size = 3, aes(color = term)), whisker_args = list(size = 0.75, aes(color = term)), dodge_size = 0.4, vars_order = c(paste0("Neighbors\nof Neighbors\nof Neighbors"), paste0("Neighbors\nof Neighbors"), paste0("Neighbors"), paste0("Treated"))) + 
  theme_bw() + coord_flip() + xlab("\nPlastic Grocery Bags\n∆ Relative to Control Mean (%)") +
  geom_vline(xintercept = 0,colour = "grey60", size = 0.2) + 
  geom_vline(xintercept =  -21.68766,colour = pal[7], size = 0.2, linetype = 2) + 
  scale_shape_discrete(name = "Model") +
  scale_color_manual(name = "Type", values=c(pal[4], pal[5], pal[6], pal[7]), guide = "none") +
  # 90th perc confidence interval 
  geom_segment(aes(y=1, yend=1, x=as.numeric(results[1,7]), xend = as.numeric(results[1,8])), alpha=0.1, color = pal[7], size = 2)+  
  geom_segment(aes(y=2, yend=2, x=as.numeric(results[2,7]), xend = as.numeric(results[2,8])), alpha=0.1, color = pal[6], size = 2)+ 
  geom_segment(aes(y=3, yend=3, x=as.numeric(results[3,7]), xend = as.numeric(results[3,8])), alpha=0.1, color = pal[5], size = 2)+  
  geom_segment(aes(y=4, yend=4, x=as.numeric(results[4,7]), xend = as.numeric(results[4,8])), alpha=0.1, color = pal[4], size = 2)+ 
  # 99th perc confidence interval 
  geom_segment(aes(y=1, yend=1, x=as.numeric(results[1,9]), xend = as.numeric(results[1,10])), alpha=0.75, color = pal[7], size = 0.05)+  
  geom_segment(aes(y=2, yend=2, x=as.numeric(results[2,9]), xend = as.numeric(results[2,10])), alpha=0.75, color = pal[6], size = 0.05)+ 
  geom_segment(aes(y=3, yend=3, x=as.numeric(results[3,9]), xend = as.numeric(results[3,10])), alpha=0.75, color = pal[5], size = 0.05)+  
  geom_segment(aes(y=4, yend=4, x=as.numeric(results[4,9]), xend = as.numeric(results[4,10])), alpha=0.75, color = pal[4], size = 0.05)+ 
  theme(legend.position = "none", 
        axis.text.x=element_text(size = textSize-4), 
        axis.text.y=element_text(size = textSize), 
        axis.title.x=element_text(size = textSize, margin = margin(t = -5, r = -20, b = -5, l = -20)), 
        axis.title.y=element_text(size = textSize, margin = margin(t = 0, r = 0, b = 0, l = -15)),
        legend.text=element_text(size=textSize-5), 
        legend.title=element_text(size=textSize-5), 
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.ticks=element_line(size=1), 
        axis.ticks.length = unit(0.15, "cm")) 
a <- a+guides(color = "none")

ggsave("figures/figure_3_partb.png", width = 8.7, height = 6.2, units = "cm")

