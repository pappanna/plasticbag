#############################################################################################
# Plastic Bag Ban
# Robustness checks of main TWFE analysis, spatial aggregation
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
setwd(paste0(directory, "replication/"))

## figure parameters 
textSize <- 10

# Import Data -------------------------------------------------------------------------------

# county x month
load("data/processed/02_data_merged/02_merged_county_month.rda")
data <- data %>% mutate(treat = treatedAny, 
                        ym = month + (year - 2016)*12, 
                        firstYM = month(firstEffect) + (year(firstEffect)-2016)*12 )

dataMainCounty <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataMainCounty <- dataMainCounty %>% dplyr::select(type, county, firstYM, treat, ym, percPlasticBag)

# zip x month
load("data/processed/02_data_merged/02_merged_zip_month.rda")
data <- data %>% mutate(treat = treatedAny, 
                        ym = month + (year - 2016)*12, 
                        firstYM = month(firstEffect) + (year(firstEffect)-2016)*12 )

dataMainZip <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataMainZip <- dataMainZip %>% dplyr::select(type, zip, firstYM, treat, ym, percPlasticBag)

# bigcell x month
load("data/processed/02_data_merged/02_merged_bigcell_month.rda")
data <- data %>% mutate(treat = treatedAny, 
                        ym = month + (year - 2016)*12, 
                        firstYM = month(firstEffect) + (year(firstEffect)-2016)*12 )

dataMainBig <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataMainBig <- dataMainBig %>% dplyr::select(type, groupID1, zip, firstYM, treat, ym, percPlasticBag)

# cell x month
load("data/processed/02_data_merged/02_merged_cell_month.rda")
data <- data %>% mutate(treat = treatedAny, 
                        ym = month + (year - 2016)*12, 
                        firstYM = month(firstEffect) + (year(firstEffect)-2016)*12 )

dataMain <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataMain <- dataMain %>% dplyr::select(type, groupID01, zip, firstYM, treat, ym, percPlasticBag)

# smallcell x month
load("data/processed/02_data_merged/02_merged_smallcell_month.rda")
data <- data %>% mutate(treat = treatedAny, 
                        ym = month + (year - 2016)*12, 
                        firstYM = month(firstEffect) + (year(firstEffect)-2016)*12 )

dataMainSmall <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataMainSmall <- dataMainSmall %>% dplyr::select(type, groupID001, zip, firstYM, treat, ym, percPlasticBag)


# Main Headline Results: Cell x Month -----------------------------------------------------------------------

# TWFE 
model <- felm(data = dataMain, percPlasticBag ~ treat | factor(ym) + groupID01 | 0 | zip)
summary(model)
nobs <- nobs(model)
control_mean <- dataMain %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean <- control_mean$mean
results_model <- tidy(model, conf.int=TRUE) %>% mutate(model = "TWFE", space= paste0("0.01 Gridcell\nn = ", nobs)) %>% mutate(estimate = estimate / control_mean, std.error = std.error / control_mean) %>% dplyr::select(model, space, estimate, std.error)

# Borusyak et al. 2021
borusyak <- did_imputation(data = dataMain, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results <- c('Borusyak et al. (2021)', paste0("0.01 Gridcell\nn = ", nobs), borusyak$estimate/control_mean, borusyak$std.error/control_mean)
results_model <- rbind(results_model, borusyak_results)

# Main Headline Results: County x Month -----------------------------------------------------------------------

# TWFE 
modelCounty <- felm(data = dataMainCounty, percPlasticBag ~ treat | factor(ym) + county| 0 | county)
summary(modelCounty)
nobsCounty <- nobs(modelCounty)
control_meanCounty <- dataMain %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_meanCounty <- control_meanCounty$mean
results_modelCounty <- tidy(modelCounty, conf.int=TRUE) %>% mutate(model = "TWFE", space= paste0("County\nn = ", nobsCounty)) %>% mutate(estimate = estimate / control_meanCounty, std.error = std.error / control_meanCounty) %>% dplyr::select(model, space, estimate, std.error)

# Borusyak et al. 2021
borusyakCounty <- did_imputation(data = dataMainCounty, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "county", cluster_var = "county")
borusyak_resultsCounty <- c('Borusyak et al. (2021)', paste0("County\nn = ", nobsCounty), borusyakCounty$estimate/control_meanCounty, borusyakCounty$std.error/control_meanCounty)
results_modelCounty <- rbind(results_modelCounty, borusyak_resultsCounty)

# Main Headline Results: Zip x Month -----------------------------------------------------------------------

# TWFE 
modelZip <- felm(data = dataMainZip, percPlasticBag ~ treat | factor(ym) + zip| 0 | zip)
summary(modelZip)
nobsZip <- nobs(modelZip)
control_meanZip <- dataMain %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_meanZip <- control_meanZip$mean
results_modelZip <- tidy(modelZip, conf.int=TRUE) %>% mutate(model = "TWFE", space= paste0("Zip Code\nn = ", nobsZip)) %>% mutate(estimate = estimate / control_meanZip, std.error = std.error / control_meanZip) %>% dplyr::select(model, space, estimate, std.error)

# Borusyak et al. 2021
borusyakZip <- did_imputation(data = dataMainZip, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "zip", cluster_var = "zip")
borusyak_resultsZip <- c('Borusyak et al. (2021)', paste0("Zip Code\nn = ", nobsZip), borusyakZip$estimate/control_meanZip, borusyakZip$std.error/control_meanZip)
results_modelZip <- rbind(results_modelZip, borusyak_resultsZip)


# Main Headline Results: BigCell x Month -----------------------------------------------------------------------

# TWFE 
modelBig <- felm(data = dataMainBig, percPlasticBag ~ treat | factor(ym) + groupID1 | 0 | zip)
summary(modelBig)
nobsBig <- nobs(modelBig)
control_meanBig <- dataMainBig %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_meanBig <- control_meanBig$mean
results_modelBig <- tidy(modelBig, conf.int=TRUE) %>% mutate(model = "TWFE", space= paste0("0.1 Gridcell\nn = ", nobsBig)) %>% mutate(estimate = estimate / control_meanBig, std.error = std.error / control_meanBig) %>% dplyr::select(model, space, estimate, std.error)

# Borusyak et al. 2021
borusyakBig <- did_imputation(data = dataMainBig, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID1", cluster_var = "zip")
borusyak_resultsBig <- c('Borusyak et al. (2021)', paste0("0.1 Gridcell\nn = ", nobsBig), borusyakBig$estimate/control_meanBig, borusyakBig$std.error/control_meanBig)
results_modelBig <- rbind(results_modelBig, borusyak_resultsBig)


# Main Headline Results: SmallCell x Month -----------------------------------------------------------------------

# TWFE 
modelSmall <- felm(data = dataMainSmall, percPlasticBag ~ treat | factor(ym) + groupID001 | 0 | zip)
summary(modelSmall)
nobsSmall <- nobs(modelSmall)
control_meanSmall <- dataMainSmall %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_meanSmall <- control_meanSmall$mean
results_modelSmall <- tidy(modelSmall, conf.int=TRUE) %>% mutate(model = "TWFE", space= paste0("0.001 Gridcell\nn = ", nobsSmall)) %>% mutate(estimate = estimate / control_meanSmall, std.error = std.error / control_meanSmall) %>% dplyr::select(model, space, estimate, std.error)

# Borusyak et al. 2021
borusyakSmall <- did_imputation(data = dataMainSmall, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID001", cluster_var = "zip")
borusyak_resultsSmall <- c('Borusyak et al. (2021)', paste0("0.001 Gridcell\nn = ", nobsSmall), borusyakSmall$estimate/control_meanSmall, borusyakSmall$std.error/control_meanSmall)
results_modelSmall <- rbind(results_modelSmall, borusyak_resultsSmall)


# Robustness to Time Aggregation -------------------------------------------------------------------

# palette
pal <- pnw_palette("Shuksan2", 5)
palette <- c(pal[1], pal[5])

# combine 
results <- rbind(results_modelSmall, results_model, results_modelBig, results_modelZip, results_modelCounty) 

# add 90th and 99th percentile conf interval 
results <- results %>% mutate(estimate = as.numeric(estimate), std.error = as.numeric(std.error))
results <- results %>% mutate(estimate = estimate * 100, std.error = std.error * 100)
results <- results %>% mutate(conf.low = estimate - 1.96 * std.error, conf.high = estimate + 1.96 * std.error, 
                              conf.low.90 = estimate - 1.65 * std.error,  conf.high.90 = estimate + 1.65 * std.error, 
                              conf.low.99 = estimate - 2.58 * std.error,  conf.high.99 = estimate + 2.58 * std.error)
results <- results %>% rename(term = space)

# create plot 
a <- dwplot(results, dot_args = list(size = 3, aes(shape = model, color = model)), whisker_args = list(size = 0.75), dodge_size = 0.4, model_order = c("TWFE", "Borusyak et al. (2021)"), vars_order = c(paste0("County\nn = ", nobsCounty), paste0("Zip Code\nn = ", nobsZip), paste0("0.1 Gridcell\nn = ", nobsBig), paste0("0.01 Gridcell\nn = ", nobs), paste0("0.001 Gridcell\nn = ", nobsSmall))) + 
  scale_shape_discrete(name = "Estimator") +
  scale_color_manual(values = palette, name = "Estimator") +
  theme_bw() + coord_flip() + xlab("∆ Relative to Control Mean (%)") + ylab(paste0("\nPlastic Bags")) +
  geom_vline(xintercept = 0,colour = "grey60", size=0.2) + 
  xlim(-65, 25)+
  # 90th perc confidence interval 
  geom_segment(aes(y=0.90, yend=0.90, x=as.numeric(results[2,7]), xend = as.numeric(results[2,8])), alpha=0.04, color = palette[1], size = 2)+  
  geom_segment(aes(y=1.1, yend=1.1, x=as.numeric(results[1,7]), xend = as.numeric(results[1,8])), alpha=0.04, color = palette[2], size = 2)+ 
  geom_segment(aes(y=1.90, yend=1.90, x=as.numeric(results[4,7]), xend = as.numeric(results[4,8])), alpha=0.04, color = palette[1], size = 2)+  
  geom_segment(aes(y=2.1, yend=2.1, x=as.numeric(results[3,7]), xend = as.numeric(results[3,8])), alpha=0.04, color = palette[2], size = 2)+ 
  geom_segment(aes(y=2.90, yend=2.90, x=as.numeric(results[6,7]), xend = as.numeric(results[6,8])), alpha=0.04, color = palette[1], size = 2)+  
  geom_segment(aes(y=3.1, yend=3.1, x=as.numeric(results[5,7]), xend = as.numeric(results[5,8])), alpha=0.04, color = palette[2], size = 2)+
  geom_segment(aes(y=3.90, yend=3.90, x=as.numeric(results[8,7]), xend = as.numeric(results[8,8])), alpha=0.04, color = palette[1], size = 2)+  
  geom_segment(aes(y=4.1, yend=4.1, x=as.numeric(results[7,7]), xend = as.numeric(results[7,8])), alpha=0.04, color = palette[2], size = 2)+
  geom_segment(aes(y=4.90, yend=4.90, x=as.numeric(results[10,7]), xend = as.numeric(results[10,8])), alpha=0.04, color = palette[1], size = 2)+  
  geom_segment(aes(y=5.1, yend=5.1, x=as.numeric(results[9,7]), xend = as.numeric(results[9,8])), alpha=0.04, color = palette[2], size = 2)+
  # 99th perc confidence interval 
  geom_segment(aes(y=0.90, yend=0.90, x=as.numeric(results[2,9]), xend = as.numeric(results[2,10])), alpha=0.5, color = palette[1], size = 0.05)+  
  geom_segment(aes(y=1.1, yend=1.1, x=as.numeric(results[1,9]), xend = as.numeric(results[1,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
  geom_segment(aes(y=1.90, yend=1.90, x=as.numeric(results[4,9]), xend = as.numeric(results[4,10])), alpha=0.5, color = palette[1], size = 0.05)+  
  geom_segment(aes(y=2.1, yend=2.1, x=as.numeric(results[3,9]), xend = as.numeric(results[3,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
  geom_segment(aes(y=2.90, yend=2.90, x=as.numeric(results[6,9]), xend = as.numeric(results[6,10])), alpha=0.5, color = palette[1], size = 0.05)+  
  geom_segment(aes(y=3.1, yend=3.1, x=as.numeric(results[5,9]), xend = as.numeric(results[5,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
  geom_segment(aes(y=3.90, yend=3.90, x=as.numeric(results[8,9]), xend = as.numeric(results[8,10])), alpha=0.5, color = palette[1], size = 0.05)+  
  geom_segment(aes(y=4.1, yend=4.1, x=as.numeric(results[7,9]), xend = as.numeric(results[7,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
  geom_segment(aes(y=4.90, yend=4.90, x=as.numeric(results[10,9]), xend = as.numeric(results[10,10])), alpha=0.5, color = palette[1], size = 0.05)+  
  geom_segment(aes(y=5.1, yend=5.1, x=as.numeric(results[9,9]), xend = as.numeric(results[9,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
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

ggsave("figures/appendix/figure_s08_spatial_aggregation.png", width = 8, height = 4.25)







