#############################################################################################
# Plastic Bag Ban
# Robustness checks of main TWFE analysis, balanced panels
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
setwd(paste0(directory, "plasticbag/replication/"))

## figure parameters 
textSize <- 10

# Import Data -------------------------------------------------------------------------------

# big cell x year
load("data/processed/02_data_merged/02_merged_bigcell_year_balanced.rda")
data <- dataBalanced %>% mutate(treat = treatedAny, 
                        firstY = year(firstEffect) )

dataBigYear <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataBigYear <- dataBigYear %>% dplyr::select(type, groupID1, zip, firstY, treat, year, percPlasticBag)
dataBigYear <- dataBigYear %>% ungroup()

rm(data, dataBalanced)

# Balanced Panel: Big Cell x Year -------------------------------------------------------------

# TWFE 
modelBig <- felm(data = dataBigYear, percPlasticBag ~ treat | factor(year) + groupID1 | 0 | zip)
summary(modelBig)
nobsBig <- nobs(modelBig)
control_meanBig <- dataBigYear %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_meanBig <- control_meanBig$mean
results_modelBig <- tidy(modelBig, conf.int=TRUE) %>% mutate(model = "TWFE", location= paste0("0.1 Gridcell\nn = ", nobsBig)) %>% mutate(estimate = estimate / control_meanBig, std.error = std.error / control_meanBig) %>% dplyr::select(model, location, estimate, std.error)

# Borusyak et al. 2021
borusyakBig <- did_imputation(data = dataBigYear, yname = "percPlasticBag", gname = "firstY", tname = "year", idname = "groupID1", cluster_var = "zip")
borusyak_resultsBig <- c('Borusyak et al. (2021)', paste0("0.1 Gridcell\nn = ", nobsBig), borusyakBig$estimate/control_meanBig, borusyakBig$std.error/control_meanBig)
results_modelBig <- rbind(results_modelBig, borusyak_resultsBig)

# Callaway and Sant'anna 2021
dataBigYear <- dataBigYear %>% mutate(firstY = ifelse(is.na(firstY), 0, firstY))
atts <- att_gt(yname = "percPlasticBag", # LHS variable
               tname = "year", # time variable
               idname = "groupID1", # id variable
               gname = "firstY", # first treatment period variable
               data = dataBigYear, # data
               xformla = NULL, # no covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "zip", # cluster level
               panel = TRUE) # whether the data is panel or repeated cross-sectional
agg_effects <- aggte(atts, type = "group")
summary(agg_effects)
coef_csBig <- agg_effects$overall.att/control_meanBig 
se_csBig <- agg_effects$overall.se/control_meanBig 
cs_resultsBig <- c("Callaway and Sant'anna (2021)", paste0("0.1 Gridcell\nn = ", nobsBig), coef_csBig, se_csBig)
results_modelBig <- rbind(results_modelBig, cs_resultsBig)

# Robustness to Time Aggregation -------------------------------------------------------------------

# palette
pal <- pnw_palette("Shuksan2", 5)
palette <- c(pal[1], pal[5], pal[4])

# combine 
results <- results_modelBig

# add 90th and 99th percentile conf interval 
results <- results %>% mutate(estimate = as.numeric(estimate), std.error = as.numeric(std.error))
results <- results %>% mutate(estimate = estimate * 100, std.error = std.error * 100)
results <- results %>% mutate(conf.low = estimate - 1.96 * std.error, conf.high = estimate + 1.96 * std.error, 
                              conf.low.90 = estimate - 1.65 * std.error,  conf.high.90 = estimate + 1.65 * std.error, 
                              conf.low.99 = estimate - 2.58 * std.error,  conf.high.99 = estimate + 2.58 * std.error)
results <- results %>% rename(term = location)

# create plot 
a <- dwplot(results, dot_args = list(size = 3, aes(shape = model, color = model)), whisker_args = list(size = 0.75), dodge_size = 0.4, model_order = rev(c("Borusyak et al. (2021)", "TWFE", "Callaway and Sant'anna (2021)"))) + 
  scale_shape_discrete(name = "Estimator") +
  scale_color_manual(values = palette, name = "Estimator") +
  theme_bw() + coord_flip() + xlab("∆ Relative to Control Mean (%)") + ylab(paste0("\nPlastic Bags")) +
  geom_vline(xintercept = 0,colour = "grey60", size=0.2) + 
  xlim(-125, 40)+
  # 90th perc confidence interval 
  geom_segment(aes(y=0.8675, yend=0.8675, x=as.numeric(results[2,7]), xend = as.numeric(results[2,8])), alpha=0.1, color = palette[1], size = 2)+  
  geom_segment(aes(y=1, yend=1, x=as.numeric(results[1,7]), xend = as.numeric(results[1,8])), alpha=0.1, color = palette[2], size = 2)+ 
  geom_segment(aes(y=1.1325, yend=1.1325, x=as.numeric(results[3,7]), xend = as.numeric(results[3,8])), alpha=0.1, color = palette[3], size = 2)+  
  # 99th perc confidence interval 
  geom_segment(aes(y=0.867, yend=0.867, x=as.numeric(results[2,9]), xend = as.numeric(results[2,10])), alpha=0.5, color = palette[1], size = 0.05)+  
  geom_segment(aes(y=1, yend=1, x=as.numeric(results[1,9]), xend = as.numeric(results[1,10])), alpha=0.5, color = palette[2], size = 0.05)+ 
  geom_segment(aes(y=1.133, yend=1.133, x=as.numeric(results[3,9]), xend = as.numeric(results[3,10])), alpha=0.5, color = palette[3], size = 0.05)+  
  theme(legend.position = c(0.125, 0.875), 
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
a <- a+guides(shape = guide_legend(override.aes = list(color = c(palette[1], palette[2], palette[3]))))+guides(color = 'none')
ggsave("figures/appendix/figure_s09_balanced.png", width = 8, height = 4.25)

# Dynamic TWFE -------------------------------------------------------------------

dataBigYear <- dataBigYear %>% group_by(groupID1) %>% mutate(treatEver = sum(treat)) %>% ungroup()
dataBigYear <- dataBigYear %>% mutate(treatEver = ifelse(treatEver > 0, 1, 0))
dataBigYear <- dataBigYear %>% mutate(timeToTreat = ifelse(treatEver == 1, year - firstY, 0))
dataBigYear <- dataBigYear %>% mutate(yearTreated = ifelse(treatEver == 0, 10000, firstY))
dataBigYear <- dataBigYear %>% filter(timeToTreat >= -4) 

# regular TWFE 
modelDynamic <- feols(percPlasticBag ~ i(timeToTreat, treatEver, ref = -1) | groupID1 + year,  cluster = ~groupID1, data = dataBigYear)

# Sun and Abraham event study 
modelDynamicSA <- feols(percPlasticBag ~ sunab(yearTreated, year) | groupID1 + year,  cluster = ~groupID1, data = dataBigYear)

# plot 
png(file = "figures/appendix/figure_s10_dynamic.png", width = 8, height = 6, units = "in", res = 300) 
iplot(
  list(modelDynamic, modelDynamicSA),
  sep = 0.5,
  ref.line = -1,
  xlab = 'Time to Treatment',
  ylab = "Plastic Bags (% of Total Items)",
  main = " ",
  col = c(palette[2], "#88CCEE")
)
legend(
  "bottomleft",
  col = c(palette[2], "#88CCEE"),  
  pch = c(20, 17),
  legend = c("TWFE", "Sun & Abraham (2020)")
)
dev.off()

