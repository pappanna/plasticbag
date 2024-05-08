#############################################################################################
# Plastic Bag Ban
# State details 
# last modified: 03/06/24
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

# cleanup data (for cell - state walkthrough )
load("data/processed/00_data_cleanup.rda")
cleanup <- cleanup %>% distinct(groupID01, state)
states <- cleanup 
states <- states %>% mutate(ind = 1) 
states <- states %>% group_by(groupID01) %>% summarise(sum = sum(ind), state = first(state)) %>% ungroup()
states <- states %>% filter(sum == 1) %>% dplyr::select(-sum)
rm(cleanup)

# all
load("data/processed/02_data_merged/02_merged_cell_month.rda")
data <- data %>% mutate(treat = treatedAny, 
                        ym = month + (year - 2016)*12, 
                        firstYM = month(firstEffect) + (year(firstEffect)-2016)*12 )

controlMean <- data %>% filter(is.na(firstYM)) 
mean <- mean(controlMean$percPlasticBag, na.rm=TRUE)
rm(controlMean)

dataMain <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataMain <- dataMain %>% filter((lastPolicyGeo == firstPolicyGeo) | ((is.na(lastPolicyGeo) & is.na(firstPolicyGeo) & is.na(lastPolicyType) & is.na(firstPolicyType))))
dataMain <- dataMain %>% dplyr::select(type, groupID01, zip, county, firstYM, treat, ym, firstPolicyType, firstPolicyGeo, percPlasticBag)

# merge with states 
dataMain <- left_join(dataMain, states, by="groupID01")

# which states to keep 
stateKeep <- dataMain %>% mutate(ind = ifelse(!is.na(firstPolicyType), 1, 0))
stateKeep <- stateKeep %>% group_by(state) %>% summarise(sum = sum(ind))

# only keep coastal states without any legislation and coastal states with state-level legislation 
dataMain <- dataMain %>% filter(state %in% c("TX", "LA", "AL", "GA", "FL", "WA", "OR", "ME", "DE", "CT", "NJ", "CO", "NY", "VT"))
dataMain <- dataMain %>% filter(is.na(firstPolicyGeo) | firstPolicyGeo == "state")

# Regressions -------------------------------------------------------------------------------

# All states, including control 
model <- felm(data = dataMain, percPlasticBag ~ treat | factor(ym) + groupID01 | 0 | zip)
summary(model)
nobs <- nobs(model)[1]
results_model <- tidy(model, conf.int=TRUE) %>% mutate(model = "TWFE", cleanups = paste0("All\n n = ", nobs)) %>% mutate(estimate = estimate * 100, std.error = std.error * 100, conf.low = conf.low * 100, conf.high = conf.high * 100)

borusyak <- did_imputation(data = dataMain, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
coef_borusyak <- borusyak$estimate * 100
se_borusyak <- borusyak$std.error * 100

# WA
dataMainWA <- dataMain %>% filter(state %in% c("TX", "LA", "AL", "GA", "FL", "WA"))
model <- felm(data = dataMainWA, percPlasticBag ~ treat | factor(ym) + groupID01 | 0 | zip)
summary(model)
nobsWA <- nobs(model)[1]
results_modelWA <- tidy(model, conf.int=TRUE) %>% mutate(model = "TWFE", cleanups = paste0("WA\n n = ", nobsWA)) %>% mutate(estimate = estimate * 100, std.error = std.error * 100, conf.low = conf.low * 100, conf.high = conf.high * 100)

borusyak <- did_imputation(data = dataMainWA, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
coef_borusyakWA <- borusyak$estimate * 100
se_borusyakWA <- borusyak$std.error * 100

# OR
dataMainOR <- dataMain %>% filter(state %in% c("TX", "LA", "AL", "GA", "FL", "OR"))
model <- felm(data = dataMainOR, percPlasticBag ~ treat | factor(ym) + groupID01 | 0 | zip)
summary(model)
nobsOR <- nobs(model)[1]
results_modelOR <- tidy(model, conf.int=TRUE) %>% mutate(model = "TWFE", cleanups = paste0("OR\n n = ", nobsOR)) %>% mutate(estimate = estimate * 100, std.error = std.error * 100, conf.low = conf.low * 100, conf.high = conf.high * 100)

borusyak <- did_imputation(data = dataMainOR, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
coef_borusyakOR <- borusyak$estimate * 100
se_borusyakOR <- borusyak$std.error * 100

# ME
dataMainME <- dataMain %>% filter(state %in% c("TX", "LA", "AL", "GA", "FL", "ME"))
model <- felm(data = dataMainME, percPlasticBag ~ treat | factor(ym) + groupID01 | 0 | zip)
summary(model)
nobsME <- nobs(model)[1]
results_modelME <- tidy(model, conf.int=TRUE) %>% mutate(model = "TWFE", cleanups = paste0("ME\n n = ", nobsME)) %>% mutate(estimate = estimate * 100, std.error = std.error * 100, conf.low = conf.low * 100, conf.high = conf.high * 100)

borusyak <- did_imputation(data = dataMainME, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
coef_borusyakME <- borusyak$estimate * 100
se_borusyakME <- borusyak$std.error * 100

# DE
dataMainDE <- dataMain %>% filter(state %in% c("TX", "LA", "AL", "GA", "FL", "DE"))
model <- felm(data = dataMainDE, percPlasticBag ~ treat | factor(ym) + groupID01 | 0 | zip)
summary(model)
nobsDE <- nobs(model)[1]
results_modelDE <- tidy(model, conf.int=TRUE) %>% mutate(model = "TWFE", cleanups = paste0("DE\n n = ", nobsDE)) %>% mutate(estimate = estimate * 100, std.error = std.error * 100, conf.low = conf.low * 100, conf.high = conf.high * 100)

borusyak <- did_imputation(data = dataMainDE, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
coef_borusyakDE <- borusyak$estimate * 100
se_borusyakDE <- borusyak$std.error * 100

# CT
dataMainCT <- dataMain %>% filter(state %in% c("TX", "LA", "AL", "GA", "FL", "CT"))
model <- felm(data = dataMainCT, percPlasticBag ~ treat | factor(ym) + groupID01 | 0 | zip)
summary(model)
nobsCT <- nobs(model)[1]
results_modelCT <- tidy(model, conf.int=TRUE) %>% mutate(model = "TWFE", cleanups = paste0("CT\n n = ", nobsCT)) %>% mutate(estimate = estimate * 100, std.error = std.error * 100, conf.low = conf.low * 100, conf.high = conf.high * 100)

borusyak <- did_imputation(data = dataMainCT, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
coef_borusyakCT <- borusyak$estimate * 100
se_borusyakCT <- borusyak$std.error * 100

# NJ
dataMainNJ <- dataMain %>% filter(state %in% c("TX", "LA", "AL", "GA", "FL", "NJ"))
model <- felm(data = dataMainNJ, percPlasticBag ~ treat | factor(ym) + groupID01 | 0 | zip)
summary(model)
nobsNJ <- nobs(model)[1]
results_modelNJ <- tidy(model, conf.int=TRUE) %>% mutate(model = "TWFE", cleanups = paste0("NJ\n n = ", nobsNJ)) %>% mutate(estimate = estimate * 100, std.error = std.error * 100, conf.low = conf.low * 100, conf.high = conf.high * 100)

borusyak <- did_imputation(data = dataMainNJ, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
coef_borusyakNJ <- borusyak$estimate * 100
se_borusyakNJ <- borusyak$std.error * 100

# NY
dataMainNY <- dataMain %>% filter(state %in% c("TX", "LA", "AL", "GA", "FL", "NY"))
model <- felm(data = dataMainNY, percPlasticBag ~ treat | factor(ym) + groupID01 | 0 | zip)
summary(model)
nobsNY <- nobs(model)[1]
results_modelNY <- tidy(model, conf.int=TRUE) %>% mutate(model = "TWFE", cleanups = paste0("NY\n n = ", nobsNY)) %>% mutate(estimate = estimate * 100, std.error = std.error * 100, conf.low = conf.low * 100, conf.high = conf.high * 100)

borusyak <- did_imputation(data = dataMainNY, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
coef_borusyakNY <- borusyak$estimate * 100
se_borusyakNY <- borusyak$std.error * 100

# VT
dataMainVT <- dataMain %>% filter(state %in% c("TX", "LA", "AL", "GA", "FL", "VT"))
model <- felm(data = dataMainVT, percPlasticBag ~ treat | factor(ym) + groupID01 | 0 | zip)
summary(model)
nobsVT <- nobs(model)[1]
results_modelVT <- tidy(model, conf.int=TRUE) %>% mutate(model = "TWFE", cleanups = paste0("VT\n n = ", nobsVT)) %>% mutate(estimate = estimate * 100, std.error = std.error * 100, conf.low = conf.low * 100, conf.high = conf.high * 100)

borusyak <- did_imputation(data = dataMainVT, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
coef_borusyakVT <- borusyak$estimate * 100
se_borusyakVT <- borusyak$std.error * 100

# Figure ------------------------------------------------------------------------------------

pal <- pnw_palette("Starfish", 8)

results <- rbind(results_model, results_modelWA, results_modelOR, results_modelME, results_modelVT, results_modelCT, results_modelNY, results_modelNJ, results_modelDE) %>% dplyr::select(-c(term, p.value, statistic, conf.low, conf.high)) %>% mutate(term = cleanups) %>% dplyr::select(-c(cleanups))

# add other estimates
models <- c("Borusyak et al. (2021)", "Borusyak et al. (2021)", "Borusyak et al. (2021)", "Borusyak et al. (2021)", "Borusyak et al. (2021)", "Borusyak et al. (2021)", "Borusyak et al. (2021)", "Borusyak et al. (2021)", "Borusyak et al. (2021)")
estimates <- c(coef_borusyak, coef_borusyakWA, coef_borusyakOR, coef_borusyakME, coef_borusyakVT, coef_borusyakCT, coef_borusyakNY, coef_borusyakNJ, coef_borusyakDE)
ses <- c(se_borusyak, se_borusyakWA, se_borusyakOR, se_borusyakME, se_borusyakVT, se_borusyakCT, se_borusyakNY, se_borusyakNJ, se_borusyakDE)
level <-c(paste0("All\n n = ", nobs), paste0("WA\n n = ", nobsWA), paste0("OR\n n = ", nobsOR), paste0("ME\n n = ", nobsME), paste0("VT\n n = ", nobsVT), paste0("CT\n n = ", nobsCT), paste0("NY\n n = ", nobsNY), paste0("NJ\n n = ", nobsNJ), paste0("DE\n n = ", nobsDE) )
add <- tibble(term = level, estimate = estimates, std.error = ses,  model = models)
results <- rbind(results, add)

# calculate control mean 
control <- dataMain %>% filter(state %in% c("TX", "LA", "AL", "GA", "FL"))
mean <- mean(control$percPlasticBag)*100

# divide estimate / ses by mean 
results <- results %>% mutate(estimate = estimate / mean * 100, std.error = std.error / mean * 100)

# add 90th and 99th percentile conf interval 
results <- results %>% mutate(conf.low = estimate - 1.96 * std.error,  conf.high = estimate + 1.96 * std.error,
                              conf.low.90 = estimate - 1.65 * std.error,  conf.high.90 = estimate + 1.65 * std.error, 
                              conf.low.99 = estimate - 2.58 * std.error,  conf.high.99 = estimate + 2.58 * std.error)

# create plot 
a<-dwplot(results, dot_args = list(size = 3, aes(shape = model, color = term)), whisker_args = list(size = 0.75, aes(color = term)), dodge_size = 0.4, model_order = c("TWFE", "Borusyak et al. (2021)"), vars_order = rev(c(paste0("All\n n = ", nobs), paste0("WA\n n = ", nobsWA), paste0("OR\n n = ", nobsOR), paste0("ME\n n = ", nobsME), paste0("VT\n n = ", nobsVT), paste0("CT\n n = ", nobsCT), paste0("NY\n n = ", nobsNY), paste0("NJ\n n = ", nobsNJ), paste0("DE\n n = ", nobsDE)))) + 
  theme_bw() + coord_flip() + xlab("\nPlastic Grocery Bags\n∆ Rel. to Control Mean (%)") +
  geom_vline(xintercept = 0,colour = "grey60",linetype = 2) + 
  scale_shape_discrete(name = "Model") +
  scale_color_manual(name = "Type", values=c(pal[8],pal[7],pal[6],pal[5],pal[4],pal[3], pal[2], pal[1], "black"), guide = "none") +
  # 90th perc confidence interval 
  geom_segment(aes(y=0.90, yend=0.90, x=as.numeric(results[10,7]), xend = as.numeric(results[10,8])), alpha=0.04, color = "black", size = 2.5)+  # borusyak, All
  geom_segment(aes(y=1.1, yend=1.1, x=as.numeric(results[1,7]), xend = as.numeric(results[1,8])), alpha=0.04, color = "black", size = 2.5)+ # TWFE, All
  geom_segment(aes(y=1.90, yend=1.90, x=as.numeric(results[11,7]), xend = as.numeric(results[11,8])), alpha=0.04, color = pal[1], size = 2.5)+  # borusyak, All
  geom_segment(aes(y=2.1, yend=2.1, x=as.numeric(results[2,7]), xend = as.numeric(results[2,8])), alpha=0.04, color = pal[1], size = 2.5)+ # TWFE, All
  geom_segment(aes(y=2.90, yend=2.90, x=as.numeric(results[12,7]), xend = as.numeric(results[12,8])), alpha=0.04, color = pal[2], size = 2.5)+ # borusyak, coast
  geom_segment(aes(y=3.1, yend=3.1, x=as.numeric(results[3,7]), xend = as.numeric(results[3,8])), alpha=0.04, color = pal[2], size = 2.5)+ # twfe, coast
  geom_segment(aes(y=3.90, yend=3.90, x=as.numeric(results[13,7]), xend = as.numeric(results[13,8])), alpha=0.04, color = pal[3], size = 2.5)+ # borusyak, river
  geom_segment(aes(y=4.1, yend=4.1, x=as.numeric(results[4,7]), xend = as.numeric(results[4,8])), alpha=0.04, color = pal[3], size = 2.5)+ # twfe, river
  geom_segment(aes(y=4.90, yend=4.90, x=as.numeric(results[14,7]), xend = as.numeric(results[14,8])), alpha=0.04, color = pal[4], size = 2.5)+ # borusyak, river
  geom_segment(aes(y=5.1, yend=5.1, x=as.numeric(results[5,7]), xend = as.numeric(results[5,8])), alpha=0.04, color = pal[4], size = 2.5)+ # twfe, river
  geom_segment(aes(y=5.90, yend=5.90, x=as.numeric(results[15,7]), xend = as.numeric(results[15,8])), alpha=0.04, color = pal[5], size = 2.5)+ # borusyak, river
  geom_segment(aes(y=6.1, yend=6.1, x=as.numeric(results[6,7]), xend = as.numeric(results[6,8])), alpha=0.04, color = pal[5], size = 2.5)+ # twfe, river
  geom_segment(aes(y=6.90, yend=6.90, x=as.numeric(results[16,7]), xend = as.numeric(results[16,8])), alpha=0.04, color = pal[6], size = 2.5)+ # borusyak, river
  geom_segment(aes(y=7.1, yend=7.1, x=as.numeric(results[7,7]), xend = as.numeric(results[7,8])), alpha=0.04, color = pal[6], size = 2.5)+ # twfe, river
  geom_segment(aes(y=7.90, yend=7.90, x=as.numeric(results[17,7]), xend = as.numeric(results[17,8])), alpha=0.04, color = pal[7], size = 2.5)+ # borusyak, river
  geom_segment(aes(y=8.1, yend=8.1, x=as.numeric(results[8,7]), xend = as.numeric(results[8,8])), alpha=0.04, color = pal[7], size = 2.5)+ # twfe, river
  geom_segment(aes(y=8.90, yend=8.90, x=as.numeric(results[18,7]), xend = as.numeric(results[18,8])), alpha=0.04, color = pal[8], size = 2.5)+ # borusyak, river
  geom_segment(aes(y=9.1, yend=9.1, x=as.numeric(results[9,7]), xend = as.numeric(results[9,8])), alpha=0.04, color = pal[8], size = 2.5)+ # twfe, river
  # 99th percentile confidence interval 
  geom_segment(aes(y=0.90, yend=0.90, x=as.numeric(results[10,9]), xend = as.numeric(results[10,10])), alpha=0.5, color = "black", size = 0.05)+  # borusyak, All
  geom_segment(aes(y=1.1, yend=1.1, x=as.numeric(results[1,9]), xend = as.numeric(results[1,10])), alpha=0.5, color = "black", size = 0.05)+ # TWFE, All
  geom_segment(aes(y=1.90, yend=1.90, x=as.numeric(results[11,9]), xend = as.numeric(results[11,10])), alpha=0.5, color = pal[1], size = 0.05)+  # borusyak, All
  geom_segment(aes(y=2.1, yend=2.1, x=as.numeric(results[2,9]), xend = as.numeric(results[2,10])), alpha=0.5, color = pal[1], size = 0.05)+ # TWFE, All
  geom_segment(aes(y=2.90, yend=2.90, x=as.numeric(results[12,9]), xend = as.numeric(results[12,10])), alpha=0.5, color = pal[2], size = 0.05)+ # borusyak, coast
  geom_segment(aes(y=3.1, yend=3.1, x=as.numeric(results[3,9]), xend = as.numeric(results[3,10])), alpha=0.5, color = pal[2], size = 0.05)+ # twfe, coast
  geom_segment(aes(y=3.90, yend=3.90, x=as.numeric(results[13,9]), xend = as.numeric(results[13,10])), alpha=0.5, color = pal[3], size = 0.05)+ # borusyak, river
  geom_segment(aes(y=4.1, yend=4.1, x=as.numeric(results[4,9]), xend = as.numeric(results[4,10])), alpha=0.5, color = pal[3], size = 0.05)+ # twfe, river
  geom_segment(aes(y=4.90, yend=4.90, x=as.numeric(results[14,9]), xend = as.numeric(results[14,10])), alpha=0.5, color = pal[4], size = 0.05)+ # borusyak, river
  geom_segment(aes(y=5.1, yend=5.1, x=as.numeric(results[5,9]), xend = as.numeric(results[5,10])), alpha=0.5, color = pal[4], size = 0.05)+ # twfe, river
  geom_segment(aes(y=5.90, yend=5.90, x=as.numeric(results[15,9]), xend = as.numeric(results[15,10])), alpha=0.5, color = pal[5], size = 0.05)+ # borusyak, river
  geom_segment(aes(y=6.1, yend=6.1, x=as.numeric(results[6,9]), xend = as.numeric(results[6,10])), alpha=0.5, color = pal[5], size = 0.05)+ # twfe, river
  geom_segment(aes(y=6.90, yend=6.90, x=as.numeric(results[16,9]), xend = as.numeric(results[16,10])), alpha=0.5, color = pal[6], size = 0.05)+ # borusyak, river
  geom_segment(aes(y=7.1, yend=7.1, x=as.numeric(results[7,9]), xend = as.numeric(results[7,10])), alpha=0.5, color = pal[6], size = 0.05)+ # twfe, river
  geom_segment(aes(y=7.90, yend=7.90, x=as.numeric(results[17,9]), xend = as.numeric(results[17,10])), alpha=0.5, color = pal[7], size = 0.05)+ # borusyak, river
  geom_segment(aes(y=8.1, yend=8.1, x=as.numeric(results[8,9]), xend = as.numeric(results[8,10])), alpha=0.5, color = pal[7], size = 0.05)+ # twfe, river
  geom_segment(aes(y=8.90, yend=8.90, x=as.numeric(results[18,9]), xend = as.numeric(results[18,10])), alpha=0.5, color = pal[8], size = 0.05)+ # borusyak, river
  geom_segment(aes(y=9.1, yend=9.1, x=as.numeric(results[9,9]), xend = as.numeric(results[9,10])), alpha=0.5, color = pal[8], size = 0.05)+ # twfe, river
  theme(legend.position = c(0.15, 0.90),
        axis.text.x=element_text(size = textSize), 
        axis.text.y=element_text(size = textSize), 
        axis.title.x=element_text(size = textSize), 
        axis.title.y=element_text(size = textSize),
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
a <- a+guides(color = "none")
ggsave("figures/appendix/figure_s13_states.png", width = 8, height = 4, units = "in")


