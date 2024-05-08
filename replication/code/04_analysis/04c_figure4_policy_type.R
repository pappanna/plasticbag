#############################################################################################
# Plastic Bag Ban
# Heterogeneity by policy type 
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

# all
load("data/processed/02_data_merged/02_merged_cell_month.rda")
data <- data %>% mutate(treat = treatedAny, 
                        ym = month + (year - 2016)*12, 
                        firstYM = month(firstEffect) + (year(firstEffect)-2016)*12 )

controlMean <- data %>% filter(is.na(firstYM)) 
mean <- mean(controlMean$percPlasticBag, na.rm=TRUE)
rm(controlMean)

dataMain <- data %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataMain <- dataMain %>% filter((lastPolicyGeo == firstPolicyGeo) & (lastPolicyType == firstPolicyType) | (is.na(lastPolicyGeo) & is.na(firstPolicyGeo) & is.na(lastPolicyType) & is.na(firstPolicyType)))
dataMain <- dataMain %>% dplyr::select(type, groupID01, zip, county, firstYM, treat, ym, firstPolicyType, firstPolicyGeo, percPlasticBag)

treated <- nrow(dataMain %>% filter(treat == 1) %>% distinct(groupID01))

# level of policy
dataState <- dataMain %>% filter(firstPolicyGeo == "state" | (is.na(firstPolicyGeo)))
dataCounty <- dataMain %>% filter(firstPolicyGeo == "county"| (is.na(firstPolicyGeo)))
dataTown <- dataMain %>% filter(firstPolicyGeo == "town"| (is.na(firstPolicyGeo)))

treatedState <- nrow(dataState %>% filter(treat == 1) %>% distinct(groupID01))
treatedCounty <- nrow(dataCounty %>% filter(treat == 1) %>% distinct(groupID01))
treatedTown <- nrow(dataTown %>% filter(treat == 1) %>% distinct(groupID01))

# tax or ban 
dataTax <- dataMain %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "control"))
dataBan <- dataMain %>% filter(type %in% c("completeCharge", "completeNoCharge", "control"))
dataPartialBan <- dataMain %>% filter(type %in% c("partialCharge", "partialNoCharge", "control"))

treatedTax <- nrow(dataTax %>% filter(treat == 1) %>% distinct(groupID01))
treatedBan <- nrow(dataBan %>% filter(treat == 1) %>% distinct(groupID01))
treatedPartialBan <- nrow(dataPartialBan %>% filter(treat == 1) %>% distinct(groupID01))

# tax or ban by geo 
dataStateTax <- dataState %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "control"))
dataStateBan <- dataState %>% filter(type %in% c("completeCharge", "completeNoCharge", "partialCharge", "partialNoCharge", "control"))
dataStatePartialBan <- dataState %>% filter(type %in% c("partialCharge", "partialNoCharge", "control"))

dataCountyTax <- dataCounty %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "control"))
dataCountyBan <- dataCounty %>% filter(type %in% c("completeCharge", "completeNoCharge", "control"))
dataCountyPartialBan <- dataCounty %>% filter(type %in% c("partialCharge", "partialNoCharge", "control"))

dataTownTax <- dataTown %>% filter(type %in% c("chargeCharge", "chargeNoCharge", "control"))
dataTownBan <- dataTown %>% filter(type %in% c("completeCharge", "completeNoCharge", "control"))
dataTownPartialBan <- dataTown %>% filter(type %in% c("partialCharge", "partialNoCharge", "control"))

# All  Results -----------------------------------------------------------------------

# TWFE 
model <- felm(data = dataMain, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model)
control_mean <- dataMain %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean <- control_mean$mean
results_model <- tidy(model, conf.int=TRUE) %>% mutate(model = "TWFE", item = "All") %>% mutate(estimate = estimate / control_mean, std.error = std.error / control_mean) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak <- did_imputation(data = dataMain, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results <- c('Borusyak et al. (2021)', "All", borusyak$estimate/control_mean, borusyak$std.error/control_mean)
results_model <- rbind(results_model, borusyak_results)

# State  Results -----------------------------------------------------------------------

# TWFE 
model_state <- felm(data = dataState, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_state)
control_mean_state <- dataState %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean_state <- control_mean_state$mean
results_model_state <- tidy(model_state, conf.int=TRUE) %>% mutate(model = "TWFE", item = "State") %>% mutate(estimate = estimate / control_mean_state, std.error = std.error / control_mean_state) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_state <- did_imputation(data = dataState, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_state <- c('Borusyak et al. (2021)', "State", borusyak_state$estimate/control_mean_state, borusyak_state$std.error/control_mean_state)
results_model_state <- rbind(results_model_state, borusyak_results_state)

# County  Results -----------------------------------------------------------------------

# TWFE 
model_county <- felm(data = dataCounty, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_county)
control_mean_county <- dataCounty %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean_county <- control_mean_county$mean
results_model_county <- tidy(model_county, conf.int=TRUE) %>% mutate(model = "TWFE", item = "County") %>% mutate(estimate = estimate / control_mean_county, std.error = std.error / control_mean_county) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_county <- did_imputation(data = dataCounty, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_county <- c('Borusyak et al. (2021)', "County", borusyak_county$estimate/control_mean_county, borusyak_county$std.error/control_mean_county)
results_model_county <- rbind(results_model_county, borusyak_results_county)

# Town  Results -----------------------------------------------------------------------

# TWFE 
model_town <- felm(data = dataTown, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_town)
control_mean_town <- dataTown %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean_town <- control_mean_town$mean
results_model_town <- tidy(model_town, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Town") %>% mutate(estimate = estimate / control_mean_town, std.error = std.error / control_mean_town) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_town <- did_imputation(data = dataTown, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_town <- c('Borusyak et al. (2021)', "Town", borusyak_town$estimate/control_mean_town, borusyak_town$std.error/control_mean_town)
results_model_town <- rbind(results_model_town, borusyak_results_town)

# Level of Policy -------------------------------------------------------------------

pal <- pnw_palette("Shuksan", 3)

results <- rbind(results_model, results_model_state, results_model_county, results_model_town) 

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
a <- dwplot(results, dot_args = list(size = 3, aes(color = term)), whisker_args = list(size = 0.75, aes(color = term)), dodge_size = 0.4, vars_order = rev(c('All', 'State', 'County', 'Town')))+ 
  theme_bw() + coord_flip() + xlab("\nPlastic Grocery Bags\n∆ Rel. to Control Mean (%)") +
  geom_vline(xintercept = 0,colour = "grey60", size = 0.2) + 
  geom_vline(xintercept =  -27.300135,colour = 'black', size = 0.2, linetype = 2) + 
  scale_shape_discrete(name = "Model") +
  scale_color_manual(name = "Type", values=rev(c('black', pal[1], pal[2], pal[3])), guide = "none") +
  # 90th perc confidence interval 
  geom_segment(aes(y=1, yend=1, x=as.numeric(results[1,7]), xend = as.numeric(results[1,8])), alpha=0.1, color = 'black', size = 2)+  
  geom_segment(aes(y=2, yend=2, x=as.numeric(results[2,7]), xend = as.numeric(results[2,8])), alpha=0.1, color = pal[1], size = 2)+ 
  geom_segment(aes(y=3, yend=3, x=as.numeric(results[3,7]), xend = as.numeric(results[3,8])), alpha=0.1, color = pal[2], size = 2)+  
  geom_segment(aes(y=4, yend=4, x=as.numeric(results[4,7]), xend = as.numeric(results[4,8])), alpha=0.1, color = pal[3], size = 2)+ 
  # 99th perc confidence interval 
  geom_segment(aes(y=1, yend=1, x=as.numeric(results[1,9]), xend = as.numeric(results[1,10])), alpha=0.75, color = 'black', size = 0.05)+  
  geom_segment(aes(y=2, yend=2, x=as.numeric(results[2,9]), xend = as.numeric(results[2,10])), alpha=0.75, color = pal[1], size = 0.05)+ 
  geom_segment(aes(y=3, yend=3, x=as.numeric(results[3,9]), xend = as.numeric(results[3,10])), alpha=0.75, color = pal[2], size = 0.05)+  
  geom_segment(aes(y=4, yend=4, x=as.numeric(results[4,9]), xend = as.numeric(results[4,10])), alpha=0.75, color = pal[3], size = 0.05)+ 
  theme(legend.position = "none", 
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

# create a bar graph with number of treated grid cells under 
size <- data.frame(c('All', 'State', 'County', 'Town'), c(treated/1000, treatedState/1000, treatedCounty/1000, treatedTown/1000))
colnames(size) <- c('term', 'treated')
size <- size %>% mutate(term = factor(term, levels = c('All', 'State', 'County', 'Town')))

# now bar graph from size 
b <- ggplot(size, aes(x = term, y = treated, fill = term)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(name = "Type", values=(c('black', pal[1], pal[2], pal[3])), guide = "none") +
  theme_bw() + xlab("Geographic Scope of Policy") + ylab("Treated Cells\n(Thousands)") +
  scale_y_continuous(breaks=c(0, 1))+
  theme(legend.position = "none", 
        axis.text.x=element_text(size = textSize), 
        axis.text.y=element_text(size = textSize), 
        axis.title.x=element_text(size = textSize), 
        axis.title.y=element_text(size = textSize, margin=margin(r=-10, t = 20)),
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
        axis.ticks.length = unit(0.15, "cm"), 
        panel.background = element_rect(fill='grey89', colour='grey89'))

plot <- ggarrange(a, NULL, b, ncol = 1, align = "v", heights = c(2.25, -0.3, 1))

ggsave("figures/figure_4_parta.png", width = 4, height = 3.5, units = "in")

# Tax vs. Ban  Results -----------------------------------------------------------------------

## TAX ~~~~~~~~~

# TWFE 
model_tax <- felm(data = dataTax, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_tax)
control_mean_tax <- dataTax %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean_tax <- control_mean_tax$mean
results_model_tax <- tidy(model_tax, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Tax") %>% mutate(estimate = estimate / control_mean_tax, std.error = std.error / control_mean_tax) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_tax <- did_imputation(data = dataTax, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_tax <- c('Borusyak et al. (2021)', "Tax", borusyak_tax$estimate/control_mean_tax, borusyak_tax$std.error/control_mean_tax)
results_model_tax <- rbind(results_model_tax, borusyak_results_tax)

## BAN ~~~~~~~~~ -----

# TWFE 
model_ban <- felm(data = dataBan, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_ban)
control_mean_ban <- dataBan %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean_ban <- control_mean_ban$mean
results_model_ban <- tidy(model_ban, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Ban") %>% mutate(estimate = estimate / control_mean_ban, std.error = std.error / control_mean_ban) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_ban <- did_imputation(data = dataBan, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_ban <- c('Borusyak et al. (2021)', "Ban", borusyak_ban$estimate/control_mean_ban, borusyak_ban$std.error/control_mean_ban)
results_model_ban <- rbind(results_model_ban, borusyak_results_ban)


## PARTIAL BAN ~~~~~~~~~ ----- 

# TWFE 
model_partialban <- felm(data = dataPartialBan, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_partialban)
control_mean_partialban <- dataBan %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean_partialban <- control_mean_partialban$mean
results_model_partialban <- tidy(model_partialban, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Partial Ban") %>% mutate(estimate = estimate / control_mean_partialban, std.error = std.error / control_mean_partialban) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_partialban <- did_imputation(data = dataPartialBan, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_partialban <- c('Borusyak et al. (2021)', "Partial Ban", borusyak_partialban$estimate/control_mean_partialban, borusyak_partialban$std.error/control_mean_partialban)
results_model_partialban <- rbind(results_model_partialban, borusyak_results_partialban)

# Plot Type of Policy -------------------------------------------------------------------

pal <- pnw_palette("Bay", 5)
pal <- c('black', pal[1], pal[2], pal[5])

results <- rbind(results_model, results_model_tax, results_model_ban, results_model_partialban) 


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
a <- dwplot(results, dot_args = list(size = 3, aes(color = term)), whisker_args = list(size = 0.75, aes(color = term)), dodge_size = 0.4, vars_order = rev(c('All', 'Ban', 'Partial Ban', 'Tax')))+ 
  theme_bw() + coord_flip() + xlab("\nPlastic Grocery Bags\n∆ Rel. to Control Mean (%)") +
  geom_vline(xintercept = 0,colour = "grey60", size = 0.2) + 
  geom_vline(xintercept =  -27.300135,colour = 'black', size = 0.2, linetype = 2) + 
  scale_shape_discrete(name = "Model") +
  scale_color_manual(name = "Type", values=rev(pal), guide = "none") +
  # 90th perc confidence interval 
  geom_segment(aes(y=1, yend=1, x=as.numeric(results[1,7]), xend = as.numeric(results[1,8])), alpha=0.1, color = 'black', size = 2)+  
  geom_segment(aes(y=2, yend=2, x=as.numeric(results[3,7]), xend = as.numeric(results[3,8])), alpha=0.1, color = pal[2], size = 2)+ 
  geom_segment(aes(y=3, yend=3, x=as.numeric(results[4,7]), xend = as.numeric(results[4,8])), alpha=0.1, color = pal[3], size = 2)+  
  geom_segment(aes(y=4, yend=4, x=as.numeric(results[2,7]), xend = as.numeric(results[2,8])), alpha=0.1, color = pal[4], size = 2)+ 
  # 99th perc confidence interval 
  geom_segment(aes(y=1, yend=1, x=as.numeric(results[1,9]), xend = as.numeric(results[1,10])), alpha=0.75, color = 'black', size = 0.05)+  
  geom_segment(aes(y=2, yend=2, x=as.numeric(results[3,9]), xend = as.numeric(results[3,10])), alpha=0.75, color = pal[2], size = 0.05)+ 
  geom_segment(aes(y=3, yend=3, x=as.numeric(results[4,9]), xend = as.numeric(results[4,10])), alpha=0.75, color = pal[3], size = 0.05)+  
  geom_segment(aes(y=4, yend=4, x=as.numeric(results[2,9]), xend = as.numeric(results[2,10])), alpha=0.75, color = pal[4], size = 0.05)+ 
  theme(legend.position = "none", 
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

# create a bar graph with number of treated grid cells under 
size <- data.frame(c('All', 'Ban', 'Partial Ban', 'Tax'), c(treated/1000, treatedBan/1000, treatedPartialBan/1000, treatedTax/1000))
colnames(size) <- c('term', 'treated')
size <- size %>% mutate(term = factor(term, levels = c('All', 'Ban', 'Partial Ban', 'Tax')))

# now bar graph from size 
b <- ggplot(size, aes(x = term, y = treated, fill = term)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(name = "Type", values=(c('black', pal[2], pal[3], pal[4])), guide = "none") +
  theme_bw() + xlab("Type of Policy") + ylab("Treated Cells\n(Thousands)") +
  scale_y_continuous(breaks=c(0, 1))+
  theme(legend.position = "none", 
        axis.text.x=element_text(size = textSize), 
        axis.text.y=element_text(size = textSize), 
        axis.title.x=element_text(size = textSize), 
        axis.title.y=element_text(size = textSize, margin=margin(r=-10, t = 20)),
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
        axis.ticks.length = unit(0.15, "cm"), 
        panel.background = element_rect(fill='grey89', colour='grey89'))

plot <- ggarrange(a, NULL, b, ncol = 1, align = "v", heights = c(2.25, -0.3, 1))

ggsave("figures/figure_4_partb.png", width = 4, height = 3.5, units = "in")


# 2x2 Results, Taxes -----------------------------------------------------------------------

# State ---- 

# TWFE 
results_model_state_tax <- results_model_tax %>% mutate(model = "TWFE", item = "State Tax", estimate = NA, std.error = NA)

# Borusyak et al. 2021
borusyak_results_state_tax <- results_model_tax %>% mutate(model = "Borusyak et al. (2021)", item = "State Tax", estimate = NA, std.error = NA)
results_model_state_tax <- rbind(results_model_state_tax, borusyak_results_state_tax)

# County ---- 

# TWFE 
model_county_tax <- felm(data = dataCountyTax, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_county_tax)
control_mean_county_tax <- dataCountyTax %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean_county_tax <- control_mean_county_tax$mean
results_model_county_tax <- tidy(model_county_tax, conf.int=TRUE) %>% mutate(model = "TWFE", item = "County Tax") %>% mutate(estimate = estimate / control_mean_county_tax, std.error = std.error / control_mean_county_tax) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_county_tax <- did_imputation(data = dataCountyTax, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_county_tax <- c('Borusyak et al. (2021)', "County Tax", borusyak_county_tax$estimate/control_mean_county_tax, borusyak_county_tax$std.error/control_mean_county_tax)
results_model_county_tax <- rbind(results_model_county_tax, borusyak_results_county_tax)

# Town ---- 

# TWFE 
model_town_tax <- felm(data = dataTownTax, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_town_tax)
control_mean_town_tax <- dataTownTax %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean_town_tax <- control_mean_town_tax$mean
results_model_town_tax <- tidy(model_town_tax, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Town Tax") %>% mutate(estimate = estimate / control_mean_town_tax, std.error = std.error / control_mean_town_tax) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_town_tax <- did_imputation(data = dataTownTax, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_town_tax <- c('Borusyak et al. (2021)', "Town Tax", borusyak_town_tax$estimate/control_mean_town_tax, borusyak_town_tax$std.error/control_mean_town_tax)
results_model_town_tax <- rbind(results_model_town_tax, borusyak_results_town_tax)

# State Results -----------------------------------------------------------------------

# TWFE 
model_state_ban <- felm(data = dataStateBan, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_state_ban)
control_mean_state_ban <- dataStateBan %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean_state_ban <- control_mean_state_ban$mean
results_model_state_ban <- tidy(model_state_ban, conf.int=TRUE) %>% mutate(model = "TWFE", item = "State Ban") %>% mutate(estimate = estimate / control_mean_state_ban, std.error = std.error / control_mean_state_ban) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_state_ban <- did_imputation(data = dataStateBan, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_state_ban <- c('Borusyak et al. (2021)', "State Ban", borusyak_state_ban$estimate/control_mean_state_ban, borusyak_state_ban$std.error/control_mean_state_ban)
results_model_state_ban <- rbind(results_model_state_ban, borusyak_results_state_ban)

# County  Results -----------------------------------------------------------------------

# TWFE 
results_model_county_ban <- results_model_ban %>% mutate(model = "TWFE", item = "County Ban", estimate = NA, std.error = NA)

# Borusyak et al. 2021
borusyak_results_county_ban <- results_model_ban %>% mutate(model = "Borusyak et al. (2021)", item = "County Ban", estimate = NA, std.error = NA)
results_model_county_ban <- rbind(results_model_county_ban, borusyak_results_county_ban)


# Town  Results -----------------------------------------------------------------------

# TWFE 
model_town_ban <- felm(data = dataTownBan, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_town_ban)
control_mean_town_ban <- dataTownBan %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean_town_ban <- control_mean_town_ban$mean
results_model_town_ban <- tidy(model_town_ban, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Town Ban") %>% mutate(estimate = estimate / control_mean_town_ban, std.error = std.error / control_mean_town_ban) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_town_ban <- did_imputation(data = dataTownBan, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_town_ban <- c('Borusyak et al. (2021)', "Town Ban", borusyak_town_ban$estimate/control_mean_town_ban, borusyak_town_ban$std.error/control_mean_town_ban)
results_model_town_ban <- rbind(results_model_town_ban, borusyak_results_town_ban)

# State Results -----------------------------------------------------------------------

# TWFE 
model_state_partialban <- felm(data = dataStatePartialBan, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_state_partialban)
control_mean_state_partialban <- dataStatePartialBan %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean_state_partialban <- control_mean_state_partialban$mean
results_model_state_partialban <- tidy(model_state_partialban, conf.int=TRUE) %>% mutate(model = "TWFE", item = "State Partial Ban") %>% mutate(estimate = estimate / control_mean_state_partialban, std.error = std.error / control_mean_state_partialban) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_state_partialban <- did_imputation(data = dataStatePartialBan, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_state_partialban <- c('Borusyak et al. (2021)', "State Partial Ban", borusyak_state_partialban$estimate/control_mean_state_partialban, borusyak_state_partialban$std.error/control_mean_state_partialban)
results_model_state_partialban <- rbind(results_model_state_partialban, borusyak_results_state_partialban)

# County  Results -----------------------------------------------------------------------

# TWFE 
model_county_partialban <- felm(data = dataCountyPartialBan, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_county_partialban)
control_mean_county_partialban <- dataCountyPartialBan %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean_county_partialban <- control_mean_county_partialban$mean
results_model_county_partialban <- tidy(model_county_partialban, conf.int=TRUE) %>% mutate(model = "TWFE", item = "County Partial Ban") %>% mutate(estimate = estimate / control_mean_county_partialban, std.error = std.error / control_mean_county_partialban) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_county_partialban <- did_imputation(data = dataCountyPartialBan, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_county_partialban <- c('Borusyak et al. (2021)', "County Partial Ban", borusyak_county_partialban$estimate/control_mean_county_partialban, borusyak_county_partialban$std.error/control_mean_county_partialban)
results_model_county_partialban <- rbind(results_model_county_partialban, borusyak_results_county_partialban)

# Town  Results -----------------------------------------------------------------------

# TWFE 
model_town_partialban <- felm(data = dataTownPartialBan, percPlasticBag ~ treat | factor(ym) + factor(groupID01) | 0 | zip)
summary(model_town_partialban)
control_mean_town_partialban <- dataTownPartialBan %>% filter(type == "control") %>% summarise(mean = mean(percPlasticBag, na.rm=T))
control_mean_town_partialban <- control_mean_town_partialban$mean
results_model_town_partialban <- tidy(model_town_partialban, conf.int=TRUE) %>% mutate(model = "TWFE", item = "Town Partial Ban") %>% mutate(estimate = estimate / control_mean_town_partialban, std.error = std.error / control_mean_town_partialban) %>% dplyr::select(model, item, estimate, std.error)

# Borusyak et al. 2021
borusyak_town_partialban <- did_imputation(data = dataTownPartialBan, yname = "percPlasticBag", gname = "firstYM", tname = "ym", idname = "groupID01", cluster_var = "zip")
borusyak_results_town_partialban <- c('Borusyak et al. (2021)', "Town Partial Ban", borusyak_town_partialban$estimate/control_mean_town_partialban, borusyak_town_partialban$std.error/control_mean_town_partialban)
results_model_town_partialban <- rbind(results_model_town_partialban, borusyak_results_town_partialban)

# Level of Policy -------------------------------------------------------------------

pal <- pnw_palette("Sunset2", 3)

# combine all the results 
results <- rbind(results_model_tax, results_model_state_tax, results_model_county_tax, results_model_town_tax,
                 results_model_ban, results_model_state_ban, results_model_county_ban, results_model_town_ban, 
                 results_model_partialban, results_model_state_partialban, results_model_county_partialban, results_model_town_partialban) 

results <- results %>% distinct()

nObs <- c(nobs(model_tax), nobs(model_tax),
          NA, NA, nobs(model_county_tax), nobs(model_county_tax), NA, NA, 
          nobs(model_ban), nobs(model_ban), 
          nobs(model_state_ban),  nobs(model_state_ban),  NA, NA,  nobs(model_town_ban),  nobs(model_town_ban), 
          nobs(model_partialban), nobs(model_partialban), 
          nobs(model_state_partialban), nobs(model_state_partialban), nobs(model_county_partialban), nobs(model_county_partialban), nobs(model_town_partialban), nobs(model_town_partialban))
          
results <- cbind(results, nObs)

# replace town taxes, very few observations 
results <- results %>% mutate(estimate = ifelse(item == "Town Tax", NA, estimate),
                              std.error = ifelse(item == "Town Tax", NA, std.error))

# convert to numeric 
results <- results %>% mutate(estimate = as.numeric(estimate), std.error = as.numeric(std.error))

# calculate p values 
results <- results %>% mutate(p = 2 * pt(-abs(estimate/std.error), nObs))

results <- results %>% mutate(sig = ifelse(p < 0.01, "***",
                                            ifelse(p < 0.05, "**",
                                                  ifelse(p < 0.10, "*", ""))))

# create display text 
results <- results %>% mutate(estimate = estimate * 100, std.error = std.error * 100)
results <- results %>% mutate(displayText = paste0(round(estimate, 1), sig))
results <- results %>% mutate(displayText = paste0(displayText, "\n(", round(std.error, 1), ")"))
results <- results %>% mutate(displayText = ifelse(displayText == "NANA\n(NA)", "No or\nVery Few\nPolicies", displayText))

# policy type and level, if policy type contains "state", "county", etc. 
results <- results %>% mutate(policyLevel = ifelse(str_detect(item, "State"), "State", 
                                                  ifelse(str_detect(item, "County"), "County", 
                                                         ifelse(str_detect(item, "Town"), "Town", "All"))))
# now the same for ban / partial ban / tax 
results <- results %>% mutate(policyType = ifelse(str_detect(item, "Ban") & !str_detect(item, "Partial Ban"), "Ban", 
                                                 ifelse(str_detect(item, "Partial Ban"), "Partial Ban", "Tax")))


# plot with results 
results <- results %>% mutate(policyLevel = factor(policyLevel, levels = c("Town", "County", "State", "All")), 
                              policyType = factor(policyType, levels = c("Ban", "Partial Ban", "Tax", "All")))  

# create plot for interaction of geographic level and policy types 
palColor <- pnw_palette("Shuksan", 3)
palBorder <- c(pnw_palette("Bay", 5)[1], pnw_palette("Bay", 5)[2], pnw_palette("Bay", 5)[5]) 

ggplot(results %>% filter(model == "Borusyak et al. (2021)" & policyLevel != "All"), aes(policyType, policyLevel)) +
  geom_tile(aes(fill = policyLevel, color=policyType, alpha=abs(estimate)), linewidth=0.75, width=0.9, height=0.9) +
  geom_text(aes(label = displayText, size = 0.8)) +
  scale_fill_manual(values=c(palColor[3], palColor[2], palColor[1]), na.value="lightgray") + 
  scale_color_manual(values=c(palBorder[1], palBorder[2], palBorder[3]), na.value="lightgray") + 
  scale_alpha(range = c(0.40, 0.80), na.value=0)+
  xlab("Type of Policy") + ylab("Geographic Scope of Policy")+ 
  theme_bw()+
  theme(legend.position = "none", 
        axis.text.x=element_text(size = textSize), 
        axis.text.y=element_text(size = textSize, angle=90, vjust = 0.5, hjust=0.5), 
        axis.title.x=element_text(size = textSize, margin=margin(t=5, b=5)), 
        axis.title.y=element_text(size = textSize, margin=margin(t=5, b=5)), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.ticks = element_blank())
ggsave("figures/figure_4_partc.png", width = 4, height = 6, units = "in")


