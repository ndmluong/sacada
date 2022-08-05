# PACKAGES #####
library(tibble)
library(reshape2)
library(plotly)
library(data.table)
library(gridExtra)
library(earlyR)
library(incidence)
library(EnvStats)
library(EpiEstim)
library(viridis)
library(ggpubr)
library(tidyverse)

# FUNCTIONS #####
source("functions/functions.R")

# # OUTPUT EXTRACTION / ONLY WITH MIGALE >>>>> go to checkpoint ####
# ## Raw output from separate seeds ####
# # (check the XlSX file for the scenario code !)
# # NB: the number of elements found in raw_FM1, raw_RM2... may vary depending on the simulation progress
# raw_FM1 <- f_checkout(scenario = "FM1")
# raw_FM2 <- f_checkout(scenario = "FM2")
# raw_FM3 <- f_checkout(scenario = "FM3")
# raw_FM4 <- f_checkout(scenario = "FM4")
# 
# raw_FM1b <- f_checkout(scenario = "FM1b")
# raw_FM2b <- f_checkout(scenario = "FM2b")
# raw_FM3b <- f_checkout(scenario = "FM3b")
# raw_FM4b <- f_checkout(scenario = "FM4b")
# 
# ## Summarize raw output ####
# FM1 <- f_summaryOutput(rawoutput = raw_FM1)
# FM2 <- f_summaryOutput(rawoutput = raw_FM2)
# FM3 <- f_summaryOutput(rawoutput = raw_FM3)
# FM4 <- f_summaryOutput(rawoutput = raw_FM4)
# 
# FM1b <- f_summaryOutput(rawoutput = raw_FM1b)
# FM2b <- f_summaryOutput(rawoutput = raw_FM2b)
# FM3b <- f_summaryOutput(rawoutput = raw_FM3b)
# FM4b <- f_summaryOutput(rawoutput = raw_FM4b)

# >>>> checkpoint 1 <<<<< ####
## NB: check if the RData is available in the simulation_output directory
load(file = "simulation_output/output_FM.RData")

# PLOT ####
## Contaminated workers ####
f_plotContaminatedWorkers(IL = FM1$IL, IS = FM1$IS)
f_plotContaminatedWorkers(IL = FM2$IL, IS = FM2$IS)
f_plotContaminatedWorkers(IL = FM3$IL, IS = FM3$IS)
f_plotContaminatedWorkers(IL = FM4$IL, IS = FM4$IS)

## Contaminated workers: detailed plots for random seeds
f_plotContaminatedWorkers(IL = FM1$IL, IS = FM1$IS, seed_select = sample(FM1$all_seeds, 20), detailed_plot = TRUE, wrap.nrow = 4)
f_plotContaminatedWorkers(IL = FM2$IL, IS = FM2$IS, seed_select = sample(FM2$all_seeds, 20), detailed_plot = TRUE, wrap.nrow = 4)
f_plotContaminatedWorkers(IL = FM3$IL, IS = FM3$IS, seed_select = sample(FM3$all_seeds, 20), detailed_plot = TRUE, wrap.nrow = 4)
f_plotContaminatedWorkers(IL = FM4$IL, IS = FM4$IS, seed_select = sample(FM4$all_seeds, 20), detailed_plot = TRUE, wrap.nrow = 4)


# INDICATORS SUMMARY ####
## Cumulative number of infected workers after the whole period
f_smrzCumulContaWorkers(IL = FM1$IL, IS = FM1$IS)
f_smrzCumulContaWorkers(IL = FM1$IL, IS = FM1$IS)$cumul %>% mean()

f_smrzCumulContaWorkers(IL = FM2$IL, IS = FM2$IS)$cumul %>% mean()
f_smrzCumulContaWorkers(IL = FM3$IL, IS = FM3$IS)$cumul %>% mean()
f_smrzCumulContaWorkers(IL = FM4$IL, IS = FM4$IS)$cumul %>% mean()

## Food contamination
f_smrzAverageFoodContaRatio(FPS = FM1$FPS, detection = 3)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM2$FPS, detection = 3)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM3$FPS, detection = 3)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM4$FPS, detection = 3)$contaratio %>% mean()

f_smrzAverageFoodContaRatio(FPS = FM1$FPS, detection = 4)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM2$FPS, detection = 4)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM3$FPS, detection = 4)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM4$FPS, detection = 4)$contaratio %>% mean()

f_smrzAverageFoodContaRatio(FPS = FM1$FPS, detection = 5)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM2$FPS, detection = 5)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM3$FPS, detection = 5)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM4$FPS, detection = 5)$contaratio %>% mean()

f_smrzAverageFoodContaRatio(FPS = FM1$FPS, detection = 6)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM2$FPS, detection = 6)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM3$FPS, detection = 6)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM4$FPS, detection = 6)$contaratio %>% mean()


## Surfaces contamination
f_smrzAverageSurfacesContaRatio(SS = FM1$SS, plant = FM1$MyPlant, detection = 5)$contaratio %>% mean()
f_smrzAverageSurfacesContaRatio(SS = FM1$SS, plant = FM1$MyPlant, detection = 5)$contaratio %>% sd()

f_smrzAverageSurfacesContaRatio(SS = FM2$SS, plant = FM1$MyPlant, detection = 5)$contaratio %>% mean()
f_smrzAverageSurfacesContaRatio(SS = FM2$SS, plant = FM1$MyPlant, detection = 5)$contaratio %>% sd()

f_smrzAverageSurfacesContaRatio(SS = FM3$SS, plant = FM1$MyPlant, detection = 5)$contaratio %>% mean()
f_smrzAverageSurfacesContaRatio(SS = FM4$SS, plant = FM1$MyPlant, detection = 5)$contaratio %>% mean()

## Transmission rate
f_smrzRt(IS = FM1$IS, prm_conta = FM1$Parms_Conta, prm_workers = FM1$Parms_Workers)$Rt %>% mean(., na.rm=TRUE)
f_smrzRt(IS = FM1$IS, prm_conta = FM1$Parms_Conta, prm_workers = FM1$Parms_Workers)$Rt %>% sd(., na.rm=TRUE)

f_smrzRt(IS = FM2$IS, prm_conta = FM2$Parms_Conta, prm_workers = FM2$Parms_Workers)$Rt %>% mean(., na.rm=TRUE)
f_smrzRt(IS = FM2$IS, prm_conta = FM2$Parms_Conta, prm_workers = FM2$Parms_Workers)$Rt %>% sd(., na.rm=TRUE)

f_smrzRt(IS = FM3$IS, prm_conta = FM3$Parms_Conta, prm_workers = FM3$Parms_Workers)$Rt %>% mean(., na.rm=TRUE)
f_smrzRt(IS = FM3$IS, prm_conta = FM3$Parms_Conta, prm_workers = FM3$Parms_Workers)$Rt %>% sd(., na.rm=TRUE)

f_smrzRt(IS = FM4$IS, prm_conta = FM4$Parms_Conta, prm_workers = FM4$Parms_Workers)$Rt %>% mean(., na.rm=TRUE)
f_smrzRt(IS = FM4$IS, prm_conta = FM4$Parms_Conta, prm_workers = FM4$Parms_Workers)$Rt %>% sd(., na.rm=TRUE)






