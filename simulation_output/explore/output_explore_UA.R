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
library(cowplot)
library(tidyverse)

# LOADING SIMULATED DATA ####
## MCvy 12/22 - Cross effect of mask wearing and social distancing ####
# load(file = "simulation_output/20220919_output_MCvy.RData") # archives
load(file = "simulation_output/output_UA_1.RData")

UA <- list(UA_RNAr_min, UA_RNAr_max,
           UA_pAsymp_min, UA_pAsymp_max,
           UA_SAR_min, UA_SAR_max)

gdata::keep(UA, sure = TRUE)

# FUNCTIONS #####
source("functions/functions.R")

# DATA PRETREATMENT ####
## Factors levels ####
SMR_UA <- expand.grid(RNAr = c(100, 500, 1000),
                      SAR = c(0.5, 0.7, 0.8),
                      pAsymp = c(0.7, 0.8, 0.95))

