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

# OUTPUT EXTRACTION / ONLY WITH MIGALE ####
## Raw output from separate seeds
raw_UA_RNAr_min <- f_checkout(scenario = "UA_RNAr_min")
raw_UA_RNAr_max <- f_checkout(scenario = "UA_RNAr_max")

raw_UA_SAR_min <- f_checkout(scenario = "UA_SAR_min")
raw_UA_SAR_max <- f_checkout(scenario = "UA_SAR_max")

raw_UA_pAsymp_min <- f_checkout(scenario = "UA_pAsymp_min")
raw_UA_pAsymp_max <- f_checkout(scenario = "UA_pAsymp_max")

raw_UA_psneeze_min <- f_checkout(scenario = "UA_psneeze_min")
raw_UA_psneeze_max <- f_checkout(scenario = "UA_psneeze_max")

raw_UA_Vsed_A1 <- f_checkout(scenario = "UA_Vsed_A1")
raw_UA_Vsed_A2 <- f_checkout(scenario = "UA_Vsed_A2")

raw_UA_rS2F_min <- f_checkout(scenario = "UA_rS2F_min")
raw_UA_rS2F_max <- f_checkout(scenario = "UA_rS2F_max")

raw_UA_tileprop_min <- f_checkout(scenario = "UA_tileprop_min")
raw_UA_tileprop_max <- f_checkout(scenario = "UA_tileprop_max")


## Summary
UA_RNAr_min <- f_summaryOutput(rawoutput = raw_UA_RNAr_min)
UA_RNAr_max <- f_summaryOutput(rawoutput = raw_UA_RNAr_max)

UA_SAR_min <- f_summaryOutput(rawoutput = raw_UA_SAR_min)
UA_SAR_max <- f_summaryOutput(rawoutput = raw_UA_SAR_max)

UA_pAsymp_min <- f_summaryOutput(rawoutput = raw_UA_pAsymp_min)
UA_pAsymp_max <- f_summaryOutput(rawoutput = raw_UA_pAsymp_max)

UA_psneeze_min <- f_summaryOutput(rawoutput = raw_UA_psneeze_min)
UA_psneeze_max <- f_summaryOutput(rawoutput = raw_UA_psneeze_max) 

UA_Vsed_A1 <- f_summaryOutput(rawoutput = raw_UA_Vsed_A1)
UA_Vsed_A2 <- f_summaryOutput(rawoutput = raw_UA_Vsed_A2)

UA_rS2F_min <- f_summaryOutput(rawoutput = raw_UA_rS2F_min)
UA_rS2F_max <- f_summaryOutput(rawoutput = raw_UA_rS2F_max)

UA_tileprop_min <- f_summaryOutput(rawoutput = raw_UA_tileprop_min)
UA_tileprop_max <- f_summaryOutput(rawoutput = raw_UA_tileprop_max)



### save
gdata::keep(UA_RNAr_min, UA_RNAr_max,
            UA_SAR_min, UA_SAR_max,
            UA_pAsymp_min, UA_pAsymp_max,
            UA_psneeze_min, UA_psneeze_max,
            UA_Vsed_A1, UA_Vsed_A2,
            UA_rS2F_min, UA_rS2F_max,
            UA_tileprop_min, UA_tileprop_max,
            sure = TRUE)

save.image(file = "simulation_output/output_UA.RData")
