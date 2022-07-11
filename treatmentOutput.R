# PACKAGES #####
library(ggplot2)
library(tibble)
library(reshape2)
library(plotly)
library(stringr)
library(data.table)
library(dplyr)
library(gridExtra)
library(earlyR)
library(viridis)
library(incidence)

# LOAD OUTPUT ####
load("simulation_output/outputNDL_runConv1_10001_10010.RData")

# FUNCTIONS #####
source("functions/functions.R")

f_plotOutput(IL = IL, IS = IS)

View(OUTPUT_allseeds[[5]]$FP_summary)

f_summaryRt(IS, Parms_Conta)
