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
raw_M0Cvy11 <- f_checkout(scenario = "M0Cvy11")
raw_M0Cvy14 <- f_checkout(scenario = "M0Cvy14")
raw_M0Cvy17 <- f_checkout(scenario = "M0Cvy17")
raw_M0Cvy20 <- f_checkout(scenario = "M0Cvy20")

raw_M25Cvy11 <- f_checkout(scenario = "M25Cvy11")
raw_M25Cvy14 <- f_checkout(scenario = "M25Cvy14")
raw_M25Cvy17 <- f_checkout(scenario = "M25Cvy17")
raw_M25Cvy20 <- f_checkout(scenario = "M25Cvy20")

raw_M50Cvy11 <- f_checkout(scenario = "M50Cvy11")
raw_M50Cvy14 <- f_checkout(scenario = "M50Cvy14")
raw_M50Cvy17 <- f_checkout(scenario = "M50Cvy17")
raw_M50Cvy20 <- f_checkout(scenario = "M50Cvy20")

raw_M80Cvy11 <- f_checkout(scenario = "M80Cvy11")
raw_M80Cvy14 <- f_checkout(scenario = "M80Cvy14")
raw_M80Cvy17 <- f_checkout(scenario = "M80Cvy17")
raw_M80Cvy20 <- f_checkout(scenario = "M80Cvy20")

raw_M100Cvy11 <- f_checkout(scenario = "M100Cvy11")
raw_M100Cvy14 <- f_checkout(scenario = "M100Cvy14")
raw_M100Cvy17 <- f_checkout(scenario = "M100Cvy17")
raw_M100Cvy20 <- f_checkout(scenario = "M100Cvy20")


## Summary
M0Cvy11 <- f_summaryOutput(rawoutput = raw_M0Cvy11)
M0Cvy14 <- f_summaryOutput(rawoutput = raw_M0Cvy14)
M0Cvy17 <- f_summaryOutput(rawoutput = raw_M0Cvy17)
M0Cvy20 <- f_summaryOutput(rawoutput = raw_M0Cvy20)
M25Cvy11 <- f_summaryOutput(rawoutput = raw_M25Cvy11)
M25Cvy14 <- f_summaryOutput(rawoutput = raw_M25Cvy14)
M25Cvy17 <- f_summaryOutput(rawoutput = raw_M25Cvy17)
M25Cvy20 <- f_summaryOutput(rawoutput = raw_M25Cvy20)
M50Cvy11 <- f_summaryOutput(rawoutput = raw_M50Cvy11)
M50Cvy14 <- f_summaryOutput(rawoutput = raw_M50Cvy14)
M50Cvy17 <- f_summaryOutput(rawoutput = raw_M50Cvy17)
M50Cvy20 <- f_summaryOutput(rawoutput = raw_M50Cvy20)
M80Cvy11 <- f_summaryOutput(rawoutput = raw_M80Cvy11)
M80Cvy14 <- f_summaryOutput(rawoutput = raw_M80Cvy14)
M80Cvy17 <- f_summaryOutput(rawoutput = raw_M80Cvy17)
M80Cvy20 <- f_summaryOutput(rawoutput = raw_M80Cvy20)
M100Cvy11 <- f_summaryOutput(rawoutput = raw_M100Cvy11)
M100Cvy14 <- f_summaryOutput(rawoutput = raw_M100Cvy14)
M100Cvy17 <- f_summaryOutput(rawoutput = raw_M100Cvy17)
M100Cvy20 <- f_summaryOutput(rawoutput = raw_M100Cvy20)


### save
gdata::keep(M0Cvy11, M0Cvy14, M0Cvy17, M0Cvy20,
            M25Cvy11, M25Cvy14, M25Cvy17, M25Cvy20,
            M50Cvy11, M50Cvy14, M50Cvy17, M50Cvy20,
            M80Cvy11, M80Cvy14, M80Cvy17, M80Cvy20,
            M100Cvy11, M100Cvy14, M100Cvy17, M100Cvy20,
            sure = TRUE)
# save.image(file = "simulation_output/20220919_output_MCvy.RData")
save.image(file = "simulation_output/20221201_output_MCvy.RData")





