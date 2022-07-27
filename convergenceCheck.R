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
library(incidence)
library(purrr)
library(EnvStats)
library(EpiEstim)

# Combine results from different simulations ####
ALLOUTPUT <- list()

## 10001:10010
load("simulation_output/outputNDL_runConv1_10001_10010.RData")
ALLOUTPUT <- append(ALLOUTPUT,
                 OUTPUT_allseeds)

## 10011:10050
load("simulation_output/outputNDL_runConv1_10011_10050.RData")
ALLOUTPUT <- append(ALLOUTPUT,
                 OUTPUT_allseeds)

## 10051:10100
load("simulation_output/outputNDL_runConv1_10051_10100.RData")
ALLOUTPUT <- append(ALLOUTPUT,
                 OUTPUT_allseeds)

## 10501:10550
load("simulation_output/outputSD_runConv1_10501_10550.RData")
ALLOUTPUT <- append(ALLOUTPUT,
                 OUTPUT_allseeds)

## 10601:10700
load("simulation_output/outputSD_runConv1_10601_10700.RData")
ALLOUTPUT <- append(ALLOUTPUT,
                 OUTPUT_allseeds)

# Clear environment
rm(IL, IS, OUTPUT_allseeds, all_seed, ST_Begin, ST_End, output_filename)

# Update functions #####
source("functions/functions.R")

# SAVE CONVERGENCE DATA #####
# save.image("simulation_output/202207_ConvergenceCheckData.RData")


# >>> Checkpoint <<< ####
# TREATMENT OF CONVERGENCE DATA ####
## Load data (optional) ####
load("simulation_output/202207_ConvergenceCheckData.RData")

## Success rate ####
# Discard all null elements (simulations skipped due to errors)
ALLOUTPUT %>%
  purrr::discard(., is.null) -> OUTPUT ## sucess rate: 187/250

# Rename the elements of the output by the seed number
all_seeds <- sapply(OUTPUT, FUN = function(x) x$seed)
names(OUTPUT) <- all_seeds

## IL: Infection logs for all seeds ####
lapply(OUTPUT, FUN = function(x) {
  ILx <- data.frame(seed = rep(x$seed, nrow(x$InfectionLog)),
                    x$InfectionLog)
  return(ILx)
}) %>%
  data.table::rbindlist() %>%
  arrange(., seed, W_ID) -> IL

IL$seed <- as.factor(IL$seed)
IL$InfectionSource <- as.factor(IL$InfectionSource)


## IS: Infection logs for all seeds ####
lapply(OUTPUT, FUN = function(x) x$InfectionSummary) %>%
  data.table::rbindlist() %>%
  relocate(., seed) %>%
  arrange(., seed, Day) -> IS

IS$seed <- as.factor(IS$seed)

## SS: Contamination of the surfaces ####
lapply(OUTPUT, FUN = function(x) {
  SSx <- data.frame(seed = rep(x$seed, nrow(x$S_summary)),
                    x$S_summary)
  return(SSx)
}) %>%
  data.table::rbindlist() %>%
  arrange(., seed, Day) -> SS

SS$seed <- as.factor(SS$seed)

## FPS: Contamination of the food portions ####
lapply(OUTPUT, FUN = function(x) {
  FPSx <- data.frame(seed = rep(x$seed, nrow(x$FP_summary)),
                     x$FP_summary)
  return(FPSx)
}) %>%
  data.table::rbindlist() %>%
  arrange(., seed, Day) -> FPS

FPS$seed <- as.factor(FPS$seed)


f_plotOutput(IL = IL, IS = IS, detailed_plot = TRUE,
             seed_select = sample(all_seeds, 15), wrap.nrow = 3)



