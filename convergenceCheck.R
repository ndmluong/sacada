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
library(viridis)

# Combine results from different simulations ####
ALLOUTPUT <- list()

## 10001:10010
load("simulation_output/outputNDL_runConv1_10001_10010.RData")
ALLOUTPUT <- append(ALLOUTPUT, OUTPUT_allseeds)
gdata::keep(ALLOUTPUT, sure = TRUE)

## 10011:10050
load("simulation_output/outputNDL_runConv1_10011_10050.RData")
ALLOUTPUT <- append(ALLOUTPUT, OUTPUT_allseeds)
gdata::keep(ALLOUTPUT, sure = TRUE)

## 10051:10100
load("simulation_output/outputNDL_runConv1_10051_10100.RData")
ALLOUTPUT <- append(ALLOUTPUT, OUTPUT_allseeds)
gdata::keep(ALLOUTPUT, sure = TRUE)

## 10501:10550
load("simulation_output/outputSD_runConv1_10501_10550.RData")
ALLOUTPUT <- append(ALLOUTPUT, OUTPUT_allseeds)
gdata::keep(ALLOUTPUT, sure = TRUE)

## 10601:10700
load("simulation_output/outputSD_runConv1_10601_10700.RData")
ALLOUTPUT <- append(ALLOUTPUT, OUTPUT_allseeds)

# Clear environment
gdata::keep(ALLOUTPUT,
            Parms_Air, Parms_Conta, Parms_Food, Parms_Plant, Parms_Surfaces, Parms_Time, Parms_Workers,
            sure = TRUE)


# Save full output data #####
# save.image("simulation_output/202207_ConvergenceCheck_FullData.RData")




















# >>>>>>>>>> Checkpoint 1 <<<<<<<<<< ####

# Load full output data #####
# load("simulation_output/202207_ConvergenceCheck_FullData.RData")


# Lighten the convergence output data ####
# Extract the plant structure
MyPlant <- ALLOUTPUT[[2]]$MyPlant

# Keeping essential results
lapply(ALLOUTPUT, 
       function(x) {
  if (!is.null(x)) {
    x[names(x) %in% c("seed", "S_summary", "FP_summary", "InfectionLog", "InfectionSummary") == TRUE]
  } else return(NULL)
}) -> AllOutput

# Cleaning environment
rm(ALLOUTPUT)

# Save light output data #####
# save.image("simulation_output/202207_ConvergenceCheck_LightData.RData")

















# >>>>>>>>>> Checkpoint 2 <<<<<<<<<< ####

# Load light output data #####
# load("simulation_output/202207_ConvergenceCheckData_LightData.RData")

# Load all updated functions (optional) #####
source("functions/functions.R")

# Summaries extraction ####
## Success rate ####
# Discard all null elements (simulations skipped due to errors)
AllOutput %>%
  purrr::discard(., is.null) -> Output # success rate: 187/250

# Rename the elements of the output by the seed number
all_seeds <- sapply(Output, function(x) x$seed)
names(Output) <- all_seeds

## IL - Infection logs for all seeds ####
lapply(Output, function(x) {
  ILx <- data.frame(seed = rep(x$seed, nrow(x$InfectionLog)),
                    x$InfectionLog)
  return(ILx)
}) %>%
  data.table::rbindlist() %>%
  arrange(., seed, W_ID) -> IL

IL$seed <- as.factor(IL$seed)
IL$InfectionSource <- as.factor(IL$InfectionSource)


## IS - Infection status summary for all seeds ####
lapply(Output, FUN = function(x) x$InfectionSummary) %>%
  data.table::rbindlist() %>%
  relocate(., seed) %>%
  arrange(., seed, Day) -> IS

IS$seed <- as.factor(IS$seed)

## SS: Contamination of the surfaces ####
lapply(Output, function(x) {
  SSx <- data.frame(seed = rep(x$seed, nrow(x$S_summary)),
                    x$S_summary)
  return(SSx)
}) %>%
  data.table::rbindlist() %>%
  arrange(., seed, Day) -> SS

SS$seed <- as.factor(SS$seed)

## FPS: Contamination of the food portions ####
lapply(Output, function(x) {
  FPSx <- data.frame(seed = rep(x$seed, nrow(x$FP_summary)),
                     x$FP_summary)
  return(FPSx)
}) %>%
  data.table::rbindlist() %>%
  arrange(., seed, Day) -> FPS

FPS$seed <- as.factor(FPS$seed)

# Save all summaries #####
# save.image("simulation_output/202207_ConvergenceCheck_Summary.RData")























# >>>>>>>>>> Checkpoint 3 <<<<<<<<<< ####

# Load all summaries #####
# load("simulation_output/202207_ConvergenceCheck_Summary.RData")

# Plot - Infected workers ####
f_plotContaminatedWorkers(IL = IL, IS = IS)
f_plotContaminatedWorkers(IL = IL, IS = IS,
                          seed_select = sample(all_seeds, 5), detailed_plot = T, wrap.nrow = 1)
# >>> some interesting plots : 10527, 10658, 10669, 10627, 10536, 10601
f_plotContaminatedWorkers(IL = IL, IS = IS,
                          seed_select = c(10527, 10658, 10669, 10627, 10536, 10601),
                          detailed_plot = T, wrap.nrow = 1)


