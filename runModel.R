##### PACKAGES #####
library(ggplot2)
library(tibble)
library(reshape2)
library(plotly)
library(stringr)
library(data.table)
library(dplyr)
library(gridExtra)
library(EnvStats)

##### FUNCTIONS #####
source("functions/functions_actions.R")
source("functions/functions_air.R")
source("functions/functions_contamination.R")
source("functions/functions_dailyWork.R")
source("functions/functions_food.R")
source("functions/functions_master.R")
source("functions/functions_plant.R")
source("functions/functions_plot.R")
source("functions/functions_run.R")
source("functions/functions_surfaces.R")
source("functions/functions_time.R")
source("functions/functions_workers.R")


##### PARAMETERS #####
## Check the scripts for more details / change parameter values if needed
source("parameters/parameters_plant.R") ## PLANT
source("parameters/parameters_time.R") ## TIME
source("parameters/parameters_workers.R") ## WORKERS
source("parameters/parameters_food.R") ## FOOD PORTIONS
source("parameters/parameters_air.R") ## AIR
source("parameters/parameters_conta.R") ## CONTAMINATION
# source("parameters/parameters_surfaces.R")

##### SIMULATION #####
######################## SCENARIO S01 (default) ################################
all_seed <- 103:105

ST_S01Begin <- Sys.time() ## simulation time checkpoint
lapply(all_seed, FUN = function(x) {
  OUTPUT_seedx <- tryCatch(f_run_3M(prm_plant = Parms_Plant,
                                    prm_time = Parms_Time,
                                    prm_workers = Parms_Workers,
                                    prm_air = Parms_Air,
                                    prm_conta = Parms_Conta,
                                    seed = x),
                           error = function(e) NULL)
  return(OUTPUT_seedx)
}) -> OUTPUT
ST_S01End <- Sys.time() ## simulation time checkpoint

### OUTPUT TREATMENT : Infection summary
lapply(OUTPUT, function(x) {return(x$InfectionSummary)}) %>%
  data.table::rbindlist() -> IS
IS$seed <- as.factor(IS$seed)

### OUTPUT TREATMENT : Infection source (for all workers and all simulations)
lapply(OUTPUT, function(x) {
  return(data.frame(x$InfectionLog,
                    seed = rep(x$seed, nrow(x$InfectionLog))))
}) %>%
  data.table::rbindlist() -> IL
IL$seed <- as.factor(IL$seed)
IL$InfectionSource <- as.factor(IL$InfectionSource)

#### SAVE SIMULATION RESULTS
save.image("simulation_output/OUTPUT_local20220426_NDL_S01_Sim103105_r500.RData")




############################### PLOTTING OUTPUT ################################
# load("simulation_output/OUTPUT_2022_04_07_NDL_S01.RData")
f_plotOutput(IL, IS)
f_plotOutput(IL, IS, detailed_plot = T, wrap.nrow = 1)
