##### PACKAGES #####
library(ggplot2)
library(reshape2)
library(plotly)
library(stringr)
library(data.table)
library(dplyr)
library(gridExtra)
library(EnvStats)
library(viridis)

##### FUNCTIONS #####
source("functions/functions_actions.R")
source("functions/functions_air.R")
source("functions/functions_contamination.R")
source("functions/functions_dailyWork.R")
source("functions/functions_module_master_proposition_modif.R")
source("functions/functions_plant.R")
source("functions/functions_plot.R")
source("functions/functions_run.R")
source("functions/functions_time.R")
source("functions/functions_workers.R")


##### PARAMETERS #####
## Check the scripts for more details / change parameter values if needed
source("parameters/parameters_plant.R") ## PLANT
source("parameters/parameters_time.R") ## TIME
source("parameters/parameters_workers.R") ## WORKERS
source("parameters/parameters_air.R") ## AIR
source("parameters/parameters_conta.R") ## CONTAMINATION


##### SIMULATION #####
# ### one simulation, ex: seed 408
# ST1 <- Sys.time() ## simulation time checkpoint
# 
# OUTPUT_seed100 <- f_run_2M(prm_plant = Parms_Plant,
#                            prm_time = Parms_Time,
#                            prm_workers = Parms_Workers,
#                            prm_air = Parms_Air,
#                            prm_conta = Parms_Conta,
#                            seed = 100)
# ST2 <- Sys.time() ## simulation time checkpoint

### several simulations
all_seed <- 121:130 ## change if needed

ST3 <- Sys.time() ## simulation time checkpoint
lapply(all_seed, FUN = function(x) {
  OUTPUT_seedx <- tryCatch(f_run_2M(prm_plant = Parms_Plant,
                                    prm_time = Parms_Time,
                                    prm_workers = Parms_Workers,
                                    prm_air = Parms_Air,
                                    prm_conta = Parms_Conta,
                                    seed = x),
                           error = function(e) NULL)
  return(OUTPUT_seedx)
}) -> OUTPUT
ST4 <- Sys.time() ## simulation time checkpoint

### OUTPUT TREATMENT : Infection summary
lapply(OUTPUT, function(x) {return(x$InfectionSummary)}) %>%
  data.table::rbindlist() -> IS
IS$seed <- as.factor(IS$seed)

### OUTPUT TREATMENT : Infection source (for all workers and all simulations)
lapply(OUTPUT, function(x) {return(x$InfectionLog)}) %>%
  data.table::rbindlist() -> IL
IL$seed <- as.factor(IL$seed)
IL$InfectionSource <- as.factor(IL$InfectionSource)

# show infection sources
tapply(IL$InfectionSource, IL$seed, summary)

#### SAVE SIMULATION RESULTS
save.image("simulation_output/...............RData")
## ex. save.image("simulation_output/2022_02_06_initiales_s121_s130.Rdata) ?


############################
##### PLOTTING OUTPUT ######
############################
f_plotOutput(IL, IS)
f_plotOutput(IL, IS, seed_select = c(121, 125, 126, 127, 130), detailed_plot = T)
f_plotOutput(IL, IS, detailed_plot = T, wrap.nrow = 2)

