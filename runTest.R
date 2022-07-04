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

# FUNCTIONS #####
source("functions/functions.R")

# PARAMETERS ####
## Values by default #####
source("parameters/parameters.R")

## Changes for analyses (check the XLSX file)
Parms_Workers$prev <- 100/100000
Parms_Time$NDays <- 28
Parms_Food$worker_rhythm$porcine <- c("logistic1" = 0.025,
                                      "cutter1" = 0.05,
                                      "cutter2" = 0.025,
                                      "logistic2" = 0.125)

# SIMULATION ####
## seed number ####
all_seed <- 20001

## output file name /!\ ####
output_filename <- "output_runTestbis.RData"

## RUN ####
{
  ST_Begin <- Sys.time() ## simulation time checkpoint
  lapply(all_seed, function(x) {
    OUTPUT_seedx <- tryCatch(f_run_4M(prm_plant = Parms_Plant,
                                      prm_time = Parms_Time,
                                      prm_workers = Parms_Workers,
                                      prm_air = Parms_Air,
                                      prm_conta = Parms_Conta,
                                      prm_surfaces = Parms_Surfaces,
                                      prm_food = Parms_Food,
                                      seed = x),
                             error = function(e) {
                               write(paste(Sys.time(),"- seed", x),
                                     file = "error_log.txt", append = TRUE) 
                             })
    return(OUTPUT_seedx)
  }) -> OUTPUT_allseeds
  ST_End <- Sys.time() ## simulation time checkpoint
}


# OUTPUT ####
## Treatment ####
### Infection summary ####
IS <- f_IS(OUTPUT_allseeds)

### Infection log (infection sources) ####
IL <- f_IL(OUTPUT_allseeds)

# ## Plot ####
# #### Workers contamination ####
# f_plotOutput(IL = IL, IS = IS)
# f_plotOutput(IL = IL, IS = IS, detailed_plot = T)
# 
# 
# 
# ## Indicators #### (voir avec Steven/Laurent)
# ### Rt ####
# f_summaryRt(IS = IS, prm_conta = Parms_Conta)


# SAVE RESULTS ####
save.image(file = paste("simulation_output/", output_filename, sep=""))
