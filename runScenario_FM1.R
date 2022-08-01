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
## All values by default


# SIMULATION ####
## seed number ####
MySeed <- 11001

## output file name /!\ ####
output_filename <- paste("output_FM1_", MySeed,".RData", sep ="")



## RUN ####
OUTPUT_seedx <- tryCatch(f_run_4M(prm_plant = Parms_Plant,
                                  prm_time = Parms_Time,
                                  prm_workers = Parms_Workers,
                                  prm_air = Parms_Air,
                                  prm_conta = Parms_Conta,
                                  prm_surfaces = Parms_Surfaces,
                                  prm_food = Parms_Food,
                                  seed = MySeed),
                         error = function(e) {
                           write(paste(Sys.time(),"- seed", x),
                                 file = "error_log.txt", append = TRUE)})


# SAVE RESULTS ####
save.image(file = paste("simulation_output/", output_filename, sep=""))
