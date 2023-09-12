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

# PARAMETERS ####
## Values by default #####
source("parameters/parameters.R")

## Parameters changes for analyses  ####
## Uncertainty analysis - UA_pSneeze_max (infectious/(a)symptomatic) (check the XLSX file)
Parms_Air$p_sneeze[["infectious"]] <- 0.004
Parms_Air$p_sneeze[["symptomatic"]] <- 5/60
Parms_Air$p_sneeze[["asymptomatic"]] <- 0.16


# FUNCTIONS #####
source("functions/functions.R")

## > seed number ####
MySeed <- commandArgs(trailingOnly = TRUE) # extracted from bash

## > output file name  ####
output_filename <- paste("simulation_output/", MySeed, "/", "output_UA_psneeze_max_", MySeed,".RData", sep ="")

## > Run ####
STbegin <- Sys.time()
OUTPUT_seedx <- f_run_4M(prm_plant = Parms_Plant,
                         prm_time = Parms_Time,
                         prm_workers = Parms_Workers,
                         prm_air = Parms_Air,
                         prm_conta = Parms_Conta,
                         prm_surfaces = Parms_Surfaces,
                         prm_food = Parms_Food,
                         fulloutput = FALSE,
                         seed = MySeed)
STend <- Sys.time()

# Clear environment
gdata::keep(OUTPUT_seedx,
            output_filename, STbegin, STend,
            Parms_Air, Parms_Conta, Parms_Food, Parms_Plant, Parms_Surfaces, Parms_Time, Parms_Workers,
            sure = TRUE)


# SAVE RESULTS ####
save.image(file = output_filename)
