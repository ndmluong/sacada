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
## Scenario FMAir1800 - air renewal of the cutting room ! (check the XLSX file)
Parms_Plant$Air_renewal <- 1800 


# FUNCTIONS #####
source("functions/functions.R")


# SIMULATION ####
## > seed number ####
MySeed <- commandArgs(trailingOnly = TRUE) # extracted from bash

## > output file name  ####
output_filename <- paste("simulation_output/", MySeed, "/", "output_FMAir1800_", MySeed,".RData", sep ="")

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
