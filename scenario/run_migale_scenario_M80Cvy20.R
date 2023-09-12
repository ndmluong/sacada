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
## Mask wearing - 
Parms_Workers$pMaskAcceptability[[Parms_Workers$MaskType]] <- 0.8
## Social distancing : conveyor dimension (change position if needed)
Parms_Plant$Objects$cvy$dim.X <- 20
Parms_Plant$Objects$cvy$pos.X <- 0.15
Parms_Plant$Objects$cvy$pos.Y <- 0.55

Parms_Plant$Objects$cvy2$dim.X <- 20
Parms_Plant$Objects$cvy2$pos.X <- 0.15
Parms_Plant$Objects$cvy2$pos.Y <- 0.35

Parms_Plant$Objects$epm1$pos.X <- 0.95
Parms_Plant$Objects$epm1$pos.Y <- 0.45

Parms_Plant$Spaces$gate$pos.Y <- 0.65
Parms_Plant$Spaces$office$pos.Y <- 0.65

# FUNCTIONS #####
source("functions/functions.R")

# f_plotPlant(Plant = f_createPlant(prm = Parms_Plant), prm = Parms_Plant) # visualize the plant


# SIMULATION ####
## > seed number ####
MySeed <- commandArgs(trailingOnly = TRUE) # extracted from bash

## > output file name  ####
output_filename <- paste("simulation_output/", MySeed, "/", "output_M80Cvy20_", MySeed,".RData", sep ="")

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
