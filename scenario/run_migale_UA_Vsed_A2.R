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
## Uncertainty analysis - UA_Vsed_A2 (check the XLSX file)



# FUNCTIONS #####
source("functions/functions.R")


f_sed_time <- function(
    prm_plant,
    prm_air
) {
  # # Sedimentation Velocity
  Vsed <<- matrix(f_Vsed(prm_air),nrow=1) / 13.6 # (m.s-1)  ####################### CHANGE !
  
  # Height of each room 
  H_room <- matrix(c(prm_plant$dim.Z,
                     unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$dim.Z))))))
  
  return(H_room%*%(Vsed^-1))
}




## > seed number ####
MySeed <- commandArgs(trailingOnly = TRUE) # extracted from bash

## > output file name  ####
output_filename <- paste("simulation_output/", MySeed, "/", "output_UA_Vsed_A2_", MySeed,".RData", sep ="")

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
