#test code Du
##### PACKAGES #####
library(ggplot2)
library(reshape2)
library(plotly)
library(stringr)
library(data.table)
library(dplyr)

##### FUNCTIONS #####
source("functions/functions_plant.R")
source("functions/functions_plot.R")
source("functions/functions_air.R")
#source("functions/functions_air_module.R")
source("functions/functions_food.R")
source("functions/functions_workers.R")
source("functions/functions_surfaces.R")
source("functions/functions_actions.R")
source("functions/functions_time.R")

##### PARAMETERS #####
## Check the scripts for more details / change parameter values if needed
## PLANT
source("parameters/parameters_plant.R")
## TIME
source("parameters/parameters_time.R")
## WORKERS
source("parameters/parameters_workers.R")
## AIR
source("parameters/parameters_air.R")
## FOOD PORTIONS
source("parameters/parameters_food.R")


##### PLANT #####
## Create the plant
MyPlant <- f_createPlant(prm = Parms_Plant)
## Plot
g_emptyPlant <- f_plotPlant(Plant = MyPlant,
                            prm = Parms_Plant)

##### WORKERS #####
## OPTION 1: LOAD PRE-CREATED WORKERS
load("2021_12_08_100Workers_365days_Step5_TypeTeamShiftActiveCounterSchedule.RData")

## OPTION 2: CREATE NEW WORKERS
## (approx. 45 minutes of simulation for 100 workers, 365 days and 5-minute time step)
seed = 408
MyWorkers <- f_initWorkers(prm = Parms_Workers, prm_time = Parms_Time, seed = seed)
MyWorkers <- f_setupSchedule(W = MyWorkers, prm = Parms_Workers, seed = seed)

## SCHEDULE VISUALISATION
## Plot schedule for all workers during a given period
f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 90)
## Plot schedule for some considered workers
f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 90, SHOW_ID = 1:60)
## Plot schedule with information at one given day
f_summaryWorkersAtDay(MyWorkers, Dfocus = 43)
f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 90, SHOW_ID = 1:60, Dfocus = 43)