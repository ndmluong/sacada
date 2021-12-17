#test code Du
##### PACKAGES #####
library(ggplot2)
library(reshape2)
library(plotly)
library(stringr)
library(data.table)
library(dplyr)

load("2021_12_16_100Workers_56days_Step5_Schedule_Location_Mask.RData")

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
source("functions/functions_dailyWork.R")


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
g_emptyPlant
ggplotly(g_emptyPlant)


##### WORKERS #####
### SCHEDULE ###
# # OPTION 1: LOAD PRE-CREATED WORKERS
# load("2021_12_15_100Workers_56days_Step5_Schedule.RData")

# OPTION 2: CREATE NEW WORKERS
# (approx. 2 minutes of simulation for 100 workers, 56 days and 5-minute time step)
seed = 408
Parms_Time$NDays <- 56
Parms_Time$Step <- 5
MyWorkers <- f_initWorkers(prm = Parms_Workers, prm_time = Parms_Time, seed = seed)
MyWorkers <- f_setupSchedule(W = MyWorkers, prm = Parms_Workers, seed = seed)

# SCHEDULE VISUALISATION
# Plot schedule for all workers during a given period
f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 20)
# Plot schedule for some considered workers
f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 20, SHOW_ID = 1:60)
# Plot schedule with information at one given day
f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 56, SHOW_ID = 1:60, Dfocus = 1)


### ASSIGN LOCATION BASED ON SCHEDULE ###
# OPTION 2: RE-RUN
LastDay <- max(MyWorkers$Day)
WorkingDays <- subset(MyWorkers,
                      !Weekday %in% c("Saturday", "Sunday") & Day < LastDay)$Day %>% unique() %>% sort()
OtherDays <- subset(MyWorkers, !Day %in% WorkingDays)$Day %>% unique() %>% sort()

## Assign location based on schedule
lapply(WorkingDays, FUN = function(x) {
  d1 <- subset(MyWorkers, Day == x)
  d1 <- f_dailyWork_AllTeams(Plant = MyPlant, W = d1,D = x, dt = Parms_Time$Step, seed = seed+x)
  return(d1)
}) %>%
  rbindlist() %>%
  rbind(subset(MyWorkers, Day %in% OtherDays)) %>%
  dplyr::arrange(t_ind, W_ID) -> MyWorkers
gc() # free unused R memory

# save.image("2021_12_15_100Workers_56days_Step5_Schedule_Location.RData")


### WEARING MASK ###
set.seed(seed)
by(data = MyWorkers,
   INDICES = MyWorkers$Day,
   FUN = f_dailyMaskWearing, ## check the script 
   probMask = Parms_Workers$pMaskAcceptability["Surgical mask"]) %>%
  data.table::rbindlist() %>%
  dplyr::arrange(t_ind, W_ID) -> MyWorkers

save.image("2021_12_16_100Workers_56days_Step5_Schedule_Location_Mask.RData")





