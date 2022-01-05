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

# # OPTION 2: CREATE NEW WORKERS
# # (approx. 2 minutes of simulation for 100 workers, 56 days and 5-minute time step)
# seed = 408
# Parms_Time$NDays <- 56
# Parms_Time$Step <- 5
# MyWorkers <- f_initWorkers(prm = Parms_Workers, prm_time = Parms_Time, seed = seed)
# MyWorkers <- f_setupSchedule(W = MyWorkers, prm = Parms_Workers, seed = seed)
# 
# # SCHEDULE VISUALISATION
# # Plot schedule for all workers during a given period
# f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 20)
# # Plot schedule for some considered workers
# f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 20, SHOW_ID = 1:60)
# # Plot schedule with information at one given day
# f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 56, SHOW_ID = 1:60, Dfocus = 4)
# 
# 
# ### ASSIGN LOCATION BASED ON SCHEDULE ###
# # OPTION 2: RE-RUN
# LastDay <- max(MyWorkers$Day)
# WorkingDays <- subset(MyWorkers,
#                       !Weekday %in% c("Saturday", "Sunday") & Day < LastDay)$Day %>% unique() %>% sort()
# OtherDays <- subset(MyWorkers, !Day %in% WorkingDays)$Day %>% unique() %>% sort()
# 
# ## Assign location based on schedule
# lapply(WorkingDays, FUN = function(x) {
#   d1 <- subset(MyWorkers, Day == x)
#   d1 <- f_dailyWork_AllTeams(Plant = MyPlant, W = d1,D = x, dt = Parms_Time$Step, seed = seed+x)
#   return(d1)
# }) %>%
#   rbindlist() %>%
#   rbind(subset(MyWorkers, Day %in% OtherDays)) %>%
#   dplyr::arrange(t_ind, W_ID) -> MyWorkers
# gc() # free unused R memory
# 
# # save.image("2021_12_15_100Workers_56days_Step5_Schedule_Location.RData")
# 
# 
# ### WEARING MASK ###
# set.seed(seed)
# by(data = MyWorkers,
#    INDICES = MyWorkers$Day,
#    FUN = f_dailyMaskWearing, ## check the script 
#    probMask = Parms_Workers$pMaskAcceptability["Surgical mask"]) %>%
#   data.table::rbindlist() %>%
#   dplyr::arrange(t_ind, W_ID) -> MyWorkers
# 
# save.image("2021_12_16_100Workers_56days_Step5_Schedule_Location_Mask.RData")
# 
# MyWorkers$W_location[is.na(MyWorkers$W_location)] <- "Home"
# set.seed(seed)
# ## Random state (in day) of the firts initialized contaminated worker
# MyWorkers$W_statusCounter[which(MyWorkers$W_status == "contaminated")] <- sample(Parms_Workers$ContaBeginDay:Parms_Workers$ContaEndDay, size = 1)
# 
# save.image("2022_01_03_100Workers_56days_Step5_Schedule_Location_Mask_StatusCounter.RData")


### CONTAMINATIONS ###
W <- MyWorkers

day <- 2
prev = 500/100000
prev = Parms_Workers$prev

set.seed(seed)

W <- dplyr::arrange(W, t_ind, W_ID)

WDm1 <- subset(W, Day == day-1 & Hour == 0 & Min == 0)

WDF <- subset(W, Day == day)
WD <- subset(WDF, Hour == 0 & Min == 0)


WD$W_status <- WDm1$W_status
WD$W_statusCounter <- WDm1$W_statusCounter + 1
WD$W_status[WD$W_statusCounter > 15] <- "not contaminated"
WD$W_statusCounter[WD$W_statusCounter > 15] <- NA

## inci = 237/100000
lapply(unique(WD$W_ID), FUN = function(x) {
  W1 <- subset(WD, W_ID == x)
  if (W1$W_status == "not contaminated") {
    if (rbinom(1,1,prob = prev) == 1) {
      W1$W_status <- "contaminated"
      W1$W_statusCounter <- 1
    }
  }
  return(W1)
}) %>% 
  rbindlist() %>%
  dplyr::arrange(t_ind, W_ID) -> WD

WDF[which(WDF$Day == day & WDF$Hour == 0 & WDF$Min == 0), ] <- WD


by(WDF, ## for the considered agent
   INDICES = WDF$W_ID, ## processing by day
   FUN = function(x) {
     return(f_replicateIndividualtime2time(Agent = x, Invariant = "W_status", time_begin = c(0,0), time_end = c(23,55), dt = Parms_Time$Step))
   }) %>%
  data.table::rbindlist() %>%
  dplyr::arrange(t_ind, W_ID) -> WDtest


W[which(W$Day == day), ] <- WDF

