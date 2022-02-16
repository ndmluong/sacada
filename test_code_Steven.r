#test code Du
##### PACKAGES #####
library(ggplot2)
library(reshape2)
library(plotly)
library(stringr)
library(data.table)
library(dplyr)
library(gridExtra)

##### FUNCTIONS #####
source("functions/functions_actions.R")
source("functions/functions_air.R")
source("functions/functions_contamination.R")
source("functions/functions_dailyWork.R")
# source("functions/functions_food.R")
# source("functions/functions_module_master.R")
source("functions/functions_module_master_proposition_modif.R")
source("functions/functions_plant.R")
source("functions/functions_plot.R")
source("functions/functions_run.R")
# source("functions/functions_surfaces.R")
source("functions/functions_time.R")
source("functions/functions_workers.R")


##### PARAMETERS #####
## Check the scripts for more details / change parameter values if needed
source("parameters/parameters_plant.R") ## PLANT
source("parameters/parameters_time.R") ## TIME
source("parameters/parameters_workers.R") ## WORKERS
source("parameters/parameters_air.R") ## AIR
source("parameters/parameters_conta.R") ## CONTAMINATION


##### PLANT #####
## Create the plant
MyPlant <- f_createPlant(prm = Parms_Plant)
## Plot
g_emptyPlant <- f_plotPlant(Plant = MyPlant,
                            prm = Parms_Plant)
# g_emptyPlant
# ggplotly(g_emptyPlant)

seed = 408

##### WORKERS #####
### SCHEDULE ###
# Create workers
# (approx. 2 minutes of simulation for 100 workers, 56 days and 5-minute time step)
MyWorkers <- f_initWorkers(prm = Parms_Workers, prm_time = Parms_Time, seed = seed)
MyWorkers <- f_setupSchedule(W = MyWorkers, prm = Parms_Workers, seed = seed)

# SCHEDULE VISUALISATION
# Plot schedule for all workers during a given period
f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 20)
# Plot schedule for some considered workers
f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 28, SHOW_ID = 1:60)
# Plot schedule with information at one given day
f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 28, SHOW_ID = 1:60, Dfocus = 9)


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

MyWorkers$W_location[is.na(MyWorkers$W_location)] <- "Home"

### WEARING MASK ###
by(data = MyWorkers,
   INDICES = MyWorkers$Day,
   FUN = f_dailyMaskWearing, ## check the script
   probMask = Parms_Workers$pMaskAcceptability[Parms_Workers$MaskType]) %>%
  data.table::rbindlist() %>%
  dplyr::arrange(t_ind, W_ID) -> MyWorkers

## Random state (in day) of the first initialized contaminated worker
MyWorkers <- f_initStatusCounterDay1(W = MyWorkers, prm_workers = Parms_Workers, prm_time = Parms_Time, seed = seed)

#### Inter-individuals variability in the viral load (RNA load), in log10 copies/ml
indi_viral_load <- f_individual_viral_load(prm_workers = Parms_Workers,
                                           prm_conta = Parms_Conta)

#### Save a checkpoint here if need to avoid re-simulating everytime
save.image("test_checkpoint1.RData")

load("test_checkpoint1.RData")


### AEROSOL ###
## --> Update f_Module_Master to take into account the individual variability in viral load
## --> Update f_Module_Master to take into account the individual variability in viral load
## --> Update f_Module_Master to take into account the individual variability in viral load

MyAir <- f_initAir(prm = Parms_Plant, prm_time = Parms_Time, prm_air = Parms_Air)
AIR_ID <- c(Parms_Plant$label,
            unname(unlist(lapply(Parms_Plant$Spaces, function (x) return(x$label)))))

Method_calc <- f_Air_Criteria_Calc(Parms_Plant,Parms_Air) ## Check if the droplets stay in air

## Assign 0 values for the first time index (required for the first run of f_Module_Master)
MyAir[MyAir$t_ind==0, 2:(1+length(Parms_Air$Droplet_class))] = matrix(0,7,4) * Method_calc

## --> Update f_Module_Master to take into account the individual variability in viral load
## --> Update f_Module_Master to take into account the individual variability in viral load
## --> Update f_Module_Master to take into account the individual variability in viral load




### INFECTION LOG ###
InfectionLog <- data.frame(W_ID = unique(MyWorkers$W_ID),
                           InfectedDay = rep(NA, Parms_Workers$NWorkers),
                           InfectionSource = rep(NA, Parms_Workers$NWorkers))

Infected_init <- subset(MyWorkers, Day == 1 & W_status == "infected")$W_ID %>% unique()
InfectionLog$InfectedDay[InfectionLog$W_ID %in% Infected_init] <- 1
InfectionLog$InfectionSource[InfectionLog$W_ID %in% Infected_init] <- "initialised"


### RUN CONTAMINATION ###
for (day in 2:5) {
  CONTA <- f_dailyContamination(W = MyWorkers,
                                MyAir = MyAir,
                                day = day,
                                indi_viral_load = indi_viral_load,
                                prm_plant = Parms_Plant,
                                prm_workers = Parms_Workers,
                                prm_time = Parms_Time,
                                prm_air = Parms_Air,
                                prm_conta = Parms_Conta,
                                inf_log = InfectionLog,
                                seed = seed)
  MyWorkers <- CONTA$W
  MyAir <- CONTA$MyAir
  InfectionLog <- CONTA$inf_log
  Expocum <- CONTA$Expocum
  Virion_dose <- CONTA$Virion_dose
}


InfectionSummary <- data.frame(Day = seq(1,max(MyWorkers$Day-1)),
                               Infected_cumul = rep(NA,max(MyWorkers$Day-1)),
                               Infectious = rep(NA, max(MyWorkers$Day-1)),
                               Symptomatic = rep(NA, max(MyWorkers$Day-1)),
                               Asymptomatic = rep(NA, max(MyWorkers$Day-1)),
                               Recovered_cumul = rep(NA, max(MyWorkers$Day-1)),
                               seed = rep(seed, max(MyWorkers$Day-1)))

for (day in 1:nrow(InfectionSummary)) {
  dW <- subset(W, Hour == 0 & Min == 0 & Day == day)
  InfectionSummary$Infected_cumul[day] <- length(dW$W_statusCounter[dW$W_statusCounter > 0])
  InfectionSummary$Infectious[day] <- length(dW$W_statusCounter[dW$W_status %in% c("infectious", "symptomatic", "asymptomatic")])
  InfectionSummary$Symptomatic[day] <- length(dW$W_status[dW$W_status == "symptomatic"])
  InfectionSummary$Asymptomatic[day] <- length(dW$W_status[dW$W_status == "asymptomatic"])
  InfectionSummary$Recovered_cumul[day] <- length(dW$W_statusCounter[dW$W_status == "recovered"])
}

OUTPUT <- list(seed = seed,
               MyPlant = MyPlant,
               MyWorkers = MyWorkers,
               MyAir = MyAir,
               InfectionSummary = InfectionSummary)