#test code Du
##### PACKAGES #####
library(ggplot2)
library(reshape2)
library(plotly)
library(stringr)
library(data.table)
library(dplyr)

##### FUNCTIONS #####
source("functions/functions_actions.R")
source("functions/functions_air.R")
# source("functions/functions_air_module.R")
source("functions/functions_contamination.R")
source("functions/functions_dailyWork.R")
# source("functions/functions_food.R")
# source("functions/functions_module_master.R")
source("functions/functions_module_master_proposition_modif.R")
source("functions/functions_plant.R")
source("functions/functions_plot.R")
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
set.seed(seed)
by(data = MyWorkers,
   INDICES = MyWorkers$Day,
   FUN = f_dailyMaskWearing, ## check the script
   probMask = Parms_Workers$pMaskAcceptability["Surgical mask"]) %>%
  data.table::rbindlist() %>%
  dplyr::arrange(t_ind, W_ID) -> MyWorkers

## Random state (in day) of the first initialized contaminated worker
MyWorkers <- f_initStatusCounterDay1(W = MyWorkers, prm_workers = Parms_Workers, prm_time = Parms_Time, seed = seed)

save.image("2022_01_20_checkpoint1.RData")


### AEROSOL ###
# prm_plant = Parms_Plant
# prm_air = Parms_Air
# prm_time = Parms_Time
# prm_workers = Parms_Workers
source("functions/functions_module_master_proposition_modif.R")
source("functions/functions_contamination.R")

MyAir <- f_initAir(prm = Parms_Plant, prm_time = Parms_Time, prm_air = Parms_Air)
AIR_ID <- c(Parms_Plant$label,
            unname(unlist(lapply(Parms_Plant$Spaces, function (x) return(x$label)))))

Method_calc <- f_Air_Criteria_Calc(Parms_Plant,Parms_Air) ## Check if the droplets stay in air

## Assign 0 values for the first time index (required for the first run of f_Module_Master)
MyAir[MyAir$t_ind==0, 2:(1+length(Parms_Air$Droplet_class))] = matrix(0,7,4) * Method_calc 

# Dose_tot <- matrix(0, Parms_Workers$NWorkers, max(MyWorkers$Day)-1) # nb workers * nb day


## RUN MODEL
W <- MyWorkers



W <- f_dailyContamination(W = W,
                          MyAir = MyAir,
                          day = 5,
                          prm_plant = Parms_Plant,
                          prm_workers = Parms_Workers,
                          prm_time = Parms_Time,
                          prm_air = Parms_Air,
                          prm_conta = Parms_Conta,
                          seed = seed)

for (day in 2:15) {
  W <- f_dailyContamination(W = W,
                            MyAir = MyAir,
                            day = day,
                            prm_plant = Parms_Plant,
                            prm_workers = Parms_Workers,
                            prm_time = Parms_Time,
                            prm_air = Parms_Air,
                            prm_conta = Parms_Conta,
                            seed = seed)
}



### Contamination via communities
day <- 2
# WD <- subset(W, Day == day & Hour == 0 & Min == 0)

W_com <- subset(W408, Day == day & Hour == 0 & Min == 0 & !is.na(W_communes))

SusceptibleWorkers <- W_com$W_ID[W_com$W_status == "susceptible"] ## ! %in% InfectedWorkers

sapply(SusceptibleWorkers, FUN = function(wi) { ## for each susceptible worker
  comm_ID <- W_com$W_communes[W_com$W_ID == wi] %>% as.character()
  comm <- subset(W_com, W_communes == comm_ID)
  
  W_infected_source <- comm$W_statusCounter[comm$W_statusCounter > 0 & comm$W_statusCounter < 16]
  
  if (length(W_infected_source) > 0) {
    resp_wi <- rbinom(n=1, size=1, prob = 0.1)
    if (resp_wi > 0) {
      writeLines(paste("- Worker ", wi,
                       " infected probably by the workers ", W_infected_source,
                       " within the community ", comm_ID, sep = ""))
    }
  } else {
    resp_wi <- 0
  }
  return(resp_wi)
})



NewInfectedWorkers_Comm <- CommInfection_resp[CommInfection_resp > 0] %>% names()

if (length(NewInfectedWorkers_Comm) > 0) {
  InfectedWorker
}



# plot on same grid, each series colored differently -- 
# good if the series have same scale
MyAir[MyAir$AIR_ID=='Waste area',2:6]<-0

MyAir[MyAir$AIR_ID=='Arrival gate',2:6]<-0
MyAir[MyAir$AIR_ID=='Entry hall',2:6]<-0
MyAir[MyAir$AIR_ID=='Cooling area',2:6]<-0


ggplot() + 
  geom_point(MyAir, mapping=aes(x=t_ind, y=d01), colour="red") +
  geom_point(MyAir, mapping=aes(x=t_ind, y=d02), colour="blue") + 
  geom_point(MyAir, mapping=aes(x=t_ind, y=d03), colour="green") + 
  geom_point(MyAir, mapping=aes(x=t_ind, y=d04), colour="orange") + 
  
  facet_grid(AIR_ID ~ .)

ggplot(subset(MyAir,AIR_ID=="Cutting Room")) + 
  geom_point( mapping=aes(x=t_ind, y=d01), colour="red") +
  geom_point( mapping=aes(x=t_ind, y=d02), colour="blue") + 
  geom_point( mapping=aes(x=t_ind, y=d03), colour="green") + 
  geom_point( mapping=aes(x=t_ind, y=d04), colour="orange") -> g1

ggplot(subset(MyAir,AIR_ID=="Cooling area")) + 
  geom_point( mapping=aes(x=t_ind, y=d01), colour="red") +
  geom_point( mapping=aes(x=t_ind, y=d02), colour="blue") + 
  geom_point( mapping=aes(x=t_ind, y=d03), colour="green") + 
  geom_point( mapping=aes(x=t_ind, y=d04), colour="orange") -> g2


ggplotly(g1)
ggplotly(g2)
ggplot(subset(MyAir,AIR_ID=="Cutting Room"  & t_ind>250 & t_ind<400)) + 
  geom_line( mapping=aes(x=t_ind, y=d01), colour="red") +
  geom_line( mapping=aes(x=t_ind, y=d02), colour="blue") + 
  geom_line( mapping=aes(x=t_ind, y=d03), colour="green") + 
  geom_line( mapping=aes(x=t_ind, y=d04), colour="orange")   


ggplot(MyAir, aes(t_ind,d01)) + geom_point(aes(colour = AIR_ID))

ggplot(MyAir, aes(t_ind,d02)) + geom_point(aes(colour = AIR_ID))
ggplot(MyAir, aes(t_ind,d03)) + geom_point(aes(colour = AIR_ID))
ggplot(MyAir, aes(t_ind,d04)) + geom_point(aes(colour = AIR_ID))


# Number of contaminated employees in the rooms
# Number of non-contaminated employees in the rooms 








Delta_Cd < -f_AirModuleCalc(V_rooms,V_renew,S_rooms)

Cd[i+1] = Cd[i]+Delta_Cd*dt/V_rooms # Number of droplets (m-3)


