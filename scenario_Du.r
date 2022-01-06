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
# source("functions/functions_module_master_proposition_modif.R")
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
# source("parameters/parameters_food.R") ## FOOD PORTIONS


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
f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 56, SHOW_ID = 1:60, Dfocus = 4)


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

save.image("2022_01_06_100Workers_56days_Step5_Schedule_Location_Mask_StatusCounter_SIRday0.RData")

### AEROSOL ###
# prm_plant = Parms_Plant
# prm_air = Parms_Air
# prm_time = Parms_Time
# prm_workers = Parms_Workers

MyAir <- f_initAir(prm = Parms_Plant, prm_time = Parms_Time, prm_air = Parms_Air)
AIR_ID <- c(Parms_Plant$label,
            unname(unlist(lapply(Parms_Plant$Spaces, function (x) return(x$label)))))


## CALCULATION THE SUM OF EXPOSITION AT A GIVEN DAY
day <- 1

f_Who_is(Sub_MyWorkers = subset(MyWorkers, t_ind == 0),
         prm_plant = Parms_Plant,
         NWorkers = Parms_Workers$NWorkers,
         ind = 0) # OK

Dose_per_class1 <- matrix(0,100,1)
Dose_per_class2 <- matrix(0,100,1)
Dose_per_class3 <- matrix(0,100,1)
Dose_per_class4 <- matrix(0,100,1)

print(paste("Day ", day, sep = ""))

ind_min <- subset(MyWorkers, Day == day)$t_ind %>% min()
ind_max <- subset(MyWorkers, Day == day)$t_ind %>% max()

OUT <- f_Module_Master(MyAir = MyAir,
                       prm_plant = Parms_Plant,
                       prm_air = Parms_Air,
                       prm_time = Parms_Time,
                       prm_workers = Parms_Workers,
                       ind_min = 0, ind_max = 287)

MyAir <- OUT[[1]]
Expocum <-OUT[[2]]
Viral_Load <-5e8
P=Viral_Load*prm_air$d_Vol

### VERIFIER LA DOSE PER CLASS: Parametre size en particulier
Dose_per_class1[Expocum[,1]>0] <- rbinom(n = sum(Expocum[,1]>0),size= round(Expocum[Expocum[,1]>0,1]), prob = P[1])
Dose_per_class2[Expocum[,2]>0] <- rbinom(n = sum(Expocum[,2]>0),size= round(Expocum[Expocum[,2]>0,2]), prob = P[2])
Dose_per_class3[Expocum[,3]>0] <- rbinom(n = sum(Expocum[,3]>0),size= round(Expocum[Expocum[,3]>0,3]), prob = P[3])
Dose_per_class4[Expocum[,4]>0] <- rbinom(n = sum(Expocum[,4]>0),size= round(Expocum[Expocum[,4]>0,4]), prob = P[4])

Dose_tot[,Day]=Dose_per_class1+Dose_per_class2+Dose_per_class3+Dose_per_class4
# Dose_per_class <- lapply(1:length(prm_air$Droplet_class), function (x) 
#   return(rbinom(n = sum(Expocum[,x]>0),size= round(Expocum[Expocum[,x]>0,x]), prob = P[x])))
# Dose_tot[,Day] <- Reduce("+",Dose_per_class[[1:4]])

Risk=0
Risk_W=0
New_sicks=0  
New_sicks_W=0

Risk = 1-exp(-Dose_tot[,Day]/50)
Risk_W=unique(MyWorkers$W_ID[Risk>0])
New_sicks <- rbinom(n = sum(Risk>0), size = 1, prob = Risk[Risk>0])
New_sicks_W <- Risk_W[New_sicks>0]

print(New_sicks_W)

for (IDW in New_sicks_W){
  MyWorkers$W_status[MyWorkers$W_ID==IDW & MyWorkers$t_ind>ind_max] <-"contaminated"
}







### CONTAMINATIONS ###
W <- MyWorkers




W$W_statusCounter[which(W$W_status == "initialiseds infected")] <- sample(Parms_Workers$ContaBeginDay:Parms_Workers$ContaEndDay, size = 1)


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















### CODE STEVEN
Dose_tot<-matrix(0,prm_workers$NWorkers,max(MyWorkers$Day))
N_contaminated=0
# for (Day in unique(MyWorkers$Day-1) ){
MyWorkers$W_location[is.na(MyWorkers$W_location)]<-"home"
for (Day in 1:57 ){
  
  Dose_per_class1 <- matrix(0,100,1)
  Dose_per_class2 <- matrix(0,100,1)
  Dose_per_class3 <- matrix(0,100,1)
  Dose_per_class4 <- matrix(0,100,1)
  
  print(Day)
  ind_min <- 24*60/Parms_Time$Step*(Day-1)
  ind_max <- 24*60/Parms_Time$Step*(Day)-1
  
  N_contaminated[Day] = sum (MyWorkers$W_status[MyWorkers$t_ind==ind_min] =="contaminated")
  print(N_contaminated[Day])
  OUT <- f_Module_Master(prm_plant, prm_air, prm_time, prm_workers,ind_min,ind_max)
  
  MyAir <- OUT[[1]]
  Expocum <-OUT[[2]]
  Viral_Load <-5e8
  P=Viral_Load*prm_air$d_Vol
  
  ### VERIFIER LA DOSE PER CLASS: Parametre size en particulier
  Dose_per_class1[Expocum[,1]>0] <- rbinom(n = sum(Expocum[,1]>0),size= round(Expocum[Expocum[,1]>0,1]), prob = P[1])
  Dose_per_class2[Expocum[,2]>0] <- rbinom(n = sum(Expocum[,2]>0),size= round(Expocum[Expocum[,2]>0,2]), prob = P[2])
  Dose_per_class3[Expocum[,3]>0] <- rbinom(n = sum(Expocum[,3]>0),size= round(Expocum[Expocum[,3]>0,3]), prob = P[3])
  Dose_per_class4[Expocum[,4]>0] <- rbinom(n = sum(Expocum[,4]>0),size= round(Expocum[Expocum[,4]>0,4]), prob = P[4])
  
  Dose_tot[,Day]=Dose_per_class1+Dose_per_class2+Dose_per_class3+Dose_per_class4
  # Dose_per_class <- lapply(1:length(prm_air$Droplet_class), function (x) 
  #   return(rbinom(n = sum(Expocum[,x]>0),size= round(Expocum[Expocum[,x]>0,x]), prob = P[x])))
  # Dose_tot[,Day] <- Reduce("+",Dose_per_class[[1:4]])
  
  Risk=0
  Risk_W=0
  New_sicks=0  
  New_sicks_W=0
  
  Risk = 1-exp(-Dose_tot[,Day]/50)
  Risk_W=unique(MyWorkers$W_ID[Risk>0])
  New_sicks <- rbinom(n = sum(Risk>0), size = 1, prob = Risk[Risk>0])
  New_sicks_W <- Risk_W[New_sicks>0]
  
  print(New_sicks_W)
  
  for (IDW in New_sicks_W){
    MyWorkers$W_status[MyWorkers$W_ID==IDW & MyWorkers$t_ind>ind_max] <-"contaminated"
  }
  
}
plot(x = seq(1:length(N_contaminated)),y = N_contaminated)


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


