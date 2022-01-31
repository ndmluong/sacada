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

for (day in 6:15) {
  CONTA <- f_dailyContamination(W = W,
                                MyAir = MyAir,
                                day = day,
                                prm_plant = Parms_Plant,
                                prm_workers = Parms_Workers,
                                prm_time = Parms_Time,
                                prm_air = Parms_Air,
                                prm_conta = Parms_Conta,
                                seed = seed)
  W <- CONTA$W
  MyAir <- CONTA$MyAir
}



InfectionSummary <- data.frame(Day = seq(1,56),
                               Infected_cumul = rep(NA,56),
                               Infectious = rep(NA, 56),
                               Symptomatic = rep(NA, 56),
                               Asymptomatic = rep(NA, 56),
                               Recovered_cumul = rep(NA,56))

for (day in 1:nrow(InfectionSummary)) {
  dW <- subset(W, Hour == 0 & Min == 0 & Day == day)
  InfectionSummary$Infected_cumul[day] <- length(dW$W_statusCounter[dW$W_statusCounter > 0])
  InfectionSummary$Infectious[day] <- length(dW$W_statusCounter[dW$W_statusCounter > 2 & dW$W_statusCounter < 12])
  InfectionSummary$Symptomatic[day] <- length(dW$W_status[dW$W_status == "symptomatic"])
  InfectionSummary$Asymptomatic[day] <- length(dW$W_status[dW$W_status == "asymptomatic"])
  InfectionSummary$Recovered_cumul[day] <- length(dW$W_statusCounter[dW$W_statusCounter > 15])
}



ggplot(data=InfectionSummary) +
  geom_line(aes(x = Day, y = Infected_cumul), colour = "red", size = 2) +
  geom_line(aes(x = Day, y = Recovered_cumul), colour = "darkgreen", size = 2) +
  theme(axis.ticks=element_blank(),
        #legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face="bold", size=15),
        axis.title = element_text(face="bold", size=10),
        axis.text = element_text(size=10),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  scale_x_continuous(breaks = seq(1, 56, by = 7)) +
  scale_y_continuous(breaks = seq(0, max(InfectionSummary$Infected_cumul), by = 2)) +
  labs(title = "Cumulative number of infected / recovered workers") +
  xlab("time (day)") + ylab("Cumulative number of workers") -> g1

ggplot(data=InfectionSummary) +
  geom_line(aes(x = Day, y = Infectious), colour = "darkorange", size = 2) +
  geom_line(aes(x = Day, y = Symptomatic), colour = "orange") +
  theme(axis.ticks=element_blank(),
        #legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face="bold", size=15),
        axis.title = element_text(face="bold", size=10),
        axis.text = element_text(size=10),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  scale_x_continuous(breaks = seq(1, 56, by = 7)) +
  scale_y_continuous(breaks = seq(0, max(InfectionSummary$Infected_cumul), by = 2)) +
  coord_cartesian(ylim = c(0, max(InfectionSummary$Infected_cumul))) +
  labs(title = "Daily number of infectious workers") +
  xlab("time (day)") + ylab("Daily number of workers") -> g2

gridExtra::grid.arrange(g1,g2,nrow=2)



# plot on same grid, each series colored differently -- 
# good if the series have same scale
MyAir[MyAir$AIR_ID=='Waste area',2:6]<-0

MyAir[MyAir$AIR_ID=='Arrival gate',2:6]<-0
MyAir[MyAir$AIR_ID=='Entry hall',2:6]<-0
MyAir[MyAir$AIR_ID=='Cooling area',2:6]<-0


ggplot() + 
  geom_line(MyAir, mapping=aes(x=t_ind, y=d01), colour="red") +
  geom_line(MyAir, mapping=aes(x=t_ind, y=d02), colour="blue") + 
  geom_line(MyAir, mapping=aes(x=t_ind, y=d03), colour="green") + 
  geom_line(MyAir, mapping=aes(x=t_ind, y=d04), colour="orange") + 
  xlab("time index") + ylab("number of droplets") +
  theme(axis.ticks=element_blank(),
        #legend.position = "none",
        #panel.background=element_rect(fill="grey"),
        plot.title = element_text(face="bold", size=15),
        axis.title = element_text(face="bold", size=10),
        axis.text = element_text(size=10)) + 
  scale_x_continuous(breaks = seq(1, max(MyAir$t_ind), by = 2016)) +
  facet_grid(AIR_ID ~ .) -> g3





# for (wi in 1:length(W_SymptomBegin)) {
#   WD$W_status[which(WD$W_ID == W_SymptomBegin[wi] &
#                      WD$W_statusCounter == prm_workers$SymptomDay)] <- SymptomEvent[wi]
# }

WD <- MyWorkers

W_SymptomBegin <- c("W012", "W018", "W033")
SymptomEvent <- c("S1","S2","S3")
names(SymptomEvent) <- W_SymptomBegin

WComp <- subset(WD, !W_ID %in% W_SymptomBegin)

lapply(W_SymptomBegin, FUN = function(x) {
  W1 <- subset(WD, W_ID == x)
  W1$W_status[which(W1$Day == 1)] <- unname(SymptomEvent[x])
  return(W1)
}) %>%
  data.table::rbindlist() %>%
  rbind(subset(WD, !W_ID %in% W_SymptomBegin)) -> aa
  

