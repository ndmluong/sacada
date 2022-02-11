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
for (day in 2:(max(MyWorkers$Day)-1)) {
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


# ST1 <- Sys.time()
# OUTPUT_seed216 <- f_run_2M(prm_plant = Parms_Plant,
#                            prm_time = Parms_Time,
#                            prm_workers = Parms_Workers,
#                            prm_air = Parms_Air,
#                            prm_conta = Parms_Conta,
#                            seed = 216)
# ST2 <- Sys.time()
# OUTPUT_seed116 <- f_run_2M(prm_plant = Parms_Plant,
#                            prm_time = Parms_Time,
#                            prm_workers = Parms_Workers,
#                            prm_air = Parms_Air,
#                            prm_conta = Parms_Conta,
#                            seed = 116)
# OUTPUT_seed408 <- f_run_2M(prm_plant = Parms_Plant,
#                            prm_time = Parms_Time,
#                            prm_workers = Parms_Workers,
#                            prm_air = Parms_Air,
#                            prm_conta = Parms_Conta,
#                            seed = 408)
# OUTPUT_seed311 <- f_run_2M(prm_plant = Parms_Plant,
#                            prm_time = Parms_Time,
#                            prm_workers = Parms_Workers,
#                            prm_air = Parms_Air,
#                            prm_conta = Parms_Conta,
#                            seed = 311)
# OUTPUT_seed525 <- f_run_2M(prm_plant = Parms_Plant,
#                            prm_time = Parms_Time,
#                            prm_workers = Parms_Workers,
#                            prm_air = Parms_Air,
#                            prm_conta = Parms_Conta,
#                            seed = 525)
# OUTPUT_seed625 <- f_run_2M(prm_plant = Parms_Plant,
#                            prm_time = Parms_Time,
#                            prm_workers = Parms_Workers,
#                            prm_air = Parms_Air,
#                            prm_conta = Parms_Conta,
#                            seed = 625)
# OUTPUT_seed125 <- f_run_2M(prm_plant = Parms_Plant,
#                            prm_time = Parms_Time,
#                            prm_workers = Parms_Workers,
#                            prm_air = Parms_Air,
#                            prm_conta = Parms_Conta,
#                            seed = 125)
# OUTPUT_seed404 <- f_run_2M(prm_plant = Parms_Plant,
#                            prm_time = Parms_Time,
#                            prm_workers = Parms_Workers,
#                            prm_air = Parms_Air,
#                            prm_conta = Parms_Conta,
#                            seed = 404)
# OUTPUT_seed189 <- f_run_2M(prm_plant = Parms_Plant,
#                            prm_time = Parms_Time,
#                            prm_workers = Parms_Workers,
#                            prm_air = Parms_Air,
#                            prm_conta = Parms_Conta,
#                            seed = 189)
# OUTPUT_seed155 <- f_run_2M(prm_plant = Parms_Plant,
#                            prm_time = Parms_Time,
#                            prm_workers = Parms_Workers,
#                            prm_air = Parms_Air,
#                            prm_conta = Parms_Conta,
#                            seed = 155)
# ST3 <- Sys.time()
# 
# save.image("simulation_output/OUTPUT_2022_02_01.RData")

ggplot(data=IS) +
  geom_line(aes(x = Day, y = Infected_cumul, group = seed, colour = seed), size = 0.5) +
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
  scale_x_continuous(breaks = seq(1, max(IS$Day)+1, by = 7)) +
  scale_y_continuous(breaks = seq(0, max(IS$Infected_cumul)+5, by = 5)) +
  coord_cartesian(ylim = c(0, max(IS$Infected_cumul)+5),
                  xlim = c(0, max(IS$Day)+1)) +
  stat_summary(aes(x=Day, y=Infected_cumul), fun = mean, geom="line", size = 2, colour = "black") + 
  labs(title = "Cumulative number of infected workers",
       subtitle = paste(length(unique(IS$seed)), "independent simulations and the average curve")) +
  xlab("time (day)") + ylab("number of workers") -> g1
g1

### 1 seed, without threshold
ggplot(data=subset(IS, seed == 116)) +
  geom_ribbon(aes(x = Day, ymax = Symptomatic, ymin = 0), fill = "chocolate4", alpha = 0.9) +
  geom_ribbon(aes(x = Day, ymin = Symptomatic, ymax = Symptomatic+Asymptomatic), fill = "chocolate4", alpha = 0.7) +
  geom_ribbon(aes(x = Day, ymin = Symptomatic+Asymptomatic, ymax = InfectiousPeriod), fill = "chocolate4", alpha = 0.5) +
  geom_ribbon(aes(x = Day, ymin = InfectiousPeriod, ymax = InfectiousPeriod + NonInfectious), fill = "darkgray", colour = "darkgray", alpha = 0.95) +
  geom_ribbon(aes(x = Day, ymin = InfectiousPeriod + NonInfectious, ymax = Positive), fill = "darkgray", colour = "darkgray", alpha = 0.65) +
  geom_ribbon(aes(x = Day, ymin = Positive, ymax = Infected_cumul), fill = "darkgreen", colour = "darkgreen", alpha = 0.7) +
  geom_line(aes(x = Day, y = Infected_cumul), colour = "red", size = 3) +
  geom_line(aes(x = Day, y = InfectiousPeriod), colour = "chocolate4", size = 1.5) +
  geom_line(aes(x = Day, y = Positive), colour = "black", size = 2) +
  #geom_hline(yintercept = 15, colour = "navyblue", linetype = "dashed") +
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
  facet_grid(. ~ seed) +
  scale_x_continuous(breaks = seq(1, 56, by = 7)) +
  scale_y_continuous(breaks = seq(0, max(IS$Infected_cumul), by = 2)) +
  coord_cartesian(ylim = c(0, max(IS$Infected_cumul))) +
  labs(title = "Evolution of the number of infected workers",
       subtitle = "Cumulative and daily number of workers depending on their sanitary status") +
  xlab("time (day)") + ylab("number of workers")

### 5 seeds, without / with threshold
ggplot(data=subset(IS, seed %in% c(125, 216, 189, 311, 408))) +
  geom_ribbon(aes(x = Day, ymax = Symptomatic, ymin = 0), fill = "chocolate4", alpha = 0.9) +
  geom_ribbon(aes(x = Day, ymin = Symptomatic, ymax = Symptomatic+Asymptomatic), fill = "chocolate4", alpha = 0.7) +
  geom_ribbon(aes(x = Day, ymin = Symptomatic+Asymptomatic, ymax = InfectiousPeriod), fill = "chocolate4", alpha = 0.5) +
  geom_ribbon(aes(x = Day, ymin = InfectiousPeriod, ymax = InfectiousPeriod + NonInfectious), fill = "darkgray", colour = "darkgray", alpha = 0.95) +
  geom_ribbon(aes(x = Day, ymin = InfectiousPeriod + NonInfectious, ymax = Positive), fill = "darkgray", colour = "darkgray", alpha = 0.65) +
  geom_ribbon(aes(x = Day, ymin = Positive, ymax = Infected_cumul), fill = "darkgreen", colour = "darkgreen", alpha = 0.7) +
  geom_line(aes(x = Day, y = Infected_cumul), colour = "red", size = 3) +
  geom_line(aes(x = Day, y = InfectiousPeriod), colour = "chocolate4", size = 1.5) +
  geom_line(aes(x = Day, y = Positive), colour = "black", size = 2) +
  geom_hline(yintercept = 15, colour = "navyblue", linetype = "dashed") +
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
  facet_grid(. ~ seed) +
  scale_x_continuous(breaks = seq(1, 56, by = 7)) +
  scale_y_continuous(breaks = seq(0, max(IS$Infected_cumul), by = 2)) +
  coord_cartesian(ylim = c(0, max(IS$Infected_cumul))) +
  labs(title = "Cumulative number of infected workers") +
  xlab("time (day)") + ylab("number of workers")

### all seeds, with threshold
ggplot(data=IS) +
  geom_ribbon(aes(x = Day, ymax = Symptomatic, ymin = 0), fill = "chocolate4", alpha = 0.9) +
  geom_ribbon(aes(x = Day, ymin = Symptomatic, ymax = Symptomatic+Asymptomatic), fill = "chocolate4", alpha = 0.7) +
  geom_ribbon(aes(x = Day, ymin = Symptomatic+Asymptomatic, ymax = InfectiousPeriod), fill = "chocolate4", alpha = 0.5) +
  geom_ribbon(aes(x = Day, ymin = InfectiousPeriod, ymax = InfectiousPeriod + NonInfectious), fill = "darkgray", colour = "darkgray", alpha = 0.95) +
  geom_ribbon(aes(x = Day, ymin = InfectiousPeriod + NonInfectious, ymax = Positive), fill = "darkgray", colour = "darkgray", alpha = 0.65) +
  geom_ribbon(aes(x = Day, ymin = Positive, ymax = Infected_cumul), fill = "darkgreen", colour = "darkgreen", alpha = 0.7) +
  geom_line(aes(x = Day, y = Infected_cumul), colour = "red", size = 3) +
  geom_line(aes(x = Day, y = InfectiousPeriod), colour = "chocolate4", size = 1.5) +
  geom_line(aes(x = Day, y = Positive), colour = "black", size = 2) +
  geom_hline(yintercept = 15, colour = "navyblue", linetype = "dashed") +
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
  facet_grid(. ~ seed) +
  scale_x_continuous(breaks = seq(1, 56, by = 7)) +
  scale_y_continuous(breaks = seq(0, max(IS$Infected_cumul), by = 2)) +
  coord_cartesian(ylim = c(0, max(IS$Infected_cumul))) +
  labs(title = "Cumulative number of infected workers") +
  xlab("time (day)") + ylab("number of workers") -> g2


ggplot() + 
  geom_line(OUTPUT_seed408$MyAir, mapping=aes(x=t_ind, y=d01), colour="red") +
  geom_line(OUTPUT_seed408$MyAir, mapping=aes(x=t_ind, y=d02), colour="blue") + 
  geom_line(OUTPUT_seed408$MyAir, mapping=aes(x=t_ind, y=d03), colour="green") + 
  geom_line(OUTPUT_seed408$MyAir, mapping=aes(x=t_ind, y=d04), colour="orange") + 
  xlab("time index") + ylab("number of droplets per m3") +
  theme(axis.ticks=element_blank(),
        #legend.position = "none",
        #panel.background=element_rect(fill="grey"),
        plot.title = element_text(face="bold", size=15),
        axis.title = element_text(face="bold", size=10),
        axis.text = element_text(size=10)) + 
  scale_x_continuous(breaks = seq(1, max(MyAir$t_ind), by = 2016)) +
  facet_grid(AIR_ID ~ .) -> g3




##### Covariate: Number of individual-days
IS <- data.frame(IS,
                 NID = (100 - IS$Infected_cumul) * IS$Day,
                 log10NID = log10((100 - IS$Infected_cumul) * IS$Day),
                 Infectious_interday = rep(NA, nrow(IS)))

all_seed <- unique(IS$seed) %>% as.character
for (i in all_seed) {
  for (j in 2:max(IS$Day)) {
    IS$Infectious_interday[IS$seed == i & IS$Day == j] <- abs(IS$InfectiousPeriod[IS$seed == i & IS$Day == j] - IS$InfectiousPeriod[IS$seed == i & IS$Day == j-1])
  }
}

ISEnd <- subset(IS, Day == max(IS$Day))

tapply(ISEnd$log10NID, ISEnd$seed, max)

tapply(IS$InfectiousPeriod, IS$seed, FUN = function(x) {
  if (max(x)-15 >= 0) {return(-1)} else return(0)
}) -> alpha1

tapply(IS$InfectiousPeriod, IS$seed, FUN = function(x) {
  if (max(x)-15 >= 0) {return(0)} else return(1)
}) -> alpha2

simulation_score <- alpha1 / tapply(ISEnd$log10NID, ISEnd$seed, max) + alpha2 * tapply(ISEnd$log10NID, ISEnd$seed, max)



score_df <- data.frame(seed = names(simulation_score),
                       simulation_score = unname(simulation_score),
                       plant_stop = simulation_score <= 0)
score_df <- arrange(score_df, simulation_score)
score_df$seed <- factor(score_df$seed, levels = unique(score_df$seed))

ggplot(data = score_df) +
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face="bold", size=15),
        axis.title = element_text(face="bold", size=10),
        axis.text = element_text(size=10),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="lightgrey"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_hline(yintercept = 0, colour = "black", size = 2) +
  scale_color_manual(values = c("TRUE"="red","FALSE"="darkgreen")) +
  geom_point(aes(x=seed, y=simulation_score, colour = plant_stop), size=3) + 
  geom_segment(aes(x=seed, xend=seed, yend = 0, y=simulation_score, colour = plant_stop), size = 2)
