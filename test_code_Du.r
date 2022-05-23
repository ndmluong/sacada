##### PACKAGES #####
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

##### FUNCTIONS #####
source("functions/functions_actions.R")
source("functions/functions_air.R")
source("functions/functions_contamination.R")
source("functions/functions_dailyWork.R")
source("functions/functions_food.R")
source("functions/functions_master.R")
source("functions/functions_plant.R")
source("functions/functions_plot.R")
source("functions/functions_run.R")
source("functions/functions_surfaces.R")
source("functions/functions_time.R")
source("functions/functions_workers.R")


##### PARAMETERS #####
## Check the scripts for more details / change parameter values if needed
source("parameters/parameters_plant.R") ## PLANT
source("parameters/parameters_time.R") ## TIME
source("parameters/parameters_workers.R") ## WORKERS
source("parameters/parameters_food.R") ## FOOD PORTIONS
source("parameters/parameters_air.R") ## AIR
source("parameters/parameters_conta.R") ## CONTAMINATION
source("parameters/parameters_surfaces.R") ## SURFACES


##### PLANT #####
## Create the plant
MyPlant <- f_createPlant(prm = Parms_Plant)
## Plot
g_emptyPlant <- f_plotPlant(Plant = MyPlant,
                            prm = Parms_Plant)
# g_emptyPlant
# ggplotly(g_emptyPlant)

seed = 103

##### WORKERS #####
### SCHEDULE ###
# Create workers : MyWorkers
MyWorkers <- f_initWorkers(prm = Parms_Workers, prm_time = Parms_Time, seed = seed)
MyWorkers <- f_setupSchedule(W = MyWorkers, prm = Parms_Workers, seed = seed)

# # SCHEDULE VISUALISATION
# # Plot schedule with information at one given day
# f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 28, SHOW_ID = 1:60, Dfocus = 9)


### ASSIGN LOCATION BASED ON SCHEDULE ###
WorkingDays <- subset(MyWorkers,
                      !Weekday %in% c("Saturday", "Sunday") & Day < max(MyWorkers$Day))$Day %>% unique() %>% sort()
# OtherDays <- subset(MyWorkers, !Day %in% WorkingDays)$Day %>% unique() %>% sort()

## Check worker type composition for each team
MyWorkers <- f_checkdailyWorkerType(W = MyWorkers, day = 1)

## Assign location based on schedule for the first day (day 1)
d1 <- subset(MyWorkers, Day == 1)
d1 <- f_dailyWork_AllTeams(Plant = MyPlant, W = d1, D = 1, dt = Step, seed = seed+1)
d1$location[is.na(d1$location)] <- "Home"
rbind(d1, subset(MyWorkers, Day != 1)) %>%
  arrange(., t_ind, W_ID) -> MyWorkers
rm(d1)

### WEARING MASK ###
f_dailyMaskWearing(WD = subset(MyWorkers, Day == 1),
                   probMask = Parms_Workers$pMaskAcceptability[Parms_Workers$MaskType]) %>%
  rbind(., subset(MyWorkers, Day != 1)) %>%
  arrange(.,t_ind, W_ID) -> MyWorkers

## Random state (in day) of the first initialized contaminated worker
MyWorkers <- f_initStatusCounterDay1(W = MyWorkers, prm_workers = Parms_Workers, prm_time = Parms_Time, seed = seed)

#### Inter-individuals variability in the viral load (RNA load), in log10 copies/ml
indi_viral_load <- f_individual_viral_load(prm_workers = Parms_Workers,
                                           prm_air = Parms_Air,
                                           prm_conta = Parms_Conta)

##### AEROSOL #####
MyAir <- f_initAir(prm = Parms_Plant, prm_time = Parms_Time, prm_air = Parms_Air)

##### INFECTION LOG #####
## Infection log indicating the contamination day and sources of every workers over time
InfectionLog <- data.frame(W_ID = unique(MyWorkers$W_ID),
                           InfectedDay = rep(NA, Parms_Workers$NWorkers),
                           InfectionSource = rep(NA, Parms_Workers$NWorkers))

Infected_init <- subset(MyWorkers, Day == 1 & W_status == "infected")$W_ID %>% unique()
InfectionLog$InfectedDay[InfectionLog$W_ID %in% Infected_init] <- 1
InfectionLog$InfectionSource[InfectionLog$W_ID %in% Infected_init] <- "initialised"

##### SURFACES #####
## Initialize the surfaces for the day 1 ###
MySurfaces <- f_initSurfaces(P = MyPlant$P,
                             prm_plant = Parms_Plant,
                             prm_time = Parms_Time,
                             day = 1)

##### FOOD PORTIONS #####
## Initialize the food portion for the day 1 ###
MyFood <- f_ProcessFood(prm_food = Parms_Food,
                        prm_workers = Parms_Workers,
                        prm_time = Parms_Time,
                        W = MyWorkers,
                        day = 1) ## Day 1

Expocum <- list()

FP_summary <- data.frame(Day = numeric(),
                         nb_carcass = numeric(),
                         nb_FP = numeric(),
                         pos_FP = numeric())

S_summary <- data.frame(Day = numeric(),
                        pos_S = numeric(),
                        S_ID = character())

## save.image("checkpoint0.RData")
#### SIMULATING DAILY CONTAMINATIONS ####
writeLines("***** Simulating daily contamination *****")

for (day in 2:6) {
  CONTA <- f_dailyContamination(MyAir = MyAir,
                                W = MyWorkers,
                                S = MySurfaces,
                                FP = MyFood,
                                day = day,
                                indi_viral_load = indi_viral_load,
                                prm_plant = Parms_Plant,
                                prm_workers = Parms_Workers,
                                prm_time = Parms_Time,
                                prm_air = Parms_Air,
                                prm_conta = Parms_Conta,
                                prm_surfaces = Parms_Surfaces,
                                inf_log = InfectionLog,
                                seed = seed)
  
  ## Processing CONTA (1) - FOOD
  ## Food portions - Contamination summary
  FP_End <- subset(CONTA$FP, location == "Cooling area")
  FP_summary %>% 
    add_row(Day = day-1,
            nb_carcass = length(unique(FP_End$carcass_ID)),
            nb_FP = length(unique(FP_End$FP_ID)),
            pos_FP = length(FP_End$RNA_load[FP_End$RNA_load > Parms_Surfaces$pos_threshold])) -> FP_summary
  
  
  ## Surfaces - Contamination summary      
  S_End <- subset(CONTA$S, t_ind == max(CONTA$S$t_ind))
  S_summary %>%
    add_row(Day = day-1,
            pos_S = length(S_End$RNA_load[S_End$RNA_load >= Parms_Surfaces$pos_threshold]),
            S_ID = subset(S_End, RNA_load >= Parms_Surfaces$pos_threshold)$S_ID %>% paste(., collapse = ", ")) -> S_summary
  
  MyWorkers <- CONTA$W
  MyAir <- CONTA$MyAir
  InfectionLog <- CONTA$inf_log
  Expocum[[day]] <- CONTA$Expocum
  
  ## Processing CONTA (1) - WORKERS
  ## set possible absence for symptomatic worker(s)
  writeLines(">>> Setting possible absence for symptomatic workers")
  MyWorkers <- f_setAbsence(W = MyWorkers, day = day, prm_workers = Parms_Workers)
  
  ## Check if there is any missing logistic workers in every team
  if (day %in% WorkingDays) {
    MyWorkers <- f_checkdailyWorkerType(W = MyWorkers, day = day)
  }
  
  ## Assign location based on schedule and the number of active workers for the current day
  if (day %in% WorkingDays) {
    d1 <- subset(MyWorkers, Day == day)
    d1 <- f_dailyWork_AllTeams(Plant = MyPlant, W = d1, D = day, dt = Step, seed = seed+day)
    d1$location[is.na(d1$location)] <- "Home"
    rbind(d1, subset(MyWorkers, Day != day)) %>%
      arrange(., t_ind, W_ID) -> MyWorkers
    rm(d1)
  } else {
    MyWorkers$location[is.na(MyWorkers$location)] <- "Home"
  }
  
  # Wearing mask #
  f_dailyMaskWearing(WD = subset(MyWorkers, Day == day),
                     probMask = Parms_Workers$pMaskAcceptability[Parms_Workers$MaskType]) %>%
    rbind(., subset(MyWorkers, Day != day)) %>%
    arrange(.,t_ind, W_ID) -> MyWorkers
  
  ## Initialize the surfaces for the day ###
  MySurfaces <- f_initSurfaces(P = MyPlant$P,
                               prm_plant = Parms_Plant,
                               prm_time = Parms_Time,
                               day = day)
  
  ##### FOOD PORTIONS #####
  ## Initialize the food portion for the day  ###
  if (day %in% WorkingDays) {
    MyFood <- f_ProcessFood(prm_food = Parms_Food,
                            prm_workers = Parms_Workers,
                            prm_time = Parms_Time,
                            W = MyWorkers,
                            day = day)
  } else {
    MyFood <- data.frame(FP_ID = character(), carcass_ID = character(), t_ind = numeric(),
                         coords_ID = character(), coordX = numeric(), coordY = numeric(), location = character(), RNA_load = numeric())
  }
  
}






#### SUMMARY FOR ALL INFECTIONS AFTER 42 DAYS ####
writeLines("***** Infection: Summary processing *****")
InfectionSummary <- data.frame(Day = seq(1,max(MyWorkers$Day-1)),
                               Infected = rep(NA, max(MyWorkers$Day-1)),
                               Infectious = rep(NA, max(MyWorkers$Day-1)),
                               Symptomatic = rep(NA, max(MyWorkers$Day-1)),
                               Asymptomatic = rep(NA, max(MyWorkers$Day-1)),
                               InfectiousPeriod = rep(NA, max(MyWorkers$Day-1)),
                               NonInfectious = rep(NA, max(MyWorkers$Day-1)),
                               Positive = rep(NA, max(MyWorkers$Day-1)),
                               Infected_cumul = rep(NA,max(MyWorkers$Day-1)),
                               Recovered_cumul = rep(NA, max(MyWorkers$Day-1)),
                               seed = rep(seed, max(MyWorkers$Day-1)))

for (day in 1:nrow(InfectionSummary)) {
  dW <- subset(MyWorkers, Hour == 0 & Min == 0 & Day == day)
  InfectionSummary$Infected[day] <- length(dW$W_status[dW$W_status == "infected"])
  InfectionSummary$Infectious[day] <- length(dW$W_status[dW$W_status == "infectious"])
  InfectionSummary$Symptomatic[day] <- length(dW$W_status[dW$W_status == "symptomatic"])
  InfectionSummary$Asymptomatic[day] <- length(dW$W_status[dW$W_status == "asymptomatic"])
  InfectionSummary$InfectiousPeriod[day] <- length(dW$W_status[dW$W_status %in% c("infectious", "symptomatic", "asymptomatic")])
  InfectionSummary$NonInfectious[day] <- length(dW$W_status[dW$W_status == "non-infectious"])
  InfectionSummary$Infected_cumul[day] <- length(dW$W_statusCounter[dW$W_statusCounter > 0])
  InfectionSummary$Recovered_cumul[day] <- length(dW$W_status[dW$W_status == "recovered"])
}

InfectionSummary$Positive <- InfectionSummary$Infected_cumul - InfectionSummary$Recovered_cumul 

OUTPUT <- list(seed = seed,
               MyPlant = MyPlant,
               MyWorkers = MyWorkers,
               MyAir = MyAir,
               MySurfaces = MySurfaces,
               InfectionLog = InfectionLog,
               InfectionSummary = InfectionSummary,
               Expocum = Expocum)

















##### Estimating R #####
source("functions/functions_contamination.R")
Parms_Conta <- append(Parms_Conta, list(SerialInterval = list(original = c("mu" = 3.96, "sigma" = 4.75),
                                                 delta = c("mu" = 3.4, "sigma" = 0.35), ## Zhanwei Du et al. 2022
                                                 omicron = c("mu" = 3.1, "sigma" = 0.15), ## Zhanwei Du et al. 2022
                                                 alpha = c("mu" = NA, "sigma" = NA)) ))

lapply(unique(IS$seed), FUN = function(seedx) {
  writeLines(paste("seed", seedx))
  ISsub <- subset(IS, seed == seedx)
  f_estimateR(ISsub = ISsub,
              prm_conta = Parms_Conta)
}) 




ISsub <- subset(IS, seed == 103)


ISsub <- tibble::add_column(ISsub, Incidence = NA, .after = "Day")
ISsub$Incidence[1] <- ISsub$Infected_cumul[1]
for (i in 2:nrow(ISsub)) {
  ISsub$Incidence[i] <- ISsub$Infected_cumul[i] - ISsub$Infected_cumul[i-1]
}

onset <- rep(ISsub$Day, ISsub$Incidence)
inci <- incidence(onset)
plot(inci, border = "white")

R_coeff <- get_R(inci,
                 si_mean = Parms_Conta$SerialInterval[["delta"]][["mu"]],
                 si_sd = 0.5)
R_coeff$si
R_val <- sample_R(R_coeff, 1000)
summary(R_val)
hist(R_val)


plot(R_coeff)

Parms_Conta$SerialInterval[["original"]][["mu"]]

