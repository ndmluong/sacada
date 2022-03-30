#test code Du
##### PACKAGES #####
#library(tidyverse)
library(ggplot2)
library(tibble)
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
# Create workers : MyWorkers
MyWorkers <- f_initWorkers(prm = Parms_Workers, prm_time = Parms_Time, seed = seed)
MyWorkers <- f_setupSchedule(W = MyWorkers, prm = Parms_Workers, seed = seed)

# # SCHEDULE VISUALISATION
# # Plot schedule for all workers during a given period
# f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 20)
# # Plot schedule for some considered workers
# f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 28, SHOW_ID = 1:60)
# # Plot schedule with information at one given day
# f_plotSchedule(MyWorkers, Dmin = 1, Dmax = 28, SHOW_ID = 1:60, Dfocus = 9)


### ASSIGN LOCATION BASED ON SCHEDULE ###
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

MyWorkers$location[is.na(MyWorkers$location)] <- "Home"

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
## Initialise the surfaces for the day 1 ###
MySurfaces <- f_initSurfaces(P = MyPlant$P,
                             prm_plant = Parms_Plant,
                             prm_time = Parms_Time,
                             day = 1)

##### FOOD PORTIONS #####
## Initialise the food portion for the day 1 ###
# f_allocateCarcassTimeIndex(Parms_Food, Parms_Workers, Parms_Time, W=MyWorkers, day=1)
FoodInitStruct <- f_initFood(prm_food = Parms_Food,
                             prm_workers = Parms_Workers,
                             prm_time = Parms_Time,
                             W = MyWorkers,
                             day = 1) ## Day 1
MyFood <- FoodInitStruct$FP
MyCarcasses <- FoodInitStruct$c_alloc
duration_ti <- FoodInitStruct$duration_ti
rm(FoodInitStruct)


##### CheckPoint1 #####
## save.image("CheckPoint1.RData")







### CIRCULATION OF ONE GIVEN CARCASS INSIDE THE PLANT
# f_circulateCarcass(Plant = MyPlant, FPcarc = subset(MyFood, carcass_ID == "0001"), carcass_ID = "0001", t_ind = 60, prm_food = Parms_Food, prm_time = Parms_Time) -> aa
## test OK

# MF15_42 <- list()
# 
# for (day in 1:42) {
#   FoodInitStruct <- f_initFood(prm_food = Parms_Food,
#                                prm_workers = Parms_Workers,
#                                prm_time = Parms_Time,
#                                W = MyWorkers,
#                                day = day) ## first day
#   MyFood <- FoodInitStruct$FP
#   MyCarcasses <- FoodInitStruct$c_alloc
#   duration_ti <- FoodInitStruct$duration_ti
#   lapply(unique(MyFood$carcass_ID)[1:15], ## test sur 15 carcasses
#          FUN = function(x) {
#            writeLines(paste("Circulating the carcass ", x, sep=""))
#            FPcarc0 <- filter(MyFood, carcass_ID == x)
#            ti <- min(FPcarc0$t_ind)
#            MF0 <- f_circulateCarcass(Plant = MyPlant, FPcarc = FPcarc0, carcass_ID = x, 
#                                      t_ind = ti,
#                                      prm_food = Parms_Food, prm_time = Parms_Time)
#            return(MF0)
#          }) %>%
#     rbindlist(.) -> MF15
#   
#   MF15_42 <- list(MF15_42, MF15)
# }


## Test Updated Module Air
W <- MyWorkers

ti <- 65
SubS <- subset(MySurfaces, t_ind == ti)
SubW <- subset(MyWorkers, t_ind == ti)
SubW$W_status[SubW$W_ID %in% c("W003", "W006", "W012", "W017", "W028", "W032", "W056", "W087", "W080")] <- c("infectious", "asymptomatic", "symptomatic", "symptomatic", "symptomatic", "symptomatic", "symptomatic", "symptomatic", "symptomatic")

WhoIs <- f_Who_is(SubW = SubW, prm_plant = Parms_Plant)
Sneeze <- f_Sneeze(SubW = SubW, SubS = SubS,Rooms = WhoIs$Rooms, prm_air = Parms_Air,prm_time = Parms_Time)


MASTER <- f_Module_Master(MyAir = MyAir,W = MyWorkers, S = MySurfaces, prm_plant = Parms_Plant,prm_air = Parms_Air,prm_time = Parms_Time,prm_workers = Parms_Workers,ind_min = 0, ind_max = 287, seed=408
                          )










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



# 
# InfectionSummary <- data.frame(Day = seq(1,max(MyWorkers$Day-1)),
#                                Infected_cumul = rep(NA,max(MyWorkers$Day-1)),
#                                Infectious = rep(NA, max(MyWorkers$Day-1)),
#                                Symptomatic = rep(NA, max(MyWorkers$Day-1)),
#                                Asymptomatic = rep(NA, max(MyWorkers$Day-1)),
#                                Recovered_cumul = rep(NA, max(MyWorkers$Day-1)),
#                                seed = rep(seed, max(MyWorkers$Day-1)))
# 
# for (day in 1:nrow(InfectionSummary)) {
#   dW <- subset(W, Hour == 0 & Min == 0 & Day == day)
#   InfectionSummary$Infected_cumul[day] <- length(dW$W_statusCounter[dW$W_statusCounter > 0])
#   InfectionSummary$Infectious[day] <- length(dW$W_statusCounter[dW$W_status %in% c("infectious", "symptomatic", "asymptomatic")])
#   InfectionSummary$Symptomatic[day] <- length(dW$W_status[dW$W_status == "symptomatic"])
#   InfectionSummary$Asymptomatic[day] <- length(dW$W_status[dW$W_status == "asymptomatic"])
#   InfectionSummary$Recovered_cumul[day] <- length(dW$W_statusCounter[dW$W_status == "recovered"])
# }
# 
# OUTPUT <- list(seed = seed,
#                MyPlant = MyPlant,
#                MyWorkers = MyWorkers,
#                MyAir = MyAir,
#                InfectionSummary = InfectionSummary)
# 
# 
# 
# 



############################### SCENARIO S02 ################################
all_seed <- 201:210

Parms_Workers$pMaskAcceptability[Parms_Workers$MaskType] <- 0 ## remove all masks
Parms_Plant$Air_renewal <- 1000 ## remove air renewal of the cutting room

ST_S02Begin <- Sys.time() ## simulation time checkpoint
lapply(all_seed, FUN = function(x) {
  OUTPUT_seedx <- tryCatch(f_run_2M(prm_plant = Parms_Plant,
                                    prm_time = Parms_Time,
                                    prm_workers = Parms_Workers,
                                    prm_air = Parms_Air,
                                    prm_conta = Parms_Conta,
                                    seed = x),
                           error = function(e) NULL)
  return(OUTPUT_seedx)
}) -> OUTPUT
ST_S02End <- Sys.time() ## simulation time checkpoint

### OUTPUT TREATMENT : Infection summary
lapply(OUTPUT, function(x) {return(x$InfectionSummary)}) %>%
  data.table::rbindlist() -> IS
IS$seed <- as.factor(IS$seed)

### OUTPUT TREATMENT : Infection source (for all workers and all simulations)
lapply(OUTPUT, function(x) {return(x$InfectionLog)}) %>%
  data.table::rbindlist() -> IL
IL$seed <- as.factor(IL$seed)
IL$InfectionSource <- as.factor(IL$InfectionSource)

#### SAVE SIMULATION RESULTS
save.image("simulation_output/OUTPUT_2022_02_16_NDL_S02.RData")




############################### SCENARIO S03 ################################
all_seed <- 301:310

Parms_Workers$pMaskAcceptability[Parms_Workers$MaskType] <- 0.8 
Parms_Plant$Air_renewal <- 0 ## remove air renewal of the cutting room

ST_S03Begin <- Sys.time() ## simulation time checkpoint
lapply(all_seed, FUN = function(x) {
  OUTPUT_seedx <- tryCatch(f_run_2M(prm_plant = Parms_Plant,
                                    prm_time = Parms_Time,
                                    prm_workers = Parms_Workers,
                                    prm_air = Parms_Air,
                                    prm_conta = Parms_Conta,
                                    seed = x),
                           error = function(e) NULL)
  return(OUTPUT_seedx)
}) -> OUTPUT
ST_S03End <- Sys.time() ## simulation time checkpoint

### OUTPUT TREATMENT : Infection summary
lapply(OUTPUT, function(x) {return(x$InfectionSummary)}) %>%
  data.table::rbindlist() -> IS
IS$seed <- as.factor(IS$seed)

### OUTPUT TREATMENT : Infection source (for all workers and all simulations)
lapply(OUTPUT, function(x) {return(x$InfectionLog)}) %>%
  data.table::rbindlist() -> IL
IL$seed <- as.factor(IL$seed)
IL$InfectionSource <- as.factor(IL$InfectionSource)

#### SAVE SIMULATION RESULTS
save.image("simulation_output/OUTPUT_2022_02_16_NDL_S03.RData")
rm(ST_S03Begin, ST_S03End, OUTPUT, IL, IS)
gc()


############################### SCENARIO S04 ################################
all_seed <- 401:410

Parms_Workers$pMaskAcceptability[Parms_Workers$MaskType] <- 0
Parms_Plant$Air_renewal <- 0 ## remove air renewal of the cutting room

ST_S04Begin <- Sys.time() ## simulation time checkpoint
lapply(all_seed, FUN = function(x) {
  OUTPUT_seedx <- tryCatch(f_run_2M(prm_plant = Parms_Plant,
                                    prm_time = Parms_Time,
                                    prm_workers = Parms_Workers,
                                    prm_air = Parms_Air,
                                    prm_conta = Parms_Conta,
                                    seed = x),
                           error = function(e) NULL)
  return(OUTPUT_seedx)
}) -> OUTPUT
ST_S04End <- Sys.time() ## simulation time checkpoint

### OUTPUT TREATMENT : Infection summary
lapply(OUTPUT, function(x) {return(x$InfectionSummary)}) %>%
  data.table::rbindlist() -> IS
IS$seed <- as.factor(IS$seed)

### OUTPUT TREATMENT : Infection source (for all workers and all simulations)
lapply(OUTPUT, function(x) {return(x$InfectionLog)}) %>%
  data.table::rbindlist() -> IL
IL$seed <- as.factor(IL$seed)
IL$InfectionSource <- as.factor(IL$InfectionSource)

#### SAVE SIMULATION RESULTS
save.image("simulation_output/OUTPUT_2022_02_16_NDL_S04.RData")
rm(ST_S04Begin, ST_S04End, OUTPUT, IL, IS)
gc()


############################### PLOTTING OUTPUT ################################
load("simulation_output/OUTPUT_2022_02_16_NDL_S01.RData")
load("simulation_output/OUTPUT_2022_02_16_NDL_S02.RData")
load("simulation_output/OUTPUT_2022_02_16_NDL_S03.RData")
load("simulation_output/OUTPUT_2022_02_16_NDL_S04.RData")
f_plotOutput(IL, IS)
f_plotOutput(IL, IS, detailed_plot = T, wrap.nrow = 2)






















day <- 1
NPortions_carcass <- Parms_Food$Ncarcass_daily[Parms_Food$meat] / Parms_Food$CSU[Parms_Food$meat]
expand.grid(str_pad(day, width = 2, pad = "0"),
            str_pad(1:Parms_Food$Ncarcass_daily[Parms_Food$meat], width = 3, pad = "0"),
            str_pad(1:NPortions_carcass, width = 4, pad = "0")) %>%
  as.data.frame(.) %>%
  `colnames<-`(., c("day", "carcass", "portion")) %>%
  apply(., 1, function(id) {
    paste(id["day"], id["carcass"], id["portion"], sep = "_")}) %>%
  paste(str_sub(Parms_Food$meat, 1, 1) %>% str_to_upper(.), ., sep = "_") %>%
  sort(.) -> ID
