# PACKAGES #####
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
library(EnvStats)




# FUNCTIONS #####
source("functions/functions.R")



# PARAMETERS ####
## Values by default #####
source("parameters/parameters.R")

## Changes for analyses (check the XLSX file)
Parms_Workers$prev <- 100/100000
Parms_Time$NDays <- 28






# SIMULATION ####
## seed numbers
all_seed <- 20001




##### PLANT #####
## Create the plant
MyPlant <- f_createPlant(prm = Parms_Plant)
## Plot
g_emptyPlant <- f_plotPlant(Plant = MyPlant,
                            prm = Parms_Plant)
# g_emptyPlant
# ggplotly(g_emptyPlant)

seed = 20001

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

for (day in 2:(max(MyWorkers$Day)-1)) {
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

# save.image("test_2022_05_23_checkpoint1.RData") ## day 2 to 18



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
               S_summary = S_summary,
               FP_summary = FP_summary,
               InfectionLog = InfectionLog,
               InfectionSummary = InfectionSummary,
               Expocum = Expocum)



### OUTPUT TREATMENT : Infection summary
IS <- InfectionSummary
IS$seed <- as.factor(IS$seed)



### OUTPUT TREATMENT : Infection source (for all workers and all simulations)
IL <- data.frame(InfectionLog, seed = rep(seed, nrow(InfectionLog)))
IL$InfectionSource[is.na(IL$InfectionSource)] <- "not_infected"
IL$seed <- as.factor(IL$seed)
IL$InfectionSource <- as.factor(IL$InfectionSource)

tapply(IL$InfectionSource, IL$seed, summary) %>%
  sapply(., FUN = function(x) {
    if (length(x) > 4) {
      return(as.vector(x))
    } else {
      allsourcenames <- c("aerosol", "community", "epidemy", "initialised", "not_infected")
      missingsources <- setdiff(allsourcenames, names(x))
      updatesources <- c(as.vector(x), rep(0, length(missingsources)))
      names(updatesources) <- c(names(x), missingsources)
      return(updatesources)
    } 
  }) %>%
  t() %>%
  as.data.frame() %>%
  select(order(colnames(.)))-> ILF


f_plotOutput(IL = IL, IS = IS, seed_select = 1, detailed_plot = T)








##### Estimating R #####
# source("functions/functions_contamination.R")
Parms_Conta <- append(Parms_Conta, list(SerialInterval = list(original = c("mu" = 3.96, "sigma" = 4.75),
                                                              delta = c("mu" = 3.4, "sigma" = 0.35), ## Zhanwei Du et al. 2022
                                                              omicron = c("mu" = 3.1, "sigma" = 0.15), ## Zhanwei Du et al. 2022
                                                              alpha = c("mu" = NA, "sigma" = NA)) ))

lapply(unique(IS$seed), FUN = function(seedx) {
  writeLines(paste("seed", seedx))
  ISsub <- subset(IS, seed == seedx)
  f_estimateRt(ISsub = ISsub,
               prm_conta = Parms_Conta)
})



ISsub <- subset(IS, seed == 20001)


ISsub <- tibble::add_column(ISsub, Incidence = NA, .after = "Day")
ISsub$Incidence[1] <- ISsub$Infected_cumul[1]
for (i in 2:nrow(ISsub)) {
  ISsub$Incidence[i] <- ISsub$Infected_cumul[i] - ISsub$Infected_cumul[i-1]
}

onset <- rep(ISsub$Day, ISsub$Incidence)
inci <- incidence(onset)
plot(inci, border = "white")

Rt_res <- earlyR::get_R(inci,
                        si_mean = Parms_Conta$SerialInterval[[Parms_Conta$VoC]][["mu"]],
                        si_sd = Parms_Conta$SerialInterval[[Parms_Conta$VoC]][["sigma"]])

Rt_val <- sample_R(Rt_res, 1000)
summary(Rt_val)
set.seed(20001)
R_val <- rgamma(n=1000, shape = Rt_res$si$parameters$shape, scale = Rt_res$si$parameters$scale)
hist(R_val)


plot(R_coeff)

Parms_Conta$SerialInterval[["original"]][["mu"]]

