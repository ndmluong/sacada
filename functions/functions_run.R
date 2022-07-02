# ##### f_run_2M #####
# f_run_2M <- function(
#   prm_plant,
#   prm_time,
#   prm_workers,
#   prm_air,
#   prm_conta,
#   seed
# ) {
#   writeLines("================================= BEGIN =============================================")
#   writeLines(paste("Two-module model run - seed ", seed, sep = ""))
#   writeLines("=====================================================================================")
#   
#   ##### PLANT #####
#   ## Create the plant
#   MyPlant <- f_createPlant(prm = prm_plant)
#   
#   ##### WORKERS #####
#   ### SCHEDULE ###
#   # Create workers
#   MyWorkers <- f_initWorkers(prm = prm_workers, prm_time = prm_time, seed = seed)
#   MyWorkers <- f_setupSchedule(W = MyWorkers, prm = prm_workers, seed = seed)
#   
#   ### ASSIGN LOCATION BASED ON SCHEDULE ###
#   writeLines("***** Processing daily work for all teams *****")
#   LastDay <- max(MyWorkers$Day)
#   WorkingDays <- subset(MyWorkers,
#                         !Weekday %in% c("Saturday", "Sunday") & Day < LastDay)$Day %>% unique() %>% sort()
#   OtherDays <- subset(MyWorkers, !Day %in% WorkingDays)$Day %>% unique() %>% sort()
#   
#   ## Assign location based on schedule
#   lapply(WorkingDays, FUN = function(x) {
#     d1 <- subset(MyWorkers, Day == x)
#     d1 <- f_dailyWork_AllTeams(Plant = MyPlant, W = d1,D = x, dt = prm_time$Step, seed = seed+x)
#     return(d1)
#   }) %>%
#     rbindlist() %>%
#     rbind(subset(MyWorkers, Day %in% OtherDays)) %>%
#     dplyr::arrange(t_ind, W_ID) -> MyWorkers
#   gc() # free unused R memory
#   
#   MyWorkers$location[is.na(MyWorkers$location)] <- "Home"
#   
#   ### WEARING MASK ###
#   writeLines("***** Processing 'Mask wearing' status *****")
#   by(data = MyWorkers,
#      INDICES = MyWorkers$Day,
#      FUN = f_dailyMaskWearing, ## check the script
#      probMask = prm_workers$pMaskAcceptability[prm_workers$MaskType]) %>%
#     data.table::rbindlist() %>%
#     dplyr::arrange(t_ind, W_ID) -> MyWorkers
#   
#   
#   ## Random state (in day) of the first initialized contaminated worker
#   MyWorkers <- f_initStatusCounterDay1(W = MyWorkers, prm_workers = prm_workers, prm_time = prm_time, seed = seed)
#   
#   #### Inter-individuals variability in the viral load (RNA load), in log10 copies/ml
#   indi_viral_load <- f_individual_viral_load(prm_workers = prm_workers,
#                                              prm_conta = prm_conta)
#   names(indi_viral_load) <- unique(MyWorkers$ID) %>% sort()
#   
#   
#   ### AEROSOL ###
#   ## Initializing the agents (classes of droplets)
#   MyAir <- f_initAir(prm = prm_plant, prm_time = prm_time, prm_air = prm_air)
#   AIR_ID <- c(prm_plant$label,
#               unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$label)))))
#   ## Check if the droplets stay in air
#   Method_calc <<- f_Air_Criteria_Calc(prm_plant,prm_air) 
#   
#   ## Assign 0 values for the first time index (required for the first run of f_Module_Master)
#   MyAir[MyAir$t_ind==0, 2:(1+length(prm_air$Droplet_class))] = matrix(0,7,4) * Method_calc 
#   
#   
#   ### INFECTION LOG ###
#   InfectionLog <- data.frame(W_ID = unique(MyWorkers$W_ID),
#                              InfectedDay = rep(NA, prm_workers$NWorkers),
#                              InfectionSource = rep(NA, prm_workers$NWorkers),
#                              seed = rep(seed, prm_workers$NWorkers))
#   
#   Infected_init <- subset(MyWorkers, Day == 1 & W_status == "infected")$W_ID %>% unique()
#   InfectionLog$InfectedDay[InfectionLog$W_ID %in% Infected_init] <- 1
#   InfectionLog$InfectionSource[InfectionLog$W_ID %in% Infected_init] <- "initialised"
#   
#   
#   Expocum <- list()
#   
#   #### SIMULATING DAILY CONTAMINATIONS ####
#   writeLines("***** Simulating daily contamination *****")
#   for (day in 2:(max(MyWorkers$Day)-1)) {
#     CONTA <- f_dailyContamination(W = MyWorkers,
#                                   MyAir = MyAir,
#                                   day = day,
#                                   indi_viral_load = indi_viral_load,
#                                   prm_plant = prm_plant,
#                                   prm_workers = prm_workers,
#                                   prm_time = prm_time,
#                                   prm_air = prm_air,
#                                   prm_conta = prm_conta,
#                                   inf_log = InfectionLog,
#                                   seed = seed)
#     
#     MyWorkers <- CONTA$W
#     ## set possible absence for symptomatic worker(s)
#     MyWorkers <- f_setAbsence(W = MyWorkers, day = day, prm_workers = prm_workers)
#     
#     MyAir <- CONTA$MyAir
#     InfectionLog <- CONTA$inf_log
#     Expocum[[day]] <- CONTA$Expocum
#   }
#   
#   #### SUMMARY FOR ALL INFECTIONS ####
#   writeLines("***** Infection: Summary processing *****")
#   InfectionSummary <- data.frame(Day = seq(1,max(MyWorkers$Day-1)),
#                                  Infected = rep(NA, max(MyWorkers$Day-1)),
#                                  Infectious = rep(NA, max(MyWorkers$Day-1)),
#                                  Symptomatic = rep(NA, max(MyWorkers$Day-1)),
#                                  Asymptomatic = rep(NA, max(MyWorkers$Day-1)),
#                                  InfectiousPeriod = rep(NA, max(MyWorkers$Day-1)),
#                                  NonInfectious = rep(NA, max(MyWorkers$Day-1)),
#                                  Positive = rep(NA, max(MyWorkers$Day-1)),
#                                  Infected_cumul = rep(NA,max(MyWorkers$Day-1)),
#                                  Recovered_cumul = rep(NA, max(MyWorkers$Day-1)),
#                                  seed = rep(seed, max(MyWorkers$Day-1)))
#   
#   for (day in 1:nrow(InfectionSummary)) {
#     dW <- subset(MyWorkers, Hour == 0 & Min == 0 & Day == day)
#     InfectionSummary$Infected[day] <- length(dW$W_status[dW$W_status == "infected"])
#     InfectionSummary$Infectious[day] <- length(dW$W_status[dW$W_status == "infectious"])
#     InfectionSummary$Symptomatic[day] <- length(dW$W_status[dW$W_status == "symptomatic"])
#     InfectionSummary$Asymptomatic[day] <- length(dW$W_status[dW$W_status == "asymptomatic"])
#     InfectionSummary$InfectiousPeriod[day] <- length(dW$W_status[dW$W_status %in% c("infectious", "symptomatic", "asymptomatic")])
#     InfectionSummary$NonInfectious[day] <- length(dW$W_status[dW$W_status == "non-infectious"])
#     InfectionSummary$Infected_cumul[day] <- length(dW$W_statusCounter[dW$W_statusCounter > 0])
#     InfectionSummary$Recovered_cumul[day] <- length(dW$W_status[dW$W_status == "recovered"])
#   }
#   
#   InfectionSummary$Positive <- InfectionSummary$Infected_cumul - InfectionSummary$Recovered_cumul 
#   
#   OUTPUT <- list(seed = seed,
#                  MyPlant = MyPlant,
#                  MyWorkers = MyWorkers,
#                  MyAir = MyAir,
#                  InfectionLog = InfectionLog,
#                  InfectionSummary = InfectionSummary,
#                  Expocum = Expocum)
#   writeLines("=====================================================================================")
#   writeLines(paste("Two-module model run - seed ", seed, " - successfully done !", sep = ""))
#   writeLines("====================================== END ==========================================")
#   return(OUTPUT)
#   
# }
# 
# 
# 
# 
# 
# 
# ##### f_run_3M #####
# f_run_3M <- function(
#     prm_plant,
#     prm_time,
#     prm_workers,
#     prm_air,
#     prm_conta,
#     seed
# ) {
#   writeLines("================================= BEGIN =============================================")
#   writeLines(paste("Three-module model run - seed ", seed, sep = ""))
#   writeLines("=====================================================================================")
#   
#   ##### PLANT #####
#   ## Create the plant
#   MyPlant <- f_createPlant(prm = prm_plant)
#   
#   ##### WORKERS #####
#   ### SCHEDULE ###
#   # Create workers
#   MyWorkers <- f_initWorkers(prm = prm_workers, prm_time = prm_time, seed = seed)
#   MyWorkers <- f_setupSchedule(W = MyWorkers, prm = prm_workers, seed = seed)
#   
#   ### ASSIGN LOCATION BASED ON SCHEDULE ###
#   writeLines("***** Processing daily work for all teams *****")
#   LastDay <- max(MyWorkers$Day)
#   WorkingDays <- subset(MyWorkers,
#                         !Weekday %in% c("Saturday", "Sunday") & Day < LastDay)$Day %>% unique() %>% sort()
#   OtherDays <- subset(MyWorkers, !Day %in% WorkingDays)$Day %>% unique() %>% sort()
#   
#   ## Assign location based on schedule
#   lapply(WorkingDays, FUN = function(x) {
#     d1 <- subset(MyWorkers, Day == x)
#     d1 <- f_dailyWork_AllTeams(Plant = MyPlant, W = d1,D = x, dt = prm_time$Step, seed = seed+x)
#     return(d1)
#   }) %>%
#     rbindlist() %>%
#     rbind(subset(MyWorkers, Day %in% OtherDays)) %>%
#     dplyr::arrange(t_ind, W_ID) -> MyWorkers
#   gc() # free unused R memory
#   
#   MyWorkers$location[is.na(MyWorkers$location)] <- "Home"
#   
#   ### WEARING MASK ###
#   writeLines("***** Processing 'Mask wearing' status *****")
#   by(data = MyWorkers,
#      INDICES = MyWorkers$Day,
#      FUN = f_dailyMaskWearing, ## check the script
#      probMask = prm_workers$pMaskAcceptability[prm_workers$MaskType]) %>%
#     data.table::rbindlist() %>%
#     dplyr::arrange(t_ind, W_ID) -> MyWorkers
#   
#   ## Random state (in day) of the first initialized contaminated worker
#   MyWorkers <- f_initStatusCounterDay1(W = MyWorkers, prm_workers = Parms_Workers, prm_time = Parms_Time, seed = seed)
#   
#   #### Inter-individuals variability in the viral load (RNA load), in log10 copies/ml
#   indi_viral_load <- f_individual_viral_load(prm_workers = Parms_Workers,
#                                              prm_air = Parms_Air,
#                                              prm_conta = Parms_Conta)
#   
#   ##### AEROSOL #####
#   MyAir <- f_initAir(prm = Parms_Plant, prm_time = Parms_Time, prm_air = Parms_Air)
#   
#   ##### INFECTION LOG #####
#   ## Infection log indicating the contamination day and sources of every workers over time
#   InfectionLog <- data.frame(W_ID = unique(MyWorkers$W_ID),
#                              InfectedDay = rep(NA, Parms_Workers$NWorkers),
#                              InfectionSource = rep(NA, Parms_Workers$NWorkers))
#   
#   Infected_init <- subset(MyWorkers, Day == 1 & W_status == "infected")$W_ID %>% unique()
#   InfectionLog$InfectedDay[InfectionLog$W_ID %in% Infected_init] <- 1
#   InfectionLog$InfectionSource[InfectionLog$W_ID %in% Infected_init] <- "initialised"
#   
#   ##### SURFACES #####
#   ## Initialise the surfaces for the day 1 ###
#   MySurfaces <- f_initSurfaces(P = MyPlant$P,
#                                prm_plant = Parms_Plant,
#                                prm_time = Parms_Time,
#                                day = 1)
#   
#   Expocum <- list()
#   
#   #### SIMULATING DAILY CONTAMINATIONS ####
#   writeLines("***** Simulating daily contamination *****")
#   for (day in 2:(max(MyWorkers$Day)-1)) {
#     MySurfaces <- rbind(MySurfaces,
#                         f_initSurfaces(P = MyPlant$P,    
#                                        prm_plant = Parms_Plant,
#                                        prm_time = Parms_Time,
#                                        day = day))
#     CONTA <- f_dailyContamination(MyAir = MyAir,
#                                   W = MyWorkers,
#                                   S = MySurfaces,
#                                   day = day,
#                                   indi_viral_load = indi_viral_load,
#                                   prm_plant = Parms_Plant,
#                                   prm_workers = Parms_Workers,
#                                   prm_time = Parms_Time,
#                                   prm_air = Parms_Air,
#                                   prm_conta = Parms_Conta,
#                                   inf_log = InfectionLog,
#                                   seed = seed)
#     MyWorkers <- CONTA$W
#     ## set possible absence for symptomatic worker(s)
#     MyWorkers <- f_setAbsence(W = MyWorkers, day = day, prm_workers = prm_workers)
#     
#     MyAir <- CONTA$MyAir
#     MySurfaces <- CONTA$S
#     InfectionLog <- CONTA$inf_log
#     Expocum[[day]] <- CONTA$Expocum
#   }
#   
#   #### SUMMARY FOR ALL INFECTIONS ####
#   writeLines("***** Infection: Summary processing *****")
#   InfectionSummary <- data.frame(Day = seq(1,max(MyWorkers$Day-1)),
#                                  Infected = rep(NA, max(MyWorkers$Day-1)),
#                                  Infectious = rep(NA, max(MyWorkers$Day-1)),
#                                  Symptomatic = rep(NA, max(MyWorkers$Day-1)),
#                                  Asymptomatic = rep(NA, max(MyWorkers$Day-1)),
#                                  InfectiousPeriod = rep(NA, max(MyWorkers$Day-1)),
#                                  NonInfectious = rep(NA, max(MyWorkers$Day-1)),
#                                  Positive = rep(NA, max(MyWorkers$Day-1)),
#                                  Infected_cumul = rep(NA,max(MyWorkers$Day-1)),
#                                  Recovered_cumul = rep(NA, max(MyWorkers$Day-1)),
#                                  seed = rep(seed, max(MyWorkers$Day-1)))
#   
#   for (day in 1:nrow(InfectionSummary)) {
#     dW <- subset(MyWorkers, Hour == 0 & Min == 0 & Day == day)
#     InfectionSummary$Infected[day] <- length(dW$W_status[dW$W_status == "infected"])
#     InfectionSummary$Infectious[day] <- length(dW$W_status[dW$W_status == "infectious"])
#     InfectionSummary$Symptomatic[day] <- length(dW$W_status[dW$W_status == "symptomatic"])
#     InfectionSummary$Asymptomatic[day] <- length(dW$W_status[dW$W_status == "asymptomatic"])
#     InfectionSummary$InfectiousPeriod[day] <- length(dW$W_status[dW$W_status %in% c("infectious", "symptomatic", "asymptomatic")])
#     InfectionSummary$NonInfectious[day] <- length(dW$W_status[dW$W_status == "non-infectious"])
#     InfectionSummary$Infected_cumul[day] <- length(dW$W_statusCounter[dW$W_statusCounter > 0])
#     InfectionSummary$Recovered_cumul[day] <- length(dW$W_status[dW$W_status == "recovered"])
#   }
#   
#   InfectionSummary$Positive <- InfectionSummary$Infected_cumul - InfectionSummary$Recovered_cumul 
#   
#   OUTPUT <- list(seed = seed,
#                  MyPlant = MyPlant,
#                  MyWorkers = MyWorkers,
#                  MyAir = MyAir,
#                  MySurfaces = MySurfaces,
#                  InfectionLog = InfectionLog,
#                  InfectionSummary = InfectionSummary,
#                  Expocum = Expocum)
#   writeLines("=====================================================================================")
#   writeLines(paste("Three-module model run - seed ", seed, " - successfully done !", sep = ""))
#   writeLines("====================================== END ==========================================")
#   return(OUTPUT)
#   
# }







##### f_run_4M #####
f_run_4M <- function(
    prm_plant,
    prm_time,
    prm_workers,
    prm_air,
    prm_conta,
    prm_surfaces,
    prm_food,
    seed
) {
  writeLines("================================= BEGIN =============================================")
  writeLines(paste("Four-module model run - seed ", seed, sep = ""))
  writeLines("=====================================================================================")
  
  ##### PLANT #####
  ## Create the plant
  MyPlant <- f_createPlant(prm = prm_plant)
  
  ##### WORKERS #####
  ### SCHEDULE ###
  # Create workers : MyWorkers
  MyWorkers <- f_initWorkers(prm = prm_workers, prm_time = prm_time, seed = seed)
  MyWorkers <- f_setupSchedule(W = MyWorkers, prm = prm_workers, seed = seed)
  
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
                     probMask = prm_workers$pMaskAcceptability[prm_workers$MaskType]) %>%
    rbind(., subset(MyWorkers, Day != 1)) %>%
    arrange(.,t_ind, W_ID) -> MyWorkers
  
  ## Random state (in day) of the first initialized contaminated worker
  MyWorkers <- f_initStatusCounterDay1(W = MyWorkers, prm_workers = prm_workers, prm_time = prm_time, seed = seed)
  
  #### Inter-individuals variability in the viral load (RNA load), in log10 copies/ml
  indi_viral_load <- f_individual_viral_load(prm_workers = prm_workers,
                                             prm_air = prm_air,
                                             prm_conta = prm_conta)
  
  ##### AEROSOL #####
  MyAir <- f_initAir(prm = prm_plant, prm_time = prm_time, prm_air = prm_air)
  
  ##### INFECTION LOG #####
  ## Infection log indicating the contamination day and sources of every workers over time
  InfectionLog <- data.frame(W_ID = unique(MyWorkers$W_ID),
                             InfectedDay = rep(NA, prm_workers$NWorkers),
                             InfectionSource = rep(NA, prm_workers$NWorkers))
  
  Infected_init <- subset(MyWorkers, Day == 1 & W_status == "infected")$W_ID %>% unique()
  InfectionLog$InfectedDay[InfectionLog$W_ID %in% Infected_init] <- 1
  InfectionLog$InfectionSource[InfectionLog$W_ID %in% Infected_init] <- "initialised"
  
  ##### SURFACES #####
  ## Initialize the surfaces for the day 1 ###
  MySurfaces <- f_initSurfaces(P = MyPlant$P,
                               prm_plant = prm_plant,
                               prm_time = prm_time,
                               day = 1)
  
  ##### FOOD PORTIONS #####
  ## Initialize the food portion for the day 1 ###
  MyFood <- f_ProcessFood(prm_food = prm_food,
                          prm_workers = prm_workers,
                          prm_time = prm_time,
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
                                  prm_plant = prm_plant,
                                  prm_workers = prm_workers,
                                  prm_time = prm_time,
                                  prm_air = prm_air,
                                  prm_conta = prm_conta,
                                  prm_surfaces = prm_surfaces,
                                  inf_log = InfectionLog,
                                  seed = seed)
    
    ## Processing CONTA (1) - FOOD
    ## Food portions - Contamination summary
    FP_End <- subset(CONTA$FP, location == "Cooling area")
    FP_summary %>% 
      add_row(Day = day-1,
              nb_carcass = length(unique(FP_End$carcass_ID)),
              nb_FP = length(unique(FP_End$FP_ID)),
              pos_FP = length(FP_End$RNA_load[FP_End$RNA_load > prm_surfaces$pos_threshold])) -> FP_summary
    
    
    ## Surfaces - Contamination summary      
    S_End <- subset(CONTA$S, t_ind == max(CONTA$S$t_ind))
    S_summary %>%
      add_row(Day = day-1,
              pos_S = length(S_End$RNA_load[S_End$RNA_load >= prm_surfaces$pos_threshold]),
              S_ID = subset(S_End, RNA_load >= prm_surfaces$pos_threshold)$S_ID %>% paste(., collapse = ", ")) -> S_summary
    
    MyWorkers <- CONTA$W
    MyAir <- CONTA$MyAir
    InfectionLog <- CONTA$inf_log
    Expocum[[day]] <- CONTA$Expocum
    
    ## Processing CONTA (1) - WORKERS
    ## set possible absence for symptomatic worker(s)
    writeLines(">>> Setting possible absence for symptomatic workers")
    MyWorkers <- f_setAbsence(W = MyWorkers, day = day, prm_workers = prm_workers)
    
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
                       probMask = prm_workers$pMaskAcceptability[prm_workers$MaskType]) %>%
      rbind(., subset(MyWorkers, Day != day)) %>%
      arrange(.,t_ind, W_ID) -> MyWorkers
    
    ## Initialize the surfaces for the day ###
    MySurfaces <- f_initSurfaces(P = MyPlant$P,
                                 prm_plant = prm_plant,
                                 prm_time = prm_time,
                                 day = day)
    
    ##### FOOD PORTIONS #####
    ## Initialize the food portion for the day  ###
    if (day %in% WorkingDays) {
      MyFood <- f_ProcessFood(prm_food = prm_food,
                              prm_workers = prm_workers,
                              prm_time = prm_time,
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
                 S_summary = S_summary,
                 FP_summary = FP_summary,
                 InfectionLog = InfectionLog,
                 InfectionSummary = InfectionSummary,
                 Expocum = Expocum)
  writeLines("=====================================================================================")
  writeLines(paste("Four-module model run - seed ", seed, " - successfully done !", sep = ""))
  writeLines("====================================== END ==========================================")
  return(OUTPUT)
  
}


#### f_IL ####
f_IL <- function(OUTPUT_allseeds) {
  lapply(OUTPUT_allseeds, function(x) {
    return(data.frame(x$InfectionLog,
                      seed = rep(x$seed, nrow(x$InfectionLog))))
  }) %>%
    data.table::rbindlist() -> IL
  IL$seed <- as.factor(IL$seed)
  IL$InfectionSource <- as.factor(IL$InfectionSource)
  return(IL)
}

f_IS <- function(OUTPUT_allseeds) {
  lapply(OUTPUT_allseeds, function(x) {return(x$InfectionSummary)}) %>%
    data.table::rbindlist() -> IS
  IS$seed <- as.factor(IS$seed)
  return(IS)
}

f_summaryRt <- function(
  IS,
  prm_conta
) {
  lapply(as.numeric(levels(IS$seed)), function(x) {
    f_estimateRt(ISsub = subset(IS, seed == x),
                 prm_conta = prm_conta,
                 seed = x)
  }) %>%
    data.table::rbindlist() -> Rt_summary
  return(Rt_summary)
}
