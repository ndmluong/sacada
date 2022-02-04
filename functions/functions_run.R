f_run_2M <- function(
  prm_plant,
  prm_time,
  prm_workers,
  prm_air,
  prm_conta,
  seed
) {
  writeLines("================================= BEGIN =============================================")
  writeLines(paste("Two-module model run - seed ", seed, sep = ""))
  writeLines("=====================================================================================")
  
  ##### PLANT #####
  ## Create the plant
  MyPlant <- f_createPlant(prm = prm_plant)
  
  ##### WORKERS #####
  ### SCHEDULE ###
  # Create workers
  MyWorkers <- f_initWorkers(prm = prm_workers, prm_time = prm_time, seed = seed)
  MyWorkers <- f_setupSchedule(W = MyWorkers, prm = prm_workers, seed = seed)
  
  ### ASSIGN LOCATION BASED ON SCHEDULE ###
  writeLines("================ Processing daily work for all teams ================================")
  LastDay <- max(MyWorkers$Day)
  WorkingDays <- subset(MyWorkers,
                        !Weekday %in% c("Saturday", "Sunday") & Day < LastDay)$Day %>% unique() %>% sort()
  OtherDays <- subset(MyWorkers, !Day %in% WorkingDays)$Day %>% unique() %>% sort()
  
  ## Assign location based on schedule
  lapply(WorkingDays, FUN = function(x) {
    d1 <- subset(MyWorkers, Day == x)
    d1 <- f_dailyWork_AllTeams(Plant = MyPlant, W = d1,D = x, dt = prm_time$Step, seed = seed+x)
    return(d1)
  }) %>%
    rbindlist() %>%
    rbind(subset(MyWorkers, Day %in% OtherDays)) %>%
    dplyr::arrange(t_ind, W_ID) -> MyWorkers
  gc() # free unused R memory
  
  MyWorkers$W_location[is.na(MyWorkers$W_location)] <- "Home"
  
  ### WEARING MASK ###
  writeLines("================ Processing 'Mask wearing' status ===================================")
  by(data = MyWorkers,
     INDICES = MyWorkers$Day,
     FUN = f_dailyMaskWearing, ## check the script
     probMask = prm_workers$pMaskAcceptability[prm_workers$MaskType]) %>%
    data.table::rbindlist() %>%
    dplyr::arrange(t_ind, W_ID) -> MyWorkers
  
  
  ## Random state (in day) of the first initialized contaminated worker
  MyWorkers <- f_initStatusCounterDay1(W = MyWorkers, prm_workers = prm_workers, prm_time = prm_time, seed = seed)
  
  #### Inter-individuals variability in the viral load (RNA load), in log10 copies/ml
  indi_viral_load <- f_individual_viral_load(prm_workers = Parms_Workers,
                                             prm_conta = Parms_Conta)
  
  ### AEROSOL ###
  ## Initializing the agents (classes of droplets)
  MyAir <- f_initAir(prm = prm_plant, prm_time = prm_time, prm_air = prm_air)
  AIR_ID <- c(prm_plant$label,
              unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$label)))))
  ## Check if the droplets stay in air
  Method_calc <<- f_Air_Criteria_Calc(prm_plant,prm_air) 
  
  ## Assign 0 values for the first time index (required for the first run of f_Module_Master)
  MyAir[MyAir$t_ind==0, 2:(1+length(prm_air$Droplet_class))] = matrix(0,7,4) * Method_calc 
  
  
  ### INFECTION LOG ###
  InfectionLog <- data.frame(W_ID = unique(MyWorkers$W_ID),
                             InfectedDay = rep(NA, prm_workers$NWorkers),
                             InfectionSource = rep(NA, prm_workers$NWorkers),
                             seed = rep(seed, prm_workers))
  
  Infected_init <- subset(MyWorkers, Day == 1 & W_status == "infected")$W_ID %>% unique()
  InfectionLog$InfectedDay[InfectionLog$W_ID %in% Infected_init] <- 1
  InfectionLog$InfectionSource[InfectionLog$W_ID %in% Infected_init] <- "initialised"
  
  
  #### SIMULATING DAILY CONTAMINATIONS ####
  writeLines("================= Simulating daily contamination ====================================")
  for (day in 2:(max(MyWorkers$Day)-1)) {
    CONTA <- f_dailyContamination(W = MyWorkers,
                                  MyAir = MyAir,
                                  day = day,
                                  prm_plant = prm_plant,
                                  prm_workers = prm_workers,
                                  prm_time = prm_time,
                                  prm_air = prm_air,
                                  prm_conta = prm_conta,
                                  inf_log = InfectionLog,
                                  seed = seed)
    MyWorkers <- CONTA$W
    MyAir <- CONTA$MyAir
    InfectionLog <- CONTA$inf_log
  }
  
  #### SUMMARY FOR ALL INFECTIONS ####
  writeLines("=================== Infection: Summary processing ===================================")
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
                 InfectionLog = InfectionLog,
                 InfectionSummary = InfectionSummary)
  writeLines("=====================================================================================")
  writeLines(paste("Two-module model run - seed ", seed, " - successfully done !", sep = ""))
  writeLines("====================================== END ==========================================")
  return(OUTPUT)
  
}
