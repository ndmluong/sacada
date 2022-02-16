##### f_updateStatusByCounter #####
f_updateStatusByCounter <- function(
  W,
  day,
  prm_workers
) {
  WD <- subset(W, Day == day)
  Wcomp <- subset(W, Day != day)
  
  WD$W_status <- as.character(WD$W_status)
  
  WD$W_status[which(WD$W_statusCounter == 0 )] <- "susceptible"
  WD$W_status[which(WD$W_statusCounter %in% prm_workers$InfectedDay:(prm_workers$InfectiousDay-1))] <- "infected"
  WD$W_status[which(WD$W_statusCounter %in% prm_workers$InfectiousDay:(prm_workers$SymptomDay-1))] <- "infectious"
  
  ## ID of the workers entering the "symptom period" (presenting the Counter at SymptomDay)
  W_SymptomBegin <- subset(WD, W_statusCounter == prm_workers$SymptomDay)$W_ID %>% unique()
  
  if (length(W_SymptomBegin) > 0) { ## if there is any worker(s) entrying the symptom period at the given day
    SymptomEvent <- sample(c("symptomatic", "asymptomatic"), ## random sampling for these workers for presenting a symptom or not
                           size = length(W_SymptomBegin), replace = T,
                           prob = c(1 - prm_workers$pAsymptom, prm_workers$pAsymptom))
    writeLines(paste("/!\\ Begin of the symptomatic period for the worker(s): ", toString(W_SymptomBegin),
                     "\nwith their respective symptoms development as follows: ", toString(SymptomEvent), sep=""))
    # ## /!\ NON-OPTIMIZED CODE
    # for (wi in 1:length(W_SymptomBegin)) {
    #   WD$W_status[which(WD$W_ID == W_SymptomBegin[wi] &
    #                      WD$W_statusCounter == prm_workers$SymptomDay)] <- SymptomEvent[wi]
    # }
    # ## /!\ NON-OPTIMIZED CODE (END)
    
    ## /!\ OPTIMIZED CODE
    names(SymptomEvent) <- W_SymptomBegin # rename the symptom vector using the corresponding worker ID
    lapply(W_SymptomBegin, FUN = function(x) { ## for each worker entering the symptom period
      W1 <- subset(WD, W_ID == x) ## extract the data subset corresponding to each worker
      ## then assign his corresponding symptomatic/asymptomatic status at the symptom day
      W1$W_status[which(W1$W_statusCounter == prm_workers$SymptomDay)] <- unname(SymptomEvent[x]) 
      return(W1)
    }) %>%
      data.table::rbindlist() %>% ## transform to data frame and combine with the data subset associated with the other workers
      rbind(subset(WD, !W_ID %in% W_SymptomBegin)) -> WD
    ## /!\ OPTIMIZED CODE (END)
  }
  
  ## ID of the workers during the "symptom period", knowing already that they became symptomatic or symptomatic
  W_SymptomPeriod <- subset(WD, W_statusCounter %in% (prm_workers$SymptomDay+1):(prm_workers$NonInfectiousDay-1))$W_ID %>% unique()
  
  if (length(W_SymptomPeriod) > 0) {
    ## /!\ NON-OPTIMIZED CODE
    for (wi in 1:length(W_SymptomPeriod)) {
      SymptomStatus <- subset(W,
                              W_ID == W_SymptomPeriod[wi] &
                                W_statusCounter == prm_workers$SymptomDay)$W_status %>% unique()
      WD$W_status[which(WD$W_ID == W_SymptomPeriod[wi] &
                         WD$W_statusCounter %in% (prm_workers$SymptomDay+1):(prm_workers$NonInfectiousDay-1))] <- SymptomStatus
    }
    ## /!\ NON-OPTIMIZED CODE (END)
    
    # ## /!\ OPTIMIZED CODE
    # lapply(W_SymptomBegin, FUN = function(x) {
    #   W1 <- subset(W, W_ID == x)
    #   SymptomStatus <- subset(W1, W_statusCounter == prm_workers$SymptomDay)$W_status %>% unique()
    #   W1$W_status[which(W1$W_statusCounter %in% (prm_workers$SymptomDay+1):(prm_workers$NonInfectiousDay-1))] <- SymptomStatus
    # }) %>%
    #   data.table::rbindlist() %>%
    #   rbind(subset(WD, !W_ID %in% W_SymptomBegin)) -> WD
    # ## /!\ OPTIMIZED CODE (END)
  }
  
  WD$W_status[which(WD$W_statusCounter %in% prm_workers$NonInfectiousDay:(prm_workers$RecoveredDay-1))] <- "non-infectious"
  WD$W_status[which(WD$W_statusCounter >= prm_workers$RecoveredDay)] <- "recovered"
  
  rbind(WD, Wcomp) %>%
    dplyr::arrange(t_ind, W_ID) -> W
  
  return(W)
}

##### f_initStatusCounterDay1 #####
f_initStatusCounterDay1 <- function(
  W,
  prm_workers,
  prm_time,
  seed = NULL
) {
  if (!is.null(seed)) {set.seed(seed)}
  
  W1 <- subset(W, Day == 1)
  Wcomp <- subset(W, Day != 1)
  
  by(W1, ## for the considered day
     INDICES = W1$W_ID, ## processing by worker
     FUN = function(x) {
       return(f_replicateIndividualtime2time(Agent = x, Invariant = "W_status", time_begin = c(0,0), time_end = c(23,55), dt = prm_time$Step))
     }) %>%
    data.table::rbindlist() %>%
    rbind(Wcomp) %>%
    dplyr::arrange(t_ind, W_ID) -> W
  
  W$W_status <- as.character(W$W_status)
  
  # ## Random state (in day) of the first initialized contaminated worker
  # W$W_statusCounter[which(W$Day == 1 & W$W_status == "initialised as infected")] <- sample(prm_workers$InfectedDay:prm_workers$ContaEndDay, size = 1)
  W$W_statusCounter[which(W$Day == 1 & W$W_status == "initialised as infected")] <- 1
  W$W_statusCounter[which(W$Day == 1 & W$W_status == "susceptible")] <- 0
  
  W <- f_updateStatusByCounter(W = W, day = 1, prm_workers = prm_workers)
  
  return(W)
}

##### f_DRM_Watanabe() Watanabe's dose-response function #####
f_DRM_Watanabe <- function(
  dose, ## (numeric, vector) the dose(s) inhaled by different individuals (the number of virions)
  r ## (numeric) the form parameter of the Watanabe dose-response model
) {
  
  P_infection <- 1 - exp(- r * dose)
  
  return(P_infection)
}

##### f_individual_viral_load ####
f_individual_viral_load <- function(
  prm_workers,
  prm_conta
) {
  RNA_dist_parms <- prm_conta$RNA_dist[[prm_conta$VoC]]
  
  indi_viral_load <- EnvStats::rtri(n = prm_workers$NWorkers,
                                    min = RNA_dist_parms["min"],
                                    mode = RNA_dist_parms["mode"],
                                    max = RNA_dist_parms["max"])
  
  return(indi_viral_load)
}

#### f_dailyContamination #####
f_dailyContamination <- function(
  W,
  MyAir,
  day,
  indi_viral_load,
  prm_plant,
  prm_workers,
  prm_time,
  prm_air,
  prm_conta,
  inf_log,
  seed = NULL,
  ...
) {
  W_ID <- unique(W$W_ID) %>% sort()
  
  if (!is.null(seed)) {set.seed(seed+day)}
  
  writeLines(paste("\n ===================== Daily contamination : Day ", day, " ===========================", sep =""))
  
  InfectedWorkers <- subset(W, Day == day-1 & W_statusCounter>0)$W_ID %>% unique() # already infected
  
  ################### INFECTION SOURCE 1: AEROSOL ################################
  writeLines("\n (i) Aerosol")
  # The total cumulative number of the droplets
  # inhaled by each worker on the PREVIOUS day
  # writeLines("- Calculating the cumulative number of droplets inhaled by each worker")
  MASTER <- f_Module_Master(MyAir = MyAir,
                            MyWorkers = W,
                            indi_viral_load = indi_viral_load,
                            prm_plant = prm_plant,
                            prm_air = prm_air,
                            prm_time = prm_time,
                            prm_workers = prm_workers,
                            ind_min = subset(W, Day == day-1)$t_ind %>% min(),
                            ind_max = subset(W, Day == day-1)$t_ind %>% max()) # OK

  MyAir <- MASTER[[1]] # Updating the cumulative number of droplets for every days OK
  # Contaminated droplets ~=copie RNA
  Expocum <- MASTER[[2]] # sum inhaled OF THE GIVEN DAY day

  RNA_virion_ratio <- prm_conta$RNA_virion_ratio # the ratio between the number of RNA copies and virions

  ##### ASSUMPTION 1 : 1 RNA copies per droplet !
  # The percentage of droplets contaminated by RNA copies
  # Dose_per_class1 <- rep(0, prm_workers$NWorkers)
  # Dose_per_class2 <- rep(0, prm_workers$NWorkers)
  # Dose_per_class3 <- rep(0, prm_workers$NWorkers)
  # Dose_per_class4 <- rep(0, prm_workers$NWorkers)
  # 
  # Dose_per_class1[Expocum[,1]>0] <- rbinom(n = sum(Expocum[,1]>0),size= round(Expocum[Expocum[,1]>0,1]), prob = P[1])
  # Dose_per_class2[Expocum[,2]>0] <- rbinom(n = sum(Expocum[,2]>0),size= round(Expocum[Expocum[,2]>0,2]), prob = P[2])
  # Dose_per_class3[Expocum[,3]>0] <- rbinom(n = sum(Expocum[,3]>0),size= round(Expocum[Expocum[,3]>0,3]), prob = P[3])
  # Dose_per_class4[Expocum[,4]>0] <- rbinom(n = sum(Expocum[,4]>0),size= round(Expocum[Expocum[,4]>0,4]), prob = P[4])

  # Total dose of infectious virus for every classes inhaled by each worker at the day day
  Virion_dose = (Expocum[,1]+ Expocum[,2]+ 
                   Expocum[,3] + Expocum[,4])/RNA_virion_ratio

  ##### ASSUMTION 1 (END)

  # # ##### ASSUMPTION 2 : Calculate the number of RNA copies per droplet for each droplet class
  # # set.seed(seed+day)
  # # Nd <- rpois(n = length(Parms_Air$Droplet_class),
  # #             lambda = VPR)
  # # 
  # # RNA_per_class1 <- round(Expocum[, 1]) * Nd[1]
  # # RNA_per_class2 <- round(Expocum[, 2]) * Nd[2]
  # # RNA_per_class3 <- round(Expocum[, 3]) * Nd[3]
  # # RNA_per_class4 <- round(Expocum[, 4]) * Nd[4]
  # # 
  # # RNA_tot = RNA_per_class1 + RNA_per_class2 + RNA_per_class3 + RNA_per_class4 # number of RNA copies inhaled by each worker
  # # 
  # # Virion_dose <- RNA_tot / RNA_virion_ratio
  # # ##### ASSUMTION 2 (END)

  # ##### ASSUMPTION 3 : Calculate the number of virion per droplet for each droplet class
  # # by applying the RNA-Virion ratio from the beginning
  # VPR <- VP / RNA_virion_ratio
  # 
  # Ndr <- rpois(n = length(prm_air$Droplet_class),
  #              lambda = VPR)

  # Virion_dose <- round(Expocum[,1]) * Ndr[1] + round(Expocum[,2]) * Ndr[2] + round(Expocum[,3]) * Ndr[3] + round(Expocum[,4]) * Ndr[4]
  ##### ASSUMTION 3 (END)

  ##### DOSE-RESPONSE MODEL : WATANABE MODEL
  # r: best fit for the form parameter of the Watanabe's dose-response model
  # Calculate the infection probability for each worker
  P_infection <- f_DRM_Watanabe(dose = Virion_dose,
                                r = prm_conta$DRM1_r)

  # The response of each worker (get contaminated or not) based on their respective infection probability
  resp <- rbinom(n = length(P_infection), size = 1, prob = P_infection)

  # ID of the new workers infected via aerosol (response = 1) if they were not infected previously
  NewInfectedWorkers_Air <- W_ID[resp == 1 & (! W_ID %in% InfectedWorkers)]

  if (length(NewInfectedWorkers_Air) > 0) { # if there are workers getting infected via the aerosol through infection probability
    writeLines(paste(">>> Newly infected workers via aerosol : ID(s)", NewInfectedWorkers_Air, " <<<"))
    InfectedWorkers <- c(InfectedWorkers, NewInfectedWorkers_Air) %>% unique # combine with the workers already infected
    
    # Update the infection log
    inf_log$InfectedDay[inf_log$W_ID %in% NewInfectedWorkers_Air] <- day 
    inf_log$InfectionSource[inf_log$W_ID %in% NewInfectedWorkers_Air] <- "aerosol"
    
  } else {writeLines(">>> Newly infected workers via aerosol : 0 <<<")}
  

  
  
  
  ################### INFECTION PATH 2: REGIONAL PREVALENCE ####################
  writeLines("\n (ii) Regional epidemic situation")
  
  # Looking for the susceptible workers
  SusceptibleWorkers <- W_ID[! W_ID %in% InfectedWorkers]
  
  Infection_Regional <- rbinom(n = length(SusceptibleWorkers),
                               size = 1,
                               prob = prm_workers$prev)
  
  NewInfectedWorkers_Reg <- SusceptibleWorkers[Infection_Regional == 1] # looking for their corresponding IDs
  
  if (length(NewInfectedWorkers_Reg) > 0) { # if there are workers getting infected due to regional epidemic situations
    writeLines(paste(">>> Newly infected workers due to regional epidemic situation : ID(s)", toString(NewInfectedWorkers_Reg), " <<<"))
    InfectedWorkers <- c(InfectedWorkers, NewInfectedWorkers_Reg) %>% unique # combine with the workers already infected
    
    # Update the infection log
    inf_log$InfectedDay[inf_log$W_ID %in% NewInfectedWorkers_Reg] <- day 
    inf_log$InfectionSource[inf_log$W_ID %in% NewInfectedWorkers_Reg] <- "epidemy"
    
  } else {writeLines(">>> Newly infected workers due to regional epidemic situation : 0 <<<")}
  
  
  ################### INFECTION PATH 3: COMMUNITY ACTIVITY #####################
  writeLines("\n (iii) Community activities")

  # Extracting data associated with the workers having community activities
  W_com <- subset(W, Day == day-1 & Hour == 0 & Min == 0 & !is.na(W_communes))

  # Among the above workers, looking for the ones who have not been infected yet
  SusceptibleWorkers <- W_com$W_ID[! W_com$W_ID %in% InfectedWorkers]

  # For each of these susceptible worker, denoted wi
  sapply(SusceptibleWorkers, FUN = function(wi) {
    # Focus on his community
    comm_ID <- W_com$W_communes[W_com$W_ID == wi] %>% as.character()
    comm <- subset(W_com, W_communes == comm_ID)

    # Identify if there are potential source of infection (infectious workers)
    W_infected_source <- comm$W_ID[comm$W_statusCounter >= prm_workers$InfectiousDay &
                                     comm$W_statusCounter < prm_workers$NonInfectiousDay]

    if (length(W_infected_source) > 0) { # if there is at least one infected worker within the community
      ## the worker wi has a risk to get infected
      resp_wi <- rbinom(n=1, size=1, prob = prm_workers$daily_SAR[prm_workers$SAR_sim]) ## check parameters_workers

      if (resp_wi > 0) { # if the worker wi actually gets infected, show infection information
        writeLines(paste("- Worker ", wi,
                         " infected probably by the worker(s) ", toString(W_infected_source),
                         " within the community ", comm_ID, sep = ""))
      }
    }
    else { # otherwise, if there is not any infected worker within the community
      resp_wi <- 0 # the worker wi has 0 risk to get infected
    }
    
    return(resp_wi)
  }) -> CommInfection_resp

  # Gathering all workers possibly get infected through community activities
  NewInfectedWorkers_Comm <- CommInfection_resp[CommInfection_resp > 0] %>% names()


  # Combine with other workers already infected via other infection source
  if (length(NewInfectedWorkers_Comm) > 0) {
    InfectedWorkers <- c(InfectedWorkers, NewInfectedWorkers_Comm) %>% unique()
    
    # Update the infection log
    inf_log$InfectedDay[inf_log$W_ID %in% NewInfectedWorkers_Comm] <- day 
    inf_log$InfectionSource[inf_log$W_ID %in% NewInfectedWorkers_Comm] <- "community"
    
  } else {
    writeLines(">>> Newly infected workers due to community activities : 0 <<<")
  }
  
  
  ################### UPDATING COUNTERS #####################
  writeLines("\n Updating data")
  # By default, copy all status counters from the previous day
  W$W_statusCounter[which(W$Day == day)] <- W$W_statusCounter[which(W$Day == day-1)] 
  # Update the status counters for the infected workers (including the previously and newly infected ones)
  W$W_statusCounter[which(W$Day == day & W$W_ID %in% InfectedWorkers)] <- W$W_statusCounter[which(W$Day == day-1 & W$W_ID %in% InfectedWorkers)] + 1 
  # Update the status for all workers based on their respective counter
  W <- f_updateStatusByCounter(W = W, day = day, prm_workers = prm_workers)
  
  
  return(list(W = W, MyAir = MyAir, inf_log = inf_log, Virion_dose = Virion_dose, Expocum = Expocum))
  ## END OF FUNCTION
}
