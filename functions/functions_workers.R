##### PACKAGES #####
library(ggplot2)
library(reshape2)
library(plotly)
library(stringr)

##### f_initWorkers() FUNCTION TO INITIALISE A SET OF WORKERS #####
f_initWorkers <- function(
  ## Function allowing to initialize a set of workers with different infection states and mask behaviour
  ## depending on probability parameters provided as input argument.
  #### INPUT
  prm, ## (list) the parameters associated with all attributes of the workers with at least the following elements
  ## (check the script "parameters_workers.R")
  prm_time, ## (list) the parameters associated with the timetable
  ## (check the script "parameters_time.R")
  seed = NULL,
  ...
  #### OUTPUT
  ## W (dataframe): the initialised workers
  ##  - $W_ID (character/string): the ID of each worker ("W001","W002",...)
  ##  - $W_state (character/string): infection state ("infected"/"not infected",...)
  ##  - $W_mask (character/string): behavior ("mask"/"no mask",...)
  ##  - $coordX (numeric): coordinates in the plant, initialized as NA
  ##  - $coordY (numeric): coordinates in the plant, initialized as NA
  ##  - $location (character): location in the plant ("Entry hall", "W.C.",...), initialized as NA
) {
  #### BEGIN OF FUNCTION
  if (!is.null(seed)) {set.seed(seed)}
  
  ### ID of the workers (value "W001","W002", ..)
  W_ID <- paste("W", stringr::str_pad(seq(1:prm$NWorkers), width=3, pad="0"), sep="")
  
  ### Total number of the time indexes
  writeLines("=========== Initializing workers: Step 1/2 - Processing time indexes ===========")
  writeLines("Processing time indexes")
  NTime <- prm_time$NDays * 1440 / prm_time$Step ## amplitude Ndays in days, time step in minutes
  t_ind <- rep(0:NTime, each = prm$NWorkers)
  writeLines(paste("Time indexes going from 0 to", NTime))
  
  ## Convert time indexes to D,H,M
  dt = prm_time$Step
  writeLines("Converting time indexes into D:H:M format")
  t_time <- t_ind %>% sapply(FUN = function(x) {
    f_convertTime("ind2time", dt=dt, t_ind = x)
    })
  Day <- t_time[1,]
  Hour <- t_time[2,]
  Min <- t_time[3,]
  writeLines("Converting time into week numbers and weekdays format")
  Week <- Day %>% sapply(FUN = f_Day2Week)
  Weekday <- Day %>% sapply(FUN = f_Day2Weekday)
  
  
  ### States (infected/not infected) of the workers
  # Time index 0:
  # HYPOTHESE : ONE INDIVIDUAL CONTAMINATED AT DAY 0 ?
  writeLines("=========== Initializing workers: Step 2/2 - Workers attributes ===========")
  writeLines("Assign the first random contaminated worker")
  W_status0 <- rep("susceptible", prm$NWorkers)
  set.seed(seed)
  W_status0[sample(1:prm$NWorkers, prm$nContaminated_Init)] <- "initialised as infected"
  # Initialization for the subsequent time indexes as NA
  W_status <- c(W_status0, rep(NA, prm$NWorkers * NTime))
  
  ## Status counter
  W_statusCounter <- rep(NA, prm$NWorkers*(NTime+1))
  
  ### Behavior (mask/no mask) of the workers
  # At the time index 0
  # The proportion of workers wearing a mask depending on the type of mask
  writeLines("Simulating mask acceptability")
  pMask <- prm$pMaskAcceptability[prm$MaskType] %>% unname
  
  # Wearing mask or not for each worker
  set.seed(seed)
  W_mask0 <- sample(c("mask", "no mask"), replace = T,
                    size = prm$NWorkers,
                    prob = c(pMask,1-pMask))
  # Initialization for the subsequent time indexes as NA
  W_mask <- c(W_mask0, rep(NA, prm$NWorkers * NTime))
  
  ### Assign the types and teams for all workers
  # at time 0
  writeLines("Assign workers types and teams")
  W_TT <- f_assignWorkersTypeTeam(prm = prm, seed=seed)  ## check the function f_assignWorkersTypeTeam
  W_type0 <- W_TT$W_type
  W_team0 <- W_TT$W_team
  
  # Replicate the type of workers for all subsequent time indexes
  W_type <- as.factor(rep(W_type0, NTime+1)) ## + 1 (including the time index 0)
  W_team <- as.factor(c(W_team0, rep(NA, prm$NWorkers * NTime))) ## (including the time index 0)
  
  ## Initialize the shift of all workers
  W_shift <- rep(NA, prm$NWorkers * (NTime+1))
  
  ### Initialize coordinates and location of the workers (NA)
  coordX <- rep(NA, prm$NWorkers*(NTime+1))
  coordY <- rep(NA, prm$NWorkers*(NTime+1))
  location <- rep(NA, prm$NWorkers*(NTime+1))
  
  ### Initialize the working (or not-working phase) of the workers (NA)
  W_active <- rep("active", prm$NWorkers*(NTime+1)) ## active by default
  W_activeCounter <- rep(NA, prm$NWorkers*(NTime+1))
  
  ## Community activities
  ## Assign the workers to the different communes if applicable
  writeLines("Assign workers community")
  AssignedCommunity <- f_assignCommunes(prm, seed = seed) ## check the (sub)function f_assignCommunes()
  
  W_communityActivity <- AssignedCommunity$W_communityActivity0 %>% rep(NTime+1) 
  W_communes <- AssignedCommunity$W_communes0 %>% rep(NTime+1)
  
  ### Output
  W <- data.frame(W_ID = rep(W_ID, NTime+1),
                  W_status = W_status,
                  W_statusCounter = W_statusCounter,
                  W_mask = W_mask,
                  W_type = W_type,
                  W_team = W_team,
                  W_shift = W_shift,
                  W_active = W_active,
                  W_activeCounter = W_activeCounter,
                  W_communityActivity = W_communityActivity,
                  W_communes = W_communes,
                  coordX = coordX,
                  coordY = coordY,
                  location = location,
                  t_ind = t_ind,
                  Week = Week,
                  Weekday = Weekday,
                  Day = Day,
                  Hour = Hour,
                  Min = Min
  )
  writeLines("============================ Workers initialized ============================")

  return(W)
  #### END OF FUNCTION
}


##### f_assignCommunes() SUB-FUNCTION TO ASSIGN WORKERS TO DIFFERENT COMMUNES #####
f_assignCommunes <- function(
  prm, ## (list) the parameters associated with all attributes of the workers with at least the following elements
  ## (check the script "parameters_workers.R")
  ...
) {
  #### BEGIN OF FUNCTION
  ### Community activities
  # The workers with/without community activities
  # set.seed(seed)
  W_communityActivity0 <- sample(x = c(T,F),
                                 size = prm$NWorkers, replace = T,
                                 prob = c(prm$pCommunityActivity, 1-prm$pCommunityActivity))
  
  ## Assign the workers to the different communes if applicable
  # Total number of communes (groups) depending on the total number with activities and the average number of workers per commune
  # The number of communes
  NfullCommunes <- length(which(W_communityActivity0)) %/% prm$N_perCommunityActivity
  leftover <- length(which(W_communityActivity0)) %% prm$N_perCommunityActivity
  
  if (leftover <= 1) {
    LastCommunes <- NfullCommunes
  } else {
    LastCommunes <- NfullCommunes + 1
  }
  
  # Initialize the communes names 
  Communes0 <- paste("Com", stringr::str_pad(seq(1:LastCommunes), width=3, pad="0"), sep="")
  
  ## Communes of each worker
  W_communes0 <- rep(NA, prm$NWorkers)
  workers_index <- sample(which(W_communityActivity0)) ## randomize the indexes of the workers with community activities
  
  for (ic in 1:NfullCommunes) {
    ind_begin <- ic * prm$N_perCommunityActivity - (prm$N_perCommunityActivity - 1)
    ind_end <- ic * prm$N_perCommunityActivity
    W_communes0[workers_index[ind_begin:ind_end]] <- Communes0[ic]
  }
  
  if (LastCommunes > NfullCommunes) { ## s'il reste au moins 2 personnes en trop
    ind_begin <- LastCommunes * prm$N_perCommunityActivity - (prm$N_perCommunityActivity - 1)
    ind_end <- ind_begin + leftover - 1
    W_communes0[workers_index[ind_begin:ind_end]] <- Communes0[LastCommunes]
  } else {
    if (leftover == 1) {
      LastWorkers <- LastCommunes * prm$N_perCommunityActivity + 1
      W_communes0[workers_index[LastWorkers]] <- Communes0[LastCommunes]
    }
  }
  
  AssignedCommunity <- data.frame(W_communityActivity0 = W_communityActivity0,
                                  W_communes0 = W_communes0)
  
  return(AssignedCommunity)
  #### END OF FUNCTION
}


##### f_assignWorkersTypeTeam() SUB-FUNCTION OF f_initWorkers() TO ASSIGN WORKERS FOR DIFFERENT TYPES AND TEAMS #####
f_assignWorkersTypeTeam <- function(
  ## INTPUT
  prm,
  ##  prm: list of parameters associated with workers (check the script "parameters_workers.R")
  ...
  ## OUTPUT: list of two elements
  ## - W_type
  ## - W_team
) {
  ## The proportion between the types of workers
  pType <- prm$pType
  ## The global proportion between the teams
  pTeam <- prm$pTeam
  
  
  ## Calculate the proportions of combining the types and team, p_TT
  ## Nomenclature: C1_TA (cutter1, teamA); L2_TB (logistic2, teamB), T1_TR (transverse1, transverse)
  p_TT <- c("C1_TA" = unname(pType["cutter1"] * pTeam["teamA"]),
            "C1_TB" = unname(pType["cutter1"] * pTeam["teamB"]),
            "C2_TA" = unname(pType["cutter2"] * pTeam["teamA"]),
            "C2_TB" = unname(pType["cutter2"] * pTeam["teamB"]),
            "L1_TA" = unname(pType["logistic1"] * pTeam["teamA"]),
            "L1_TB" = unname(pType["logistic1"] * pTeam["teamB"]),
            "L2_TA" = unname(pType["logistic2"] * pTeam["teamA"]),
            "L2_TB" = unname(pType["logistic2"] * pTeam["teamB"]),
            "T1_TR" = unname(pType["transverse1"]),
            "T2_TR" = unname(pType["transverse2"])
            )
  
  ## Sampling the workers with combined characteristics (type and team)
  W_TT <- sample(x = names(p_TT),
                 size = prm$NWorkers, replace = T,
                 prob = unname(p_TT))
  
  ## Create two distinct columns for type and team of workers
  W_type <- rep(NA, prm$NWorkers)
  W_team <- rep(NA, prm$NWorkers)
  
  ## Rename the values
  for (i in 1:prm$NWorkers) {
    switch(stringr::str_sub(W_TT[i], 1, 2),
           "C1" = {W_type[i] <- "cutter1"},
           "C2" = {W_type[i] <- "cutter2"},
           "L1" = {W_type[i] <- "logistic1"},
           "L2" = {W_type[i] <- "logistic2"},
           "T1" = {W_type[i] <- "transverse1"},
           "T2" = {W_type[i] <- "transverse2"}
    )
    switch(stringr::str_sub(W_TT[i], 4, 5),
           "TA" = {W_team[i] <- "teamA"},
           "TB" = {W_team[i] <- "teamB"},
           "TR" = {W_team[i] <- "transverse"}
    )
  }

  return(list(W_type = W_type,
              W_team = W_team))
}


##### f_changeTeamWeekly() FUNCTION TO ASSIGN POSSIBLE CHANGES OF WORKERS TEAM FROM ONE WEEK TO ANOTHER #####
f_changeTeamWeekly <- function(
  ## Function to assign possible team changes (A to B and vice versa) from one week to another and
  ## only for a user-given proportion of workers
  ## INTPUT
  W, ## data.frame for all workers (output of the function f_initWorkers)
  prob, ## the proportion of workers susceptible to change
  week_from, ## the week from which the workers change their teams
  seed = NULL,
  ...
) {
  W$W_team <- as.character(W$W_team)
  all_ID <- unique(W$W_ID) ## ID of all workers
  
  ## EXCEPTIONAL CASE: if it's the first week (recently initialized data,
  ## meaning that only the first rows of the week are filled (day 1, OhOO))
  ## Copy the beginning of the first day of the week to all other days of the week
  if (week_from == 1) { 
    # for (i in 1:length(all_ID)) {
    #   tmp <- W$W_team[which(W$W_ID == all_ID[i] & W$Weekday == "Monday" & W$Hour == 0 & W$Min == 0 &
    #                           W$Week == week_from)]
    #   W$W_team[which(W$W_ID == all_ID[i] & W$Week == week_from)] <- tmp
    # }
    
    ## /!\ Optimized code (begin)
    lapply(all_ID, FUN = function(x) {
      d1 <- subset(W, W_ID == x)
      d1$W_team[which(d1$Week == week_from)] <- d1$W_team[which(d1$Weekday == "Monday" & d1$Hour == 0 & d1$Min == 0 &
                                                                     d1$Week == week_from)]
      return(d1)
    }) %>% data.table::rbindlist() -> W
    ## /!\ Optimized code (end)
  }

  # Copy and paste the team of every workers from one week to another without any changes
  # for (i in 1:length(all_ID)) {
  # tmp <- W$W_team[which(W$W_ID == all_ID[i] & W$Weekday == "Monday" & W$Hour == 0 & W$Min == 0 &
  #                       W$Week == week_from)]
  # W$W_team[which(W$W_ID == all_ID[i] &
  #                  W$Week == (week_from+1))] <- tmp
  # }
  
  ## /!\ Optimized code (begin)
  lapply(all_ID, FUN = function(x) {
    d1 <- subset(W, W_ID == x)
    d1$W_team[which(d1$Week == (week_from+1))] <- d1$W_team[which(d1$Weekday == "Monday" & d1$Hour == 0 & d1$Min == 0 &
                                                                       d1$Week == week_from)]
    return(d1)
  }) %>% data.table::rbindlist() -> W
  ## /!\ Optimized code (end)
  
  ### CHANGE TEAM FOR SOME WORKERS ###
  ## Worker in each team of the week
  teamA_ID <- unique(subset(W, W_team == "teamA" & Week == week_from)$W_ID)
  teamB_ID <- unique(subset(W, W_team == "teamB" & Week == week_from)$W_ID)

  ## Number of workers to be switched from one team to another
  nb_changes <- rbinom(1, length(teamA_ID), prob = prob)
  if (nb_changes == 0) {nb_changes <- 1} ## at least one worker

  ## Choose random workers to change team
  W_AtoB_ID <- sample(teamA_ID, size = nb_changes)
  W_BtoA_ID <- sample(teamB_ID, size = nb_changes)
  
  for (i in 1:nb_changes) { ## for each pair of workers changing the team
    w_Ai <- W_AtoB_ID[i] ## worker Ai to be switched with Bi
    w_Bi <- W_BtoA_ID[i] ## worker Bi to be switched with Ai
    ## assign their new respective teams for the week + 1
    W$W_team[which(W$Week == (week_from+1) & W$W_ID == w_Ai)] <- "teamB"
    W$W_team[which(W$Week == (week_from+1) & W$W_ID == w_Bi)] <- "teamA"
  }

  W$W_team <- as.factor(W$W_team)
  
  #W <- dplyr::arrange(W, t_ind, W_ID)
  
  return(W)
}


##### f_assignWorkersShift() FUNCTION TO ASSIGN THE SHIFT FOR WORKERS DEPENDING ON THEIR TYPE AND TEAM #####
f_assignWorkersShift <- function(
  ## Function to assign the shift for all workers depending on their respective
  ## assigned worker types and teams.
  ## /!\ ONLY RUN WHEN THE FUNCTION f_changeTeamWeekly() HAS BEEN SUCCESFULLY RUN
  ## INPUT
  W ## data.frame for all workers (output of the function f_initWorkers)
  ## OUTPUT
  ## W: updated W with the assigned team for all workers over time
) {
  apply(W, 1, function(x) {
    if (x['W_team'] == "teamA" & (as.numeric(x['Week']) %% 2 == 0)) {return("morning")} ## even week
    if (x['W_team'] == "teamA" & (as.numeric(x['Week']) %% 2 != 0)) {return("afternoon")} ## odd week
    if (x['W_team'] == "teamB" & (as.numeric(x['Week']) %% 2 == 0)) {return("afternoon")} ## even week
    if (x['W_team'] == "teamB" & (as.numeric(x['Week']) %% 2 != 0)) {return("morning")} ## odd week
    if (x['W_type'] == "transverse1") {return("day shift")} ## cleaner
    if (x['W_type'] == "transverse2") {return("night shift")} ## cleaner
  }) %>% unlist -> W$W_shift
  
  return(W)
}



##### f_initActiveCounter() FUNCTION TO ASSIGN THE RESTING DAY FOR ALL WORKERS #####
f_initActiveCounter <- function(
  ## Function to assign the resting day for all workers and over the overall timetable
  ## Rules:
  ##  - No active workers on Saturday and Sunday,
  ##  - One-week holiday period after six-week active period
  ##  - No inter-workers synchronization for holiday/active periods
  ## INPUT
  W, ## data.frame (output of the )
  seed = NULL,
  ...
  ## OUTPUT
  ## W: The data frame with initialized active counters at the first time index for all workers
) {
  NWorkers <- length(unique(W$W_ID))
  ## Assgin the active status of Saturday and Sunday in to "weekend"
  W$W_active[which(W$Weekday %in% c("Saturday", "Sunday"))] <- "weekend"
  
  ## Initialize random active counters for each workers at the first time index t_ind
  ## using 5-day gap between their respective counters (-5,0,5,10,15,20,25)
  set.seed(seed)
  W$W_activeCounter[which(W$t_ind == 0)] <- sample(x = seq(-5, 25, by=5),
                                            size = NWorkers,
                                            replace = T,
                                            prob = rep(1/7, 7)) 
  return(W)
}

##### f_calculateIndividualActiveCounter() SUB-FUNCTION OF f_calculateActiveCounter() ##### 
f_calculateIndividualActiveCounter <- function(
  ## Function allowing to calculate the active counter of one worker day by day
  ## over his/her whole time table
  ## INPUT
  W, ## (Subset of) data frame associated with ONE WORKER and ONLY DATA AT 0:00 EVERY DAY
  ...
  ## OUTPUT
  ## W: the updated data frame with calculated active counters for the considered worker and for every day 
) {
  if (W$W_activeCounter[1] < 0) {
    W$W_active[1] <- "holiday"
  }
  for (i in 2:nrow(W)) {
    if (W$W_active[i] %in% c("active", "holiday")) {
      W$W_activeCounter[i] <- W$W_activeCounter[i-1] + 1
    } else {
      W$W_activeCounter[i] <- W$W_activeCounter[i-1]
    }
    if (W$W_activeCounter[i] == 30) {
      W$W_activeCounter[i] <- -5
    }
    if (W$W_activeCounter[i] < 0 & ! W$Weekday[i] %in% c("Saturday", "Sunday")) {
      W$W_active[i] <- "holiday"
    }
  }
  return(W)
}

##### f_calculateActiveCounter() FUNCTION TO CALCULATE ACTIVE COUNTERS FOR ALL WORKERS #####
f_calculateActiveCounter <- function(
  ## Function to calculate active counters for all workers
  ## and to fill cases with missing data (cases associated with every time step of each day not filled yet)
  ## INTPUT
  W ## Data frame with the active counters initialized at the first time index
  ## OUTPUT
  ## OUTPUT: data frame with calculated active counters for every workers
) {
  writeLines("Processing active counter for all workers")
  ## data manipulation: row_ID added, necessary for matching values after calculation of active counters
  ## W <- data.frame(row_ID = 1:nrow(W), W) 
  ## Extract the data at the beginning of every day
  d00 <- subset(W, Hour == 0 & Min == 0)
  dComp <- subset(W, !(Hour == 0 & Min == 0)) ## the complementary subset
  
  ## Calculate the active counters for all workers, one by one, store in the list tmp
  by(data = d00,
     INDICES = d00$W_ID,
     FUN = f_calculateIndividualActiveCounter) -> tmp
  
  ## Data table / list manipulation for updating W with the calculated active counters
  AllCounters <- data.table::rbindlist(tmp)
  
  # ## Matching by the rows ID to copy values obtained from AllCounters to W
  # ROWS <- AllCounters$row_ID
  # for (r in 1:length(ROWS)) {
  #   W[which(W$row_ID == ROWS[r]), ] <- AllCounters[which(AllCounters$row_ID == ROWS[r]),]
  # }
  
  ## Re-arrange the data frame
  rbind(AllCounters, dComp) %>% 
    dplyr::arrange(t_ind, W_ID) -> W
  
  ## Replicate information for the resting (not filled) cases of every day
  ALL_ID <- unique(as.character(W$W_ID))
  # # ## Replicate information individually for every workers, one by one
  # # ## and combine in one data frame OUTPUT
  # # 
  # ########### /!\ Non-Optimized code (begin)
  # # OUTPUT <- data.frame()
  # # for (i in 1:length(ALL_ID)) {
  # #   writeLines(paste("Worker",ALL_ID[i]))
  # #   d1W <- subset(W, W_ID == ALL_ID[i])
  # #   d1W <- f_replicateIndividualDaily(Agent = d1W,
  # #                                     Invariant = c("W_activeCounter", "W_active"))
  # #   OUTPUT <- rbind(OUTPUT, d1W)
  # # }
  # ############ /!\ Non-Optimized code (end)

  ############ /!\ Optimized code (begin)
  lapply(ALL_ID, FUN = function(x) {
    writeLines(paste("Worker", x))
    d1 <- subset(W, W_ID == x)
    d1 <- f_replicateIndividualDaily(Agent = d1,
                                     Invariant = "W_activeCounter")
    d1 <- f_replicateIndividualDaily(Agent = d1,
                                     Invariant = c("W_active"))
    return(d1)
  }) %>%
    data.table::rbindlist() %>%
    dplyr::arrange(t_ind, W_ID) -> OUTPUT
  ############ /!\ Optimized code (end)
  writeLines("======= Processing active counter for all workers: done ==========")
  return(OUTPUT)
}

##### f_setupSchedule() FUNCTION TO SET UP THE OVERALL SCHEDULE FOR ALL WORKERS #####
f_setupSchedule <- function(
  W,
  prm,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  ## Simulate the weekly team changes
  for (i in 1:(max(W$Week)-1)) {
    writeLines(paste("Simulating weekly team changes - Week", i))
    W <- f_changeTeamWeekly(W = W,
                            prob = prm$pChangeTeam,
                            week_from = i)
  }
  
  ## Assign working shift for all workers
  writeLines("Assign working shifts for all workers")
  W <- f_assignWorkersShift(W)
  
  ## Calculate the active counter of every workers and fill the case 0h00
  W <- f_initActiveCounter(W = W, seed=seed+1)
  
  W <- f_calculateActiveCounter(W = W)
  
  return(W)
}

