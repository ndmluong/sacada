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
  ...
  #### OUTPUT
  ## W (dataframe): the initialised workers
  ##  - $W_ID (character/string): the ID of each worker ("W001","W002",...)
  ##  - $W_state (character/string): infection state ("infected"/"not infected",...)
  ##  - $W_mask (character/string): behavior ("mask"/"no mask",...)
  ##  - $W_coordX (numeric): coordinates in the plant, initialized as NA
  ##  - $W_coordY (numeric): coordinates in the plant, initialized as NA
  ##  - $W_location (character): location in the plant ("Entry hall", "W.C.",...), initialized as NA
) {
  #### BEGIN OF FUNCTION
  set.seed(seed)
  ### ID of the workers (value "W001","W002", ..)
  W_ID <- paste("W", stringr::str_pad(seq(1:prm$NWorkers), width=3, pad="0"), sep="")
  
  ### Total number of the time indexes
  print("Processing time indexes")
  NTime <- prm_time$NDays * 1440 / prm_time$Step ## amplitude Ndays in days, time step in minutes
  t_ind <- rep(0:NTime, each = prm$NWorkers)
  ## Convert time indexes to D,H,M
  t_time <- t_ind %>% sapply(FUN = function(x) f_convertTime("ind2time", dt=prm_time$Step, t_ind = x))
  Day <- t_time[1,]
  Hour <- t_time[2,]
  Min <- t_time[3,]
  Week <- Day %>% sapply(FUN = f_Day2Week)
  Weekday <- Day %>% sapply(FUN = f_Day2Weekday)
  
  
  ### States (infected/not infected) of the workers
  # Time index 0:
  # HYPOTHESE : ONE INDIVIDUAL CONTAMINATED AT DAY 0 ?
  print("Assign the first random contaminated indivual")
  W_status0 <- rep("not contaminated", prm$NWorkers)
  set.seed(seed)
  W_status0[sample(1:prm$NWorkers, 1)] <- "contaminated"
  # Initialization for the subsequent time indexes as NA
  W_status <- as.factor(c(W_status0, rep(NA, prm$NWorkers * NTime)))
  
  ### Behavior (mask/no mask) of the workers
  # At the time index 0
  # The proportion of workers wearing a mask depending on the type of mask
  pMask <- prm$pMaskAcceptability[prm$MaskType] %>% unname
  
  # Wearing mask or not for each worker
  set.seed(seed)
  W_mask0 <- sample(c("mask", "no mask"), replace = T,
                    size = prm$NWorkers,
                    prob = c(pMask,1-pMask))
  # Initialization for the subsequent time indexes as NA
  W_mask <- as.factor(c(W_mask0, rep(NA, prm$NWorkers * NTime)))
  
  ### Assign the types and teams for all workers
  # at time 0
  print("Assign workers types and teams")
  W_TT <- f_assignWorkersTypeTeam(prm = prm, seed=seed)  ## check the function f_assignWorkersTypeTeam
  W_type0 <- W_TT$W_type
  W_team0 <- W_TT$W_team
  
  # Replicate the type of workers for all subsequent time indexes
  W_type <- as.factor(rep(W_type0, NTime+1)) ## + 1 (including the time index 0)
  W_team <- as.factor(c(W_team0, rep(NA, prm$NWorkers * NTime))) ## (including the time index 0)
  
  ## Initialize the shift of all workers
  W_shift <- rep(NA, prm$NWorkers * (NTime+1))
  
  ### Initialize coordinates and location of the workers (NA)
  W_coordX <- rep(NA, prm$NWorkers*(NTime+1))
  W_coordY <- rep(NA, prm$NWorkers*(NTime+1))
  W_location <- rep(NA, prm$NWorkers*(NTime+1))
  
  ### Initialize the working (or not-working phase) of the workers (NA)
  W_active <- rep("active", prm$NWorkers*(NTime+1))
  W_activeCounter <- rep(NA, prm$NWorkers*(NTime+1))
  
  ## Community activities
  ## Assign the workers to the different communes if applicable
  print("Assign workers community")
  AssignedCommunity <- f_assignCommunes(prm, seed = seed) ## check the (sub)function f_assignCommunes()
  
  W_communityActivity <- AssignedCommunity$W_communityActivity0 %>% rep(NTime+1) 
  W_communes <- AssignedCommunity$W_communes0 %>% rep(NTime+1) %>% as.factor
  
  ### Output
  W <- data.frame(W_ID = rep(W_ID, NTime+1),
                  W_status = W_status,
                  W_mask = W_mask,
                  W_type = W_type,
                  W_team = W_team,
                  W_shift = W_shift,
                  W_active = W_active,
                  W_activeCounter = W_activeCounter,
                  W_communityActivity = W_communityActivity,
                  W_communes = W_communes,
                  W_coordX = W_coordX,
                  W_coordY = W_coordY,
                  W_location = W_location,
                  t_ind = t_ind,
                  Week = Week,
                  Weekday = Weekday,
                  Day = Day,
                  Hour = Hour,
                  Min = Min
  )
  
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
  set.seed(seed)
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
  set.seed(seed)
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
  ## Func
  ## INTPUT
  W, ## data.frame for all workers (output of the function)
  prob,
  week_from,
  seed = NULL,
  ...
) {
  W$W_team <- as.character(W$W_team)
  
  all_ID <- unique(W$W_ID)
  
  ## EXCEPTIONAL CASE: if it's the first week (recently initialized data)
  ## Copy the beginning of the first day of the week to all other days of the week
  if (week_from == 1) { 
    for (i in 1:length(all_ID)) {
      tmp <- W$W_team[which(W$W_ID == all_ID[i] & W$Weekday == "Monday" & W$Hour == 0 & W$Min == 0 &
                              W$Week == week_from)]
      W$W_team[which(W$W_ID == all_ID[i] & W$Week == week_from)] <- tmp
    }
  }
  
  ## Worker in each team of the week
  teamA_ID <- unique(subset(W, W_team == "teamA" & Week == week_from)$W_ID)
  teamB_ID <- unique(subset(W, W_team == "teamB" & Week == week_from)$W_ID)
  
  ## Copy and paste the team of every workers from one week to another
  for (i in 1:length(all_ID)) {
    tmp <- W$W_team[which(W$W_ID == all_ID[i] & W$Weekday == "Monday" & W$Hour == 0 & W$Min == 0 &
                          W$Week == week_from)]
    W$W_team[which(W$W_ID == all_ID[i] & 
                     W$Week == (week_from+1))] <- tmp
  }

  ### CHANGE TEAM FOR SOME WORKERS ###
  ## Number of workers to be switched from one team to another
  nb_changes <- rbinom(1, length(teamA_ID), prob = prob)
  if (nb_changes == 0) {nb_changes <- nb_changes + 1} ## at least one worker
  
  ## Choose random workers to change team
  set.seed(seed)
  W_AtoB_ID <- sample(teamA_ID, size = nb_changes)
  set.seed(seed)
  W_BtoA_ID <- sample(teamB_ID, size = nb_changes)
  
  for (i in 1:nb_changes) { ## for each pair of workers changing the team
    w_Ai <- W_AtoB_ID[i] ## worker Ai to be switched with Bi
    w_Bi <- W_BtoA_ID[i]
    W$W_team[which(W$Week == (week_from+1) & W$W_ID == w_Ai)] <- "teamB"
    W$W_team[which(W$Week == (week_from+1) & W$W_ID == w_Bi)] <- "teamA"
  }
  
  W$W_team <- as.factor(W$W_team)
  
  return(W)
}


##### f_assignWorkersShift() FUNCTION TO ASSIGN THE SHIFT FOR WORKERS DEPENDING ON THEIR TYPE AND TEAM #####
f_assignWorkersShift <- function(
  ## INPUT
  W ## data.frame for all workers (output of the function)
  ## OUTPUT
  ## W_OUT: updated W with the assigned team for all workers over time
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



##### f_assignRestingDay() FUNCTION TO ASSIGN THE RESTING DAY FOR ALL WORKERS #####
f_assignRestingDay <- function(
  W, ## data.frame (output of the )
  ...
) {
  
  ### tous les samedi et tous les dimanches, tout le monde devient "resting day"
  
  
  ### tous les jours de la semaine il y a 20% en idle 
  
  ### initialiser les compteurs des actifs: par tranche de 5: 0, 5, 10, 15, 20, 25
  return(W)
}