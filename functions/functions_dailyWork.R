##### f_dailyWork_AM #####
f_dailyWork_AM <- function(
  Plant,
  W,
  D,
  dt,
  seed = NULL
) {
  # writeLines(paste(">>> Day ", D, " - Morning team", sep=""))
  active <- subset(W, W_active == "active" & Day == D)
  
  AM_ID <- subset(active, W_shift == "morning")$W_ID %>% unique()
  AM_C1_ID <- subset(active, W_shift == "morning" & W_type == "cutter1")$W_ID %>% unique()
  AM_C2_ID <- subset(active, W_shift == "morning" & W_type == "cutter2")$W_ID %>% unique()
  AM_L1_ID <- subset(active, W_shift == "morning" & W_type == "logistic1")$W_ID %>% unique()
  AM_L2_ID <- subset(active, W_shift == "morning" & W_type == "logistic2")$W_ID %>% unique()
  
  L1_location <- c("Arrival gate", "WS-Conveyor1-Head")
  L2_location <- c("WS-Conveyor2-Tail", "Waste area", "WS-Equipment 1", "Cooling area")
  
  ## 5:00 AM
  W <- f_moveWorkers(Plant, W, AM_ID, "Entry hall", t_ind=f_convertTime("time2ind",D,5,0,dt=dt))
  
  ## 5:05 AM
  t_ind <- f_convertTime("time2ind",D,5,5,dt=dt)
  W <- f_assignCuttersPosition(Plant, W, t_ind=t_ind, cuttertype = 1, shift = "morning")
  W <- f_assignCuttersPosition(Plant, W, t_ind=t_ind, cuttertype = 2, shift = "morning")
  W <- f_moveWorkers(Plant, W, AM_L1_ID, to=sample(L1_location,1), t_ind=t_ind)
  W <- f_moveWorkers(Plant, W, AM_L2_ID, to=sample(L2_location,1), t_ind=t_ind)
  
  ## from 5:05 AM to 12:55
  t_ind_begin <- f_convertTime("time2ind",D,5,5,dt=dt)
  t_ind_end <- f_convertTime("time2ind",D,12,55,dt=dt)
  
  W <- f_replicateWorkerstime2time(W, AM_C1_ID, "location", c(5,5), c(12,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, AM_C1_ID, "coordX", c(5,5), c(12,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, AM_C1_ID, "coordY", c(5,5), c(12,55), D=D, dt=dt)
  
  W <- f_replicateWorkerstime2time(W, AM_C2_ID, "location", c(5,5), c(12,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, AM_C2_ID, "coordX", c(5,5), c(12,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, AM_C2_ID, "coordY", c(5,5), c(12,55), D=D, dt=dt)
  
  # logistic1 workers - morning
  Wsub <- subset(W, t_ind >= t_ind_begin & t_ind <= t_ind_end & W_ID %in% AM_L1_ID)
  Wcomp <- subset(W, !(t_ind >= t_ind_begin & t_ind <= t_ind_end & W_ID %in% AM_L1_ID))
  lapply(AM_L1_ID, FUN = function(x) {
    d1 <- subset(Wsub, W_ID == x)
    for (ti in t_ind_begin:t_ind_end) {
      d1 <- f_moveWorkers(Plant, d1, selectW = x, to = sample(L1_location,1), t_ind = ti)
    }
    return(d1)
  }) %>% rbindlist() %>% rbind(Wcomp) -> W
  
  # logistic2 workers - morning
  Wsub <- subset(W, t_ind >= t_ind_begin & t_ind <= t_ind_end & W_ID %in% AM_L2_ID)
  Wcomp <- subset(W, !(t_ind >= t_ind_begin & t_ind <= t_ind_end & W_ID %in% AM_L2_ID))
  lapply(AM_L2_ID, FUN = function(x) {
    d1 <- subset(Wsub, W_ID == x)
    for (ti in t_ind_begin:t_ind_end) {
      d1 <- f_moveWorkers(Plant, d1, selectW = x, to = sample(L2_location,1), t_ind = ti)
    }
    return(d1)
  }) %>% rbindlist() %>% rbind(Wcomp) -> W
  
  # random moves of all workers of the morning to W.C. (2 times between 5h5 and 12h55)
  Wsub <- subset(W, W_ID %in% AM_ID & t_ind >= t_ind_begin & t_ind <= t_ind_end)
  Wcomp <- subset(W, !(W_ID %in% AM_ID & t_ind >= t_ind_begin & t_ind <= t_ind_end))
  lapply(AM_ID, FUN = function(x) {
    d1 <- subset(Wsub, W_ID == x)
    for (ti in sample(t_ind_begin:t_ind_end,2)) {
      d1 <- f_moveWorkers(Plant, d1, selectW = x, to = "W.C.", t_ind = ti)
    }
    return(d1)
  }) %>% rbindlist() %>% rbind(Wcomp) -> W
  
  ####
  ## nothing between 9:05 to 9:45 AM
  ####
  
  ## 13:00:
  W <- f_moveWorkers(Plant, W, selectW=AM_ID, to = "Entry hall", t_ind=f_convertTime("time2ind",D,13,0,dt=dt))
  
  return(W)
}

##### f_dailyWork_T1 #####
f_dailyWork_T1 <- function(
  Plant,
  W,
  D,
  dt,
  seed = NULL
) {
  
  active <- subset(W, W_active == "active" & Day == D)
  
  T1_ID <- subset(active, W_type == "transverse1")$W_ID %>% unique()
  
  if (length(T1_ID) >= 1) { # if there is at least one transverse1 worker
    T1_location <- unique(Plant$L$location)
    T1_location <- T1_location[!T1_location %in% c("Entry hall", "W.C.", "Conveyor1", "Conveyor2", "Equipment 1", "Office")]
    
    # 9h
    W <- f_moveWorkers(Plant, W, selectW=T1_ID, to="Entry hall", t_ind=f_convertTime("time2ind",D,9,0,dt=dt))
    
    # 9h05 - 11h55
    W <- f_moveWorkers(Plant, W, selectW = T1_ID, to = "Office", t_ind = f_convertTime("time2ind",dt=dt,D,9,5))
    
    W <- f_replicateWorkerstime2time(W, selectW = T1_ID, Invariant = "location",
                                     time_begin = c(9,5), time_end=c(11,55), D=D, dt=dt)
    W <- f_replicateWorkerstime2time(W, selectW = T1_ID, Invariant = "coordX",
                                     time_begin = c(9,5), time_end=c(11,55), D=D, dt=dt)
    W <- f_replicateWorkerstime2time(W, selectW = T1_ID, Invariant = "coordY",
                                     time_begin = c(9,5), time_end=c(11,55), D=D, dt=dt)
    
    t_ind_begin <- f_convertTime("time2ind",dt=dt,D,9,5)
    t_ind_end <- f_convertTime("time2ind",dt=dt,D,11,55)
    Wsub <- subset(W, W_ID %in% T1_ID & t_ind >= t_ind_begin & t_ind <= t_ind_end)
    Wcomp <- subset(W, !(W_ID %in% T1_ID & t_ind >= t_ind_begin & t_ind <= t_ind_end))
    
    lapply(T1_ID, FUN = function(x) { ## 3 random moves in the processing plant
      d1 <- subset(Wsub, W_ID == x)
      for (ti in sample(t_ind_begin:t_ind_end, 3)) {
        d1 <- f_moveWorkers(Plant, d1, selectW = x, to = sample(T1_location,1), t_ind = ti)
      }
      return(d1)
    }) %>% rbindlist() %>% rbind(Wcomp) -> W
    
    lapply(T1_ID, FUN = function(x) { ## 2 random moves in the W.C.
      d1 <- subset(Wsub, W_ID == x)
      for (ti in sample(t_ind_begin:t_ind_end,2)) {
        d1 <- f_moveWorkers(Plant, d1, selectW = x, to = "W.C.", t_ind = ti)
      }
      return(d1)
    }) %>% rbindlist() %>% rbind(Wcomp) -> W
    
    W <- f_moveWorkers(Plant, W, selectW = T1_ID, to="Entry hall", t_ind=f_convertTime("time2ind",D,12,0,dt=dt))
    
    ## nothing between 12h05-12h40
    
    # 12h45
    W <- f_moveWorkers(Plant, W, selectW = T1_ID, to="Entry hall", t_ind=f_convertTime("time2ind",D,12,45,dt=dt))
    
    # 12h50-16h55
    W <- f_moveWorkers(Plant, W, selectW = T1_ID, to="Office", t_ind=f_convertTime("time2ind",D,12,50,dt=dt))
    
    W <- f_replicateWorkerstime2time(W, selectW = T1_ID, Invariant = "location",
                                     time_begin = c(12,50), time_end=c(16,55), D=D, dt=dt)
    W <- f_replicateWorkerstime2time(W, selectW = T1_ID, Invariant = "coordX",
                                     time_begin = c(12,50), time_end=c(16,55), D=D, dt=dt)
    W <- f_replicateWorkerstime2time(W, selectW = T1_ID, Invariant = "coordY",
                                     time_begin = c(12,50), time_end=c(16,55), D=D, dt=dt)
    
    t_ind_begin <- f_convertTime("time2ind",dt=dt,D,12,50)
    t_ind_end <- f_convertTime("time2ind",dt=dt,D,16,55)
    Wsub <- subset(W, W_ID %in% T1_ID & t_ind >= t_ind_begin & t_ind <= t_ind_end)
    Wcomp <- subset(W, !(W_ID %in% T1_ID & t_ind >= t_ind_begin & t_ind <= t_ind_end))
    lapply(T1_ID, FUN = function(x) { ## 3 random moves in the processing plant
      d1 <- subset(Wsub, W_ID == x)
      for (ti in sample(t_ind_begin:t_ind_end, 3)) {
        d1 <- f_moveWorkers(Plant, d1, selectW = x, to = sample(T1_location,1), t_ind = ti)
      }
      return(d1)
    }) %>% rbindlist() %>% rbind(Wcomp) -> W
    
    lapply(T1_ID, FUN = function(x) { ## 2 random moves in the W.C.
      d1 <- subset(Wsub, W_ID == x)
      for (ti in sample(t_ind_begin:t_ind_end,2)) {
        d1 <- f_moveWorkers(Plant, d1, selectW = x, to = "W.C.", t_ind = ti)
      }
      return(d1)
    }) %>% rbindlist() %>% rbind(Wcomp) -> W
    
    W <- f_moveWorkers(Plant, W, selectW = T1_ID, to="Entry hall", t_ind=f_convertTime("time2ind",D,17,0,dt=dt))
  }
  
  if (length(T1_ID) == 0) { # otherwise, if there is not any transverse1 worker
    
    writeLines(paste("/!\\ warnings: day ", D, ": missing transverse1 worker during the day shift", sep =""))
  }
  
  return(W)
}




##### f_dailyWork_PM #####
f_dailyWork_PM <- function(
  Plant,
  W,
  D,
  dt,
  seed = NULL
) {
  # writeLines(paste(">>> Day ", D, " - Afternoon team", sep=""))
  active <- subset(W, W_active == "active" & Day == D)
  
  PM_ID <- subset(active, W_shift == "afternoon")$W_ID %>% unique()
  PM_C1_ID <- subset(active, W_shift == "afternoon" & W_type == "cutter1")$W_ID %>% unique()
  PM_C2_ID <- subset(active, W_shift == "afternoon" & W_type == "cutter2")$W_ID %>% unique()
  PM_L1_ID <- subset(active, W_shift == "afternoon" & W_type == "logistic1")$W_ID %>% unique()
  PM_L2_ID <- subset(active, W_shift == "afternoon" & W_type == "logistic2")$W_ID %>% unique()
  
  L1_location <- c("Arrival gate", "WS-Conveyor1-Head")
  L2_location <- c("WS-Conveyor2-Tail", "Waste area", "WS-Equipment 1", "Cooling area")
  
  ## 13:00
  W <- f_moveWorkers(Plant, W, PM_ID, "Entry hall", t_ind=f_convertTime("time2ind",D,13,0,dt=dt))
  
  ## 13:05
  t_ind <- f_convertTime("time2ind",D,13,5,dt=dt)
  W <- f_assignCuttersPosition(Plant, W, t_ind=t_ind, cuttertype = 1, shift = "afternoon")
  W <- f_assignCuttersPosition(Plant, W, t_ind=t_ind, cuttertype = 2, shift = "afternoon")
  W <- f_moveWorkers(Plant, W, PM_L1_ID, to=sample(L1_location,1), t_ind=t_ind)
  W <- f_moveWorkers(Plant, W, PM_L2_ID, to=sample(L2_location,1), t_ind=t_ind)
  
  ## from 13:05 to 20:55
  t_ind_begin <- f_convertTime("time2ind",D,13,5,dt=dt)
  t_ind_end <- f_convertTime("time2ind",D,20,55,dt=dt)
  
  W <- f_replicateWorkerstime2time(W, PM_C1_ID, "location", c(13,5), c(20,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, PM_C1_ID, "coordX", c(13,5), c(20,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, PM_C1_ID, "coordY", c(13,5), c(20,55), D=D, dt=dt)
  
  W <- f_replicateWorkerstime2time(W, PM_C2_ID, "location", c(13,5), c(20,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, PM_C2_ID, "coordX", c(13,5), c(20,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, PM_C2_ID, "coordY", c(13,5), c(20,55), D=D, dt=dt)
  
  # logistic1 workers - afternoon - random moves in their workspace
  Wsub <- subset(W, t_ind >= t_ind_begin & t_ind <= t_ind_end & W_ID %in% PM_L1_ID)
  Wcomp <- subset(W, !(t_ind >= t_ind_begin & t_ind <= t_ind_end & W_ID %in% PM_L1_ID))
  lapply(PM_L1_ID, FUN = function(x) {
    d1 <- subset(Wsub, W_ID == x)
    for (ti in t_ind_begin:t_ind_end) {
      d1 <- f_moveWorkers(Plant, d1, selectW = x, to = sample(L1_location,1), t_ind = ti)
    }
    return(d1)
  }) %>% rbindlist() %>% rbind(Wcomp) -> W
  
  # logistic2 workers - afternoon - random moves in their workspace
  Wsub <- subset(W, t_ind >= t_ind_begin & t_ind <= t_ind_end & W_ID %in% PM_L2_ID)
  Wcomp <- subset(W, !(t_ind >= t_ind_begin & t_ind <= t_ind_end & W_ID %in% PM_L2_ID))
  lapply(PM_L2_ID, FUN = function(x) {
    d1 <- subset(Wsub, W_ID == x)
    for (ti in t_ind_begin:t_ind_end) {
      d1 <- f_moveWorkers(Plant, d1, selectW = x, to = sample(L2_location,1), t_ind = ti)
    }
    return(d1)
  }) %>% rbindlist() %>% rbind(Wcomp) -> W
  
  # random moves of all workers of the afternoon to W.C. (2 times between 14h05 and 21h55)
  Wsub <- subset(W, W_ID %in% PM_ID & t_ind >= t_ind_begin & t_ind <= t_ind_end)
  Wcomp <- subset(W, !(W_ID %in% PM_ID & t_ind >= t_ind_begin & t_ind <= t_ind_end))
  lapply(PM_ID, FUN = function(x) {
    d1 <- subset(Wsub, W_ID == x)
    for (ti in sample(t_ind_begin:t_ind_end, 2)) { # twice
      d1 <- f_moveWorkers(Plant, d1, selectW = x, to = "W.C.", t_ind = ti)
    }
    return(d1)
  }) %>% rbindlist() %>% rbind(Wcomp) -> W
  
  ####
  ## nothing between 18:00 to 18:45 AM
  ####
  
  ## 13:00:
  W <- f_moveWorkers(Plant, W, selectW=PM_ID, to="Entry hall", t_ind=f_convertTime("time2ind",D,22,0,dt=dt))

  return(W)
}




##### f_dailyWork_T2 #####
f_dailyWork_T2 <- function(
  Plant,
  W,
  D,
  dt,
  seed = NULL
) {
  active <- subset(W, W_active == "active" & Day == D)
  
  T2_ID <- subset(active, W_type == "transverse2")$W_ID %>% unique()
  
  if (length(T2_ID) >= 1) { # if there is at least one transverse2 worker
    
    T2_location <- unique(Plant$L$location)
    T2_location <- T2_location[!T2_location %in% c("Conveyor1", "Conveyor2", "Equipment 1")]
    
    ## 22h
    W <- f_moveWorkers(Plant, W, selectW=T2_ID, to="Entry hall", t_ind=f_convertTime("time2ind",D,22,0,dt=dt))
    
    # random moves between different locations 22h05-23h55
    t_ind_begin <- f_convertTime("time2ind",D,22,5,dt=dt)
    t_ind_end <- f_convertTime("time2ind",D,23,50,dt=dt)
    Wsub <- subset(W, W_ID %in% T2_ID & t_ind >= t_ind_begin & t_ind <= t_ind_end)
    Wcomp <- subset(W, !(W_ID %in% T2_ID & t_ind >= t_ind_begin & t_ind <= t_ind_end))
    
    lapply(T2_ID, FUN = function(x) {
      d1 <- subset(Wsub, W_ID == x)
      for (ti in t_ind_begin:t_ind_end) {
        d1 <- f_moveWorkers(Plant, d1, selectW = x, to = sample(T2_location,1), t_ind = ti)
      }
      return(d1)
    }) %>% rbindlist() %>% rbind(Wcomp) -> W
    
    W <- f_moveWorkers(Plant, W, selectW = T2_ID, to="Entry hall", t_ind = f_convertTime("time2ind",D,23,55,dt=dt))
  }
  else {
    writeLines(paste("/!\\ warnings: day ", D, ": missing transverse2 worker during the night shift", sep =""))
  }
  
  return(W)
}



##### f_dailyWork_AllTeams #####
f_dailyWork_AllTeams <- function(
  Plant,
  W,
  D,
  dt,
  seed = NULL
) {
  # if (!is.null(seed)) {set.seed(seed)}
  writeLines(paste(">>> Daily work: Assign positions for all active workers from different teams (day ", D,")", sep=""))
  
  W <- f_dailyWork_AM(Plant, W, D, dt)
  W <- f_dailyWork_T1(Plant, W, D, dt)
  W <- f_dailyWork_PM(Plant, W, D, dt)
  W <- f_dailyWork_T2(Plant, W, D, dt)
  
  W <- arrange(W, t_ind, W_ID)
  return(W)
}

##### f_checkdailyWorkerType #####
f_checkdailyWorkerType <- function(
  W,
  D
) {
  
  Wsub <- subset(W, Day == D & Hour == 0 & Min == 0 & W_active == "active")
  compo <- table(Wsub$W_team, Wsub$W_type)[c("teamA", "teamB") , c("cutter1", "cutter2", "logistic1", "logistic2")]
  
  critical <- c("teamA" = FALSE,
                "teamB" = FALSE)
  
  for (team in c("teamA", "teamB")) { ## for each team (A or B / morning or afternoon depending on the week)
    
    # Critical shift if insufficient number of workers !
    if (sum(compo[team , c("logistic1", "logistic2", "cutter1", "cutter2")]) < 4) {
      critical[[team]] <- TRUE
      writeLines("Critical shift: insufficient number of workers (less than 4 workers) for the shift !")
    } 
    else { # if it is not a critical shift 
      ## if there is not a sufficient number of cutters for the shift (at least 2)
      if (sum(compo[team , c("cutter1", "cutter2")]) < 2) {
        critical[[team]] <- TRUE
        writeLines("Critical shift: insufficient number of cutters (less than 2 cutters) for the shift !")
      }
      else {
        ## if there is any missing cutter1 worker
        if (compo[team , "cutter1"] == 0 ) {
          ## take 1 random cutter2 worker to become cutter1 worker
          changingWorker_ID <- subset(Wsub, W_team == team & W_type == "cutter2")$W_ID %>% sample(., 1) 
          W$W_type[W$Day == D & W$W_ID == changingWorker_ID] <- "cutter1"
          writeLines(paste("/!\\ Missing cutter1 workers - ", team, " - Transfer : worker ", changingWorker_ID," (cutter2 -> cutter1)", sep = ""))
        }
        
        ## if there is any missing cutter2 worker
        if (compo[team , "cutter2"] == 0 ) {
          ## take 1 random cutter1 worker to become cutter2 worker
          changingWorker_ID <- subset(Wsub, W_team == team & W_type == "cutter1")$W_ID %>% sample(., 1) ## take 1 random logistic2 worker to become logistic1 worker
          W$W_type[W$Day == D & W$W_ID == changingWorker_ID] <- "cutter2"
          writeLines(paste("/!\\ Missing cutter2 workers - ", team, " - Transfer : worker ", changingWorker_ID," (cutter1 -> cutter2)", sep = ""))
        }
        
        ## if there is any missing logistic1 or logistic2 worker
        if (compo[team , "logistic1"] == 0 | compo[team, "logistic2"] == 0) { 
          
          ## if there are at least 2 logistic workers for switching between them
          if (sum(compo[team , c("logistic1", "logistic2")]) >= 2) { 
            if (compo[team , "logistic1"] == 0) { ## if the missing worker is logistic1
              changingWorker_ID <- subset(Wsub, W_team == team & W_type == "logistic2")$W_ID %>% sample(., 1) ## take 1 random logistic2 worker to become logistic1 worker
              W$W_type[W$Day == D & W$W_ID == changingWorker_ID] <- "logistic1"
              writeLines(paste("/!\\ Missing logistic1 workers - ", team, " - Transfer : worker ", changingWorker_ID," (logistic2 -> logistic1)", sep = ""))
            }
            else { ## otherwise, if the missing worker is logistic2
              changingWorker_ID <- subset(Wsub, W_team == team & W_type == "logistic1")$W_ID %>% sample(., 1) ## take 1 random logistic1 worker to become logistic2 worker
              W$W_type[W$Day == D & W$W_ID == changingWorker_ID] <- "logistic2"
              writeLines(paste("/!\\ Missing logistic2 workers - ", team, " - Transfer : worker ", changingWorker_ID," (logistic1 -> logistic2)", sep = ""))
            }
          }
          
          ## if there are less than 2 logistic workers for switching between them: take from the cutters group
          if (sum(compo[team , c("logistic1", "logistic2")]) < 2) {
            
            ## if the missing worker is logistic1
            if (compo[team , "logistic1"] == 0) { 
              ## take a random cutter worker to become logistic1
              changingWorker_ID <- subset(Wsub, W_team == team & W_type %in% c("cutter1", "cutter2"))$W_ID %>% sample(., 1)
              W$W_type[W$Day == D & W$W_ID == changingWorker_ID] <- "logistic1" ## the taken cutter worker becomes logistic1
              writeLines(paste("/!\\ Missing logistic1 workers - ", team, " - Transfer : worker ", changingWorker_ID," (cutter1/cutter2 -> logistic1)", sep = ""))
            }
            
            ## if the missing worker is logistic2
            if (compo[team , "logistic2"] == 0) {
              ## take a random cutter worker to become logistic2
              changingWorker_ID <- subset(Wsub, W_team == team & W_type %in% c("cutter1", "cutter2"))$W_ID %>% sample(., 1)
              W$W_type[W$Day == D & W$W_ID == changingWorker_ID] <- "logistic2" ## the taken cutter worker becomes logistic2
              writeLines(paste("/!\\ Missing logistic1 workers - ", team, " - Transfer : worker ", changingWorker_ID," (cutter1/cutter2 -> logistic2)", sep = ""))
            }
          }
          
        } ## end (missing logistic1 or logistic2 workers in the current team)
      }

    } # end critical shift 
    
  } ## end (each team/shift)
  
  critical <- all(critical)
  
  return(list(W = W,
              critical = critical))
  
}



##### f_dailyMaskWearing_AllTeams #####
f_dailyMaskWearing <- function(
  WD, ## data frame associated with ONE GIVEN DAY
  probMask
) {
  D <- unique(WD$Day)
  
  active <- subset(WD, W_active == "active")
  Wcomp <- subset(WD, W_active != "active")
  
  active_ID <- unique(active$W_ID)
  
  W_mask_by_ID <- sample(c("mask", "no mask"), replace = T,
                         size = length(active_ID),
                         prob = c(probMask, 1-probMask))
  
  names(W_mask_by_ID) <- active_ID
  
  active$W_mask[which(active$W_ID==names(W_mask_by_ID))] <- unname(W_mask_by_ID)
  
  WD <- rbind(active, Wcomp)
  
  return(WD)
}

