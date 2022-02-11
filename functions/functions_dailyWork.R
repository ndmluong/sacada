##### f_dailyWork_AM #####
f_dailyWork_AM <- function(
  Plant,
  W,
  D,
  dt,
  seed = NULL
) {
  writeLines(paste(">>> Day ", D, " - Morning team", sep=""))
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
  
  W <- f_replicateWorkerstime2time(W, AM_C1_ID, "W_location", c(5,5), c(12,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, AM_C1_ID, "W_coordX", c(5,5), c(12,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, AM_C1_ID, "W_coordY", c(5,5), c(12,55), D=D, dt=dt)
  
  W <- f_replicateWorkerstime2time(W, AM_C2_ID, "W_location", c(5,5), c(12,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, AM_C2_ID, "W_coordX", c(5,5), c(12,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, AM_C2_ID, "W_coordY", c(5,5), c(12,55), D=D, dt=dt)
  
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
  writeLines(paste(">>> Day ", D, " - Transverse1 workers", sep=""))
  
  active <- subset(W, W_active == "active" & Day == D)
  
  T1_ID <- subset(active, W_type == "transverse1")$W_ID %>% unique()
  T1_location <- unique(Plant$L$Location)
  T1_location <- T1_location[!T1_location %in% c("Entry hall", "W.C.", "Conveyor1", "Conveyor2", "Equipment 1", "Office")]
  
  # 9h
  W <- f_moveWorkers(Plant, W, selectW=T1_ID, to="Entry hall", t_ind=f_convertTime("time2ind",D,9,0,dt=dt))
  
  # 9h05 - 11h55
  W <- f_moveWorkers(Plant, W, selectW = T1_ID, to = "Office", t_ind = f_convertTime("time2ind",dt=dt,D,9,5))
  
  W <- f_replicateWorkerstime2time(W, selectW = T1_ID, Invariant = "W_location",
                                   time_begin = c(9,5), time_end=c(11,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, selectW = T1_ID, Invariant = "W_coordX",
                                   time_begin = c(9,5), time_end=c(11,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, selectW = T1_ID, Invariant = "W_coordY",
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
  
  W <- f_replicateWorkerstime2time(W, selectW = T1_ID, Invariant = "W_location",
                                   time_begin = c(12,50), time_end=c(16,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, selectW = T1_ID, Invariant = "W_coordX",
                                   time_begin = c(12,50), time_end=c(16,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, selectW = T1_ID, Invariant = "W_coordY",
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
  writeLines(paste(">>> Day ", D, " - Afternoon team", sep=""))
  active <- subset(W, W_active == "active" & Day == D)
  
  PM_ID <- subset(active, W_shift == "afternoon")$W_ID %>% unique()
  PM_C1_ID <- subset(active, W_shift == "afternoon" & W_type == "cutter1")$W_ID %>% unique()
  PM_C2_ID <- subset(active, W_shift == "afternoon" & W_type == "cutter2")$W_ID %>% unique()
  PM_L1_ID <- subset(active, W_shift == "afternoon" & W_type == "logistic1")$W_ID %>% unique()
  PM_L2_ID <- subset(active, W_shift == "afternoon" & W_type == "logistic2")$W_ID %>% unique()
  
  L1_location <- c("Arrival gate", "WS-Conveyor1-Head")
  L2_location <- c("WS-Conveyor2-Tail", "Waste area", "WS-Equipment 1", "Cooling area")
  
  ## 14:00
  W <- f_moveWorkers(Plant, W, PM_ID, "Entry hall", t_ind=f_convertTime("time2ind",D,14,0,dt=dt))
  
  ## 14:05
  t_ind <- f_convertTime("time2ind",D,14,5,dt=dt)
  W <- f_assignCuttersPosition(Plant, W, t_ind=t_ind, cuttertype = 1, shift = "afternoon")
  W <- f_assignCuttersPosition(Plant, W, t_ind=t_ind, cuttertype = 2, shift = "afternoon")
  W <- f_moveWorkers(Plant, W, PM_L1_ID, to=sample(L1_location,1), t_ind=t_ind)
  W <- f_moveWorkers(Plant, W, PM_L2_ID, to=sample(L2_location,1), t_ind=t_ind)
  
  ## from 14:05 to 21:55
  t_ind_begin <- f_convertTime("time2ind",D,14,5,dt=dt)
  t_ind_end <- f_convertTime("time2ind",D,21,55,dt=dt)
  
  W <- f_replicateWorkerstime2time(W, PM_C1_ID, "W_location", c(14,5), c(21,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, PM_C1_ID, "W_coordX", c(14,5), c(21,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, PM_C1_ID, "W_coordY", c(14,5), c(21,55), D=D, dt=dt)
  
  W <- f_replicateWorkerstime2time(W, PM_C2_ID, "W_location", c(14,5), c(21,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, PM_C2_ID, "W_coordX", c(14,5), c(21,55), D=D, dt=dt)
  W <- f_replicateWorkerstime2time(W, PM_C2_ID, "W_coordY", c(14,5), c(21,55), D=D, dt=dt)
  
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
  writeLines(paste(">>> Day ", D, " - Transverse2 workers", sep=""))
  active <- subset(W, W_active == "active" & Day == D)
  
  T2_ID <- subset(active, W_type == "transverse2")$W_ID %>% unique()
  T2_location <- unique(Plant$L$Location)
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
  if (!is.null(seed)) {set.seed(seed)}
  writeLines(paste("========== Day ", D, " =========", sep=""))
  W <- f_dailyWork_AM(Plant, W, D, dt)
  W <- f_dailyWork_T1(Plant, W, D, dt)
  W <- f_dailyWork_PM(Plant, W, D, dt)
  W <- f_dailyWork_T2(Plant, W, D, dt)
  
  W <- arrange(W, t_ind, W_ID)
  return(W)
}

##### f_dailyMaskWearing_AllTeams #####
f_dailyMaskWearing <- function(
  WD, ## data frame associated with ONE GIVEN DAY
  probMask
) {
  D <- unique(WD$Day)
  
  # writeLines(paste("## Day ", D, " - Check 'Mask wearing' status of all active workers", sep=""))
  
  active <- subset(WD, W_active == "active")
  Wcomp <- subset(WD, W_active != "active")
  
  active_ID <- active$W_ID %>% unique()
  
  W_mask_by_ID <- sample(c("mask", "no mask"), replace = T,
                         size = length(active_ID),
                         prob = c(probMask, 1-probMask))
  
  names(W_mask_by_ID) <- active_ID
  
  active$W_mask[which(active$W_ID==names(W_mask_by_ID))] <- unname(W_mask_by_ID)
  
  WD <- rbind(active, Wcomp)
  
  return(WD)
}

