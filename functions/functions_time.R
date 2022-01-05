##### f_convertTime() FUNCTION TO CONVERT BETWEEN THE TIME TO TIME INDEX #####
f_convertTime <- function(
  ## Convert between the time to the time index
  ## INPUT,
  method = "time2ind", ## string: 'time2ind' or 'ind2time'
  dt,
  D = NULL, # (integer): the considered day
  H = NULL, # (integer): the considered time of the day
  M = NULL, # (integer): the considered minute of the hour
  t_ind = NULL,
  ...
  ## OUTPUT
) {
  if (method == 'time2ind') {
    #stopifnot(  !is.null(D) |  !is.null(M) | !is.null(H)  )
    ind <- round((D-1)*1440/dt + H*60/dt + M/dt)
    return(ind)
  }
  
  if (method == 'ind2time') {
    D <- (t_ind *dt) %/% 1440 + 1
    H <- ((t_ind * dt) %% 1440) %/% 60
    M <- ((t_ind * dt) %% 1440) %% 60
    return(c(D,H,M))
  }
  
}

##### f_Day2Week() FUNCTION TO CONVERT A DAY NUMBER INTO A WEEK NUMBER #####
f_Day2Week <- function(D) {
  if (D%%7 == 0) {return(D%/%7)}
  else {return(D%/%7 + 1)}
}

##### f_Day2Weekday() FUNCTION TO CONVERT A DAY NUMBER INTO A WEEKDAY #####
f_Day2Weekday <- function(D) {
  switch(as.character(D%%7),
         '0' = {return("Sunday")},
         '1' = {return("Monday")},
         '2' = {return("Tuesday")},
         '3' = {return("Wednesday")},
         '4' = {return("Thursday")},
         '5' = {return("Friday")},
         '6' = {return("Saturday")}
  )
}

##### f_replicateIndividualDaily() FUNCTION TO REPLICATE INDIVIDUAL ATTRIBUTES #####
f_replicateIndividualDaily <- function(
  Agent, ## data.frame corresponding to ONE AGENT at ALL DAY
  Invariant, ## the invariant variable(s) 
  ...
) {
  by(Agent, ## for the considered agent
     INDICES = Agent$Day, ## processing by day
     FUN = function(x) {
       if (nrow(x) > 1) { ## if the considered day has more than one time index
         x <- arrange(x, t_ind)
         x[[Invariant]][2:nrow(x)] <- x[[Invariant]][1]
       }
       return(x)
     }) %>%
    data.table::rbindlist() %>%
    dplyr::arrange(t_ind, W_ID) -> Agent
  
  return(Agent)
}


##### f_replicatetime2time() FUNCTION TO REPLICATE INDIVIDUAL ATTRIBUTES #####
f_replicateIndividualtime2time <- function(
  Agent, ## data.frame corresponding to ONE AGENT at ONE GIVEN DAY
  Invariant, ## the invariant variable(s)
  time_begin, ## vector of two element c(H,M)
  time_end, ## vector of two element c(H,M)
  dt,
  ...
) {
  D = as.numeric(unique(Agent$Day))
  
  t_ind_begin <- f_convertTime(method = "time2ind", dt=dt, D=D, H=time_begin[1], M=time_begin[2])
  t_ind_end <- f_convertTime(method = "time2ind", dt=dt, D=D, H=time_end[1], M=time_end[2])
  
  Agent[[Invariant]][which(between(Agent$t_ind,(t_ind_begin+1),t_ind_end))] <- Agent[[Invariant]][which(Agent$t_ind == t_ind_begin)]

  return(Agent)
}

f_replicateWorkerstime2time <- function(
  W,
  selectW,
  Invariant,
  time_begin,
  time_end,
  D,
  dt,
  ...
) {
  WD <- subset(W, Day == D & W_ID %in% selectW)
  Wcomp <- subset(W, !(Day == D & W_ID %in% selectW))
  
  lapply(selectW, FUN = function(x) {
    d1 <- subset(WD, W_ID == x)
    d1 <- f_replicateIndividualtime2time(Agent = d1,
                                         Invariant = Invariant,
                                         time_begin = time_begin,
                                         time_end = time_end,
                                         dt = dt)
    return(d1)
  }) %>%
    rbindlist() -> WD_output
  
  rbind(WD_output, Wcomp) -> W_output
  
  return(W_output)
}

