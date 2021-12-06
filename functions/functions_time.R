##### f_convertTime() FUNCTION TO CONVERT BETWEEN THE TIME TO TIME INDEX #####
f_convertTime <- function(
  ## Convert between the time to the time index
  ## INPUT,
  method, ## string: 'time2ind' or 'ind2time'
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
