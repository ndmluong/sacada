f_initSurfaces <- function(
  #### INPUT
  P, ## (matrix) the matrix corresponding to the details of each plant tile (output of the function f_createPlant())
  prm_time, ## (list) the parameters associated with the timetable
  ## (check the script "parameters_time.R")
  ##  - NDays, ## total number of days during the entire process
  ##  - Step ## time step (in minutes)
  ...
  #### OUTPUT
  ## S (dataframe): the initialised workers
  ##  - $S_ID (character/string): the ID of each surface unit ("W001","W002",...)
  ##  - $S_coordX (integer): the X coordinate of the surface agent
  ##  - $S_coordY (integer): the Y coordinate of the surface agent
  ##  - $S_location (character): the location of the surface agent
  ##  - $S_Nv (numeric): viral quantity (log CFU)
) {
  #### BEGIN OF FUNCTION
  ## total number of surface Agents
  NS <- nrow(P) * ncol(P)
  
  ### Total number of the time indexes 
  NTime <- prm_time$NDays * 1440 / prm_time$Step ## amplitude Ndays in days, time step in minutes
  t_ind <- rep(0:NTime, each = NS)
  
  ### The coordinates of each surface agent
  tmp <- expand.grid(S_coordX = 1:nrow(P),
                     S_coordY = 1:ncol(P))
  S_coordX <- rep(tmp$S_coordX, NTime+1)
  S_coordY <- rep(tmp$S_coordY, NTime+1)
  
  ### The location of each surface agent
  S_location <- rep(NA, NS*(NTime+1))
  for (i in 1:length(S_coordX)) {
    S_location[i] <- P[S_coordX[i], S_coordY[i]]
  }
  
  ### ID of the surfaces
  S_ID <- rep(NA, ncol(P)*nrow(P)*(NTime+1))
  for (i in 1:length(S_coordX)) {
    S_ID[i] <- paste("S",
                     "_X", stringr::str_pad(S_coordX[i], width=3, pad="0"),
                     "_Y", stringr::str_pad(S_coordY[i], width=3, pad="0"),
                     sep="")
  }
  
  ### Initialization for the viral quantity as NA at each surface unit and each time index
  S_Nv <- rep(NA, NS*(NTime+1))
  
  ### Output
  S <- data.frame(S_ID = S_ID,
                  S_coordX = S_coordX,
                  S_coordY = S_coordY,
                  S_location = S_location,
                  S_Nv = S_Nv,
                  t_ind = t_ind
  )
  
  return(S)
  #### END OF FUNCTION
}
