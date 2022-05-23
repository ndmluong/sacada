##### f_moveWorkers() FUNCTION TO MOVE THE WORKERS INSIDE THE PLANT #####
f_moveWorkers <- function(
  ## Move different types of agents from one location to another inside the pre-created plant
  ## INPUT
  Plant, # 'Plant object': list of two objects
  # L (data.frame): different locations of the plant ('entry hall', 'wc'...) and their coordinates X and Y
  # P (numeric matrix) : food processing plant 
  W, # (dataframe) The agents (workers/food portions) set: with at least these columns $
  # - W_ID  (character) ID of the agents
  # - coordX : (numeric) coordinates (X) in the plant
  # - coordY: (numeric) coordinates (Y) in the plant
  # - location: (optional) (character) current location of the agents
  selectW, ## (optional, character, vector): agents selection by ID,
  to, ## (character/string) location (e.g. "Entry hall", "WC",...)
  t_ind, ## time index
  ...
  ## OUTPUT
) {
  #### BEGIN OF FUNCTION
  loc <- subset(Plant$L, location == to) ## all coordinates associated with the new location (destination)
  
  ## change location of the selected workers for the time index t
  W$location[which((W$t_ind == t_ind) & (W$W_ID %in% selectW))] <- to

  ############ /!\ Optimized code (begin)
  Wsub <- subset(W, W_ID %in% selectW)
  Wcomp <- subset(W, !(W_ID %in% selectW))
  
  lapply(selectW, FUN = function(x) {
    d1 <- subset(Wsub, W_ID == x)
    randomcoord <- loc[sample(1:nrow(loc), size=1), ]
    d1$coordX[which(d1$t_ind == t_ind)] <- randomcoord$coordX
    d1$coordY[which(d1$t_ind == t_ind)] <- randomcoord$coordY
    return(d1)
  }) %>%
    data.table::rbindlist() %>%
    rbind(., Wcomp) -> W
  
  ############ /!\ Optimized code (end)
  
  return(W)
  #### END OF FUNCTION
}


##### f_assignCuttersPosition() FUNCTION TO ASSIGN POSITIONS FOR CUTTERS AT A GIVEN TIME #####
f_assignCuttersPosition <- function(
  Plant, ## (data frame) All locations in the plant, including the cutting work spaces
  W, ## the workers
  t_ind, ## time index
  shift,
  cuttertype=1,
  ...
) {
  ## the active cutter a the given time
  query <-
    W$t_ind == t_ind &
    W$W_active == "active" &
    W$W_shift == shift &
    W$W_type == paste("cutter", cuttertype, sep="")
  
  W_query <- subset(W, query)
  N <- nrow(W_query)
  
  ## total number of available cutting work spaces
  wsname <- paste("WS-Cutting", cuttertype, sep ="")
  WS_list <- subset(Plant$L, str_detect(location, wsname))
  N_WS <- nrow(WS_list)
  
  ## mean distance between two adjacent cutters
  mean_dist <- N_WS / N
  pos_index <- integer(N) ## the position index
  
  if (mean_dist %% 1 == 0) {
    dist_vec <- rep(mean_dist, N-1)
    ## random position of the first cutter
    ## between 1 and N_inf to avoid that the first cutter always is at the very beginning of the conveyor
    pos_index[1] <- sample(1:N, 1)
  } else {
    ## distances as an integer (between dist_inf and dist_sup)
    dist_inf <- floor(mean_dist)
    dist_sup <- ceiling(mean_dist)
    
    ## the number of workers to be assigned following the distances
    N_assigned <- solve(a = matrix(c(dist_inf, dist_sup,
                                     1,1),
                                   nrow = 2, byrow= T),
                        b = matrix(c(N_WS,
                                     N),
                                   nrow= 2, byrow = T))
    
    N_inf <- N_assigned[1]
    N_sup <- N_assigned[2]
    
    dist_vec <- c(rep(dist_inf, N_inf-1),
                  rep(dist_sup, N_sup))

    dist_vec <- sample(dist_vec) ## randomize the order of the distances
    ## random position of the first cutter
    ## between 1 and N_inf to avoid that the first cutter always is at the very beginning of the conveyor
    pos_index[1] <- sample(1:N_inf, 1)
  }

  for (wi in 2:N) {
    pos_index[wi] <- pos_index[wi-1] + dist_vec[wi-1] ## apply the distances between cutters
  }

  ## correction if the assigned position of the last worker exceeds the highest position index
  pos_index[pos_index > N_WS] <- pos_index[pos_index > N_WS] - N_WS

  ## randomize the position following the cutters
  pos_index <- sample(pos_index)

  ## the assigned work spaces for the selected workers
  assigned_WS <- WS_list$location[pos_index]

  W_output <- W

  # move the selected workers to their respective assigned work spaces
  for (wi in 1:N) {
    W_output <- f_moveWorkers(Plant = Plant,
                              W = W_output,
                              selectW = W_query$W_ID[wi],
                              to = assigned_WS[wi],
                              t_ind = t_ind)
  }
  
  return(W_output)
}


##### f_moveFood () FUNCTION TO MOVE SOME FOOD PORTIONS INSIDE THE PLANT #####
f_moveFoodPortion <- function(
  ## Move different types of agents from one location to another inside the pre-created plant
  ## INPUT
  Plant, # 'Plant object': list of two objects
  # L (data.frame): different locations of the plant ('entry hall', 'wc'...) and their coordinates X and Y
  # P (numeric matrix) : food processing plant 
  FP, # (dataframe) The agents (workers/food portions) set: with at least these columns $
  # - FP_ID:  (character) ID of the agents
  # - coordX: (numeric) X coordinates of the food portions
  # - coordY: (numeric) Y coordinates of the food portions
  # - location: (character) current location of the food portions
  selectFP, ## (character, vector): IDs of the selected food portions to move
  to, ## (character/string) location (e.g. "Entry hall", "WC",...)
  t_ind_vec, ## time index
  ...
  ## OUTPUT
) {
  #### BEGIN OF FUNCTION
  loc <- subset(Plant$L, location == to) ## all coordinates associated with the new location (destination)
  
  ## change location of the selected workers for the time index t
  FP$location[which((FP$t_ind %in% t_ind_vec) & (FP$FP_ID %in% selectFP))] <- to
  
  ############ /!\ Optimized code (begin)
  FPsub <- subset(FP, FP_ID %in% selectFP)
  FPcomp <- subset(FP, !(FP_ID %in% selectFP))
  
  lapply(selectFP, FUN = function(x) { ## for each given portion
    d1 <- subset(FPsub, FP_ID == x)
    randomcoord <- loc[sample(1:nrow(loc), size=1), ] 
    d1$coordX[which(d1$t_ind %in% t_ind_vec)] <- randomcoord$coordX
    d1$coordY[which(d1$t_ind %in% t_ind_vec)] <- randomcoord$coordY
    return(d1)
  }) %>%
    data.table::rbindlist(.) %>%
    rbind(., FPcomp) -> FP
  
  ############ /!\ Optimized code (end)
  
  return(FP)
  #### END OF FUNCTION
}


##### f_circulateCarcass FUNCTION TO CIRCULATE ONE GIVEN CARCASS INSIDE THE PLANT #####
f_circulateCarcass <- function(
  Plant, # 'Plant object': list of two objects
  # L (data.frame): different locations of the plant ('entry hall', 'wc'...) and their coordinates X and Y
  # P (numeric matrix) : food processing plant
  FPcarc, ## object containing all attributes of all agents food portions of ONE GIVEN CARCASS
  carcass_ID, ## (numeric, integer): the number (ID) of the carcass to be processed
  t_ind, ## the time index corresponding to the beginning of the cutting process (arrival gate)
  # duration_ti,  # GLOBAL VARIABLE
  prm_food,
  prm_time
) {

  ## IDs of the portion from the selected carcass to circulate
  selectFP <- unique(FPcarc$FP_ID)
  
  ### TIME INDEX FOR EACH STEP OF THE CUTTING PROCESS
  ti <- c("logistic1" = t_ind,
          "cutter1" = t_ind + duration_ti[["logistic1"]],
          "cutter2" = t_ind + sum(duration_ti[c("logistic1", "cutter1")]),
          "logistic2" = t_ind + sum(duration_ti[c("logistic1", "cutter1", "cutter2")]),
          "storage" = t_ind + sum(duration_ti[c("logistic1", "cutter1", "cutter2", "logistic2")]))

  ### CUTTING PROCESS
  ## logistic 1
  FPcarc <- f_moveFoodPortion(Plant = Plant, FP = FPcarc, selectFP = selectFP, to = "Arrival gate",
                   t_ind_vec = ti[["logistic1"]]:(ti[["cutter1"]]-1))
  ## cut 1:
  FPcarc <- f_moveFoodPortion(Plant = Plant, FP = FPcarc, selectFP = selectFP, to = "Conveyor1",
                   t_ind_vec = ti[["cutter1"]]:(ti[["cutter2"]]-1))
  ## cut 2:
  FPcarc <- f_moveFoodPortion(Plant = Plant, FP = FPcarc, selectFP = selectFP, to = "Conveyor2",
                   t_ind_vec = ti[["cutter2"]]:(ti[["logistic2"]]-1))
  ## logistic2:
  FPcarc <- f_moveFoodPortion(Plant = Plant, FP = FPcarc, selectFP = selectFP, to = "Equipment 1",
                   t_ind_vec = ti[["logistic2"]]:(ti[["storage"]]-1))
  ## storage:
  FPcarc <- f_moveFoodPortion(Plant = Plant, FP = FPcarc, selectFP = selectFP, to = "Cooling area",
                   t_ind_vec = ti[["storage"]])

  return(FPcarc)
}









