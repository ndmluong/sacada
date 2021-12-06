##### f_moveWorkers() FUNCTION TO MOVE THE WORKERS INSIDE THE PLANT #####
f_moveWorkers <- function(
  ## Move different types of agents from one location to another inside the pre-created plant
  ## INPUT
  Plant, # 'Plant object': list of two objects
  # L (data.frame): different locations of the plant ('entry hall', 'wc'...) and their coordinates X and Y
  # P (numeric matrix) : food processing plant 
  W, # (dataframe) The agents (workers/food portions) set: with at least these columns $
  # - W_ID  (character) ID of the agents
  # - W_coordX : (numeric) coordinates (X) in the plant
  # - W_coordY: (numeric) coordinates (Y) in the plant
  # - W_location: (optional) (character) current location of the agents
  selectW, ## (optional, character, vector): agents selection by ID,
  to, ## (character/string) location (e.g. "Entry hall", "WC",...)
  t_ind, ## time index
  ...
  ## OUTPUT
) {
  #### BEGIN OF FUNCTION
  P <- Plant$P ## the plant
  
  loc <- subset(Plant$L, Location == to) ## all coordinates associated with the new location (destination)
  
  ## if the agents to move are provided by their ID (vector W_ID)
  ## the number of workers to move
  N_move <- length(selectW)
  ## change location of the selected workers for the time index t
  to -> W[(W$t_ind == t_ind) & (W$W_ID %in% selectW), ]$W_location
  
  for (wi in 1:N_move) { ## for each worker to move
    randomcoord <- loc[sample(1:nrow(loc), size=1), ]
    randomcoord$coordX -> W[(W$t == t_ind) & (W$W_ID == selectW[wi]), ]$W_coordX
    randomcoord$coordY -> W[(W$t == t_ind) & (W$W_ID == selectW[wi]), ]$W_coordY
  }
  return(W)
  #### END OF FUNCTION
}




##### f_assignCuttersPosition() FUNCTION TO ASSIGN POSITIONS FOR CUTTERS AT A GIVEN TIME #####
f_assignCuttersPosition <- function(
  Plant, ## (data frame) All locations in the plant, including the cutting work spaces
  W, ## the workers
  t_ind, ## time index
  ...
) {
  ## the active cutter a the given time
  query <-
    W$t_ind == t_ind &
    W$W_active == "active" &
    W$W_type == "cutter"
  
  W_query <- subset(W, query)
  N <- nrow(W_query)
  
  ## total number of available cutting work spaces
  WS_list <- subset(Plant$L, str_detect(Location, "WS-Cutting"))
  N_WS <- nrow(WS_list)
  
  ## mean distance between two adjacent cutters
  mean_dist <- N_WS / N
  
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

  pos_index <- integer(N) ## the position index
  
  ## random position of the first cutter
  ## between 1 and N_inf to avoid that the first cutter always is at the very beginning of the conveyor
  pos_index[1] <- sample(1:N_inf, 1) 
  
  for (wi in 2:N) {
    pos_index[wi] <- pos_index[wi-1] + dist_vec[wi-1] ## apply the distances between cutters
  }
  
  ## correction if the assigned position of the last worker exceeds the highest position index
  pos_index[pos_index > N_WS] <- pos_index[pos_index > N_WS] - N_WS
  
  ## randomize the position following the cutters
  pos_index <- sample(pos_index) 

  ## the assigned work spaces for the selected workers
  assigned_WS <- WS_list$Location[pos_index]
  
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


##### f_InvariantTimeSteps() #####
f_InvariantTimeSteps <- function(
  Agents, ## the considered agents (workers, food portions or surfaces)
  selectAgents = NULL,
  Agents_type = "W", ## W, FP, ...
  Invariant, ## the invariant variable
  t_ind_from, ## time index (begin)
  t_ind_to, ## time index (end)
  ...
) {
  
  ID <- paste(Agents_type, "_ID", sep="")
  
  if (is.null(selectAgents)) {
    selectAgents <- unique(Agents[,ID])
  }
  
  for (i in 1:length(Invariant)) {
    for (t_j in (t_ind_from+1):t_ind_to) {
      for (ag in (1:length(selectAgents))) {
        query_from <- Agents[,ID] == selectAgents[ag] & Agents$t_ind == t_ind_from
        query_to <- Agents[,ID] == selectAgents[ag] & Agents$t_ind == t_j
        Agents[query_from, Invariant[i]] -> Agents[query_to, Invariant[i]]
      }
    }
  }
  
  return(Agents)
}



##### f_moveFood() FUNCTION TO MOVE THE WORKERS INSIDE THE PLANT #####
f_moveFood <- function(
  ## Move different types of agents from one location to another inside the pre-created plant
  ## INPUT
  Plant, # 'Plant object': list of two objects
  # L (data.frame): different locations of the plant ('entry hall', 'wc'...) and their coordinates X and Y
  # P (numeric matrix) : food processing plant 
  FP, # (dataframe) The agents (workers/food portions) set: with at least these columns $
  # - FP_ID  (character) ID of the agents
  # - FP_coordX : (numeric) coordinates (X) in the plant
  # - FP_coordY: (numeric) coordinates (Y) in the plant
  # - FP_location: (optional) (character) current location of the agents
  selectFP, ## (optional, character, vector): agents selection by ID,
  to, ## (character/string) location (e.g. "Entry hall", "WC",...)
  t_ind, ## time index
  ...
  ## OUTPUT
) {
  #### BEGIN OF FUNCTION
  P <- Plant$P ## the plant
  
  loc <- subset(Plant$L, Location == to) ## all coordinates associated with the new location (destination)
  
  ## if the agents to move are provided by their ID (vector W_ID)
  ## the number of workers to move
  N_move <- length(selectFP)
  ## change location of the selected workers for the time index t
  to -> FP[(FP$t_ind == t_ind) & (FP$FP_ID %in% selectFP), ]$FP_location
  
  for (fpi in 1:N_move) { ## for each worker to move
    randomcoord <- loc[sample(1:nrow(loc), size=1), ]
    randomcoord$coordX -> FP[(FP$t_ind == t_ind) & (FP$FP_ID == selectFP[fpi]), ]$FP_coordX
    randomcoord$coordY -> FP[(FP$t_ind == t_ind) & (FP$FP_ID == selectFP[fpi]), ]$FP_coordY
  }
  return(FP)
  #### END OF FUNCTION
}

