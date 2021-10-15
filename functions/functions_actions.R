

##### f_moveWorkers() FUNCTION TO MOVE THE WORKERS INSIDE THE PLANT #####
f_moveWorkers <- function(
  ## Move different types of agents from one location to another inside the pre-created plant
  ## INPUT
  Plant, # 'Plant object': list of two objects
  # L (data.frame): different locations of the plant ('entry hall', 'wc'...) and their coordinates X and Y
  # P (numeric matrix) : food processing plant 
  W, # (dataframe) The agents (workers/food portions) set: with at least these columns $
  # - W_ID / S_ID / FP_ID / AIR_ID : (character) ID of the agents
  # - W_coordX / S_coordX / FP_coordX: (numeric) coordinates (X) in the plant
  # - W_coordY / S_coordY / FP_coordY: (numeric) coordinates (Y) in the plant
  # - W_location / S_location / FP_location: (optional) (character) current location of the agents
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
  to -> W[(W$t == t_ind) & (W$W_ID %in% selectW), ]$W_location 
  ## random coordinates X of the selected workers in the given location at the time index t
  loc$coordX %>% 
    unique %>%
    sample(size=N_move, replace=T) -> 
    W[(W$t == t_ind) & (W$W_ID %in% selectW), ]$W_coordX
  ## random coordinates Y of the selected workers in the given location at the time index t
  loc$coordY %>%
    unique %>%
    sample(size=N_move, replace=T) ->
    W[(W$t == t_ind) & (W$W_ID %in% selectW), ]$W_coordY
  
  return(W)
  #### END OF FUNCTION
}



##### f_moveFood() FUNCTION TO MOVE THE FOOD PORTIONS INSIDE THE PLANT #####
f_moveFood <- function(
  ## Move different types of agents from one location to another inside the pre-created plant
  ## INPUT
  Plant, # 'Plant object': list of two objects
  # L (data.frame): different locations of the plant ('entry hall', 'wc'...) and their coordinates X and Y
  # P (numeric matrix) : food processing plant 
  
  FP, # (dataframe) The agents (workers/food portions) set: with at least these columns $
  # - W_ID / S_ID / FP_ID / AIR_ID : (character) ID of the agents
  # - W_coordX / S_coordX / FP_coordX: (numeric) coordinates (X) in the plant
  # - W_coordY / S_coordY / FP_coordY: (numeric) coordinates (Y) in the plant
  # - W_location / S_location / FP_location: (optional) (character) current location of the agents
  
  selectFP = NULL, ## (optional, character, vector): agents selection by ID,
  to, ## (character/string) location (e.g. "Entry hall", "WC",...)
  ...
  ## OUTPUT
) {
  #### BEGIN OF FUNCTION
  P <- Plant$P ## the plant
  
  loc <- subset(Plant$L,Location == to) ## all coordinates associated with the new location (destination)
  
  ## if the agents to move are provided by their ID (vector FP_ID)
  if (!is.null(selectFP)) { 
    N_select <- length(selectFP) ## the total number to be move is equal to the lenght of the vector (FP_ID)
    selectFP_index <- unname(sapply(selectFP, match, FP$FP_ID)) ## look for the selected workers (indexes) among all workers
  }
  else { ## otherwise, move all workers
    N_select <- nrow(FP)
    selectFP_index <- 1:nrow(FP)
  }
  
  for (fpi in 1:N_select) { ## for each worker to move
    FP$FP_location[selectFP_index[fpi]] <- to ## assign the new location for each 
    pos_index <- sample(1:nrow(loc), size=1,replace=T) ## choose a random position within the new location
    FP$FP_coordX[selectFP_index[fpi]] <- loc$coordX[pos_index]
    FP$FP_coordY[selectFP_index[fpi]] <- loc$coordY[pos_index]
  }
  
  return(FP)
  #### END OF FUNCTION
}



##### f_InvariantTimeSteps() #####
f_InvariantTimeSteps <- function(
  Agents,
  Invariant,
  t_ind_from,
  t_ind_to,
  ...
) {
  
  for (i in 1:length(Invariant)) {
    for (t_j in (t_ind_from+1):t_ind_to) {
      Agents[Agents$t_ind == t_j, Invariant[i]] <- Agents[Agents$t_ind == t_ind_from, Invariant[i]]
    }
  }
  return(Agents)
}
