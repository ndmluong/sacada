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
  loc <- subset(Plant$L, Location == to) ## all coordinates associated with the new location (destination)
  
  ## change location of the selected workers for the time index t
  W$W_location[which((W$t_ind == t_ind) & (W$W_ID %in% selectW))] <- to

  ############ /!\ Optimized code (begin)
  Wselect <- subset(W, W_ID %in% selectW)
  Wcomp <- subset(W, !(W_ID %in% selectW))
  
  lapply(selectW, FUN = function(x) {
    d1 <- subset(Wselect, W_ID == x)
    randomcoord <- loc[sample(1:nrow(loc), size=1), ]
    d1$W_coordX[which(d1$t_ind == t_ind)] <- randomcoord$coordX
    d1$W_coordY[which(d1$t_ind == t_ind)] <- randomcoord$coordY
    return(d1)
  }) %>%
    data.table::rbindlist() -> Wselect
  
  W <- rbind(Wselect, Wcomp)
  # rbind(Wselect, Wcomp) %>%
  #   dplyr::arrange(t_ind, W_ID) -> W
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
  WS_list <- subset(Plant$L, str_detect(Location, wsname))
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


