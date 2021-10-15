##### PACKAGES #####
library(ggplot2)
library(reshape2)
library(plotly)
library(stringr)

##### f_createPlant() FUNCTION TO CREATE PROCESSING PLANT #####
f_createPlant <- function(
  #### INPUT PARAMETERS
  prm, ## list of parameters (see the R script "parameters.R")
  ...
  #### OUTPUT VALUES: 'Plant' object: List$
  ## L (data.frame): different locations of the plant ('entry hall', 'wc'...) and their coordinates X and Y
  ## P (character matrix) : food processing plant
) {
  #### BEGIN OF FUNCTION
  # Initialize an empty space dim.X * dim.Y
  P <- matrix("empty", nrow = prm$dim.X, ncol = prm$dim.Y) 

  ## ADD ALL SPACES AND THE CORRESPONDING LOCATIONS
  ## Save all coordinates of all locations with the data frame L
  L <- data.frame(
    Location = character(),
    coordX = numeric(),
    coordY = numeric()
  )
  nbSpaces <- length(prm$Spaces) ## total number of the spaces
  for (i in 1:nbSpaces) { ## for each space i, add i inside the plant P
    tmp <- f_addSpace(P,
                      space.label = prm$Spaces[[i]]$label,
                      space.dim.X = prm$Spaces[[i]]$dim.X,
                      space.dim.Y = prm$Spaces[[i]]$dim.Y,
                      space.pos.X = prm$Spaces[[i]]$pos.X,
                      space.pos.Y = prm$Spaces[[i]]$pos.Y)
    P <- tmp$P
    L <- rbind(L, tmp$L) ## save the coordinates of the added location
  }
  colnames(L) <- c("Location", "coordX", "coordY")
  L$Location <- as.factor(L$Location)
  
  ## ADD ALL OBJECTS
  nbObjects <- length(prm$Objects)
  for (i in 1:nbObjects) {
    P <- f_addObject(P,
                     label = prm$Objects[[i]]$label,
                     obj.dim.X = prm$Objects[[i]]$dim.X,
                     obj.dim.Y = prm$Objects[[i]]$dim.Y,
                     obj.pos.X = prm$Objects[[i]]$pos.X,
                     obj.pos.Y = prm$Objects[[i]]$pos.Y)

  }
  
  ## Coordinates of the doors
  D <- f_coordDoor(L = L, prm = prm)
  
  Plant <- list(L = L, P = P, D = D)
  return(Plant)
  #### END OF FUNCTION
}

##### f_addSpace() FUNCTION TO CREATE A SPACE INSIDE A PRECREATED PLANT #####
f_addSpace <- function(
  ## INPUT
  P, ## (character matrix) The pre-created plant
  space.label, ## (character) The label of the space
  space.dim.X, ## (numeric) Dimension of the space
  space.dim.Y, ## (numeric) Dimension of the space
  space.pos.X, ## (numeric) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
  space.pos.Y, ## (numeric) Position regarding the Y axis of the plant (0: left, 0.5: middle, 1:right)
  ...
  ## OUTPUT
  ## Plant (list): list of two elements
  ## - $L (data frame): the coordinates (in X and Y axes) of all locations inside the plant ("Entry hall", "W.C.",...)
  ## - $P (character matrix): The updated plant with the added space
) {
  #### BEGIN OF FUNCTION
  ## Dimensions of the plant
  dim.X = nrow(P)
  dim.Y = ncol(P)
  
  ######################################################
  ## ("rectangle" between X0,X1,Y0,Y1)
  tmp <- max(1, floor(dim.X * space.pos.X)) 
  tmp <- tmp + space.dim.X - 1
  space.X1 <- min(tmp, dim.X)
  space.X0 <- space.X1 - space.dim.X + 1 
  
  tmp <- max(1, floor(dim.Y * space.pos.Y))
  tmp <- tmp + space.dim.Y - 1
  space.Y1 <- min(tmp, dim.Y)
  space.Y0 <- space.Y1 - space.dim.Y + 1 
  ######################################################
  
  ## Store all surface coordinates associated with this space as a location L
  x_vec <- space.X0:space.X1
  y_vec <- space.Y0:space.Y1
  xy_tab <- expand.grid(x_vec, y_vec)
  L <- data.frame(
    Location = rep(space.label, nrow(xy_tab)),
    coordX = xy_tab$Var1,
    coordY = xy_tab$Var2
  )
  
  Plant <- list(L = L, P = P)
  
  return(Plant)
  #### END OF FUNCTION
}


##### f_addObject() FUNCTION TO ADD AN OBJECT INSIDE A PRECREATED PLANT #####
f_addObject <- function(
  ## INPUT
  P, ## (numeric matrix) The pre-created plant
  label, ## (character)
  obj.dim.X, ## (numeric) Dimension of the object
  obj.dim.Y, ## (numeric) Dimension of the object
  obj.pos.X, ## (numeric) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
  obj.pos.Y, ## (numeric) Position regarding the Y axis of the plant (0: left, 0.5: middle, 1:right)
  ...
  ## OUTPUT
  ## P (numeric matrix) The updated plant with the added space
) {
  #### BEGIN OF FUNCTION
  ## Dimensions of the plant
  dim.X = nrow(P)
  dim.Y = ncol(P)
  
  ######################################################
  ## ("rectangle" between X0,X1,Y0,Y1)
  tmp <- max(1, floor(dim.X * obj.pos.X)) 
  tmp <- tmp + obj.dim.X - 1
  obj.X1 <- min(tmp, dim.X)
  obj.X0 <- obj.X1 - obj.dim.X + 1 
  
  tmp <- max(1, floor(dim.Y * obj.pos.Y))
  tmp <- tmp + obj.dim.Y - 1
  obj.Y1 <- min(tmp, dim.Y)
  obj.Y0 <- obj.Y1 - obj.dim.Y + 1 
  ######################################################
  
  ## Add the object in the pre-created plant
  for (i in obj.X0:obj.X1) {
    for (j in obj.Y0:obj.Y1) {
      P[i,j] <- label
    }
  }
  
  return(P)
  #### END OF FUNCTION
}

##### f_initWorkers() FUNCTION TO INITIALISE A SET OF WORKERS #####
f_initWorkers <- function(
  ## Function allowing to initialize a set of workers with different infection states and mask behaviour
  ## depending on probability parameters provided as input argument.
  #### INPUT
  prm, ## (list) the parameters associated with all attributes of the workers with at least the following elements
  ## (check the script "workers_parameters.R")
  ##  - $NWorkers (integer), ## total number of the workers during the entire process
  ##  - $InfectProb (numeric), ## probability of infected people among the workers
  ##  - $MaskWearingProb (numeric) ## probability of people wearing a mask
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
  ## ID of the workers (value "W001","W002", ..)
  W_ID = paste("W", stringr::str_pad(seq(1:prm$NWorkers), width=3, pad="0"), sep="")
  
  ## Initial states (infected/not infected) of the workers
  W_state <- as.factor(sample(c("infected", "not infected"),
                              size = prm$NWorkers, replace = T,
                              prob = c(prm$InfectProb, 1-prm$InfectProb)))
  
  ## Behavior (mask/no mask) of the workers
  W_mask <- as.factor(sample(c("mask", "no mask"),
                             size = prm$NWorkers, replace = T,
                             prob = c(prm$MaskWearingProb, 1-prm$MaskWearingProb)))
  
  ## Workers with a fixed or mobile work space
  W_fixed <- as.factor(sample(c("fixed", "mobile"),
                              size = prm$NWorkers, replace = T,
                              prob = c(prm$FixedWorkspaceProb, 1-prm$FixedWorkspaceProb)))
  
  ## Initialize coordinates and location of the workers (NA)
  W_coordX <- rep(NA, prm$NWorkers)
  W_coordY <- rep(NA, prm$NWorkers)
  W_location <- rep(NA, prm$NWorkers)
  
  ## Initialize the working (or not-working phase) of the workers (NA)
  W_active <- rep(NA, prm$NWorkers)
  
  t <- rep(0, prm$NWorkers)
  
  ## Output data frame
  W <- data.frame(W_ID = W_ID,
                  W_state = W_state,
                  W_mask = W_mask,
                  W_fixed = W_fixed,
                  W_active = W_active,
                  W_coordX = W_coordX,
                  W_coordY = W_coordY,
                  W_location = W_location,
                  t = t)
  
  return(W)
  #### END OF FUNCTION
}

##### f_initFood() FUNCTION TO INITIALISE A SET OF FOOD PORTIONS #####
f_initFood <- function(
  ## Function allowing to initialize a set of food portions of several types (contact/contaminated...)
  ## depending on probability parameters provided as input argument.
  #### INPUT
  prm, ## (list) the parameters associated with all attributes of the workers with at least the following elements
  ## (check the script "food_parameters.R")
  ##  - $NPortions (integer): the total number of food portions
  ##  - $PortionTypeProb (numeric vector): the probability for different type of portions (contace/contaminated...)
  ...
  #### OUTPUT
  ## W (dataframe): the initialized food portions
  ##  - $FP_ID (character/string): the ID of each food portion ("FP0001","FP0002",...)
  ##  - $FP_type (character/string): infection state ("infected"/"not infected",...)
  ##  - $FP_coordX (integer): coordinates of the food portions in the plant (initialized as NA)
  ##  - $FP_coordY (integer): coordinates of the food portions in the plant (initialized as NA)
  ##  - $FP_location (character): location of the food portions ("Arrival gate", "Waste area"...) (initialized as NA)
) {
  #### BEGIN OF FUNCTION
  ## The ID of each food portion
  FP_ID = paste("F", stringr::str_pad(seq(1:prm$NPortions), 
                                      width=4, pad="0"), sep="")
  
  ## Portions with possible contact with other surfaces: initialized with the probabilities given as model parameters
  FP_contact <- as.factor(sample(c(T,F),
                                 size = prm$NPortions, replace=T,
                                 prob = c(prm$ContactProb, 1-prm$ContactProb)))
  
  # FP_type <- as.factor(sample(c("Possible contact / not contaminated", 
  #                               "Possible contact / contaminated",
  #                               "No possible contact",
  #                               "Loss portion"),
  #                             size = prm$NPortions, replace = T,
  #                             prob = prm$PortionsTypeProv))
  
  ## Initial coordinates and location of the food portions (initialized as NA)
  FP_coordX <- rep(NA, prm$NPortions)
  FP_coordY <- rep(NA, prm$NPortions)
  FP_location <- rep(NA, prm$NPortions)
  
  ## time index initialized at 0
  t <- rep(0, prm$NPortions)
  
  FP <- data.frame(FP_ID = FP_ID,
                   FP_contact = FP_contact,
                   FP_coordX = FP_coordX,
                   FP_coordY = FP_coordY,
                   FP_location = FP_location,
                   t = t)
  
  return(FP)
  #### END OF FUNCTION
}






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
  
  selectW = NULL, ## (optional, character, vector): agents selection by ID,
  to, ## (character/string) location (e.g. "Entry hall", "WC",...)
  ...
  ## OUTPUT
) {
  #### BEGIN OF FUNCTION
  P <- Plant$P ## the plant
  
  loc <- subset(Plant$L,Location == to) ## all coordinates associated with the new location (destination)
  
  ## if the agents to move are provided by their ID (vector W_ID)
  if (!is.null(selectW)) { 
    N_select <- length(selectW) ## the total number to be move is equal to the lenght of the vector (W_ID)
    selectW_index <- unname(sapply(selectW, match, W$W_ID)) ## look for the selected workers (indexes) among all workers
  }
  else { ## otherwise, move all workers
    N_select <- nrow(W)
    selectW_index <- 1:nrow(W)
  }
  
  for (wi in 1:N_select) { ## for each worker to move
    W$W_location[selectW_index[wi]] <- to ## assign the new location for each 
    pos_index <- sample(1:nrow(loc), size=1,replace=T) ## choose a random position within the new location
    W$W_coordX[selectW_index[wi]] <- loc$coordX[pos_index]
    W$W_coordY[selectW_index[wi]] <- loc$coordY[pos_index]
  }
  
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


##### f_generateWorkspace() FUNCTION TO GENERATE AUTOMATICALLY WORKSPACE FOR EACH WORKER  #####
## IMPLEMENTATION EN COURS !
f_generateWorkspace <- function(
  ## Function for generating automatically the different workspaces (tables of 1x1m.) 
  ## around the conveyor depending on the total number of active workers
  ## INPUT
  Plant, # 'Plant object': list of two elements
  # L (data.frame): different locations of the plant ('entry hall', 'wc'...) and their coordinates X and Y
  # P (numeric matrix) : food processing plant 
  
  W, # (dataframe) The agents (workers/food portions) set: with at least these columns $
  # - W_ID / S_ID / FP_ID / AIR_ID : (character) ID of the agents
  # - W_coordX / S_coordX / FP_coordX: (numeric) coordinates (X) in the plant
  # - W_coordY / S_coordY / FP_coordY: (numeric) coordinates (Y) in the plant
  # - W_location / S_location / FP_location: (optional) (character) current location of the agents
  
  byXAxis = T, # (logical): direction of the conveyor (default: by the X axis)
  ...
  ## OUTPUT
  
) {
  #### BEGIN OF FUNCTION
  P <- Plant$P ## the plant
  
  ## total number of workspaces : total number of "active" workers ?
  subset(W, W_active == T) %>% nrow -> N
  
  ## Coordinates of conveyor
  cvy <- which(P == "Conveyor", arr.ind = T)
  colnames(cvy) <- c("coordX", "coordY")
  
  ## Generate the table around the conveyor
  switch(byXAxis,
         ## if the workspaces are placed "by row" (horizontally along the conveyor on the X axis)
         T = { 
           XAB <- min(cvy[,"coordX"]) # seeking for the coordinates X for the first table
           
           ## the tables with be placed alternatively on the one and the other side (side A and B) of the conveyor
           YA <- min(cvy[,"coordY"]) - 1 # coordinates Y of the tables on the side A (1m between the conveyor and the tables)
           YB <- max(cvy[,"coordY"]) + 1 # coordinates Y of the tables on the side B (1m between the conveyor and the tables)
           
           ## Generate the tables
           i <- 1
           while (i <= N) { ## for the table i from 1 to N
             if ((i %% 2) != 0) {
               P[XAB, YA] <- "Workspace"
             } else {
               P[XAB, YB] <- "Workspace"
               XAB <- XAB + 2
             }
             i <- i + 1 ## next workspace
           }
         },
         ## if the workspaces are placed "by column" (vertically along the conveyor on the Y axis)
         F = { 
           YAB <- min(cvy[,"coordY"]) # seeking for the coordinates Y for the first table
           
           ## the tables with be placed alternatively on the one and the other side (side A and B) of the conveyor
           XA <- min(cvy[,"coordX"]) - 2 # coordinates X of the tables on the side A (1m between the conveyor and the tables)
           XB <- max(cvy[,"coordX"]) + 2 # coordinates X of the tables on the side B (1m between the conveyor and the tables)
           
           ## Generate the tables
           j <- 1
           while (j <= N) { ## for the table i from 1 to N
             if ((j %% 2) != 0) {
               P[XA, YAB] <- "Table"
             } else {
               P[XB, YAB] <- "Table"
               YAB <- YAB + 3
             }
             j <- j + 1 ## next table
           }
         })
  
  return(cvy)
  #### END OF FUNCTION
}


