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
  
  ## CREATE BORDER WALLS
  P[1,] <- "border" 
  P[,1] <- "border"  
  P[prm$dim.X, ] <- "border"
  P[, prm$dim.Y] <- "border"

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
                      space.pos.Y = prm$Spaces[[i]]$pos.Y,
                      space.intdoor.side = prm$Spaces[[i]]$intdoor.side,
                      space.extdoor.side = prm$Spaces[[i]]$extdoor.side)
    P <- tmp$P
    L <- rbind(L, tmp$L) ## save the coordinates of the added location
  }
  colnames(L) <- c("Location", "coordX", "coordY")
  L$Location <- as.factor(L$Location)

  ## ADD ALL OBJECTS
  nbObjects <- length(prm$Objects)
  for (i in 1:nbObjects) {
    P <- f_addObject(P,
                     obj.dim.X = prm$Objects[[i]]$dim.X,
                     obj.dim.Y = prm$Objects[[i]]$dim.Y,
                     obj.pos.X = prm$Objects[[i]]$pos.X,
                     obj.pos.Y = prm$Objects[[i]]$pos.Y)

  }

  OUTPUT <- list(L = L, P = P)
  return(OUTPUT)
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
  space.intdoor.side, ## (character) 
  space.extdoor.side, ## (character) 
  ...
  ## OUTPUT
  ## P (character matrix) The updated plant with the added space
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
  
  
  ## Add a border around the space
  for (i in space.X0:space.X1) {P[i, c(space.Y0,space.Y1)] <- "border"}
  for (j in space.Y0:space.Y1) {P[c(space.X0,space.X1),j] <- "border"}
  
  ## Store all surface coordinates associated with this space as a location L
  x_vec <- (space.X0+1):(space.X1-1)
  y_vec <- (space.Y0+1):(space.Y1-1)
  xy_tab <- expand.grid(x_vec, y_vec)
  L <- data.frame(
    Location = rep(space.label, nrow(xy_tab)),
    coordX = xy_tab$Var1,
    coordY = xy_tab$Var2
  )
  
  ## CREATE DOORS
  switch(space.intdoor.side,
         "left" = {
           intdoor.pos.X <- space.X0
           intdoor.pos.Y <- ceiling(space.Y0+space.dim.Y/2)},
         "right" = {
           intdoor.pos.X <- space.X1
           intdoor.pos.Y <- ceiling(space.Y0+space.dim.Y/2)},
         "top" = {
           intdoor.pos.X <- ceiling(space.X0+space.dim.X/2)
           intdoor.pos.Y <- space.Y1},
         "bottom" = {
           intdoor.pos.X <- ceiling(space.X0+space.dim.X/2)
           intdoor.pos.Y <- space.Y0})
  switch(space.extdoor.side,
         "left" = {
           extdoor.pos.X <- space.X0
           extdoor.pos.Y <- ceiling(space.Y0+space.dim.Y/2)},
         "right" = {
           extdoor.pos.X <- space.X1
           extdoor.pos.Y <- ceiling(space.Y0+space.dim.Y/2)},
         "top" = {
           extdoor.pos.X <- ceiling(space.X0+space.dim.X/2)
           extdoor.pos.Y <- space.Y1},
         "bottom" = {
           extdoor.pos.X <- ceiling(space.X0+space.dim.X/2)
           extdoor.pos.Y <- space.Y0})
  
  ## Add the doors in the pre-created plant
  P[intdoor.pos.X, intdoor.pos.Y] <- "door" ## value 4 for doors
  P[extdoor.pos.X, extdoor.pos.Y] <- "door" ## value 4 for doors
  
  OUTPUT <- list(L = L, P = P)
  
  return(OUTPUT)
  #### END OF FUNCTION
}


##### f_addObject() FUNCTION TO ADD AN OBJECT INSIDE A PRECREATED PLANT #####
f_addObject <- function(
  ## INPUT
  P, ## (numeric matrix) The pre-created plant
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
      P[i,j] <- "equipment"
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
  
  ## Initiate coordinates and location of the workers (NA)
  W_coordX <- rep(NA, prm$NWorkers)
  W_coordY <- rep(NA, prm$NWorkers)
  W_location <- rep(NA, prm$NWorkers)
  
  W <- data.frame(W_ID = W_ID,
                  W_state = W_state,
                  W_mask = W_mask,
                  W_coordX = W_coordX,
                  W_coordY = W_coordY,
                  W_location = W_location)
  
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
  
  ## The types of each food portion
  FP_type <- as.factor(sample(c("Possible contact / not contaminated", 
                                "Possible contact / contaminated",
                                "No possible contact",
                                "Loss portion"),
                              size = prm$NPortions, replace = T,
                              prob = prm$PortionsTypeProv))
  
  ## Initiate coordinates and location of the food portions (initialized as NA)
  FP_coordX <- rep(NA, prm$NPortions)
  FP_coordY <- rep(NA, prm$NPortions)
  FP_location <- rep(NA, prm$NPortions)
  
  FP <- data.frame(FP_ID = FP_ID,
                   FP_type = FP_type,
                   FP_coordX = FP_coordX,
                   FP_coordY = FP_coordY,
                   FP_location = FP_location)
  return(FP)
  #### END OF FUNCTION
}



##### f_plotPlant() FUNCTION TO PLOT THE PROCESSING PLANT #####
f_plotPlant <- function(
  #### INPUT PARAMETERS
  P, ## (numeric matrix): food processing plant
  W = NULL, ## (optional arguments) (data.frame): information of the workers with at least these attributes
  ##  - W_ID (character): worker ID
  ##  - W_coordX (numeric): coordinates in the X axis of the worker
  ##  - W_coordY (numeric): coordinates in the X axis of the worker
  ##  - W_state (character/factor): infected/not infected worker 
  ##  - W_mask (character/factor): mask/no mask
  FP = NULL, ## (optional arguments) (data.frame): information of the food portions with at least these attributes
  ##  - F_ID (character): worker ID
  ##  - F_coordX (numeric): coordinates in the X axis of the worker
  ##  - F_coordY (numeric): coordinates in the X axis of the worker
  ##  - F_state (character/factor): infected/not infected worker 
  ##  - F_mask (character/factor): mask/no mask
  ...
) {
  #### BEGIN OF FUNCTION
  ## data transformation
  Pdf <- reshape2::melt(P, c("coordX","coordY"), value.name = "tile") ## converting matrix into dataframe for plotting
  Pdf$tile <- as.factor(Pdf$tile) ## converting the values into factors with different levels
  
  ## plotting using ggplot2 package
  g_Plant <- ggplot()+
    geom_raster(data = Pdf, mapping = aes(x=coordX ,y=coordY, fill=tile)) +
    theme(axis.ticks=element_blank(),
          legend.position = "none",
          panel.background=element_rect(fill="white"),
          plot.title = element_text(hjust = 0.5, face="bold", size=20),
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank()) +
    scale_fill_manual(values = c("border" = "black", ## border
                                 "equipment" = "darkgrey", ## furnitures/equipment
                                 "empty"= "white", ## empty space
                                 "workplace" = "navyblue", ## working position
                                 "door" = "lightgrey" ## doors
                                 )) +
    scale_colour_manual(values = c("not infected" = "darkgreen", ## sick 
                                   "infected" = "darkred", ## not sick
                                   "Possible contact / not contaminated" = "orange", ## possible contact / not contaminated
                                   "Possible contact / contaminated" = "red", ## possible contact / contaminated
                                   "No possible contact" = "blue", ## no possible contact
                                   "Loss portion" = "black") ## wasted pieces
                        ) + 
    scale_shape_manual(values = c("no mask" = 1,
                                  "mask" = 16)) +
    coord_fixed(ratio = 1) +
    labs(title = "Meat processing plant")
  
  ## If the workers set are supplied using the input parameter W
  if (!is.null(W)) {
    g_Plant <- g_Plant +
      geom_point(data = W,
                 mapping = aes(x=W_coordX, y=W_coordY, colour=W_state, shape=W_mask, W_ID=W_ID),
                 size = 3,
                 position = position_jitter(width = 0.3, height = 0.3, seed=408))
  }

  ## If the food matrices set are supplied using the input parameter FP
  if (!is.null(FP)) {
    g_Plant <- g_Plant +
      geom_point(data = FP,
                 mapping = aes(x = FP_coordX, y = FP_coordY,
                                colour = FP_type, FP_ID = FP_ID),
                 shape = 15,
                 position = position_jitterdodge(jitter.height = 0.3, dodge.width=0.3, seed=408))
  }
  
  return(g_Plant)
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