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
    coordY = numeric(),
    border = logical()
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
  
  ## ADD ALL OBJECTS
  nbObjects <- length(prm$Objects)
  for (i in 1:nbObjects) {
    tmp <- f_addObject(P,
                       label = prm$Objects[[i]]$label,
                       obj.dim.X = prm$Objects[[i]]$dim.X,
                       obj.dim.Y = prm$Objects[[i]]$dim.Y,
                       obj.pos.X = prm$Objects[[i]]$pos.X,
                       obj.pos.Y = prm$Objects[[i]]$pos.Y)
    P <- tmp$P
    L <- rbind(L, tmp$L)
  }
  
  ## Looking for the different possible workspaces in the plant
  ## for cutters
  L <- rbind(L, f_cuttingWorkspace(P)) ## check the function f_cuttingWorkspace()
  
  ## for logistic workers: begin (head) and end (tail) of the conveyor
  L <- rbind(L, f_conveyorWorkspace(P)) ## check the function f_conveyorWorkspace()
  
  ## Around the Equipment 1 (ADD IF NEEDED)
  L <- rbind(L, f_equipmentWorkspace(P, eqm_name = "Equipment 1")) ## check the function f_equipmentWorkspace()
  
  colnames(L) <- c("Location", "coordX", "coordY", "border")
  # L$Location <- as.factor(L$Location)
  
  ## Update tile labels
  for (i in 1:nrow(L)) {
    P[L$coordX[i], L$coordY[i]] <- L$Location[i]
  }
  
  Plant <- list(L = L, P = P)
  return(Plant)
  #### END OF FUNCTION
}

##### f_addSpace() SUB-FUNCTION TO CREATE A SPACE INSIDE A PLANT #####
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
  
  ## Store all surface coordinates associated with this space as a location data frame L
  x_vec <- space.X0:space.X1
  y_vec <- space.Y0:space.Y1
  xy_tab <- expand.grid(x_vec, y_vec)
  L <- data.frame(
    Location = rep(space.label, nrow(xy_tab)),
    coordX = xy_tab$Var1,
    coordY = xy_tab$Var2,
    border = rep(T, nrow(xy_tab))
  )
  
  ## Update values on the matrix P with the label of the added spaces
  for (i in 1:nrow(L)) {
    P[L$coordX[i], L$coordY[i]] <- L$Location[i]
  }
  
  Plant <- list(L = L, P = P)
  
  return(Plant)
  #### END OF FUNCTION
}


##### f_addObject() SUB-FUNCTION TO ADD AN OBJECT INSIDE A PLANT #####
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
  
  ## Store all surface coordinates associated with this space as a location data frame L
  x_vec <- obj.X0:obj.X1
  y_vec <- obj.Y0:obj.Y1
  xy_tab <- expand.grid(x_vec, y_vec)
  L <- data.frame(
    Location = rep(label, nrow(xy_tab)),
    coordX = xy_tab$Var1,
    coordY = xy_tab$Var2,
    border = rep(F, nrow(xy_tab))
  )
  
  return(list(L=L, P=P))
  #### END OF FUNCTION
}

##### f_cuttingWorkspace() SUB-FUNCTION: COORDINATES OF POSSIBLE POSITIONS FOR CUTTERS ALONG THE CONVEYOR  #####
f_cuttingWorkspace <- function(
  P,
  ...
  ## OUTPUT
  
) {
  #### BEGIN OF FUNCTION
  ## Coordinates of conveyor
  cvy <- which(P == "Conveyor", arr.ind = T) %>% as.data.frame
  colnames(cvy) <- c("coordX", "coordY")
  
  minX <- min(cvy$coordX)
  maxX <- max(cvy$coordX)
  minY <- min(cvy$coordY)
  maxY <- max(cvy$coordY)
  
  ## Check if the conveyor is along the axis X or Y
  cvy_dimX <- maxX - minX + 1
  cvy_dimY <- maxY - minY + 1
  
  if (cvy_dimY <= cvy_dimY) {
    along <- "Axis X"
  } else {along <- "Axis Y"}
  
  ## The length of the conveyor
  cvy_L <- max(cvy_dimX, cvy_dimY)
  N_WS <- 2 * cvy_L
  
  if (along == "Axis X") { ## if the conveyor is along the 'horizontal' X axis
    coordX <- c(seq(minX, maxX), ## coordinates X along the axis X (two sides)
                seq(minX, maxX))
    coordY <- c(rep(minY-1, cvy_L), ## coordinates Y +/-1 (workers close to the conveyor)
                rep(maxY+1, cvy_L)) 
  }
  
  if (along == "Axis Y") { ## if the conveyor is along the 'vertical' Y axis
    coordY <- c(seq(minY, maxY), ## coordinates Y along the axis Y (two sides)
                seq(minY, maxY))
    coordX <- c(rep(minX-1, cvy_L), ## coordinates X +/-1 (workers close to the conveyor)
                rep(maxX+1, cvy_L)) 
  }
  
  Location <- paste("WS-Cutting-", stringr::str_pad(seq(1:N_WS), width=2, pad="0"), sep="")
  
  OUTPUT <- data.frame(Location = Location,
                       coordX = coordX,
                       coordY = coordY,
                       border = rep(F, N_WS))
  
  return(OUTPUT)
  #### END OF FUNCTION
}


##### f_conveyorWorkspace() SUB-FUNCTION: COORDINATES OF POSSIBLE POSITIONS AROUND THE HEAD/TAIL OF THE CONVEYOR #####
f_conveyorWorkspace <- function(
  P,
  ...
  ## OUTPUT
  
) {
  #### BEGIN OF FUNCTION
  ## Begin / End of the conveyor
  cvy <- which(P == "Conveyor", arr.ind = T) %>% as.data.frame
  colnames(cvy) <- c("coordX", "coordY")
  
  minX <- min(cvy$coordX)
  maxX <- max(cvy$coordX)
  minY <- min(cvy$coordY)
  maxY <- max(cvy$coordY)
  
  ## Check if the conveyor is along the X or Y axis
  cvy_dimX <- maxX - minX + 1
  cvy_dimY <- maxY - minY + 1
  
  if (cvy_dimY <= cvy_dimY) {
    along <- "Axis X"
  } else {along <- "Axis Y"}
  
  ## The length of the conveyor
  cvy_L <- max(cvy_dimX, cvy_dimY)
  ## The width of the conveyor
  cvy_W <- min(cvy_dimX, cvy_dimY)
  
  if (along == "Axis X") { ## if the conveyor is along the 'horizontal' X axis
    coordX <- c(rep(minX-1, cvy_W+2), ## coordinates X: begin (head) of the horizontal conveyor
                rep(maxX+1, cvy_W+2)) ## coordinates X: end (tail) of the horizontal conveyor
    coordY <- c(seq(minY-1, maxY+1), ## coordinates Y +/- 1 : begin (head) of the conveyor
                seq(minY-1, maxY+1)) ## coordinates Y +/- 1 : end (tail) of the conveyor
  }
  
  if (along == "Axis Y") { ## if the conveyor is along the 'vertical' Y axis
    coordY <- c(rep(minY-1, cvy_W+2), ## coordinates Y : begin (head) of the vertical conveyor
                rep(maxY+1, cvy_W+2)) ## coordinates Y: end (tail) of the vertical conveyor
    coordX <- c(seq(minX-1, maxX+1), ## coordinates Y +/- 1 : begin (head) of the conveyor
                seq(minX-1, maxX+1)) ## coordinates Y +/- 1 : begin (tail) of the conveyor
  }
  
  Location <- c(rep("WS-Conveyor-Head", cvy_W+2),
                rep("WS-Conveyor-Tail", cvy_W+2))
  border <- rep(F, 2*(cvy_W+2))
  
  OUTPUT <- data.frame(Location = Location,
                       coordX = coordX,
                       coordY = coordY,
                       border = border)
  
  return(OUTPUT)
  #### END OF FUNCTION
}

##### f_equimentWorkspace() SUB-FUNCTION: COORDINATES OF POSSIBLE POSITIONS AROUND THE HEAD/TAIL OF THE CONVEYOR #####
f_equipmentWorkspace <- function(
  P,
  eqm_name,
  ...
  ## OUTPUT
  
) {
  #### BEGIN OF FUNCTION
  ## Begin / End of the conveyor
  eqm <- which(P == eqm_name, arr.ind = T) %>% as.data.frame
  colnames(eqm) <- c("coordX", "coordY")
  
  minX <- min(eqm$coordX)
  maxX <- max(eqm$coordX)
  minY <- min(eqm$coordY)
  maxY <- max(eqm$coordY)
  
  eqm_dimX <- maxX-minX+1
  eqm_dimY <- maxY-minY+1
  
  coordX <- c(seq(minX-1,maxX+1), ## top side
              seq(minX-1,maxX+1), ## bottom side
              rep(minX-1,eqm_dimY), ## left side
              rep(maxX+1,eqm_dimY) ## right side
              )

  coordY <- c(rep(maxY+1,eqm_dimX+2), ## top side
              rep(minY-1,eqm_dimX+2), ## bottom side
              seq(minY, maxY), ## left side
              seq(minY, maxY) ## right side
              )
  
  Location <- rep(paste("WS-",eqm_name, sep=""), length(coordX))

  border <- rep(F, length(coordX))

  OUTPUT <- data.frame(Location = Location,
                       coordX = coordX,
                       coordY = coordY,
                       border = border)

  return(OUTPUT)
  #### END OF FUNCTION
}

