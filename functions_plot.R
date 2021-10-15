##### f_Border() #####
f_Border <- function(
  Plant,
  ...
) {
  L <- Plant$L
  
  ## Border of the spaces (location)
  tapply(L$coordX, INDEX = L$Location, FUN = min) %>% names -> Location
  
  tapply(L$coordX, INDEX = L$Location, FUN = min) %>% unname - 0.5 -> Xmin
  tapply(L$coordX, INDEX = L$Location, FUN = max) %>% unname + 0.5 -> Xmax
  
  tapply(L$coordY, INDEX = L$Location, FUN = min) %>% unname - 0.5 -> Ymin
  tapply(L$coordY, INDEX = L$Location, FUN = max) %>% unname + 0.5 -> Ymax
  
  Border <- data.frame(Location = Location,
                       Xmin = Xmin,
                       Xmax = Xmax,
                       Ymin = Ymin,
                       Ymax = Ymax)
  
  return(Border)
}

##### f_coordDoor() #####
f_coordDoor <- function(
  prm,
  L,
  ...
  ## OUTPUT
  ## D (data frame): coordinates of all doors
) {
  nbSpaces <- length(prm$Spaces) ## total number of the spaces
  
  D <- data.frame(Location = rep(NA, nbSpaces),
                  intdoor.X0 = rep(NA, nbSpaces),
                  intdoor.X1 = rep(NA, nbSpaces),
                  intdoor.Y0 = rep(NA, nbSpaces),
                  intdoor.Y1 = rep(NA, nbSpaces),
                  intdoor.side = rep(NA, nbSpaces),
                  extdoor.X0 = rep(NA, nbSpaces),
                  extdoor.Y0 = rep(NA, nbSpaces),
                  extdoor.X1 = rep(NA, nbSpaces),
                  extdoor.Y1 = rep(NA, nbSpaces),
                  extdoor.side = rep(NA, nbSpaces)
  )
  
  for (i in 1:nbSpaces) { ## for each space i inside the plant
    
    D$Location[i] <- prm$Spaces[[i]]$label ## extract the label of the space i
    
    Lsub <- subset(L, Location == prm$Spaces[[i]]$label) ## extract the coordinates of the space i
    
    if (is.na(prm$Spaces[[i]]$intdoor.side) == F) { ## if information about the internal door is available
      D$intdoor.side[i] <- prm$Spaces[[i]]$intdoor.side
      set.seed(408)
      switch(prm$Spaces[[i]]$intdoor.side, ## check the side of the doors given as input argument
             "left" = { ## if the door is in the left side of the space
               D$intdoor.X0[i] <- min(Lsub$coordX) - 0.5
               D$intdoor.X1[i] <- min(Lsub$coordX) - 0.5
               intdoor.Y <- sample(Lsub$coordY, 1)
               D$intdoor.Y0[i] <- intdoor.Y - 0.5
               D$intdoor.Y1[i] <- intdoor.Y + 0.5},
             "right" = {
               D$intdoor.X0[i] <- max(Lsub$coordX) + 0.5
               D$intdoor.X1[i] <- max(Lsub$coordX) + 0.5
               intdoor.Y <- sample(Lsub$coordY, 1)
               D$intdoor.Y0[i] <- intdoor.Y - 0.5
               D$intdoor.Y1[i] <- intdoor.Y + 0.5},
             "top" = {
               intdoor.X <- sample(Lsub$coordX, 1)
               D$intdoor.X0[i] <- intdoor.X - 0.5
               D$intdoor.X1[i] <- intdoor.X + 0.5
               D$intdoor.Y0[i] <- max(Lsub$coordY) + 0.5
               D$intdoor.Y1[i] <- max(Lsub$coordY) + 0.5},
             "bottom" = {
               intdoor.X <- sample(Lsub$coordX, 1)
               D$intdoor.X0[i] <- intdoor.X - 0.5
               D$intdoor.X1[i] <- intdoor.X + 0.5
               D$intdoor.Y0[i] <- min(Lsub$coordY) - 0.5
               D$intdoor.Y1[i] <- min(Lsub$coordY) - 0.5})
    }
    
    if (is.na(prm$Spaces[[i]]$extdoor.side) == F) { ## if information about the external door is available
      D$extdoor.side[i] <- prm$Spaces[[i]]$extdoor.side
      set.seed(408)
      switch(prm$Spaces[[i]]$extdoor.side, ## check the side of the doors given as input argument
             "left" = { ## if the door is in the left side of the space
               D$extdoor.X0[i] <- min(Lsub$coordX) - 0.5
               D$extdoor.X1[i] <- min(Lsub$coordX) - 0.5
               extdoor.Y <- sample(Lsub$coordY, 1)
               D$extdoor.Y0[i] <- extdoor.Y - 0.5
               D$extdoor.Y1[i] <- extdoor.Y + 0.5},
             "right" = {
               D$extdoor.X0[i] <- max(Lsub$coordX) + 0.5
               D$extdoor.X1[i] <- max(Lsub$coordX) + 0.5
               extdoor.Y <- sample(Lsub$coordY, 1)
               D$extdoor.Y0[i] <- extdoor.Y - 0.5
               D$extdoor.Y1[i] <- extdoor.Y + 0.5},
             "top" = {
               extdoor.X <- sample(Lsub$coordX, 1)
               D$extdoor.X0[i] <- extdoor.X - 0.5
               D$extdoor.X1[i] <- extdoor.X + 0.5
               D$extdoor.Y0[i] <- max(Lsub$coordY) + 0.5
               D$extdoor.Y1[i] <- max(Lsub$coordY) + 0.5},
             "bottom" = {
               extdoor.X <- sample(Lsub$coordX, 1)
               D$extdoor.X0[i] <- extdoor.X - 0.5
               D$extdoor.X1[i] <- extdoor.X + 0.5
               D$extdoor.Y0[i] <- min(Lsub$coordY) - 0.5
               D$extdoor.Y1[i] <- min(Lsub$coordY) - 0.5})
    }
  }
  
  return(D)
  
}

##### f_plotPlant() FUNCTION TO PLOT THE PROCESSING PLANT #####
f_plotPlant <- function(
  #### INPUT PARAMETERS
  Plant, ## (list of two elements): food processing plant (output of the function f_createPlant)
  ##  - $L: (data frame) coordinates of all locations
  ##  - $P: (character matrix) processing plant
  ##  - $D: (data frame) coordinates of all doors
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
  prm = NULL,
  ...
) {
  #### BEGIN OF FUNCTION
  ## data transformation
  Pdf <- reshape2::melt(Plant$P, c("coordX","coordY"), value.name = "tile") ## converting matrix into dataframe for plotting
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
  
  ## Borders
  ## Coordinates of the borders of the spaces
  Border <- f_Border(Plant)
  ## Plot
  g_Plant <- g_Plant  +
    geom_segment(data = Border, aes(x = Xmin, y = Ymin, xend = Xmax, yend = Ymin), size=1.5) + # lower border
    geom_segment(data = Border, aes(x = Xmin, y = Ymax, xend = Xmax, yend = Ymax), size=1.5) + # upper border
    geom_segment(data = Border, aes(x = Xmin, y = Ymin, xend = Xmin, yend = Ymax), size=1.5) + # left border
    geom_segment(data = Border, aes(x = Xmax, y = Ymin, xend = Xmax, yend = Ymax), size=1.5) # right border
  
  ## Borders of the plant
  g_Plant <- g_Plant  +
    geom_segment(aes(x = 0.5, y = 0.5, xend = nrow(Plant$P)+0.5, yend = 0.5), size=1.5) + # lower border
    geom_segment(aes(x = 0.5, y = ncol(Plant$P)+0.5, xend = nrow(Plant$P)+0.5, yend = ncol(Plant$P)+0.5), size=1.5) + # upper border
    geom_segment(aes(x = 0.5, y = 0.5, xend = 0.5, yend = ncol(Plant$P)+0.5), size=1.5) + # left border
    geom_segment(aes(x = nrow(Plant$P)+0.5, y = 0.5, xend = nrow(Plant$P)+0.5, yend = ncol(Plant$P)+0.5), size=1.5) # right border
  
  ## Internal doors
  g_Plant <- g_Plant +
    geom_segment(data = Plant$D,
                 aes(x = intdoor.X0, xend = intdoor.X1, y = intdoor.Y0, yend = intdoor.Y1),
                 size=3, colour = "darkgray")
  ## External doors
  g_Plant <- g_Plant +
    geom_segment(data = Plant$D,
                 aes(x = extdoor.X0, xend = extdoor.X1, y = extdoor.Y0, yend = extdoor.Y1),
                 size=3, colour = "darkgreen")
  
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



