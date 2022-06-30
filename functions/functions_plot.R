##### f_plotPlant() FUNCTION TO PLOT THE EMPTY PROCESSING PLANT #####
f_plotPlant <- function(
    #### INPUT PARAMETERS
  Plant, ## (list with the following elements): food processing plant (output of the function f_createPlant())
  ##  - $L: (data frame) coordinates of all locations
  ##  - $P: (character matrix) processing plant
  prm, ## list of parameters (check the R script "parameters_plant.R")
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
          plot.subtitle = element_text(hjust = 0.5, size=15),
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank()) +
    scale_fill_manual(values = f_tile_colour(Plant$P)) +
    scale_colour_manual(values = c("not contaminated" = "darkgreen", ## sick 
                                   "contaminated" = "darkred", ## not sick
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
  Border <- f_Border(Plant) ## check function f_Border()
  
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
  
  ## Coordinates of the doors
  D <- f_coordDoor(L = Plant$L, prm = prm) ## check the function f_coordDoor()
  
  ## Internal doors
  g_Plant <- g_Plant +
    geom_segment(data = D, aes(x = intdoor.X0, xend = intdoor.X1, y = intdoor.Y0, yend = intdoor.Y1),
                 size=3, colour = "gray25") +
    geom_segment(data = D, aes(x = intdoor.X0, xend = intdoor.X1, y = intdoor.Y0, yend = intdoor.Y1),
                 size=1.2, colour = "white")
  
  ## External doors
  g_Plant <- g_Plant +
    geom_segment(data = D, aes(x = extdoor.X0, xend = extdoor.X1, y = extdoor.Y0, yend = extdoor.Y1),
                 size=3.5, colour = "gray25") +
    geom_segment(data = D, aes(x = extdoor.X0, xend = extdoor.X1, y = extdoor.Y0, yend = extdoor.Y1),
                 size=0.5, colour = "white")
  
  # ## Annotate location
  # Annotate <- data.frame(label = names(tapply(MyPlant$L$coordY, MyPlant$L$location, max)),
  #                        annotateX = unname(tapply(MyPlant$L$coordX, MyPlant$L$location, mean)),
  #                        annotateY = unname(tapply(MyPlant$L$coordY, MyPlant$L$location, max))
  # )
  # 
  # g_Plant <- g_Plant +
  #   geom_text(data = Annotate,
  #             mapping = aes(x = annotateX, y = annotateY, label = label))
  
  return(g_Plant)
  #### END OF FUNCTION
}

##### f_Border() SUB-FUNCTION OF f_plotPlant() FOR PLOTTING - DETERMINE THE COORDINATES OF THE BORDERS (SPACES) #####
f_Border <- function(
    #### INPUT 
  Plant, ## (list with the following elements): food processing plant (output of the function f_createPlant)
  ##  - $L: (data frame) coordinates of all locations
  ##  - $P: (character matrix) processing plant
  ...
  ## OUTPUT
  ## Border (data frame): coordinates of the borders (all spaces)
) {
  L <- subset(Plant$L, border == T)
  L$location <- as.factor(L$location)
  
  ## Border of the spaces (location)
  tapply(L$coordX, INDEX = L$location, FUN = min) %>% names -> location
  
  tapply(L$coordX, INDEX = L$location, FUN = min) %>% unname - 0.5 -> Xmin
  tapply(L$coordX, INDEX = L$location, FUN = max) %>% unname + 0.5 -> Xmax
  
  tapply(L$coordY, INDEX = L$location, FUN = min) %>% unname - 0.5 -> Ymin
  tapply(L$coordY, INDEX = L$location, FUN = max) %>% unname + 0.5 -> Ymax
  
  Border <- data.frame(location = location,
                       Xmin = Xmin,
                       Xmax = Xmax,
                       Ymin = Ymin,
                       Ymax = Ymax)
  
  return(Border)
}

##### f_coordDoor() SUB-FUNCTION OF f_plotPlant() FOR PLOTTING - DETERMINE THE COORDINATES OF THE DOORS (SPACES) #####
f_coordDoor <- function(
    ## INPUT
  prm, ## list of parameters (see the R script "parameters.R")
  L, ## (data frame): coordinates of all spaces (one output of the function f_createPlant())
  ...
  ## OUTPUT
  ## D (data frame): coordinates of all doors
) {
  nbSpaces <- length(prm$Spaces) ## total number of the spaces
  
  D <- data.frame(location = rep(NA, nbSpaces),
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
    
    D$location[i] <- prm$Spaces[[i]]$label ## extract the label of the space i
    
    Lsub <- subset(L, location == prm$Spaces[[i]]$label) ## extract the coordinates of the space i
    
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

##### f_tile_colour() SUB-FUNCTION OF f_plotPlant() FOR PERSONALIZING THE COLOURS OF THE PLANT TILES #####
f_tile_colour <- function(
    P # matrix : processing plant 
) {
  tile_name <- levels(as.factor(P))
  
  tile_colour <- rep("white", length(tile_name)) ## defaults colour : white
  names(tile_colour) <- tile_name
  
  tile_colour["Conveyor1"] <- "seashell2"
  tile_colour["Conveyor2"] <- "seashell2"
  tile_colour["Equipment 1"] <- "seashell3"
  
  return(tile_colour)
}

##### f_plotAgents() FUNCTION TO PLOT THE AGENTS INSIDE A PRE-PLOTTED PLANT #####
f_plotAgents <- function(
    g_emptyPlant, ## empty processing plant (ggplot2 graph object): output of the function f_plotPlant()
    W, ## (data.frame): information of the workers with at least these attributes
    FP = NULL,
    ##  - W_ID (character): worker ID
    ##  - coordX (numeric): coordinates in the X axis of the worker
    ##  - coordY (numeric): coordinates in the X axis of the worker
    ##  - W_state (character/factor): infected/not infected worker 
    ##  - W_mask (character/factor): mask/no mask
    ## time index
    ...
) {
  #### BEGIN OF FUNCTION
  ##
  W$coordX <- as.numeric(W$coordX)
  W$coordY <- as.numeric(W$coordY)
  
  W <- data.frame(W,
                  time_minutes = W$t_ind * 5)
  
  g_Plant <- g_emptyPlant +
    geom_point(data = W,
               mapping = aes(x=coordX, y=coordY, colour=W_status, shape=W_mask,
                             W_ID=W_ID, W_type=W_type, frame=time_minutes),
               size = 3, alpha = 0.5,
               position = position_jitter(width = 0.1, height = 0.1, seed=408))
  
  g_Plant <- g_Plant +
    geom_text(data = W,
              mapping = aes(x=coordX, y=coordY, label=W_ID, frame=time_minutes),
              size = 2,
              position = position_jitter(width = 0.1, height = 0.1, seed=408))
  
  if (!is.null(FP)) {
    FP$FP_coordX <- as.numeric(FP$FP_coordX)
    FP$FP_coordY <- as.numeric(FP$FP_coordY)
    FP <- data.frame(FP,
                     time_minutes = FP$t_ind * 5)
    g_Plant <- g_Plant +
      geom_point(data = FP,
                 mapping = aes(x=FP_coordX, y=FP_coordY, frame=time_minutes),
                 size = 0.8, shape = 15,
                 position = position_jitter(width = 0.3, height = 0.1, seed=408))
  }
  
  return(g_Plant)
  #### END OF FUNCTION
}


##### f_plotWorkers() FUNCTION TO PLOT THE WORKERS INSIDE A PRE-PLOTTED PLANT AT A GIVEN TIME INDEX #####
f_plotWorkers <- function(
    g_emptyPlant, ## empty processing plant (ggplot2 graph object): output of the function f_plotPlant()
    W, ## (data.frame): information of the workers with at least these attributes
    ti,
    ...
) {
  #### BEGIN OF FUNCTION
  ##
  W$coordX <- as.numeric(W$coordX)
  W$coordY <- as.numeric(W$coordY)
  Wti <- subset(W, t_ind == ti)
  Wsub <- subset(Wti, W_active == "active")
  
  g_Plant <- g_emptyPlant +
    geom_point(data = Wsub,
               mapping = aes(x = coordX, y = coordY,
                             colour = W_type,
                             shape = W_shift,
                             W_team = W_team, Week = Week, Weekday = Weekday),
               size = 2.5) +
    scale_shape_manual(name = "Working shift",
                       values=c(1,10,19,13)) + 
    scale_colour_manual(name = "Type of employees",
                        values=c("black", "black", "darkgreen", "darkgreen", "blue", "darkorange"))
  
  ## workers ID as label
  g_Plant <- g_Plant +
    geom_text(data = Wsub,
              mapping = aes(x=coordX, y=coordY, label=W_ID),
              size = 2.5,
              position = position_jitter(width = 0.1, height = 0.5, seed=408))
  
  g_Plant <- g_Plant +
    labs(subtitle = paste("Day ", unique(Wti$Day),
                          " - ", unique(Wti$Weekday),
                          " - ", unique(Wti$Hour), "h", stringr::str_pad(unique(Wti$Min), width=2, pad="0"),
                          sep = ""))
  
  return(g_Plant)
  #### END OF FUNCTION
}

##### f_plotScehedule() FUNCTION TO VISUALISE THE SCHEDULE OF ALL WORKERS #####
f_plotSchedule <- function(
    W,
    Dmin,
    Dmax,
    Dfocus = NULL,
    SHOW_ID = NULL,
    ...
) {
  df <- subset(W, Hour == 0 & Min == 0 & Day >= Dmin & Day <= Dmax)
  
  all_ID <- unique(W$W_ID)
  
  if (!is.null(SHOW_ID)) {
    selected_ID <- all_ID[SHOW_ID]
    df <- subset(df, W_ID %in% selected_ID)
  }
  
  gSchedule <- ggplot() +
    theme(axis.ticks=element_blank(),
          legend.position = "right",
          axis.text.y = element_text(face="bold", size=6),
          axis.text.x = element_text(size=8),
          plot.title = element_text(face="bold", size=16),
          panel.background=element_rect(fill="white"),
          axis.text = element_text(size=16),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank()) +
    geom_line(data = df,
              mapping = aes(x = Day, y = W_ID), colour = "lightgray") +
    geom_point(data = subset(df, W_active == "active"),
               mapping = aes(x = Day, y = W_ID,
                             colour = W_type,
                             shape = W_shift,
                             W_team = W_team, Week = Week, Weekday = Weekday),
               size = 2) +
    geom_vline(xintercept = seq(f_Day2Week(Dmin)*7 -1, Dmax, 7), size=0.5, linetype = "longdash") +
    geom_vline(xintercept = seq(f_Day2Week(Dmin)*7, Dmax, 7),  size=0.5, linetype = "longdash") +
    scale_x_continuous(breaks=seq(Dmin,Dmax,7)) +
    scale_shape_manual(name = "Working shift",
                       values=c(1,10,19,13)) + 
    scale_colour_manual(name = "Type of employees",
                        values=c("black", "black", "darkgreen", "darkgreen", "blue", "darkorange")) +
    labs(x = "Time (day)", y = "Worker ID",
         title = paste("Schedule (", length(unique(df$W_ID)), "/", length(unique(W$W_ID)), " workers shown, days ", Dmin, " to ", Dmax, ")", sep =""))
  
  if (!is.null(Dfocus)) {
    gSchedule <- gSchedule +
      geom_vline(xintercept = c(Dfocus-0.5,Dfocus+0.5),
                 size = 1.2, colour = "darkorange") +
      annotate(geom = "label",
               x = (Dmax+Dmin)/2, y = 15, colour = "darkorange",
               label = f_summaryWorkersAtDay(W = W,
                                             Dfocus = Dfocus,
                                             SHOW = F))
  }
  
  return(gSchedule)
  
}

##### f_summaryWorkersAtDay() FUNCTION TO SHOW SUMMARY SCHEDULE INFORMATION AT A GIVEN DAY #####
f_summaryWorkersAtDay <- function(
    W,
    Dfocus,
    SHOW = T
) {
  dd <- subset(W, Day == Dfocus & Hour == 0 & Min == 0)
  dtA <- subset(dd, W_active == "active" & W_team == "teamA")
  dtB <- subset(dd, W_active == "active" & W_team == "teamB")
  dtT <- subset(dd, W_active == "active" & W_team == "transverse")
  paste("Focus on Day ", Dfocus, " (Week ", unique(dd$Week), "-", unique(dd$Weekday), ")", "\n",
        "Active workers: ", sum(dd$W_active == "active"),"/", length(dd$W_active), "\n",
        "Team A (", unique(dtA$W_shift), "): ",
        sum(dtA$W_type == "cutter1"), " cutter1, ",
        sum(dtA$W_type == "cutter2"), " cutter2, ",
        sum(dtA$W_type == "logistic1"), " logistic1, ",
        sum(dtA$W_type == "logistic2"), " logistic2","\n",
        "Team B (", unique(dtB$W_shift), "): ",
        sum(dtB$W_type == "cutter1"), " cutter1, ",
        sum(dtB$W_type == "cutter2"), " cutter2, ",
        sum(dtB$W_type == "logistic1"), " logistic1, ",
        sum(dtB$W_type == "logistic2"), " logistic2","\n",
        "Transverse workers: ",
        sum(dtT$W_type == "transverse1"), " transverse1 (day shift), ",
        sum(dtT$W_type == "transverse2"), " transverse2 (night shift)",
        sep = "") -> summary_text
  
  if (SHOW == T) {
    cat(summary_text)
  } else return(summary_text)
  
}




##### f_plotOutput() FUNCTION TO PLOT SIMULATION OUTPUT SUMMARY #####
f_plotOutput <- function(
    IL,
    IS,
    seed_select = NULL,
    detailed_plot = F,
    wrap.nrow = 1
) {
  
  tapply(IL$InfectionSource, IL$seed, summary) %>%
    sapply(., FUN = function(x) {
      if (length(x) > 4) {
        return(as.vector(x))
      } else {
        allsourcenames <- c("aerosol", "community", "epidemy", "initialised", "not_infected")
        missingsources <- setdiff(allsourcenames, names(x))
        updatesources <- c(as.vector(x), rep(0, length(missingsources)))
        names(updatesources) <- c(names(x), missingsources)
        return(updatesources)
      } 
    }) %>%
    t() %>%
    as.data.frame() %>%
    select(order(colnames(.)))-> ILF
  ILF <- data.frame(seed = rownames(ILF),
                    ILF)
  rownames(ILF) <- 1:nrow(ILF)
  
  if (!is.null(seed_select)) {
    ISsub <- subset(IS, seed %in% seed_select)
    ILsub <- subset(IL, seed %in% seed_select)
    ILFsub <- subset(ILF, seed %in% seed_select)
  } else {
    ISsub <- IS
    ILsub <- IL
    ILFsub <- ILF
  }
  
  
  
  if (detailed_plot == F) {
    ggplot(data = ISsub) +
      geom_line(aes(x = Day, y = Infected_cumul, group = seed, colour = seed), size = 0.5) +
      theme(axis.ticks=element_blank(),
            #legend.position = "none",
            panel.background=element_rect(fill="white"),
            plot.title = element_text(face="bold", size=15),
            axis.title = element_text(face="bold", size=10),
            axis.text = element_text(size=10),
            panel.grid.major.y=element_line(colour="lightgrey"),
            panel.grid.major.x=element_line(colour="lightgrey"),
            panel.grid.minor.y=element_line(colour="white"),
            panel.grid.minor.x=element_line(colour="lightgrey")) +
      scale_x_continuous(breaks = seq(1, max(IS$Day)+1, by = 7)) +
      scale_y_continuous(breaks = seq(0, max(IS$Infected_cumul)+5, by = 5)) +
      coord_cartesian(ylim = c(0, max(IS$Infected_cumul)+5),
                      xlim = c(0, max(IS$Day)+1)) +
      stat_summary(data = IS, aes(x=Day, y=Infected_cumul), fun = mean, geom="line", size = 2, colour = "black") + 
      labs(title = "Cumulative number of infected workers",
           subtitle = paste(length(unique(ISsub$seed)), "individual curves and the average trend (across", length(unique(IS$seed)), "independent simulations)")) +
      xlab("time (day)") + ylab("number of workers") -> g_Output
  } 
  else {
    
    pal_viridis <- viridis::viridis(10)
    pal_turbo <- viridis::turbo(3, begin = 0.5, end = 0.9)
    
    ggplot(data = ISsub) +
      geom_ribbon(aes(x = Day, ymax = Symptomatic, ymin = 0), fill = pal_viridis[1], alpha = 0.9) +
      geom_ribbon(aes(x = Day, ymin = Symptomatic, ymax = Symptomatic+Asymptomatic), fill = pal_viridis[2], alpha = 0.7) +
      geom_ribbon(aes(x = Day, ymin = Symptomatic+Asymptomatic, ymax = InfectiousPeriod), fill = pal_viridis[3], alpha = 0.5) +
      geom_ribbon(aes(x = Day, ymin = InfectiousPeriod, ymax = InfectiousPeriod + NonInfectious), fill = pal_viridis[4], colour = pal_viridis[4], alpha = 0.95) +
      geom_ribbon(aes(x = Day, ymin = InfectiousPeriod + NonInfectious, ymax = Positive), fill = pal_viridis[5], colour = pal_viridis[5], alpha = 0.65) +
      # geom_ribbon(aes(x = Day, ymin = Positive, ymax = Infected_cumul), fill = pal_viridis[10], colour = "white", alpha = 0.7) +
      geom_line(aes(x = Day, y = Infected_cumul), colour = "black", size = 2.5) +
      geom_line(aes(x = Day, y = Positive), colour = pal_viridis[4], size = 1.5) +
      geom_line(aes(x = Day, y = InfectiousPeriod), colour = pal_viridis[1], size = 1.2) +
      # geom_hline(yintercept = 15, colour = "navyblue", linetype = "dashed") +
      theme(axis.ticks=element_blank(),
            panel.background=element_rect(fill="white"),
            plot.title = element_text(face="bold", size=15),
            axis.title = element_text(face="bold", size=10),
            axis.text = element_text(size=10),
            panel.grid.major.y=element_line(colour="lightgrey"),
            panel.grid.major.x=element_line(colour="lightgrey"),
            panel.grid.minor.y=element_line(colour="white"),
            panel.grid.minor.x=element_line(colour="lightgrey")) +
      geom_rect(data = ILFsub, aes(xmin = max(IS$Day)+0.2, xmax = max(IS$Day)+1.5, ymin = 0, ymax = initialised), fill = "gray30") + ## initially infected
      geom_rect(data = ILFsub, aes(xmin = max(IS$Day)+0.2, xmax = max(IS$Day)+1.5, ymin = initialised, ymax = initialised+aerosol), fill = pal_turbo[1]) +
      geom_rect(data = ILFsub, aes(xmin = max(IS$Day)+0.2, xmax = max(IS$Day)+1.5, ymin = initialised+aerosol, ymax = initialised+aerosol+epidemy), fill = pal_turbo[2]) +
      geom_rect(data = ILFsub, aes(xmin = max(IS$Day)+0.2, xmax = max(IS$Day)+1.5, ymin = initialised+aerosol+epidemy, ymax = initialised+aerosol+epidemy+community), fill = pal_turbo[3]) +
      facet_wrap(. ~ seed, nrow = wrap.nrow) +
      scale_x_continuous(breaks = seq(1, max(IS$Day), by = 7)) +
      scale_y_continuous(breaks = seq(0, max(IS$Infected_cumul), by = 2)) +
      scale_fill_manual(name = "Infection sources",
                        breaks = c("initialised", "aerosol", "epidemy", "community"),
                        values = c("initialised"="gray30", "aerosol"=pal_turbo[1], "epidemy"=pal_turbo[2], "community"=pal_turbo[3])) +
      coord_cartesian(ylim = c(0, max(IS$Infected_cumul))) +
      labs(title = "Evolution of the number of infected workers",
           subtitle = "Cumulative and daily number of workers depending on their sanitary status") +
      xlab("time (day)") + ylab("number of workers") -> g_Output
  }
  
  return(g_Output)
}
