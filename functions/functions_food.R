##### f_allocCarcassTimeIndex FUNCTION TO ASSIGN THE PROCESS BEGINNING TIME INDEX FOR ALL CARCASSES OF A GIVEN DAY #####
f_allocateCarcassTimeIndex <- function(
  prm_food, ## parameters associated with the food portions (check parameters_food.R)
  prm_workers,  ## parameters associated with the workers (check parameters_workers.R)
  prm_time, ## parameters associated with the simulation time (check parameters_time.R)
  W, ## data.frame containing attributes of all agents "Workers"
  day ## (integer) the given day
) {
  ## The type of meat ("bovine"/"porcine"/"veal"/"lamb")
  meat <- prm_food$meat
  
  writeLines(">>> Indexing the carcasses depending on the total number of active cutters")
  ## Total number of active cutters
  filter(W, Day == day, W_active == "active", W_type == "cutter2")$W_ID %>%
    unique() %>%
    length() -> NW
  
  ## Total daily number of carcasses to be processed depending on the total active number of cutters 2
  Ncarcass <- NW * prm_food$worker_rhythm[[meat]][["cutter2"]] * 60 * 8 # 60 min per hour and 8 hours of cutting per worker
  
  ## Average duration for each carcass (inverse of the worker_rhythm)
  duration <- round(1 / prm_food$worker_rhythm[[meat]])
  
  ## Rounding (ceiling to time step) the above duration and convert to number of indexes of each step
  duration_ti <- plyr::round_any(duration, accuracy = prm_time$Step, f = ceiling) / prm_time$Step
  
  ## The time index corresponding to the last step of the last cut
  end_process_ti <- f_convertTime(method = "time2ind", dt = prm_time$Step,
                                  D = day, H = prm_time$time_cut_end[["H"]], M = prm_time$time_cut_end[["M"]])
  
  ## The time index corresponding to the beginning of the last cut of the day
  last_cut_begin_ti <- end_process_ti - sum(duration_ti)
  
  ## The time index corresponding to the beginning of the first cut of the day
  first_cut_begin_ti <- f_convertTime(method = "time2ind", dt = prm_time$Step,
                                      D = day, H = prm_time$time_cut_begin[["H"]], M = prm_time$time_cut_begin[["M"]])
  
  ## The total number of batches of the day (one batch per time step)
  Nbatch <- last_cut_begin_ti - first_cut_begin_ti + 1
  
  ## the number of carcass per group
  Ncperbatch <- round(Ncarcass / Nbatch)
  
  # ## the duration amplitude (expressed in minutes) from the begin to the end of the cutting process
  # process_duration <- prm_time$time_cut_end["H"]*60 + prm_time$time_cut_end["M"] - (prm_time$time_cut_begin["H"]*60 + prm_time$time_cut_begin["M"])
  # process_duration %>% 
  #   plyr::round_any(., accuracy = prm_time$Step, f=round) %>% ## rounding durations with the simulation step
  #   unname(.) -> process_duration
  # 
  # ## the average duration for each batch of carcasses
  # prm_food$duration[[meat]] %>% 
  #   plyr::round_any(., accuracy = prm_time$Step, f=round) %>% ## rounding durations with the simulation step
  #   sum(.)  -> batch_duration
  # 
  # ## the total daily number of batches
  # Nbatch <- round(process_duration/batch_duration)
  # 

  # 
  # ## the first time index of the cutting processing (arrival of the first batch of carcasses)
  # f_convertTime(D=day, H=prm_time$time_cut_begin["H"], M=prm_time$time_cut_begin["M"], dt=prm_time$Step) %>%
  #   unname() -> t_ind_cut_begin
  # 
  # ## number of time index per batch
  # Ntiperbatch <- batch_duration / prm_time$Step
  # 
  # ti_vec <- seq(from = t_ind_cut_begin,
  #               by = Ntiperbatch,
  #               length.out = Nbatch)
  # 
  ## allocate the carcasses to different batches based on the average number of carcasses per batch
  carcass_allocation <- data.frame(batch_ID = rep(1:Nbatch, each=Ncperbatch),
                                   carcass_ID = 1:(Nbatch*Ncperbatch),
                                   cut_t_ind = rep(first_cut_begin_ti:last_cut_begin_ti, each = Ncperbatch))

  ## retain only the expected total number of carcasses
  carcass_allocation <- subset(carcass_allocation, carcass_ID <= Ncarcass)

  carcass_allocation$carcass_ID <- str_pad(carcass_allocation$carcass_ID, width = 4, pad = "0")
  
  carcass_allocation <- mutate(carcass_allocation, cut_t_ind_end = cut_t_ind + sum(duration_ti))
  
  ## OUTPUT
  return(list(carcass_allocation = carcass_allocation,
              duration_ti = duration_ti))
  ##### END OF FUNCTION
}


##### en cours: f_ProcessFood() FUNCTION TO INITIALISE THE AGENTS "FOOD PORTIONS" #####
f_ProcessFood <- function(
  ## Function allowing to initialize a set of food portions at a given day
  ## depending on food parameters provided as input argument.
  #### INPUT
  plant,
  prm_food, ## (list) parameters associated with all attributes of the food portions (check Parms_Food.R)
  prm_workers, ## (list) parameters associated with all attributes of the workers (check Parms_Workers.R)
  prm_time, ## (list) parameters associated with the simulation time (check Parms_Time.R)
  W, ## data.frame containing attributes of all agents "Workers" 
  day ## the considered day
  #### OUTPUT
  ## FP (dataframe): the initialized food portions
  ##  - $FP_ID (character/string): the ID of each food portion (e.g.: "B_01_025_0001", ... )
  ##  - $coordX (integer): coordinates of the food portions in the plant (initialized as NA)
  ##  - $coordY (integer): coordinates of the food portions in the plant (initialized as NA)
  ##  - $location (character): location of the food portions ("Arrival gate", "Waste area"...) (initialized as NA)
  ##  - $viral_load (numeric): total viral quantity (virions) present on the food portions (initialized as NA)
) {
  #### BEGIN OF FUNCTION
  writeLines(paste("***** Processing food portions - Day ", day, " *****", sep = ""))
  
  ## Allocate all the carcasses of the day to different time indexes (check the function f_allocate)
  output_alloc <- f_allocateCarcassTimeIndex(prm_food, prm_workers, prm_time, W, day)
  
  c_alloc <- output_alloc$carcass_allocation
  
  ## the type of meat
  meat <- prm_food$meat

  ## the number of portion per carcass
  N_FP_perCarcass <- round(prm_food$carcass_weight[meat] * prm_food$net_weight_ratio[meat] / prm_food$CSU[meat])
  
  ## Initialize the food portion data frame
  writeLines(">>> Processing time indexes for each carcass and food portion")
  apply(c_alloc, 1, FUN = function(x) { # for each carcass
    expand.grid(x[["carcass_ID"]],
                str_pad(1:N_FP_perCarcass, width=3, pad = "0")) %>% 
      `colnames<-`(., c("carcass", "portion")) -> FP_IDx
    
    FP_IDx <- paste(str_to_upper(str_sub(prm_food$meat, 1, 1)), # meat
                    str_pad(day, width=2, pad="0"), # day
                    FP_IDx$carcass, # carcass number
                    FP_IDx$portion,  # portion number
                    sep = "_")
    
    return(expand.grid(FP_IDx, x[["cut_t_ind"]]:x[["cut_t_ind_end"]]))
    
  }) %>%
    data.table::rbindlist(.) %>%
    `colnames<-`(., c("FP_ID", "t_ind")) %>%
    arrange(., t_ind, FP_ID) %>% ## arrange by time indices then portion IDs
    tibble::add_column(coordX = NA, ## X coordinates of the portion
                       coordY = NA, ## Y coordinates of the portion
                       location = NA, ## location name ("office", "conveyor1", "conveyor2"...)
                       #contact = NA, ## (logical) indicates if the portion could be in contact with other surfaces or not
                       RNA_load = 0  ## the viral load (RNA copies) present on the portion
    ) %>%
    mutate(., carcass_ID = substr(FP_ID, 6,9), .after = FP_ID) -> FP
  
  ## Circulation of the carcasses inside the processing plant
  duration_ti <- output_alloc$duration_ti

  writeLines(">>> Circulation of the carcasses between different locations inside the plant")
  pb = txtProgressBar(min = min(as.numeric(c_alloc$carcass_ID)),
                      max = max(as.numeric(c_alloc$carcass_ID)),
                      initial = min(as.numeric(c_alloc$carcass_ID)),
                      char = "-", width = 50, style = 3) 
  
  apply(c_alloc, 1, FUN = function(x) {
    setTxtProgressBar(pb, as.numeric(x[["carcass_ID"]]))
    FP1 <- subset(FP, carcass_ID == x[["carcass_ID"]])
    
    t_ind <- as.numeric(x[["cut_t_ind"]])
    
    ### TIME INDEX FOR EACH STEP OF THE CUTTING PROCESS
    ti <- c("logistic1" = t_ind,
            "cutter1" = t_ind + duration_ti[["logistic1"]],
            "cutter2" = t_ind + sum(duration_ti[c("logistic1", "cutter1")]),
            "logistic2" = t_ind + sum(duration_ti[c("logistic1", "cutter1", "cutter2")]),
            "storage" = t_ind + sum(duration_ti[c("logistic1", "cutter1", "cutter2", "logistic2")]))
    
    FP1$location[FP1$t_ind %in% ti[["logistic1"]]:(ti[["cutter1"]]-1)] <- "Arrival gate"
    FP1$location[FP1$t_ind %in% ti[["cutter1"]]:(ti[["cutter2"]]-1)] <- "Conveyor1"
    FP1$location[FP1$t_ind %in% ti[["cutter2"]]:(ti[["logistic2"]]-1)] <- "Conveyor2"
    FP1$location[FP1$t_ind %in% ti[["logistic2"]]:(ti[["storage"]]-1)] <- "Equipment 1"
    FP1$location[FP1$t_ind %in% ti[["storage"]]] <- "Cooling area"
    
    return(FP1)
    
  }) %>%
    data.table::rbindlist(.) -> FP
  close(pb)
  
  
  writeLines(">>> Circulation of the meat portions between different X-Y coordinates")
  lapply(unique(FP$location), FUN = function(l) { ## for each location l inside the plant 
    
    FPl <- subset(FP, location == l) ## extract the Food Portion data of the day associated with the location l
    l_coords <- subset(plant$L, location == l) ## extract the X,Y coordinates of the location l
    
    ## sampling random coordinates for each portion  
    randomcoords <- l_coords[sample(1:nrow(l_coords), size = nrow(FPl), replace = T),]
    
    FPl$coordX <- randomcoords$coordX
    FPl$coordY <- randomcoords$coordY
    
    return(FPl)
  }) %>%
    data.table::rbindlist(.) %>%
    arrange(., FP_ID, t_ind) %>%
    mutate(., coords_ID = paste("S",
                                str_pad(coordX, width=2, pad="0"),
                                str_pad(coordY, width = 2, pad = "0"),
                                sep = "_"),
           .before = "coordX") -> FP

  return(FP)
  #### END OF FUNCTION
}
