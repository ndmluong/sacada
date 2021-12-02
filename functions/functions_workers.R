##### PACKAGES #####
library(ggplot2)
library(reshape2)
library(plotly)
library(stringr)

##### f_initWorkers() FUNCTION TO INITIALISE A SET OF WORKERS #####
f_initWorkers <- function(
  ## Function allowing to initialize a set of workers with different infection states and mask behaviour
  ## depending on probability parameters provided as input argument.
  #### INPUT
  prm, ## (list) the parameters associated with all attributes of the workers with at least the following elements
  ## (check the script "parameters_workers.R")
  prm_time, ## (list) the parameters associated with the timetable
  ## (check the script "parameters_time.R")
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
  ### ID of the workers (value "W001","W002", ..)
  W_ID <- paste("W", stringr::str_pad(seq(1:prm$NWorkers), width=3, pad="0"), sep="")
  
  ### Total number of the time indexes 
  NTime <- prm_time$NDays * 1440 / prm_time$Step ## amplitude Ndays in days, time step in minutes
  t_ind <- rep(0:NTime, each = prm$NWorkers)
  
  ### States (infected/not infected) of the workers
  # Time index 0:
  # HYPOTHESE : ONE INDIVIDUAL CONTAMINATED AT DAY 0 ?
  W_status0 <- rep("not contaminated", prm$NWorkers)
  W_status0[sample(1:prm$NWorkers, 1)] <- "contaminated"
  # Initialization for the subsequent time indexes as NA
  W_status <- as.factor(c(W_status0, rep(NA, prm$NWorkers * NTime)))
  
  ### Behavior (mask/no mask) of the workers
  # At the time index 0
  # The proportion of workers wearing a mask depending on the type of mask
  pMask <- prm$pMaskAcceptability[prm$MaskType] %>% unname
  
  # Wearing mask or not for each worker
  W_mask0 <- sample(c("mask", "no mask"), replace = T,
                    size = prm$NWorkers,
                    prob = c(pMask,1-pMask))
  # Initialization for the subsequent time indexes as NA
  W_mask <- as.factor(c(W_mask0, rep(NA, prm$NWorkers * NTime)))
  
  ### Workers with a fixed or mobile work space
  # Time index 0
  W_type0 <- sample(names(prm$pType),
                    size = prm$NWorkers, replace = T,
                    prob = prm$pType)
  # Replicate the type of workers for all subsequent time indexes
  #W_type <- as.factor(c(W_type0, rep(NA, prm$NWorkers*NTime)))
  W_type <- as.factor(rep(W_type0, NTime+1)) ## + 1 (including the time index 0)
  
  ### Initialize coordinates and location of the workers (NA)
  W_coordX <- rep(NA, prm$NWorkers*(NTime+1))
  W_coordY <- rep(NA, prm$NWorkers*(NTime+1))
  W_location <- rep(NA, prm$NWorkers*(NTime+1))
  
  ### Initialize the working (or not-working phase) of the workers (NA)
  W_active <- rep(NA, prm$NWorkers*(NTime+1))
  W_activeCounter <- rep(NA, prm$NWorkers*(NTime+1))
  
  ## Community activities
  ## Assign the workers to the different communes if applicable
  AssignedCommunity <- f_assignCommunes(prm) ## check the (sub)function f_assignCommunes()
  
  W_communityActivity <- AssignedCommunity$W_communityActivity0 %>% rep(NTime+1) 
  W_communes <- AssignedCommunity$W_communes0 %>% rep(NTime+1) %>% as.factor
  
  ### Output
  W <- data.frame(W_ID = rep(W_ID, NTime+1),
                  W_status = W_status,
                  W_mask = W_mask,
                  W_type = W_type,
                  W_active = W_active,
                  W_activeCounter = W_activeCounter,
                  W_communityActivity = W_communityActivity,
                  W_communes = W_communes,
                  W_coordX = W_coordX,
                  W_coordY = W_coordY,
                  W_location = W_location,
                  t_ind = t_ind
  )
  
  return(W)
  #### END OF FUNCTION
}

##### f_assignCommunes() SUB-FUNCTION TO ASSIGN WORKERS TO DIFFERENT COMMUNES #####
f_assignCommunes <- function(
  prm ## (list) the parameters associated with all attributes of the workers with at least the following elements
  ## (check the script "parameters_workers.R")
) {
  #### BEGIN OF FUNCTION
  ### Community activities
  # The workers with/without community activities 
  W_communityActivity0 <- sample(x = c(T,F),
                                 size = prm$NWorkers, replace = T,
                                 prob = c(prm$pCommunityActivity, 1-prm$pCommunityActivity))
  
  ## Assign the workers to the different communes if applicable
  # Total number of communes (groups) depending on the total number with activities and the average number of workers per commune
  # The number of communes
  NfullCommunes <- length(which(W_communityActivity0)) %/% prm$N_perCommunityActivity
  leftover <- length(which(W_communityActivity0)) %% prm$N_perCommunityActivity
  
  if (leftover <= 1) {
    LastCommunes <- NfullCommunes
  } else {
    LastCommunes <- NfullCommunes + 1
  }
  
  # Initialize the communes names 
  Communes0 <- paste("Com", stringr::str_pad(seq(1:LastCommunes), width=3, pad="0"), sep="")
  
  ## Communes of each worker
  W_communes0 <- rep(NA, prm$NWorkers)
  workers_index <- sample(which(W_communityActivity0)) ## randomize the indexes of the workers with community activities
  
  for (ic in 1:NfullCommunes) {
    ind_begin <- ic * prm$N_perCommunityActivity - (prm$N_perCommunityActivity - 1)
    ind_end <- ic * prm$N_perCommunityActivity
    W_communes0[workers_index[ind_begin:ind_end]] <- Communes0[ic]
  }
  
  if (LastCommunes > NfullCommunes) { ## s'il reste au moins 2 personnes en trop
    ind_begin <- LastCommunes * prm$N_perCommunityActivity - (prm$N_perCommunityActivity - 1)
    ind_end <- ind_begin + leftover - 1
    W_communes0[workers_index[ind_begin:ind_end]] <- Communes0[LastCommunes]
  } else {
    if (leftover == 1) {
      LastWorkers <- LastCommunes * prm$N_perCommunityActivity + 1
      W_communes0[workers_index[LastWorkers]] <- Communes0[LastCommunes]
    }
  }
  
  AssignedCommunity <- data.frame(W_communityActivity0 = W_communityActivity0,
                                  W_communes0 = W_communes0)
  
  return(AssignedCommunity)
  #### END OF FUNCTION
}
