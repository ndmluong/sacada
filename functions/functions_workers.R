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
  ##  - $NWorkers (integer), ## total number of the workers during the entire process
  ##  - $InfectProb (numeric), ## probability of infected people among the workers
  ##  - $MaskWearingProb (numeric) ## probability of people wearing a mask
  prm_time, ## (list) the parameters associated with the timetable
  ## (check the script "parameters_time.R")
  ##  - NDays, ## total number of days during the entire process
  ##  - Step ## time step (in minutes)
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
  # Time index 0
  W_mask0 <- sample(names(prm$pMask),
                    size = prm$NWorkers, replace = T,
                    prob = prm$pMask)
  # Initialization for the subsequent time indexes as NA
  W_mask <- as.factor(c(W_mask0, rep(NA, prm$NWorkers * NTime)))
  
  
  ### Workers with a fixed or mobile work space
  # Time index 0
  W_type0 <- sample(names(prm$pType),
                    size = prm$NWorkers, replace = T,
                    prob = prm$pType)
  # Initialization for the subsequent time indexes as NA
  W_type <- as.factor(c(W_type0, rep(NA, prm$NWorkers*NTime)))
  
  
  ### Initialize coordinates and location of the workers (NA)
  W_coordX <- rep(NA, prm$NWorkers*(NTime+1))
  W_coordY <- rep(NA, prm$NWorkers*(NTime+1))
  W_location <- rep(NA, prm$NWorkers*(NTime+1))
  
  ### Initialize the working (or not-working phase) of the workers (NA)
  # Time index 0
  W_active0 <- sample(names(prm$pActive),
                      size = prm$NWorkers, replace = T,
                      prob = prm$pActive)
  # Initialization for the subsequent time indexes as NA
  W_active <- as.factor(c(W_active0, rep(NA, prm$NWorkers*NTime)))
  
  ### Community activities
  # The workers with/without community activities 
  W_communityActivity <- sample(x = c(T,F),
                                size = prm$NWorkers, replace = T,
                                prob = c(prm$pCommunityActivity, 1-prm$pCommunityActivity))
  ## Assign the workers to the different communes if applicable
  # Total number of communes (groups) = round (total number with activities divided by the average number of workers per commune)
  NCommunes <- round(length(which(W_communityActivity)) / prm$N_perCommunityActivity)
  # Initialize the communes names 
  Communes <- paste("Com", stringr::str_pad(seq(1:NCommunes), width=3, pad="0"), sep="")
  
  ## Communes of each worker
  W_communes <- rep(NA, prm$NWorkers)
  workers_index <- sample(which(W_communityActivity)) ## randomize the indexes of the workers with community activities
  
  for (ic in 1:NCommunes) {
    ind_begin <- ic * prm$N_perCommunityActivity - (prm$N_perCommunityActivity - 1)
    ind_end <- ic * prm$N_perCommunityActivity
    W_communes[workers_index[ind_begin:ind_end]] <- Communes[ic]
  }

  
  ### Output
  W <- data.frame(W_ID = rep(W_ID, NTime+1),
                  W_status = W_status,
                  W_mask = W_mask,
                  W_type = W_type,
                  W_active = W_active,
                  W_communityActivity = rep(W_communityActivity, NTime+1),
                  W_communes = rep(W_communes, NTime+1),
                  W_coordX = W_coordX,
                  W_coordY = W_coordY,
                  W_location = W_location,
                  t_ind = t_ind
  )
  
  return(W)
  #### END OF FUNCTION
}
