##### en cours: f_initFood() FUNCTION TO INITIALISE A SET OF FOOD PORTIONS #####
f_initFood <- function(
  ## Function allowing to initialize a set of food portions of several types (contact/contaminated...)
  ## depending on probability parameters provided as input argument.
  #### INPUT
  prm, ## (list) the parameters associated with all attributes of the workers with at least the following elements
  ## (check the script "food_parameters.R")
  ##  - $NPortions (integer): the total number of food portions
  ##  - $PortionTypeProb (numeric vector): the probability for different type of portions (contace/contaminated...)
  prm_time,
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
  ### ID of the food portion (value "FP001","FP002", ..)
  FP_ID <- paste("FP", stringr::str_pad(seq(1:prm$NPortions), width=4, pad="0"), sep="")
  
  ### Total number of the time indexes 
  NTime <- prm_time$NDays * 1440 / prm_time$Step ## amplitude Ndays in days, time step in minutes
  t_ind <- rep(0:NTime, each = prm$NPortions)
  
  ## Portions with possible contact with other surfaces: initialized with the probabilities given as model parameters
  FP_contact0 <- sample(c(T,F),
                        size = prm$NPortions, replace=T,
                        prob = c(prm$ContactProb, 1-prm$ContactProb))
  FP_contact <- as.factor(c(FP_contact0, rep(NA, prm$NPortions * NTime)))
  
  # FP_type <- as.factor(sample(c("Possible contact / not contaminated", 
  #                               "Possible contact / contaminated",
  #                               "No possible contact",
  #                               "Loss portion"),
  #                             size = prm$NPortions, replace = T,
  #                             prob = prm$PortionsTypeProv))
  
  ### Initialize coordinates and location of the workers (NA)
  FP_coordX <- rep(NA, prm$NPortions*(NTime+1))
  FP_coordY <- rep(NA, prm$NPortions*(NTime+1))
  FP_location <- rep(NA, prm$NPortions*(NTime+1))
  
  
  FP <- data.frame(FP_ID = rep(FP_ID, NTime+1),
                   FP_contact = FP_contact,
                   FP_coordX = FP_coordX,
                   FP_coordY = FP_coordY,
                   FP_location = FP_location,
                   t_ind = t_ind)
  
  return(FP)
  #### END OF FUNCTION
}
