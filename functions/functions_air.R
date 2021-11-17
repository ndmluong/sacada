### Functions used in the air modules

f_initAir <- function(
  ##############################################################################
  ## Function initializing the "Air Agent" of each room. 
  ## In each Air Agent, the cenctration of each droplet size class is defined
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
  prm_air ## (list) The parameter associated with the air parameters

){
prm = Parms_Plant
# prm_time = Parms_Time
# prm_air = Parms_Air

  #### OUTPUT
  ## AIR (dataframe): the initialised "workers"Air Agent" of each rooms. 
  ##  - $AIR_ID (character/string): the ID of each room ("entry hall","WC",...)
  ##  - $d01, $d02 ... ID : Droplet Class, number of droplet in each class over time
  ##  - t_ind, time index
  ##############################################################################  
  #### BEGIN OF FUNCTION
  ##############################################################################
  ### ID of each rooms
  # AIR_ID <- names(prm$Spaces)
  AIR_ID <- unname(unlist(lapply(prm$Spaces, function (x) return(x$label))))
  
  ### Droplet  classes d
  AIR_dclass <- paste("d", stringr::str_pad(seq(1:length(prm_air$Droplet_class)), width=2, pad="0"), sep="")
  ### Total number of the time indexes 
  NTime <- prm_time$NDays * 1440 / prm_time$Step ## amplitude Ndays in days, time step in minutes
  t_ind <- rep(0:NTime, each = length(prm$Spaces))
  
  ### Output
  AIR <- data.frame(AIR_ID = rep(AIR_ID, NTime+1),
                    setNames(data.frame(matrix(ncol = length(AIR_dclass), 
                                               nrow = length(AIR_ID)*(NTime+1))),
                                               AIR_dclass),
                    t_ind = t_ind)
  return(AIR)
  #### END OF FUNCTION

}