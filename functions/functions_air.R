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
  ##### OUTPUT 
  # Evolution of the concentration of droplets classes over time in every room 
  # MyAir (list)  rows -> Air_ID, it corresponds to one air agent for each room
  #               Colums ->  drolet classes (depends on droplet size)
  #               time   -> time ind : time index

){ 
# prm = Parms_Plant
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
  AIR_ID <- c(prm$label,
              unname(unlist(lapply(prm$Spaces, function (x) return(x$label)))))
  
  ### Droplet  classes d
  AIR_dclass <- paste("d", stringr::str_pad(seq(1:length(prm_air$Droplet_class)), width=2, pad="0"), sep="")
  ### Total number of the time indexes 
  NTime <- prm_time$NDays * 1440 / prm_time$Step ## amplitude Ndays in days, time step in minutes
  t_ind <- rep(0:NTime, each = length(prm$Spaces)+1) # +1 for the cutting room not included as 'Spaces'
  
  ### Output
  AIR <- data.frame(AIR_ID = rep(AIR_ID, NTime+1),
                    setNames(data.frame(matrix(ncol = length(AIR_dclass), 
                                               nrow = length(AIR_ID)*(NTime+1))),
                                               AIR_dclass),
                    t_ind = t_ind)
  return(AIR)
  #### END OF FUNCTION

}

f_Vsed <- function(
#############################################################################
# Calculates the sedimentation velocity of a sphere of water in the air
# According to Stokes law
# INPUT
  prm_air #(list) Parameters associated with the air characteristics
  # OUtput
  #  Vsed (m.s-1) (value, vector) sedimentation velocity for each droplet class
  ){
  # prm = Parms_Air  
  
  g = 9.81 # (m².S-1) -  Gravity
  mu = 1.76*10^(-5) # (Pas.s) - Dynamic viscosity at 10°C
  rho_eau = 1000 # (kg.m-3) - Water density
  rho_air = 1.25 # (kg.m-3) -Water density 

return(2*(prm_air$Droplet_class*10^(-6))^2*g*(rho_eau-rho_air)/(9*mu)) # Stokes'law
#### END OF FUNCTION
}

f_sed_time <-function(prm_plant, prm_air)
  {
  # prm_air = Parms_Air
  # prm_plant = Parms_Plant
  # Sedimentation Velocity
  Vsed <- matrix(f_Vsed(prm_air),nrow=1) # (m.s-1) 
  # Height of each room 
  H_room <- matrix(c(prm_plant$dim.Z,
                    unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$dim.Z))))))
   
  return(aperm(H_room%*%(Vsed^-1)))
}

f_circ_time <- function (prm_plant){
  V_rooms <- c(prm_plant$dim.X*prm_plant$dim.Y*prm_plant$dim.Z,
              unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$dim.X*x$dim.Y*x$dim.Z)))))
  AirflowRate <- c(prm_plant$AirflowRate,
              unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$AirflowRate)))))
    return(V_rooms/(AirflowRate/3600))
  }

f_Air_Criteria_Calc <-function(prm_plant, prm_air)
  # Calculations based on Kenedy et al (2020)
  # prm_air = Parms_Air
  # prm_plant = Parms_Plant
  {
  # call function for sedimentation time calculation
  sed_time <- f_sed_time(prm_plant,prm_air)  ## en sec ????????????
  # Call function for circulation time calculation
  circ_time <- f_circ_time(prm_plant) ## en heure ????????????
  circ_time_mat <- t(matrix(rep(circ_time,nrow(sed_time)),ncol = nrow(sed_time)))
  
  return(circ_time_mat/sed_time<1)
}
 
f_drop_conc_evol <- function (prm_plant,prm_air) # ... to be continued ... 
{  
  
  Method_calc <- f_Air_Criteria_Calc(Parms_Plant,Parms_Air)
  
  
  
Method <- c(prm_plant$Air_Cond_Type,
            unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$Air_Cond_Type)))))}



