##### f_initAir : Function to initialize the Air in each room #####
f_initAir <- function(
  ## Function initializing the Air of each room. 
  ## In each Air Agent, the cenctration of each droplet size class is defined
  #### INPUT
  prm_plant, ## (list) the parameters associated with all attributes of the workers with at least the following elements
  ## (check the script "parameters_workers.R")
  ##  - $NWorkers (integer), ## total number of the workers during the entire process
  ##  - $InfectProb (numeric), ## probability of infected people among the workers
  ##  - $MaskWearingProb (numeric) ## probability of people wearing a mask
  prm_time, ## (list) the parameters associated with the timetable
  ## (check the script "parameters_time.R")
  ##  - NDays, ## total number of days during the entire process
  ##  - Step ## time step (in minutes)
  prm_air ## (list) The parameter associated with the air parameters
  #### OUTPUT
  ## AIR (dataframe): the initialised "workers"Air Agent" of each rooms. 
  ##  - $AIR_ID (character/string): the ID of each room ("entry hall","WC",...)
  ##  - $d01, $d02 ... ID : Droplet Class, number of droplet in each class over time
  ##  - t_ind, time index

) { 
  #### BEGIN OF FUNCTION
  ### ID of the air in each space (including the cutting room)
  AIR_ID <- c(prm_plant$label,
              unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$label)))))
  
  ### Names of the different droplet size classes (d01, d02,...)
  AIR_dclass <<- paste("d", stringr::str_pad(seq(1:length(prm_air$Droplet_class)), width=2, pad="0"), sep="")
  
  ### Total number of the time indexes 
  NTime <- prm_time$NDays * 1440 / prm_time$Step ## amplitude Ndays in days, time step in minutes
  t_ind <- rep(0:NTime, each = length(prm_plant$Spaces)+1) # +1 for the cutting room not included as 'Spaces'
  
  ### OUTPUT
  AIR <- data.frame(AIR_ID = rep(AIR_ID, NTime+1),
                    setNames(data.frame(matrix(ncol = length(AIR_dclass), 
                                               nrow = length(AIR_ID)*(NTime+1))),
                                               AIR_dclass),
                    t_ind = t_ind)
  
  ## Determine if the different classes of droplets stay in aerosol or fall in surfaces
  Method_calc <<- f_Air_Criteria_Calc(prm_plant = prm_plant, prm_air = prm_air)
  
  ## Assign values of 0 for the first time index (required for the first run of f_Module_Master)
  AIR[AIR$t_ind==0, 2:(1+length(prm_air$Droplet_class))] = matrix(0, nrow = length(AIR_ID), ncol = length(AIR_dclass)) * Method_calc
  
  ##### UNCHANGED VALUES STORED IN GLOBAL ENVIRONMENT ####
  # Surfaces of the room (m2)
  S_rooms <<- c(prm_plant$dim.X * prm_plant$dim.Y, # surface of the cutting room
               unname(unlist(lapply(prm_plant$Spaces, function(x) return(x$dim.X * x$dim.Y)))))
  
  # Volume of the room (m3) 
  V_rooms <<- c(prm_plant$dim.X * prm_plant$dim.Y * prm_plant$dim.Z, # volume of the cutting room
               unname(unlist(lapply(prm_plant$Spaces, function(x) return(x$dim.X * x$dim.Y * x$dim.Z)))))
  
  # total number of rooms (spaces) including the cutting room
  N_rooms <<- length(Spaces_label)
  
  # Airflow rate of air renewal (m3/min) 
  # CAREFULL IF CHANGE UNITS (m3/h into m3/s) IN THE PARMS_PLANT (DEFAULTS VALUES EXPRESSED IN m3/h, check parameters_air.R) 
  V_renew <<- c(prm_plant$Air_renewal,
               unname(unlist(lapply(prm_plant$Spaces, function(x) return(x$Air_renewal)))))/ 60 
  
  return(AIR)
  #### END OF FUNCTION
}


##### f_Vsed - Function to calculate the sedimentation velocity of a water sphere in air #####
f_Vsed <- function(
  # Double-Checked !!! fonction verified and results compared to Kennedy et al. 
  # Calculate the sedimentation velocity of a sphere of water in the air
  # According to Stokes law
  # INPUT
  prm_air #(list) Parameters associated with the air characteristics
  # OUtput
  #  Vsed (m.s-1) (value, vector) sedimentation velocity for each droplet class
) {
  g = 9.81 # (m².S-1) -  Gravity
  mu = 1.77*10^(-5) # (Pas.s) - Dynamic viscosity at 10 C
  
  # - 0 C ->1.72e-05 (for later if ever we want to take the T°C on air props... )
  # - 5 ->1.75e-05
  # - 15 C ->1.79e-05
  
  rho_eau = 1000 # (kg.m-3) - Water density at 10 C
  rho_air = 1.24 # (kg.m-3) -Water density at 10 C 1.29 a 0 C 1.22*15 
  
  return((prm_air$Droplet_class*10^(-6))^2*g*(rho_eau-rho_air)/(18*mu)) # Stokes'law 
  #### END OF FUNCTION
}

##### f_sed_time - Function to calculate the sedimentation time in each room #####
f_sed_time <- function(
  prm_plant,
  prm_air
) {
  # # Sedimentation Velocity
  Vsed <<- matrix(f_Vsed(prm_air),nrow=1) # (m.s-1) 
  
  # Height of each room 
  H_room <- matrix(c(prm_plant$dim.Z,
                    unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$dim.Z))))))
  
  return(H_room%*%(Vsed^-1))
}

##### f_circ_time - Function to calculate the circulation time of droplets through each space #####
f_circ_time <- function(
  prm_plant
) {
  ## Volume of each space (including the cutting room)
  V_rooms <- c(prm_plant$dim.X*prm_plant$dim.Y*prm_plant$dim.Z,
              unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$dim.X*x$dim.Y*x$dim.Z)))))
  
  AirflowRate <- c(prm_plant$AirflowRate,
              unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$AirflowRate)))))
  
  return(V_rooms/(AirflowRate/3600))
}

##### f_Air_Criteria_Calc - Function to determine if the different droplet classes stay in aerosol or not #####
f_Air_Criteria_Calc <- function(
  # Calculations based on Kenedy et al (2020)
  prm_plant,
  prm_air
) {
  # call function for sedimentation time calculation
  sed_time <- f_sed_time(prm_plant,prm_air)  ## en sec ????????????
  
  # Call function for circulation time calculation
  circ_time <- f_circ_time(prm_plant) ## en heure ????????????
  circ_time_mat <- matrix(rep(circ_time,ncol(sed_time)),ncol = ncol(sed_time))
  
  return(circ_time_mat/sed_time<0.1)
}

# f_drop_conc_evol <- function (prm_plant,prm_air) # ... to be continued ... 
# {  
#   
#   Method_calc <<- f_Air_Criteria_Calc(Parms_Plant,Parms_Air)
#   
#   
#   
# Method <- c(prm_plant$Air_Cond_Type,
#             unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$Air_Cond_Type)))))}



