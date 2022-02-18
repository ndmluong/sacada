##### USER DEFINED PARAMETERS FOR FOOD PORTIONS #####
Parms_Food <- list(
  NPortions = 150,
  
  r_NW_nVol = 100,
  # prod_volume = 5000, #kg
  weight = 0.1, # kg CSU 
  
  ## agents FP: ID, coordX, coordY, location, nb_droplets (conta), t_ind, provenance (carcasse)
  ## agents S: ID, coordX, coordY, location, nb_droplets (conta), t_ind
  
  ## Initial proportion
  ContactProb = 0.15, ## possible contact. The portion without possible contact = 1 - possible contact
  LossProb = 0.15 ## loss portion
)