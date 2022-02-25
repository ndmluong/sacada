##### USER DEFINED PARAMETERS FOR FOOD PORTIONS #####
Parms_Food <- list(
  NPortions = 150,
  ## Initial proportion
  ContactProb = 0.15, ## possible contact. The portion without possible contact = 1 - possible contact
  LossProb = 0.15, ## loss portion
  ## the meat type used for the simulation
  meat = "bovine", 
  
  ## daily number of treated carcasses by the entire processing plant
  Ncarcass_daily = c("bovine" = 50, # /!\ ASSUMPTIONS
                     "porcine" = 100 # /!\ ASSUMPTIONS
                     ), 
  
  ## weight (in kg) of each carcass 
  carcass_weight = c("bovine" = 166, # weight AFTER MATURATION (ref. chambre-agriculture.fr)
                     "porcine" = 100 # /!\ ASSUMPTIONS
                     ), 
  
  ## net weight (in kg) of the marketable portions (fat portions, bones and wastes excluded)
  net_weight = c("bovine" = 110, # (ref. chambre-agriculture.fr)
                 "porcine" = 80 # /!\ ASSUMPTIONS
                 ),
  
  ## the average weight of each final meat portion (in kg)
  CSU = c("bovine" = 1, # /!\ assumption (according to documents from chambre-agriculture.fr)
          "porcine" = 0.8 # /!\ ASSUMPTIONS
          ),
  
  duration = list()
  ## agents FP: ID, coordX, coordY, location, nb_droplets (conta), t_ind, provenance (carcasse)
  ## agents S: ID, coordX, coordY, location, nb_droplets (conta), t_ind
  # 
  # ## Initial proportion
  # ContactProb = 0.15, ## possible contact. The portion without possible contact = 1 - possible contact
  # LossProb = 0.15 ## loss portion
)
