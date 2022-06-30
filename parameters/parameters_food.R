##### USER DEFINED PARAMETERS FOR FOOD PORTIONS #####
Parms_Food <- list(
  ## the meat type used for the simulation
  meat = "porcine", 
  
  ## working rhythm of each type of workers (ratio) expressed by the average number of carcasses processed by each worker per minutes
  worker_rhythm = list( 
    porcine = c("logistic1" = 0.025, # transfer the equivalent of one carcass per minute from the arrival gate to the conveyor
                "cutter1" = 0.05, # cut the equivalent 20% of one carcass per minute, one entire carcass every 5 minutes (assumption/experts) 
                "cutter2" = 0.025,# cut equivalent 10% of one carcass per minute, half-carcass every 5 minutes (assumption/experts) 
                "logistic2" = 0.125), # process (e.g.) and transfer the equivalent of a half-carcass per minute to the storage area
    bovine = c("logistic1" = 1, # assumptions / experts
               "cutter1" = 0.2, # assumptions / experts
               "cutter2" = 0.2,# assumptions / experts
               "logistic2" = 2), # assumptions / experts
    veal = c("logistic1" = 1, # transfer the equivalent of one carcass per minute from the arrival gate to the conveyor
             "cutter1" = 0.2, # cut the equivalent 20% of one carcass per minute, one entire carcass every 5 minutes (assumption/experts) 
             "cutter2" = 0.1,# cut equivalent 10% of one carcass per minute, half-carcass every 5 minutes (assumption/experts) 
             "logistic2" = 0.5), # process (e.g.) and transfer the equivalent of a half-carcass per minute to the storage area
    lamb = c("logistic1" = 1, # transfer the equivalent of one carcass per minute from the arrival gate to the conveyor
             "cutter1" = 0.2, # cut the equivalent 20% of one carcass per minute, one entire carcass every 5 minutes (assumption/experts) 
             "cutter2" = 0.1,# cut equivalent 10% of one carcass per minute, half-carcass every 5 minutes (assumption/experts) 
             "logistic2" = 0.5) # process (e.g.) and transfer the equivalent of a half-carcass per minute to the storage area
  ),
  
  
  ## average weight (in kg) of each carcass 
  carcass_weight = c("bovine" = 286, # weight after maturation (experts)
                     "porcine" = 75, # weight after maturation (experts)
                     "veal" = 102, # weight after maturation (experts)
                     "lamb" = 17 # weight after maturation (experts)
                     ), 
  
  ## net weight ratio of the marketable portions (fat portions, bones and wastes excluded)
  net_weight_ratio = c("bovine" = 0.60, # experts
                       "porcine" = 0.59, # experts
                       "veal" = 0.75, # experts
                       "lamb" = 0.585 # experts
                 ),
  
  ## the average weight of each final meat portion (in kg)
  CSU = c("bovine" = 0.5, # /!\ assumption 
          "porcine" = 0.5, # /!\ assumption
          "veal" = 0.5, # /!\ assumption 
          "lamb" = 0.5 # /!\ assumption
          )
)
