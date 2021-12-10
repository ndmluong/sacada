##### FUNCTIONS #####
source("functions/functions_plant.R")
source("functions/functions_plot.R")
source("functions/functions_actions.R")
source("functions/functions_air.R")
source("functions/functions_food.R")
source("functions/functions_workers.R")
source("functions/functions_surfaces.R")
##### PARAMETERS #####
## Check the scripts for more details / change parameter values if needed
## PLANT
source("parameters/parameters_plant.R")
## TIME
source("parameters/parameters_time.R")
## WORKERS
source("parameters/parameters_workers.R")
## AIR
source("parameters/parameters_air.R")
## FOOD PORTIONS
source("parameters/parameters_food.R")


#### Example ####
##### RUN #####
## Create the plant
MyPlant <- f_createPlant(prm = Parms_Plant)
## Plot
g_emptyPlant <- f_plotPlant(Plant = MyPlant,
                            prm = Parms_Plant)

### Initialize the Agents
## Initialize the workers
set.seed(130)
MyWorkers <- f_initWorkers(prm = Parms_Workers, prm_time = Parms_Time)
## No changes between the time step 0 to the step 48 (4 hours) for some attributes
MyWorkers <- f_InvariantTimeSteps(Agents = MyWorkers,
                                  Invariant = c("W_ID", "W_status", "W_mask", "W_type", "W_active"),
                                  t_ind_from = 0,
                                  t_ind_to = 48)
# Initialize the Air Agents
MyAir <- f_initAir(prm = Parms_Plant, prm_time = Parms_Time, prm_air = Parms_Air)
MySurfaces <- f_initSurfaces(P = MyPlant$P, prm_time = Parms_Time)
