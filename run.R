##### FUNCTIONS #####
source("functions/functions_init.R")
source("functions/functions_plot.R")
source("functions/functions_actions.R")

##### PARAMETERS #####
## Check the scripts for more details / change parameter values if needed
## PLANT
source("parameters/parameters_plant.R")
## TIME
source("parameters/parameters_time.R")
## WORKERS
source("parameters/parameters_workers.R")
## FOOD PORTIONS
source("parameters/parameters_food.R")

##### RUN #####
## Create the plant
MyPlant <- f_createPlant(prm = Parms_Plant)
## Plot
g_emptyPlant <- f_plotPlant(Plant = MyPlant,
                            prm = Parms_Plant)
g_emptyPlant

## Initialize the workers
set.seed(130)
MyWorkers <- f_initWorkers(prm = Parms_Workers, prm_time = Parms_Time)
View(subset(MyWorkers, t_ind == 0)) ## View the initialized workers

## No changes between the time step 0 to the step 48 (4 hours) for some attributes
MyWorkers <- f_InvariantTimeSteps(Agents = MyWorkers,
                                  Invariant = c("W_ID", "W_status", "W_mask", "W_type", "W_active"),
                                  t_ind_from = 0,
                                  t_ind_to = 48)
View(MyWorkers)

## Example
Workers_ex <- c("W003", "W006", "W016", "W010", "W011", "W020")
MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = 1,
                           selectW = Workers_ex,
                           to = "Entry hall")
## W010 : cutter



ggplotly(f_plotWorkers(g_emptyPlant = g_emptyPlant,
              W = subset(MyWorkers, t_ind <= 1)))


MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = 2,
                           selectW = c("W006", "W011"),
                           to = "Cooling area")
MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = 1,
                           selectW = c("W002", "W007", "W009"),
                           to = "Office")

