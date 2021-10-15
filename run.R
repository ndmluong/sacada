##### FUNCTIONS #####
source("functions.R")
source("functions_plot.R")

##### PROCESSING PLANT #####
## Import all plant parameters
source("parameters_plant.R") ## check this script for more details / change parameter values if needed

## Create the plant
MyPlant <- f_createPlant(prm = Parms_Plant)
## Plot
g_Plant <- f_plotPlant(Plant = MyPlant)
g_Plant



##### WORKERS #####
## Import parameters associated with all workers
source("parameters_workers.R") ## check this script for more details / change parameter values if needed

## Initialize the workers 
MyWorkers <- f_initWorkers(prm = Parms_Workers)
MyWorkers


## Examples: move all or some workers to different locations inside the plant
MyWorkers <- f_moveWorkers(Plant = MyPlant,
                           W = MyWorkers,
                           to = "Entry hall")
## Plot
g_Plant <- f_plotPlant(Plant = MyPlant,
                       W = MyWorkers)
g_Plant


## Example: move workers
MyWorkers <- f_moveWorkers(Plant = MyPlant,
                           W = MyWorkers,
                           selectW = c("W015"),
                           to = "W.C.")


##### WORKSPACES #####





##### FOOD (MEAT PORTIONS) #####
## Import parameters associated with all food portions
source("parameters_food.R") ## check this script for more details / change parameter values if needed
MyFood <- f_initFood(prm = Parms_Food)

## Move all or some food portions to different locations inside the plant
MyFood <- f_moveFood(Plant = MyPlant,
                     FP = MyFood,
                     to = "Arrival gate")
## Plotting the processing plant with food and workers
g_Plant <- f_plotPlant(P = MyPlant$P,
                       W = MyWorkers,
                       FP = MyFood)
g_Plant

## Example: move food portions
MyFood <- f_moveFood(Plant = MyPlant,
                     FP= MyFood,
                     selectFP = c("F0090", "F0100"),
                     to = "Cooling area")
MyFood <- f_moveFood(Plant = MyPlant,
                     FP= MyFood,
                     selectFP = c("F0007", "F00850"),
                     to = "Waste area")



##### INTERACTIVE PLOTS #####
ggplotly(g_Plant,
         tooltip = c("W_ID", "FP_ID", "x", "y", "shape", "colour", "fill"))
