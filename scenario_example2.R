##### FUNCTIONS #####
source("functions/functions_plant.R")
source("functions/functions_plot.R")
source("functions/functions_actions.R")
source("functions/functions_air.R")
source("functions/functions_food.R")
source("functions/functions_workers.R")

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

## Initialize the workers
set.seed(130)
MyWorkers <- f_initWorkers(prm = Parms_Workers, prm_time = Parms_Time)
## No changes between the time step 0 to the step 48 (4 hours) for some attributes
MyWorkers <- f_InvariantTimeSteps(Agents = MyWorkers,
                                  Invariant = c("W_ID", "W_status", "W_mask", "W_type", "W_active"),
                                  t_ind_from = 0,
                                  t_ind_to = 48)

## Entry (all workers)
MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = 1,
                           selectW = subset(MyWorkers, W_active == "active")$W_ID %>% unique,
                           to = "Entry hall")

#### CUTTERS ####
active_cutters <- subset(MyWorkers, W_type == "cutter" & W_active == "active" & t_ind == 2)$W_ID 

MyWorkers <- f_assignCuttersPosition(Plant = MyPlant,
                                     W = MyWorkers,
                                     t_ind = 2)
MyWorkers <- f_InvariantTimeSteps(Agents = MyWorkers,
                                  selectAgents = active_cutters,
                                  Invariant = c("W_location", "W_coordX", "W_coordY"),
                                  t_ind_from = 2,
                                  t_ind_to = 48)
for (wi in 1:length(active_cutters)) {
  wc_time <- sample(x = 3:48,
                    size = sample(1:2, 1)) ## 1-2 times
  for (t_step in wc_time) {
    MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = t_step,
                               selectW = active_cutters[wi],
                               to = "W.C.")
  }
}


#### ADMINISTRATIVE WORKERS ####
active_admin <- subset(MyWorkers, W_type == "administrative" & W_active == "active" & t_ind == 2)$W_ID 
MyWorkers <- f_moveWorkers(Plant = MyPlant,
                           W = MyWorkers,
                           t_ind = 2,
                           selectW = active_admin,
                           to = "Office")
MyWorkers <- f_InvariantTimeSteps(Agents = MyWorkers,
                                  selectAgents = active_admin,
                                  Invariant = c("W_location", "W_coordX", "W_coordY"),
                                  t_ind_from = 2,
                                  t_ind_to = 48)
for (wi in 1:length(active_admin)) {
  wc_time <- sample(x = 3:48, size=3) ## 3 times
  for (t_step in wc_time) {
    MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = t_step,
                               selectW = active_admin[wi],
                               to = "W.C.")
  }
}


#### W003 : logistic 1
for (t_step in 2:48) {
  if (t_step %% 2 == 0) {
    MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = t_step,
                               selectW = c("W003"),
                               to = "Arrival gate")
  } else {
    MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = t_step,
                               selectW = c("W003"),
                               to = "WS-Conveyor-Head")
  }
}
wc_time <- sample(x = 2:48, size=1) ## 1 fois en 4 heures
for (t_step in wc_time) {
  MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = t_step,
                             selectW = c("W003"),
                             to = "W.C.")
}

#### W016 : logistic 2
for (t_step in 3:48) {
  work <- as.character(t_step %% 3)
  switch(work,
         "0" = {MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = t_step, selectW = c("W016"), to = "WS-Conveyor-Tail")},
         "1" = {MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = t_step, selectW = c("W016"), to = "WS-Equipment 1")},
         "2" = {MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = t_step, selectW = c("W016"), to = "Cooling area")})
}
wc_time <- sample(x = 2:48, size=1) ## 1 fois en 4 heures
for (t_step in wc_time) {
  MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = t_step,
                             selectW = c("W016"),
                             to = "W.C.")
}


# f_plotAgents(g_emptyPlant = g_emptyPlant,
#              W = subset(MyWorkers, t_ind == 3))

ggplotly(f_plotAgents(g_emptyPlant = g_emptyPlant,
                      W = MyWorkers),
         tooltip = c("coordX", "coordY", "tile")) %>%
  animation_opts(redraw=T, frame = 1000, transition = 0) %>%
  animation_slider(
    currentvalue = list(prefix = "Time (in minutes): ", font = list(color="red"))
  )



###### FOOD #####
MyFood <- f_initFood(prm = Parms_Food, prm_time = Parms_Time)

## First carcass
FP_cc <- unique(MyFood$FP_ID[1:20])
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc,to = "Arrival gate", t_ind = 2)

for (t_step in 3:14) {
  MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc,to = "Conveyor", t_ind = t_step)
}

MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[1:5],to = "Equipment 1", t_ind = 4)
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[1:5],to = "Cooling area", t_ind = 5)
MyFood <- f_InvariantTimeSteps(Agents = MyFood, selectAgents = FP_cc[1:5], Agents_type = "FP", 
                               Invariant = c("FP_location", "FP_coordX", "FP_coordY"),
                               t_ind_from = 5, t_ind_to = 48)

MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[6:10],to = "Equipment 1", t_ind = 7)
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[6:10],to = "Cooling area", t_ind = 8)
MyFood <- f_InvariantTimeSteps(Agents = MyFood, selectAgents = FP_cc[6:10], Agents_type = "FP", 
                               Invariant = c("FP_location", "FP_coordX", "FP_coordY"),
                               t_ind_from = 8, t_ind_to = 48)

MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[11:15],to = "Equipment 1", t_ind = 10)
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[11:15],to = "Cooling area", t_ind = 11)
MyFood <- f_InvariantTimeSteps(Agents = MyFood, selectAgents = FP_cc[11:15], Agents_type = "FP", 
                               Invariant = c("FP_location", "FP_coordX", "FP_coordY"),
                               t_ind_from = 11, t_ind_to = 48)

MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[16:20],to = "Equipment 1", t_ind = 13)
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[16:20],to = "Cooling area", t_ind = 14)
MyFood <- f_InvariantTimeSteps(Agents = MyFood, selectAgents = FP_cc[16:20], Agents_type = "FP", 
                               Invariant = c("FP_location", "FP_coordX", "FP_coordY"),
                               t_ind_from = 14, t_ind_to = 48)


## 2nd carcass
FP_cc <- unique(MyFood$FP_ID[21:40])
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc,to = "Arrival gate", t_ind = 14)

for (t_step in 15:26) {
  MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc,to = "Conveyor", t_ind = t_step)
}

MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[1:5],to = "Equipment 1", t_ind = 16)
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[1:5],to = "Cooling area", t_ind = 17)
MyFood <- f_InvariantTimeSteps(Agents = MyFood, selectAgents = FP_cc[1:5], Agents_type = "FP", 
                               Invariant = c("FP_location", "FP_coordX", "FP_coordY"),
                               t_ind_from = 17, t_ind_to = 48)

MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[6:10],to = "Equipment 1", t_ind = 19)
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[6:10],to = "Cooling area", t_ind = 20)
MyFood <- f_InvariantTimeSteps(Agents = MyFood, selectAgents = FP_cc[6:10], Agents_type = "FP", 
                               Invariant = c("FP_location", "FP_coordX", "FP_coordY"),
                               t_ind_from = 20, t_ind_to = 48)

MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[11:15],to = "Equipment 1", t_ind = 22)
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[11:15],to = "Cooling area", t_ind = 23)
MyFood <- f_InvariantTimeSteps(Agents = MyFood, selectAgents = FP_cc[11:15], Agents_type = "FP", 
                               Invariant = c("FP_location", "FP_coordX", "FP_coordY"),
                               t_ind_from = 23, t_ind_to = 48)

MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[16:20],to = "Equipment 1", t_ind = 25)
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[16:20],to = "Cooling area", t_ind = 26)
MyFood <- f_InvariantTimeSteps(Agents = MyFood, selectAgents = FP_cc[16:20], Agents_type = "FP", 
                               Invariant = c("FP_location", "FP_coordX", "FP_coordY"),
                               t_ind_from = 26, t_ind_to = 48)


## 3rd carcass
FP_cc <- unique(MyFood$FP_ID[41:60])
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc,to = "Arrival gate", t_ind = 26)

for (t_step in 27:38) {
  MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc,to = "Conveyor", t_ind = t_step)
}

MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[1:5],to = "Equipment 1", t_ind = 28)
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[1:5],to = "Cooling area", t_ind = 29)
MyFood <- f_InvariantTimeSteps(Agents = MyFood, selectAgents = FP_cc[1:5], Agents_type = "FP", 
                               Invariant = c("FP_location", "FP_coordX", "FP_coordY"),
                               t_ind_from = 29, t_ind_to = 48)

MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[6:10],to = "Equipment 1", t_ind = 31)
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[6:10],to = "Cooling area", t_ind = 32)
MyFood <- f_InvariantTimeSteps(Agents = MyFood, selectAgents = FP_cc[6:10], Agents_type = "FP", 
                               Invariant = c("FP_location", "FP_coordX", "FP_coordY"),
                               t_ind_from = 32, t_ind_to = 48)

MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[11:15],to = "Equipment 1", t_ind = 34)
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[11:15],to = "Cooling area", t_ind = 35)
MyFood <- f_InvariantTimeSteps(Agents = MyFood, selectAgents = FP_cc[11:15], Agents_type = "FP", 
                               Invariant = c("FP_location", "FP_coordX", "FP_coordY"),
                               t_ind_from = 35, t_ind_to = 48)

MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[16:20],to = "Equipment 1", t_ind = 37)
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[16:20],to = "Cooling area", t_ind = 38)
MyFood <- f_InvariantTimeSteps(Agents = MyFood, selectAgents = FP_cc[16:20], Agents_type = "FP", 
                               Invariant = c("FP_location", "FP_coordX", "FP_coordY"),
                               t_ind_from = 38, t_ind_to = 48)

## 4th carcass
FP_cc <- unique(MyFood$FP_ID[61:80])
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc,to = "Arrival gate", t_ind = 38)

for (t_step in 39:48) {
  MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc,to = "Conveyor", t_ind = t_step)
}

MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[1:5],to = "Equipment 1", t_ind = 40)
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[1:5],to = "Cooling area", t_ind = 41)
MyFood <- f_InvariantTimeSteps(Agents = MyFood, selectAgents = FP_cc[1:5], Agents_type = "FP", 
                               Invariant = c("FP_location", "FP_coordX", "FP_coordY"),
                               t_ind_from = 41, t_ind_to = 48)

MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[6:10],to = "Equipment 1", t_ind = 43)
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[6:10],to = "Cooling area", t_ind = 44)
MyFood <- f_InvariantTimeSteps(Agents = MyFood, selectAgents = FP_cc[6:10], Agents_type = "FP", 
                               Invariant = c("FP_location", "FP_coordX", "FP_coordY"),
                               t_ind_from = 44, t_ind_to = 48)

MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[11:15],to = "Equipment 1", t_ind = 46)
MyFood <- f_moveFood(Plant = MyPlant, FP = MyFood, selectFP = FP_cc[11:15],to = "Cooling area", t_ind = 47)
MyFood <- f_InvariantTimeSteps(Agents = MyFood, selectAgents = FP_cc[11:15], Agents_type = "FP", 
                               Invariant = c("FP_location", "FP_coordX", "FP_coordY"),
                               t_ind_from = 47, t_ind_to = 48)

f_plotAgents(g_emptyPlant = g_emptyPlant,
             W = subset(MyWorkers, t_ind == 48),
             FP = subset(MyFood, t_ind == 48))

f_plotAgents(g_emptyPlant = g_emptyPlant,
             W = MyWorkers,
             FP = MyFood) %>%
  ggplotly() %>%
  animation_opts(redraw=T, frame = 1000, transition = 0) %>%
  animation_slider(
    currentvalue = list(prefix = "Time (in minutes): ", font = list(color="red"))
  )
