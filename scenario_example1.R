##### RUN #####
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

MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = 1,
                           selectW = subset(MyWorkers, W_active == "active")$W_ID %>% unique,
                           to = "Entry hall")


## cutter
active_cutters <- subset(MyWorkers, W_type == "cutter" & W_active == "active" & t_ind == 2)$W_ID 
## Administrative workers ##
active_admin <- subset(MyWorkers, W_type == "administrative" & W_active == "active" & t_ind == 2)$W_ID 

#### CUTTERS ####
MyWorkers <- f_assignCuttersPosition(Plant = MyPlant,
                                     W = MyWorkers,
                                     t_ind = 2)
MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers,
                           t_ind = 2,
                           selectW = active_admin,
                           to = "Office")

MyWorkers <- f_InvariantTimeSteps(Agents = MyWorkers,
                                  Invariant = c("W_location", "W_coordX", "W_coordY"),
                                  t_ind_from = 2,
                                  t_ind_to = 48)

for (wi in 1:length(active_cutters)) {
  wc_time <- sample(x = 3:48, size=2) ## 2 times
  for (t_step in wc_time) {
    MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = t_step,
                               selectW = active_cutters[wi],
                               to = "W.C.")
  }
}

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
for (t_step in 2:48) {
  work <- as.character(t_step %% 3)
  switch(work,
         "2" = {MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = t_step, selectW = c("W016"), to = "WS-Conveyor-Tail")},
         "0" = {MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = t_step, selectW = c("W016"), to = "WS-Equipment 1")},
         "1" = {MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = t_step, selectW = c("W016"), to = "Cooling area")})
}
wc_time <- sample(x = 2:48, size=1) ## 1 fois en 4 heures
for (t_step in wc_time) {
  MyWorkers <- f_moveWorkers(Plant = MyPlant, W = MyWorkers, t_ind = t_step,
                             selectW = c("W016"),
                             to = "W.C.")
}


f_plotWorkers(g_emptyPlant = g_emptyPlant,
              W = subset(MyWorkers, t_ind == 3))

ggplotly(f_plotWorkers(g_emptyPlant = g_emptyPlant,
                       W = MyWorkers)) %>%
  animation_opts(redraw=T, frame = 1000, transition = 0) %>%
  animation_slider(
    currentvalue = list(prefix = "Time (in minutes): ", font = list(color="red"))
  )

