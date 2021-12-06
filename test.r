#test code Du
##### FUNCTIONS #####
source("functions/functions_plant.R")
source("functions/functions_plot.R")
source("functions/functions_air.R")
source("functions/functions_food.R")
source("functions/functions_workers.R")
source("functions/functions_surfaces.R")
source("functions/functions_actions.R")
source("functions/functions_time.R")

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
seed = 408
MyWorkers <- f_initWorkers(prm = Parms_Workers, prm_time = Parms_Time, seed = seed)

for (i in 1:52) {
  print(paste("Week", i, "to", i+1))
  MyWorkers <- f_changeTeamWeekly(W= MyWorkers, prob=0.05, week_from = i, seed = seed)
}

MyWorkers <- f_assignWorkersShift(MyWorkers) ## a lancer seulement quand toutes les equipes sont attribuees

df <- subset(MyWorkers, Hour == 0 & Min == 0)

gTimestable <- ggplot() +
    theme(axis.ticks=element_blank(),
          legend.position = "top",
          axis.text.y = element_text(face="bold", size=8),
          plot.title = element_text(face="bold", size=18),
          panel.background=element_rect(fill="white"),
          axis.text = element_text(size=16),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank()) +
  geom_line(data = subset(df, Day <= 70 & !(Weekday %in% c("Saturday", "Sunday"))),
            mapping = aes(x = Day, y = W_ID), colour = "lightgray") +
  geom_point(data = subset(df, Day <= 70 & !(Weekday %in% c("Saturday", "Sunday"))),
             mapping = aes(x = Day, y = W_ID, colour = W_type, shape = W_shift,
                           W_team = W_team, Week = Week, Weekday = Weekday)) +
  geom_vline(xintercept = seq(6, 70, 7), size=0.8) +
  geom_vline(xintercept = seq(7, 70, 7), size=0.8) +
  scale_x_continuous(breaks=seq(0,70,7)) +
  scale_shape_manual(values=c(1,10,19,8)) + 
  #scale_alpha_manual(values = c(0.4,1,1,1), guide = "none") +
  scale_colour_manual(values=c("black", "black", "darkgreen", "darkgreen", "blue", "red")) +
  labs(x = "Time (day)", y = "Worker ID")

ggplotly(gTimestable)

# Initialize the Air Agents
MyAir <- f_initAir(prm = Parms_Plant, prm_time = Parms_Time, prm_air = Parms_Air)

# Initialize the Surfaces Agents
MySurfaces <- f_initSurfaces(P = MyPlant$P, prm_time = Parms_Time)
