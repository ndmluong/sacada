#test code Du
##### PACKAGES #####
library(ggplot2)
library(reshape2)
library(plotly)
library(stringr)



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

##### LOAD PRE-INITIALISED WORKERS #####
load("2021_12_07_100Workers_365days_Step5_TypeTeamShiftActiveCounter.RData")

##### DO NOT RUN (BEGIN) #####
# ####### Initialize the workers
# ## simulation time between the lines 35 and 50 : approx. 50 minutes
# Parms_Time$NDays <- 365
# Parms_Time$Step <- 5
# seed = 408
# MyWorkers <- f_initWorkers(prm = Parms_Workers, prm_time = Parms_Time, seed = seed)


# ###### To be included in a function ######
# set.seed(408)
# for (i in 1:52) {
#   print(paste("Simulating weekly team changes: Week", i))
#   MyWorkers <- f_changeTeamWeekly(W= MyWorkers, prob=Parms_Workers$pChangeTeam,
#                                   week_from = i)
# }
# rm(i)
# ## Assign Worker shift
# print("Assign working shifts for all workers")
# MyWorkers <- f_assignWorkersShift(MyWorkers) ## a lancer seulement quand toutes les equipes sont attribuees
# ####### ######
# 
# ## save.image("Workers_NDays365_Step60.RData")
# ## save.image("2021_12_06_100Workers_365days_Step60_TypeTeamShift.RData")
# ## save.image("2021_12_06_100Workers_365days_Step120_TypeTeamShift.RData")
# save.image("2021_12_06_100Workers_365days_Step5_TypeTeamShift.RData")


# ## Calculate the active counter of every workers and fill the case 0h00
# MyWorkers <- f_initActiveCounter(W = MyWorkers, seed = 409)
# MyWorkers <- f_calculateActiveCounter(MyWorkers)
# 
# save.image("2021_12_07_100Workers_365days_Step5_TypeTeamShiftActiveCounter.RData")
# write.table(MyWorkers, file = "MyWorkers.txt", sep = "\t")

##### DO NOT RUN (END) #####


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
  geom_line(data = subset(df, Day <= 70),
            mapping = aes(x = Day, y = W_ID), colour = "lightgray") +
  geom_point(data = subset(df, Day <= 70 & W_active == "active"),
             mapping = aes(x = Day, y = W_ID, colour = W_type, shape = W_shift,
                           W_team = W_team, Week = Week, Weekday = Weekday)) +
  geom_vline(xintercept = seq(6, 70, 7), size=0.8) +
  geom_vline(xintercept = seq(7, 70, 7), size=0.8) +
  scale_x_continuous(breaks=seq(0,70,7)) +
  scale_shape_manual(values=c(1,10,19,8)) + 
  #scale_alpha_manual(values = c(0.4,1,1,1), guide = "none") +
  scale_colour_manual(values=c("black", "black", "darkgreen", "darkgreen", "blue", "red")) +
  labs(x = "Time (day)", y = "Worker ID")
gTimestable
ggplotly(gTimestable)





###########################################