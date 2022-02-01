##### PACKAGES #####
library(ggplot2)
library(reshape2)
library(plotly)
library(stringr)
library(data.table)
library(dplyr)

##### FUNCTIONS #####
source("functions/functions_actions.R")
source("functions/functions_air.R")
source("functions/functions_contamination.R")
source("functions/functions_dailyWork.R")
# source("functions/functions_food.R")
# source("functions/functions_module_master.R")
source("functions/functions_module_master_proposition_modif.R")
source("functions/functions_plant.R")
source("functions/functions_plot.R")
source("functions/functions_run.R")
# source("functions/functions_surfaces.R")
source("functions/functions_time.R")
source("functions/functions_workers.R")


##### PARAMETERS #####
## Check the scripts for more details / change parameter values if needed
source("parameters/parameters_plant.R") ## PLANT
source("parameters/parameters_time.R") ## TIME
source("parameters/parameters_workers.R") ## WORKERS
source("parameters/parameters_air.R") ## AIR
source("parameters/parameters_conta.R") ## CONTAMINATION

##### SIMULATIONS #####
ST1 <- Sys.time()
OUTPUT_seed408 <- f_run_2M(prm_plant = Parms_Plant,
                           prm_time = Parms_Time,
                           prm_workers = Parms_Workers,
                           prm_air = Parms_Air,
                           prm_conta = Parms_Conta,
                           seed = 408)
OUTPUT_seed116 <- f_run_2M(prm_plant = Parms_Plant,
                           prm_time = Parms_Time,
                           prm_workers = Parms_Workers,
                           prm_air = Parms_Air,
                           prm_conta = Parms_Conta,
                           seed = 116)
OUTPUT_seed216 <- f_run_2M(prm_plant = Parms_Plant,
                           prm_time = Parms_Time,
                           prm_workers = Parms_Workers,
                           prm_air = Parms_Air,
                           prm_conta = Parms_Conta,
                           seed = 216)
OUTPUT_seed311 <- f_run_2M(prm_plant = Parms_Plant,
                           prm_time = Parms_Time,
                           prm_workers = Parms_Workers,
                           prm_air = Parms_Air,
                           prm_conta = Parms_Conta,
                           seed = 311)
OUTPUT_seed525 <- f_run_2M(prm_plant = Parms_Plant,
                           prm_time = Parms_Time,
                           prm_workers = Parms_Workers,
                           prm_air = Parms_Air,
                           prm_conta = Parms_Conta,
                           seed = 525)

##### SAVE SIMULATION RESULTS ##### (change the RData name if needeed)
# save.image("simulation_output/OUTPUT_2022_01_31.RData")

##### SUMMARIES GATHERING #####
IS <- rbind(OUTPUT_seed116$InfectionSummary,
            OUTPUT_seed216$InfectionSummary,
            OUTPUT_seed408$InfectionSummary,
            OUTPUT_seed311$InfectionSummary,
            OUTPUT_seed525$InfectionSummary)

##### PLOT: SIMULATIONS OUTPUT #####
ggplot(data=IS) +
  geom_line(aes(x = Day, y = Infected_cumul, group = seed), colour = "red", size = 2) +
  geom_line(aes(x = Day, y = Recovered_cumul, group = seed), colour = "darkgreen", size = 2) +
  theme(axis.ticks=element_blank(),
        #legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face="bold", size=15),
        axis.title = element_text(face="bold", size=10),
        axis.text = element_text(size=10),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  facet_grid(. ~ seed) +
  scale_x_continuous(breaks = seq(1, 56, by = 7)) +
  scale_y_continuous(breaks = seq(0, max(IS$Infected_cumul), by = 2)) +
  labs(title = "Cumulative number of infected / recovered workers") +
  xlab("time (day)") + ylab("Cumulative number of workers") -> g1

ggplot(data=IS) +
  geom_line(aes(x = Day, y = Infectious), colour = "darkorange", size = 2) +
  geom_line(aes(x = Day, y = Symptomatic), colour = "orange") +
  theme(axis.ticks=element_blank(),
        #legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face="bold", size=15),
        axis.title = element_text(face="bold", size=10),
        axis.text = element_text(size=10),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  facet_grid(. ~ seed) +
  scale_x_continuous(breaks = seq(1, 56, by = 7)) +
  scale_y_continuous(breaks = seq(0, max(IS$Infected_cumul), by = 2)) +
  coord_cartesian(ylim = c(0, max(IS$Infected_cumul))) +
  labs(title = "Daily number of infectious workers") +
  xlab("time (day)") + ylab("Daily number of workers") -> g2

ggplotly(g1)
ggplotly(g2)
gridExtra::grid.arrange(g1,g2,nrow=2)

ggplot(OUTPUT_seed408$MyAir) + 
  geom_line(mapping=aes(x=t_ind, y=d04), colour="orange") + 
  geom_line(mapping=aes(x=t_ind, y=d03), colour="green") + 
  geom_line(mapping=aes(x=t_ind, y=d02), colour="blue") + 
  geom_line(mapping=aes(x=t_ind, y=d01), colour="red") +
  xlab("time index") + ylab("number of droplets per m3") +
  theme(axis.ticks=element_blank(),
        #legend.position = "none",
        #panel.background=element_rect(fill="grey"),
        plot.title = element_text(face="bold", size=15),
        axis.title = element_text(face="bold", size=10),
        axis.text = element_text(size=10)) + 
  scale_x_continuous(breaks = seq(1, max(OUTPUT_seed408$MyAir$t_ind), by = 2016)) +
  facet_grid(AIR_ID ~ .) -> g3
