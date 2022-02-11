##### PACKAGES #####
library(ggplot2)
library(reshape2)
library(plotly)
library(stringr)
library(data.table)
library(dplyr)
library(gridExtra)
library(EnvStats)

##### FUNCTIONS #####
source("functions/functions_actions.R")
source("functions/functions_air.R")
source("functions/functions_contamination.R")
source("functions/functions_dailyWork.R")
source("functions/functions_module_master_proposition_modif.R")
source("functions/functions_plant.R")
source("functions/functions_plot.R")
source("functions/functions_run.R")
source("functions/functions_time.R")
source("functions/functions_workers.R")


##### PARAMETERS #####
## Check the scripts for more details / change parameter values if needed
source("parameters/parameters_plant.R") ## PLANT
source("parameters/parameters_time.R") ## TIME
source("parameters/parameters_workers.R") ## WORKERS
source("parameters/parameters_air.R") ## AIR
source("parameters/parameters_conta.R") ## CONTAMINATION


##### SIMULATION #####
### one simulation, ex: seed 408
ST1 <- Sys.time() ## simulation time checkpoint
OUTPUT_seed408 <- f_run_2M(prm_plant = Parms_Plant,
                           prm_time = Parms_Time,
                           prm_workers = Parms_Workers,
                           prm_air = Parms_Air,
                           prm_conta = Parms_Conta,
                           seed = 408)
ST2 <- Sys.time() ## simulation time checkpoint

### several simulations
all_seed <- 121:130 ## change if needed

ST3 <- Sys.time() ## simulation time checkpoint
lapply(all_seed, FUN = function(x) {
  OUTPUT_seedx <- tryCatch(f_run_2M(prm_plant = Parms_Plant,
                                    prm_time = Parms_Time,
                                    prm_workers = Parms_Workers,
                                    prm_air = Parms_Air,
                                    prm_conta = Parms_Conta,
                                    seed = x),
                           error = function(e) NULL)
  return(OUTPUT_seedx)
}) -> OUTPUT
ST4 <- Sys.time() ## simulation time checkpoint

### OUTPUT TREATMENT : Infection summary
lapply(OUTPUT, function(x) {return(x$InfectionSummary)}) %>%
  data.table::rbindlist() -> IS
IS$seed <- as.factor(IS$seed)

### OUTPUT TREATMENT : Infection source (for all workers and all simulations)
lapply(OUTPUT, function(x) {return(x$InfectionLog)}) %>%
  data.table::rbindlist() -> IL
IL$seed <- as.factor(IL$seed)
IL$InfectionSource <- as.factor(IL$InfectionSource)

# show infection sources
tapply(IL$InfectionSource, IL$seed, summary)


#### SAVE SIMULATION RESULTS
save.image("simulation_output/...............RData")
## ex. save.image("simulation_output/2022_02_06_initiales_s121_s130.Rdata) ?


##### PLOT #####
ggplot(data=IS) +
  geom_line(aes(x = Day, y = Infected_cumul, group = seed, colour = seed), size = 0.5) +
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
  scale_x_continuous(breaks = seq(1, max(IS$Day)+1, by = 7)) +
  scale_y_continuous(breaks = seq(0, max(IS$Infected_cumul)+5, by = 5)) +
  coord_cartesian(ylim = c(0, max(IS$Infected_cumul)+5),
                  xlim = c(0, max(IS$Day)+1)) +
  stat_summary(aes(x=Day, y=Infected_cumul), fun = mean, geom="line", size = 2, colour = "black") + 
  labs(title = "Cumulative number of infected workers",
       subtitle = paste(length(unique(IS$seed)), "independent simulations and the average curve")) +
  xlab("time (day)") + ylab("number of workers") -> g1
g1

### several seeds
ggplot(data=subset(IS, seed %in% c(121, 122))) + ## change/add/remove the seed numbers if needed, otherwise, for all simulations: ggplot(data=IS) +
  geom_ribbon(aes(x = Day, ymax = Symptomatic, ymin = 0), fill = "chocolate4", alpha = 0.9) + ## symptomatic
  geom_ribbon(aes(x = Day, ymin = Symptomatic, ymax = Symptomatic+Asymptomatic), fill = "chocolate4", alpha = 0.7) + ## asymptomatic
  geom_ribbon(aes(x = Day, ymin = Symptomatic+Asymptomatic, ymax = InfectiousPeriod), fill = "chocolate4", alpha = 0.5) + ## infectious pre-symptomatic
  geom_ribbon(aes(x = Day, ymin = InfectiousPeriod, ymax = InfectiousPeriod + NonInfectious), fill = "darkgray", colour = "darkgray", alpha = 0.95) + ## non infectious post-symptomatic
  geom_ribbon(aes(x = Day, ymin = InfectiousPeriod + NonInfectious, ymax = Positive), fill = "darkgray", colour = "darkgray", alpha = 0.65) + ## non-infectious (incubation)
  geom_ribbon(aes(x = Day, ymin = Positive, ymax = Infected_cumul), fill = "darkgreen", colour = "darkgreen", alpha = 0.7) + ## recovered
  geom_line(aes(x = Day, y = Infected_cumul), colour = "red", size = 3) + ## cumulative number of infected workers
  geom_line(aes(x = Day, y = InfectiousPeriod), colour = "chocolate4", size = 1.5) + ## daily infectious workers
  geom_line(aes(x = Day, y = Positive), colour = "black", size = 2) + ## daily positive workers
  geom_hline(yintercept = 15, colour = "navyblue", linetype = "dashed") + ## threshold (= 15)
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
  labs(title = "Cumulative number of infected workers") +
  xlab("time (day)") + ylab("number of workers") -> g2
g2

## evolution of the concentration of dropt
ggplot(data = OUTPUT$"121"$MyAir) +  ## simulation seed
  geom_line(mapping=aes(x=t_ind, y=d01), colour="red") +
  geom_line(mapping=aes(x=t_ind, y=d02), colour="blue") + 
  geom_line(mapping=aes(x=t_ind, y=d03), colour="green") + 
  geom_line(mapping=aes(x=t_ind, y=d04), colour="orange") + 
  xlab("time index") + ylab("concentration of droplets in aerosol (number of droplets per m3)") +
  theme(axis.ticks=element_blank(),
        plot.title = element_text(face="bold", size=15),
        axis.title = element_text(face="bold", size=10),
        axis.text = element_text(size=10)) + 
  scale_x_continuous(breaks = seq(1, max(OUTPUT$"121"$MyAir$t_ind), by = 2016)) + ## scale (2016 time indices per week)
  facet_grid(AIR_ID ~ .) -> g3
g3