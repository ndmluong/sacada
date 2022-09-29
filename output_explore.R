# PACKAGES #####
library(tibble)
library(reshape2)
library(plotly)
library(data.table)
library(gridExtra)
library(earlyR)
library(incidence)
library(EnvStats)
library(EpiEstim)
library(viridis)
library(ggpubr)
library(tidyverse)

# FUNCTIONS #####
source("functions/functions.R")

# ANALYSES ####
## FoodMicro2022 - Effect of mask ####
## NB: check if the RData is available in the simulation_output directory
load(file = "simulation_output/output_FMMaskEffect.RData")

FMMask <- list(FMMask0, FMMask20, FMMask40, FMMask60, FMMask80, FMMask90, FMMask95, FMMask100)

lapply(FMMask, function(x) {
  min_carcass <- tapply(x$FPS$nb_carcass, x$FPS$seed, function(y) min(y[y>0])) %>% as.vector() 
  max_carcass <- tapply(x$FPS$nb_carcass, x$FPS$seed, max) %>% as.vector()
  d1 <- data.frame(Mask = rep(x$Parms_Workers$pMaskAcceptability[[x$Parms_Workers$MaskType]], length(min_carcass)),
                   Prod = min_carcass / max_carcass)
  return(d1)
}) %>% rbindlist() -> loss


SMR_Mask <- data.frame(Mask = c(0, 20, 40, 60, 80, 90, 95, 100),
                       SurfacesContaInf = sapply(FMMask, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% quantile(probs = 0.025) %>% as.vector
                       }),
                       SurfacesContaMed = sapply(FMMask, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% quantile(prob = 0.5) %>% as.vector
                       }),
                       SurfacesContaSup = sapply(FMMask, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% quantile(prob = 0.975) %>% as.vector
                       }),
                       SurfacesContaMean = sapply(FMMask, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% mean() %>% as.vector
                       }),
                       SurfacesContaSd = sapply(FMMask, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% sd() %>% as.vector
                       }),
                       FoodContaInf = sapply(FMMask, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% quantile(probs = 0.025) %>% as.vector
                       }),
                       FoodContaMed = sapply(FMMask, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% quantile(probs = 0.5) %>% as.vector
                       }),
                       FoodContaSup = sapply(FMMask, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% quantile(probs = 0.975) %>% as.vector
                       }),
                       FoodContaMean = sapply(FMMask, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% mean() %>% as.vector
                       }),
                       FoodContaSd = sapply(FMMask, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% sd() %>% as.vector
                       }),
                       CumulContaWorkersInf = sapply(FMMask, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.025, na.rm = T) %>% as.vector
                       }),
                       CumulContaWorkersMed = sapply(FMMask, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.5, na.rm = T) %>% as.vector
                       }),
                       CumulContaWorkersSup = sapply(FMMask, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.975, na.rm = T) %>% as.vector
                       }),
                       CumulContaWorkersMean = sapply(FMMask, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% mean %>% as.vector
                       }),
                       CumulContaWorkersSd = sapply(FMMask, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% sd %>% as.vector
                       }),
                       ProdInf = tapply(loss$Prod, loss$Mask, function(y) quantile(y, probs = 0.025)) %>% as.vector(),
                       ProdMed = tapply(loss$Prod, loss$Mask, function(y) quantile(y, probs = 0.5)) %>% as.vector(),
                       ProdSup = tapply(loss$Prod, loss$Mask, function(y) quantile(y, probs = 0.75)) %>% as.vector(),
                       ProdMean = tapply(loss$Prod, loss$Mask, mean) %>% as.vector(),
                       ProdSd = tapply(loss$Prod, loss$Mask, sd) %>% as.vector(),
                       RtMean = sapply(FMMask, function(x) {
                         f_smrzRt(IS = x$IS, prm_conta = x$Parms_Conta, prm_workers = x$Parms_Workers)$Rt %>% mean(., na.rm=T) %>% as.vector()
                       })
)


lapply(FMMask, function(x) {
  SurfacesConta <- f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio
  FoodConta <- f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio
  d1 <- data.frame(Mask = rep(x$Parms_Workers$pMaskAcceptability[[x$Parms_Workers$MaskType]], length(SurfacesConta)),
                   SurfacesConta = SurfacesConta,
                   FoodConta = FoodConta)
  return(d1)
}) %>%
  data.table::rbindlist() -> SMR_MaskRaw




gMask_SurfacesConta <- ggplot(data = SMR_Mask) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Mask, y = SurfacesContaMean * 100), size = 1) + 
  geom_segment(mapping = aes(x = Mask, xend = Mask,
                             y = SurfacesContaInf * 100,
                             yend = SurfacesContaSup * 100), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Mask, y = SurfacesContaMean * 100), size = 5) +
  labs(title = "Surface contamination (detection of more than 5 log RNA copies)") +
  coord_cartesian(ylim = c(-1, 50)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 90, 95, 100)) +
  xlab("Mask wearing (%)") + ylab("Surfaces contamination (%)")



gMask_Prod <- ggplot(data = SMR_Mask) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Mask, y = ProdMean * 100), size = 1) + 
  geom_segment(mapping = aes(x = Mask, xend = Mask,
                             y = ProdInf * 100,
                             yend = ProdSup * 100), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Mask, y = ProdMean * 100), size = 5) +
  labs(title = "Performance of the meat processing plant") +
  coord_cartesian(ylim = c(0,100)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 90, 95, 100)) +
  xlab("Mask wearing (%)") + ylab("Ratio = Min/Max capacity (%)")


gMask_WorkersConta <- ggplot(data = SMR_Mask) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Mask, y = CumulContaWorkersMean), size = 1) + 
  geom_segment(mapping = aes(x = Mask, xend = Mask,
                             y = CumulContaWorkersInf,
                             yend = CumulContaWorkersSup), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Mask, y = CumulContaWorkersMean), size = 3) +
  labs(title = "Cumulative number of contaminated workers after 42 days") +
  xlab("Mask wearing (%)") + ylab("Number of workers")

gMask_FoodConta <- ggplot(data = SMR_Mask) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Mask, y = FoodContaMean * 100), size = 1) + 
  geom_segment(mapping = aes(x = Mask, xend = Mask,
                             y = FoodContaInf * 100,
                             yend = FoodContaSup * 100), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Mask, y = FoodContaMean * 100), size = 5) +
  labs(title = "Food contamination (detection of more than 5 log RNA copies)") +
  #coord_cartesian(ylim = c(-1, 50)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 90, 95, 100)) +
  xlab("Mask wearing (%)") + ylab("Food contamination (%)")

g_FMMask <- ggarrange(gMask_WorkersConta, gMask_SurfacesConta, gMask_FoodConta, gMask_Prod, ncol = 1)
g_FMMask
ggsave(filename="simulation_output/plot/g_FMMask.pdf", 
       plot = g_FMMask, 
       device = cairo_pdf, width = 297, height = 420, units = "mm")










## FoodMicro2022 - Effect of air renewal (withoutmask) ####
## NB: check if the RData is available in the simulation_output directory
load(file = "simulation_output/output_FMAirEffectWithoutMask.RData")

FMAirbis <- list(FMAir0bis, FMAir1200bis, FMAir2400bis, FMAir3600bis, FMAir4800bis)

Airselect <- c(0, 1200, 3600, 4800)

lapply(FMAirbis, function(x) {
  min_carcass <- tapply(x$FPS$nb_carcass, x$FPS$seed, function(y) min(y[y>0])) %>% as.vector() 
  max_carcass <- tapply(x$FPS$nb_carcass, x$FPS$seed, max) %>% as.vector()
  d1 <- data.frame(Air = rep(x$Parms_Plant$Air_renewal, length(min_carcass)),
                   Prod = min_carcass / max_carcass)
  return(d1)
}) %>% rbindlist() -> loss


SMR_Airbis <- data.frame(Air = Airselect,
                         SurfacesContaInf = sapply(FMAirbis, function(x) {
                           f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% quantile(probs = 0.025) %>% as.vector
                         }),
                         SurfacesContaMed = sapply(FMAirbis, function(x) {
                           f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% quantile(prob = 0.5) %>% as.vector
                         }),
                         SurfacesContaSup = sapply(FMAirbis, function(x) {
                           f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% quantile(prob = 0.975) %>% as.vector
                         }),
                         SurfacesContaMean = sapply(FMAirbis, function(x) {
                           f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% mean() %>% as.vector
                         }),
                         SurfacesContaSd = sapply(FMAirbis, function(x) {
                           f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% sd() %>% as.vector
                         }),
                         FoodContaInf = sapply(FMAirbis, function(x) {
                           f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% quantile(probs = 0.025) %>% as.vector
                         }),
                         FoodContaMed = sapply(FMAirbis, function(x) {
                           f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% quantile(probs = 0.5) %>% as.vector
                         }),
                         FoodContaSup = sapply(FMAirbis, function(x) {
                           f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% quantile(probs = 0.975) %>% as.vector
                         }),
                         FoodContaMean = sapply(FMAirbis, function(x) {
                           f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% mean() %>% as.vector
                         }),
                         FoodContaSd = sapply(FMAirbis, function(x) {
                           f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% sd() %>% as.vector
                         }),
                         CumulContaWorkersInf = sapply(FMAirbis, function(x) {
                           f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.025, na.rm = T) %>% as.vector
                         }),
                         CumulContaWorkersMed = sapply(FMAirbis, function(x) {
                           f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.5, na.rm = T) %>% as.vector
                         }),
                         CumulContaWorkersSup = sapply(FMAirbis, function(x) {
                           f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.975, na.rm = T) %>% as.vector
                         }),
                         CumulContaWorkersMean = sapply(FMAirbis, function(x) {
                           f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% mean %>% as.vector
                         }),
                         CumulContaWorkersSd = sapply(FMAirbis, function(x) {
                           f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% sd %>% as.vector
                         }),
                         ProdInf = tapply(loss$Prod, loss$Air, function(y) quantile(y, probs = 0.025)) %>% as.vector(),
                         ProdMed = tapply(loss$Prod, loss$Air, function(y) quantile(y, probs = 0.5)) %>% as.vector(),
                         ProdSup = tapply(loss$Prod, loss$Air, function(y) quantile(y, probs = 0.75)) %>% as.vector(),
                         ProdMean = tapply(loss$Prod, loss$Air, mean) %>% as.vector(),
                         ProdSd = tapply(loss$Prod, loss$Air, sd) %>% as.vector()
)

gAirbis_SurfacesConta <- ggplot(data = SMR_Airbis) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Air, y = SurfacesContaMean * 100), size = 1) + 
  geom_segment(mapping = aes(x = Air, xend = Air,
                             y = SurfacesContaInf * 100,
                             yend = SurfacesContaSup * 100), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Air, y = SurfacesContaMean * 100), size = 5) +
  labs(title = "Surface contamination (detection of more than 5 log RNA copies)") +
  coord_cartesian(ylim = c(-1, 50)) +
  scale_x_continuous(breaks = c(0, 1200, 2400, 3600, 4800)) +
  xlab("Air renewal (m3/h)") + ylab("Surfaces contamination (%)")
gAirbis_SurfacesConta


gAirbis_FoodConta <- ggplot(data = SMR_Airbis) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Air, y = FoodContaMean * 100), size = 1) + 
  geom_segment(mapping = aes(x = Air, xend = Air,
                             y = FoodContaInf * 100,
                             yend = FoodContaSup * 100), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Air, y = FoodContaMean * 100), size = 5) +
  labs(title = "Food contamination (detection of more than 5 log RNA copies)") +
  scale_x_continuous(breaks = c(0, 1200, 2400, 3600, 4800)) +
  xlab("Air renewal (m3/h)") + ylab("Surfaces contamination (%)")
gAirbis_FoodConta












## FoodMicro2022 - Effect of social distancing ####
## NB: check if the RData is available in the simulation_output directory
load(file = "simulation_output/output_FMCvy.RData")

FMCvy <- list(FMCvy10, FMCvy12, FMCvy16, FMCvy18)
Cvyselect <- c(10, 12,16,18)

lapply(FMCvy, function(x) {
  min_carcass <- tapply(x$FPS$nb_carcass, x$FPS$seed, function(y) min(y[y>0])) %>% as.vector() 
  max_carcass <- tapply(x$FPS$nb_carcass, x$FPS$seed, max) %>% as.vector()
  d1 <- data.frame(Cvy = rep(x$Parms_Plant$Objects$cvy$dim.X, length(min_carcass)),
                   Prod = min_carcass / max_carcass)
  return(d1)
}) %>% rbindlist() -> loss



loss <- subset(loss, Cvy %in% Cvyselect)
SMR_Cvy <- subset(SMR_Cvy, Cvy %in% Cvyselect)

SMR_Cvy <- data.frame(Cvy = Cvyselect,
                      SurfacesContaInf = sapply(FMCvy, function(x) {
                        f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 3)$contaratio %>% quantile(probs = 0.025) %>% as.vector
                      }),
                      SurfacesContaMed = sapply(FMCvy, function(x) {
                        f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 3)$contaratio %>% quantile(prob = 0.5) %>% as.vector
                      }),
                      SurfacesContaSup = sapply(FMCvy, function(x) {
                        f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 3)$contaratio %>% quantile(prob = 0.975) %>% as.vector
                      }),
                      SurfacesContaMean = sapply(FMCvy, function(x) {
                        f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 3)$contaratio %>% mean() %>% as.vector
                      }),
                      SurfacesContaSd = sapply(FMCvy, function(x) {
                        f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 3)$contaratio %>% sd() %>% as.vector
                      }),
                      FoodContaInf = sapply(FMCvy, function(x) {
                        f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 3)$contaratio %>% quantile(probs = 0.025) %>% as.vector
                      }),
                      FoodContaMed = sapply(FMCvy, function(x) {
                        f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 3)$contaratio %>% quantile(probs = 0.5) %>% as.vector
                      }),
                      FoodContaSup = sapply(FMCvy, function(x) {
                        f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 3)$contaratio %>% quantile(probs = 0.975) %>% as.vector
                      }),
                      FoodContaMean = sapply(FMCvy, function(x) {
                        f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 3)$contaratio %>% mean() %>% as.vector
                      }),
                      FoodContaSd = sapply(FMCvy, function(x) {
                        f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 3)$contaratio %>% sd() %>% as.vector
                      }),
                      CumulContaWorkersInf = sapply(FMCvy, function(x) {
                        f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.025, na.rm = T) %>% as.vector
                      }),
                      CumulContaWorkersMed = sapply(FMCvy, function(x) {
                        f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.5, na.rm = T) %>% as.vector
                      }),
                      CumulContaWorkersSup = sapply(FMCvy, function(x) {
                        f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.975, na.rm = T) %>% as.vector
                      }),
                      CumulContaWorkersMean = sapply(FMCvy, function(x) {
                        f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% mean %>% as.vector
                      }),
                      CumulContaWorkersSd = sapply(FMCvy, function(x) {
                        f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% sd %>% as.vector
                      }),
                      ProdInf = tapply(loss$Prod, loss$Cvy, function(y) quantile(y, probs = 0.025)) %>% as.vector(),
                      ProdMed = tapply(loss$Prod, loss$Cvy, function(y) quantile(y, probs = 0.5)) %>% as.vector(),
                      ProdSup = tapply(loss$Prod, loss$Cvy, function(y) quantile(y, probs = 0.75)) %>% as.vector(),
                      ProdMean = tapply(loss$Prod, loss$Cvy, mean) %>% as.vector(),
                      ProdSd = tapply(loss$Prod, loss$Cvy, sd) %>% as.vector()
)




gCvy_SurfacesConta <- ggplot(data = SMR_Cvy) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Cvy, y = SurfacesContaMean * 100), size = 1) + 
  geom_segment(mapping = aes(x = Cvy, xend = Cvy,
                             y = SurfacesContaInf * 100,
                             yend = SurfacesContaSup * 100), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Cvy, y = SurfacesContaMean * 100), size = 5) +
  labs(title = "Surfaces contamination (detection of more than 5 log RNA copies)") +
  coord_cartesian(ylim = c(-1, 20)) +
  scale_x_continuous(breaks = Cvyselect) +
  xlab("Conveyor length (m)") + ylab("Surfaces contamination (%)")



gCvy_Prod <- ggplot(data = SMR_Cvy) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Cvy, y = ProdMean * 100), size = 1) + 
  geom_segment(mapping = aes(x = Cvy, xend = Cvy,
                             y = ProdInf * 100,
                             yend = ProdSup * 100), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Cvy, y = ProdMean * 100), size = 5) +
  labs(title = "Performance of the meat processing plant") +
  coord_cartesian(ylim = c(80,90)) +
  scale_x_continuous(breaks = Cvyselect) +
  xlab("Conveyor length (m)") + ylab("Ratio = Min/Max capacity (%)")


gCvy_WorkersConta <- ggplot(data = SMR_Cvy) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Cvy, y = CumulContaWorkersMean), size = 1) + 
  geom_segment(mapping = aes(x = Cvy, xend = Cvy,
                             y = CumulContaWorkersInf,
                             yend = CumulContaWorkersSup), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Cvy, y = CumulContaWorkersMean), size = 3) +
  scale_x_continuous(breaks = Cvyselect) +
  labs(title = "Cumulative number of contaminated workers after 42 days") +
  xlab("Conveyor length (m)") + ylab("Number of workers")

gCvy_FoodConta <- ggplot(data = SMR_Cvy) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Cvy, y = FoodContaMean * 100), size = 1) + 
  geom_segment(mapping = aes(x = Cvy, xend = Cvy,
                             y = FoodContaInf * 100,
                             yend = FoodContaSup * 100), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Cvy, y = FoodContaMean * 100), size = 5) +
  labs(title = "Food contamination (detection of more than 5 log RNA copies)") +
  #coord_cartesian(ylim = c(-1, 50)) +
  scale_x_continuous(breaks = Cvyselect) +
  xlab("Conveyor length (m)") + ylab("Food contamination (%)")

g_FMCvy <- ggarrange(gCvy_WorkersConta, gCvy_SurfacesConta, gCvy_FoodConta, gCvy_Prod, ncol = 1)
g_FMCvy
ggsave(filename="simulation_output/plot/g_FMCvy.pdf", 
       plot = g_FMCvy, 
       device = cairo_pdf, width = 297, height = 420, units = "mm")












## September 2022 - Cross effect of mask wearing and social distancing ####
load(file = "simulation_output/20220919_output_MCvy.RData")

MCvy <- list(M0Cvy11, M0Cvy14, M0Cvy17, M0Cvy20,
             M25Cvy11, M25Cvy14, M25Cvy17, M25Cvy20,
             M50Cvy11, M50Cvy14, M50Cvy17, M50Cvy20,
             M80Cvy11, M80Cvy14, M80Cvy17, M80Cvy20,
             M100Cvy11, M100Cvy14, M100Cvy17, M100Cvy20)

gdata::keep(MCvy, sure = TRUE)
source("functions/functions.R")

Mask <- c(rep(0,4), rep(25,4), rep(50,4), rep(80,4), rep(100,4))
Cvy <- rep(c(11,14,17,20), 5)





lapply(MCvy, function(x) {
  min_carcass <- tapply(x$FPS$nb_carcass, x$FPS$seed, function(y) min(y[y>0])) %>% as.vector() 
  max_carcass <- tapply(x$FPS$nb_carcass, x$FPS$seed, max) %>% as.vector()
  d1 <- data.frame(Mask = rep(x$Parms_Workers$pMaskAcceptability[["Surgical mask"]], length(min_carcass)),
                   Cvy = rep(x$Parms_Plant$Objects$cvy$dim.X, length(min_carcass)),
                   Prod = min_carcass / max_carcass)
  return(d1)
}) %>% data.table::rbindlist() -> loss

SMR_MCvy <- data.frame(Mask = Mask,
                       Cvy = Cvy,
                       SurfacesContaInf = sapply(MCvy, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 3)$contaratio %>% quantile(probs = 0.025) %>% as.vector
                       }),
                       SurfacesContaMed = sapply(MCvy, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 3)$contaratio %>% quantile(prob = 0.5) %>% as.vector
                       }),
                       SurfacesContaSup = sapply(MCvy, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 3)$contaratio %>% quantile(prob = 0.975) %>% as.vector
                       }),
                       SurfacesContaMean = sapply(MCvy, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 3)$contaratio %>% mean() %>% as.vector
                       }),
                       SurfacesContaSd = sapply(MCvy, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 3)$contaratio %>% sd() %>% as.vector
                       }),
                       FoodContaInf = sapply(MCvy, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 3)$contaratio %>% quantile(probs = 0.025) %>% as.vector
                       }),
                       FoodContaMed = sapply(MCvy, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 3)$contaratio %>% quantile(probs = 0.5) %>% as.vector
                       }),
                       FoodContaSup = sapply(MCvy, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 3)$contaratio %>% quantile(probs = 0.975) %>% as.vector
                       }),
                       FoodContaMean = sapply(MCvy, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 3)$contaratio %>% mean() %>% as.vector
                       }),
                       FoodContaSd = sapply(MCvy, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 3)$contaratio %>% sd() %>% as.vector
                       }),
                       CumulContaWorkersInf = sapply(MCvy, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.025, na.rm = T) %>% as.vector
                       }),
                       CumulContaWorkersMed = sapply(MCvy, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.5, na.rm = T) %>% as.vector
                       }),
                       CumulContaWorkersSup = sapply(MCvy, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.975, na.rm = T) %>% as.vector
                       }),
                       CumulContaWorkersMean = sapply(MCvy, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% mean %>% as.vector
                       }),
                       CumulContaWorkersSd = sapply(MCvy, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% sd %>% as.vector
                       }),
                       ProdInf = tapply(loss$Prod, loss$Cvy, function(y) quantile(y, probs = 0.025)) %>% as.vector(),
                       ProdMed = tapply(loss$Prod, loss$Cvy, function(y) quantile(y, probs = 0.5)) %>% as.vector(),
                       ProdSup = tapply(loss$Prod, loss$Cvy, function(y) quantile(y, probs = 0.75)) %>% as.vector(),
                       ProdMean = tapply(loss$Prod, loss$Cvy, mean) %>% as.vector(),
                       ProdSd = tapply(loss$Prod, loss$Cvy, sd) %>% as.vector()
)


gMCvy_WorkersConta <- ggplot(data = SMR_MCvy) + 
  theme(axis.ticks=element_blank(),
        #legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_point(mapping = aes(x = Mask, y = Cvy, size = CumulContaWorkersSup), shape =1, colour = "gray") + 
  geom_point(mapping = aes(x = Mask, y = Cvy, size = CumulContaWorkersMean)) + 
  #geom_point(mapping = aes(x = Mask, y = Cvy, size = CumulContaWorkersInf), colour = "gray") + 
  labs(title = "Workers contamination (cumulative after 42 days)\n 40-50 simulations / point") +
  scale_x_continuous(breaks = unique(Mask)) +
  scale_y_continuous(breaks = unique(Cvy)) +
  scale_size(range = c(1,15)) +
  xlab("Mask wearing (%)") + ylab("Length of conveyors (m)")
ggplotly(gMCvy_WorkersConta)


gMCvy_SurfacesConta <- ggplot(data = mutate(SMR_MCvy,
                                            SurfacesContaSup100 = SurfacesContaSup*100,
                                            SurfacesContaMean100 = SurfacesContaMean*100)) + 
  theme(axis.ticks=element_blank(),
        #legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_point(mapping = aes(x = Mask, y = Cvy, size = SurfacesContaSup100), shape =1, colour = "gray") + 
  geom_point(mapping = aes(x = Mask, y = Cvy, size = SurfacesContaMean100), colour = "navyblue") + 
  #geom_point(mapping = aes(x = Mask, y = Cvy, size = CumulContaWorkersInf), colour = "gray") + 
  labs(title = "Daily surfaces contamination ratio (across 42 days)\n 40-50 simulations / point") +
  scale_x_continuous(breaks = unique(Mask)) +
  scale_y_continuous(breaks = unique(Cvy)) +
  scale_size(range = c(1,15)) +
  xlab("Mask wearing (%)") + ylab("Length of conveyors (m)")
ggplotly(gMCvy_SurfacesConta)



gMCvy_FoodConta <- ggplot(data = mutate(SMR_MCvy,
                                        FoodContaSup100 = FoodContaSup*100,
                                        FoodContaMean100 = FoodContaMean*100)) + 
  theme(axis.ticks=element_blank(),
        #legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_point(mapping = aes(x = Mask, y = Cvy, size = FoodContaSup100), shape =1, colour = "gray") + 
  geom_point(mapping = aes(x = Mask, y = Cvy, size = FoodContaMean100), colour = "darkorange") + 
  #geom_point(mapping = aes(x = Mask, y = Cvy, size = CumulContaWorkersInf), colour = "gray") + 
  labs(title = "Daily Food contamination ratio (across 42 days)\n 40-50 simulations / point") +
  scale_x_continuous(breaks = unique(Mask)) +
  scale_y_continuous(breaks = unique(Cvy)) +
  scale_size(range = c(1,15)) +
  xlab("Mask wearing (%)") + ylab("Length of conveyors (m)")
ggplotly(gMCvy_FoodConta)



write.csv(SMR_MCvy, file = "simulation_output/MCVy_detection3log.csv")




