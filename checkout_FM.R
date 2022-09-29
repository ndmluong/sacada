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

# # OUTPUT EXTRACTION / ONLY WITH MIGALE >>>>> go to checkpoint ####
# ## Raw output from separate seeds ####
# # (check the XlSX file for the scenario code !)
# # NB: the number of elements found in raw_FM1, raw_RM2... may vary depending on the simulation progress
# raw_FM1 <- f_checkout(scenario = "FM1")
# raw_FM2 <- f_checkout(scenario = "FM2")
# raw_FM3 <- f_checkout(scenario = "FM3")
# raw_FM4 <- f_checkout(scenario = "FM4")
# 
# raw_FM1b <- f_checkout(scenario = "FM1b")
# raw_FM2b <- f_checkout(scenario = "FM2b")
# raw_FM3b <- f_checkout(scenario = "FM3b")
# raw_FM4b <- f_checkout(scenario = "FM4b")
# 
# ## Summarize raw output ####
# FM1 <- f_summaryOutput(rawoutput = raw_FM1)
# FM2 <- f_summaryOutput(rawoutput = raw_FM2)
# FM3 <- f_summaryOutput(rawoutput = raw_FM3)
# FM4 <- f_summaryOutput(rawoutput = raw_FM4)
# 
# FM1b <- f_summaryOutput(rawoutput = raw_FM1b)
# FM2b <- f_summaryOutput(rawoutput = raw_FM2b)
# FM3b <- f_summaryOutput(rawoutput = raw_FM3b)
# FM4b <- f_summaryOutput(rawoutput = raw_FM4b)

# >>>> checkpoint 1 <<<<< ####
## NB: check if the RData is available in the simulation_output directory
load(file = "simulation_output/output_FM.RData")

# PLOT ####
## Contaminated workers ####
f_plotContaminatedWorkers(IL = FM1$IL, IS = FM1$IS)
f_plotContaminatedWorkers(IL = FM2$IL, IS = FM2$IS)
f_plotContaminatedWorkers(IL = FM3$IL, IS = FM3$IS)
f_plotContaminatedWorkers(IL = FM4$IL, IS = FM4$IS)

## Contaminated workers: detailed plots for random seeds
f_plotContaminatedWorkers(IL = FM1$IL, IS = FM1$IS, seed_select = sample(FM1$all_seeds, 20), detailed_plot = TRUE, wrap.nrow = 4)
f_plotContaminatedWorkers(IL = FM2$IL, IS = FM2$IS, seed_select = sample(FM2$all_seeds, 20), detailed_plot = TRUE, wrap.nrow = 4)
f_plotContaminatedWorkers(IL = FM3$IL, IS = FM3$IS, seed_select = sample(FM3$all_seeds, 20), detailed_plot = TRUE, wrap.nrow = 4)
f_plotContaminatedWorkers(IL = FM4$IL, IS = FM4$IS, seed_select = sample(FM4$all_seeds, 20), detailed_plot = TRUE, wrap.nrow = 4)


# INDICATORS SUMMARY ####
## Cumulative number of infected workers after the whole period
f_smrzCumulContaWorkers(IL = FM1$IL, IS = FM1$IS)
f_smrzCumulContaWorkers(IL = FM1$IL, IS = FM1$IS)$cumul %>% mean()

f_smrzCumulContaWorkers(IL = FM2$IL, IS = FM2$IS)$cumul %>% mean()
f_smrzCumulContaWorkers(IL = FM3$IL, IS = FM3$IS)$cumul %>% mean()
f_smrzCumulContaWorkers(IL = FM4$IL, IS = FM4$IS)$cumul %>% mean()

## Food contamination
f_smrzAverageFoodContaRatio(FPS = FM1$FPS, detection = 3)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM2$FPS, detection = 3)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM3$FPS, detection = 3)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM4$FPS, detection = 3)$contaratio %>% mean()

f_smrzAverageFoodContaRatio(FPS = FM1$FPS, detection = 4)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM2$FPS, detection = 4)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM3$FPS, detection = 4)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM4$FPS, detection = 4)$contaratio %>% mean()

f_smrzAverageFoodContaRatio(FPS = FM1$FPS, detection = 5)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM2$FPS, detection = 5)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM3$FPS, detection = 5)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM4$FPS, detection = 5)$contaratio %>% mean()

f_smrzAverageFoodContaRatio(FPS = FM1$FPS, detection = 6)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM2$FPS, detection = 6)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM3$FPS, detection = 6)$contaratio %>% mean()
f_smrzAverageFoodContaRatio(FPS = FM4$FPS, detection = 6)$contaratio %>% mean()


## Surfaces contamination
f_smrzAverageSurfacesContaRatio(SS = FM1$SS, plant = FM1$MyPlant, detection = 5)$contaratio %>% mean()
f_smrzAverageSurfacesContaRatio(SS = FM1$SS, plant = FM1$MyPlant, detection = 5)$contaratio %>% sd()

f_smrzAverageSurfacesContaRatio(SS = FM2$SS, plant = FM1$MyPlant, detection = 5)$contaratio %>% mean()
f_smrzAverageSurfacesContaRatio(SS = FM2$SS, plant = FM1$MyPlant, detection = 5)$contaratio %>% sd()

f_smrzAverageSurfacesContaRatio(SS = FM3$SS, plant = FM1$MyPlant, detection = 5)$contaratio %>% mean()
f_smrzAverageSurfacesContaRatio(SS = FM4$SS, plant = FM1$MyPlant, detection = 5)$contaratio %>% mean()

## Transmission rate
f_smrzRt(IS = FM1$IS, prm_conta = FM1$Parms_Conta, prm_workers = FM1$Parms_Workers)$Rt %>% mean(., na.rm=TRUE)
f_smrzRt(IS = FM1$IS, prm_conta = FM1$Parms_Conta, prm_workers = FM1$Parms_Workers)$Rt %>% sd(., na.rm=TRUE)

f_smrzRt(IS = FM2$IS, prm_conta = FM2$Parms_Conta, prm_workers = FM2$Parms_Workers)$Rt %>% mean(., na.rm=TRUE)
f_smrzRt(IS = FM2$IS, prm_conta = FM2$Parms_Conta, prm_workers = FM2$Parms_Workers)$Rt %>% sd(., na.rm=TRUE)

f_smrzRt(IS = FM3$IS, prm_conta = FM3$Parms_Conta, prm_workers = FM3$Parms_Workers)$Rt %>% mean(., na.rm=TRUE)
f_smrzRt(IS = FM3$IS, prm_conta = FM3$Parms_Conta, prm_workers = FM3$Parms_Workers)$Rt %>% sd(., na.rm=TRUE)

f_smrzRt(IS = FM4$IS, prm_conta = FM4$Parms_Conta, prm_workers = FM4$Parms_Workers)$Rt %>% mean(., na.rm=TRUE)
f_smrzRt(IS = FM4$IS, prm_conta = FM4$Parms_Conta, prm_workers = FM4$Parms_Workers)$Rt %>% sd(., na.rm=TRUE)



f_smrzRt(IS = FMMask0$IS, prm_conta = FMMask0$Parms_Conta, prm_workers = FMMask0$Parms_Workers)$Rt %>% mean(., na.rm=TRUE)
f_smrzRt(IS = FMMask20$IS, prm_conta = FMMask20$Parms_Conta, prm_workers = FMMask20$Parms_Workers)$Rt %>% mean(., na.rm=TRUE)
f_smrzRt(IS = FMMask40$IS, prm_conta = FMMask40$Parms_Conta, prm_workers = FMMask40$Parms_Workers)$Rt %>% mean(., na.rm=TRUE)
f_smrzRt(IS = FMMask60$IS, prm_conta = FMMask60$Parms_Conta, prm_workers = FMMask60$Parms_Workers)$Rt %>% mean(., na.rm=TRUE)
f_smrzRt(IS = FMMask80$IS, prm_conta = FMMask80$Parms_Conta, prm_workers = FMMask80$Parms_Workers)$Rt %>% mean(., na.rm=TRUE)
f_smrzRt(IS = FMMask90$IS, prm_conta = FMMask90$Parms_Conta, prm_workers = FMMask90$Parms_Workers)$Rt %>% mean(., na.rm=TRUE)
f_smrzRt(IS = FMMask95$IS, prm_conta = FMMask95$Parms_Conta, prm_workers = FMMask95$Parms_Workers)$Rt %>% mean(., na.rm=TRUE)
f_smrzRt(IS = FMMask100$IS, prm_conta = FMMask100$Parms_Conta, prm_workers = FMMask100$Parms_Workers)$Rt %>% mean(., na.rm=TRUE)




# >>>> checkpoint 2 <<<<< ####
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
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% quantile(probs = 0.25) %>% as.vector
                       }),
                       SurfacesContaMed = sapply(FMMask, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% quantile(prob = 0.5) %>% as.vector
                       }),
                       SurfacesContaSup = sapply(FMMask, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% quantile(prob = 0.75) %>% as.vector
                       }),
                       SurfacesContaMean = sapply(FMMask, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% mean() %>% as.vector
                       }),
                       SurfacesContaSd = sapply(FMMask, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% sd() %>% as.vector
                       }),
                       FoodContaInf = sapply(FMMask, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% quantile(probs = 0.25) %>% as.vector
                       }),
                       FoodContaMed = sapply(FMMask, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% quantile(probs = 0.5) %>% as.vector
                       }),
                       FoodContaSup = sapply(FMMask, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% quantile(probs = 0.75) %>% as.vector
                       }),
                       FoodContaMean = sapply(FMMask, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% mean() %>% as.vector
                       }),
                       FoodContaSd = sapply(FMMask, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% sd() %>% as.vector
                       }),
                       CumulContaWorkersInf = sapply(FMMask, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.25, na.rm = T) %>% as.vector
                       }),
                       CumulContaWorkersMed = sapply(FMMask, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.5, na.rm = T) %>% as.vector
                       }),
                       CumulContaWorkersSup = sapply(FMMask, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.75, na.rm = T) %>% as.vector
                       }),
                       CumulContaWorkersMean = sapply(FMMask, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% mean %>% as.vector
                       }),
                       CumulContaWorkersSd = sapply(FMMask, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% sd %>% as.vector
                       }),
                       ProdInf = tapply(loss$Prod, loss$Mask, function(y) quantile(y, probs = 0.25)) %>% as.vector(),
                       ProdMed = tapply(loss$Prod, loss$Mask, function(y) quantile(y, probs = 0.5)) %>% as.vector(),
                       ProdSup = tapply(loss$Prod, loss$Mask, function(y) quantile(y, probs = 0.75)) %>% as.vector(),
                       ProdMean = tapply(loss$Prod, loss$Mask, mean) %>% as.vector(),
                       ProdSd = tapply(loss$Prod, loss$Mask, sd) %>% as.vector()
                       )


# lapply(FMMask, function(x) {
#   SurfacesConta <- f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio
#   FoodConta <- f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio
#   d1 <- data.frame(Mask = rep(x$Parms_Workers$pMaskAcceptability[[x$Parms_Workers$MaskType]], length(SurfacesConta)),
#                    SurfacesConta = SurfacesConta,
#                    FoodConta = FoodConta)
#   return(d1)
# }) %>%
#   data.table::rbindlist() -> SMR
# 
# SMR$Mask <- as.factor(SMR$Mask)
# 
# ggplot(data = SMR) + 
#   theme(axis.ticks=element_blank(),
#         legend.position = "none",
#         panel.background=element_rect(fill="white"),
#         plot.title = element_text(face = "bold", size=10),
#         axis.title = element_text(size=10),
#         axis.text = element_text(size=10),
#         panel.grid.major.y=element_line(colour="lightgrey"),
#         panel.grid.major.x=element_line(colour="lightgrey"),
#         panel.grid.minor.y=element_line(colour="white"),
#         panel.grid.minor.x=element_line(colour="lightgrey")) +
#   geom_violin(mapping = aes(x = Mask, y = SurfacesConta))


SMR_Mask <- subset(SMR_Mask, ! Mask %in% c(0, 80))

gMask_SurfacesConta <- ggplot(data = SMR_Mask) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Mask, y = SurfacesContaMean * 100), size = 1) + 
  geom_segment(mapping = aes(x = Mask, xend = Mask,
                             y = (SurfacesContaMean + SurfacesContaSd) * 100,
                             yend = (SurfacesContaMean - SurfacesContaSd) * 100,), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Mask, y = SurfacesContaMean * 100), size = 3) +
  labs(title = "Surface contamination") +
  xlab("Mask wearing (%)") + ylab("Surface contamination (%)")
  
# ggplot(data = subset(SMR_Mask, ! Mask %in% c(0,80))) +
#   theme(axis.ticks=element_blank(),
#         legend.position = "none",
#         panel.background=element_rect(fill="white"),
#         plot.title = element_text(face = "bold", size=14),
#         axis.title = element_text(size=10),
#         axis.text = element_text(size=10),
#         panel.grid.major.y=element_line(colour="lightgrey"),
#         panel.grid.major.x=element_line(colour="lightgrey"),
#         panel.grid.minor.y=element_line(colour="white"),
#         panel.grid.minor.x=element_line(colour="lightgrey")) +
#   geom_line(mapping = aes(x = Mask, y = SurfacesContaMed * 100), size = 1.5, colour = "blue") +
#   geom_segment(mapping = aes(x = Mask, xend = Mask,
#                              y = SurfacesContaSup * 100,
#                              yend = SurfacesContaInf * 100), size = 1.5, colour = "lightblue") +
#   geom_point(mapping = aes(x = Mask, y = SurfacesContaMed * 100), size = 3) +
#   labs(title = "Surface contamination") +
#   xlab("Mask wearing (%)") + ylab("Surface contamination (%)")

gMask_FoodConta <- ggplot(data = SMR_Mask) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Mask, y = FoodContaMean * 100), size = 1) + 
  geom_segment(mapping = aes(x = Mask, xend = Mask,
                             y = (FoodContaMean + FoodContaSd) * 100,
                             yend = (FoodContaMean - FoodContaSd) * 100), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Mask, y = FoodContaMean * 100), size = 3) +
  labs(title = "Food contamination") +
  xlab("Mask wearing (%)") + ylab("Food contamination (%)")

gMask_WorkersConta <- ggplot(data = SMR_Mask) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Mask, y = CumulContaWorkersMean), size = 1) + 
  geom_segment(mapping = aes(x = Mask, xend = Mask,
                             y = CumulContaWorkersMean + CumulContaWorkersSd,
                             yend = CumulContaWorkersMean - CumulContaWorkersSd), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Mask, y = CumulContaWorkersMean), size = 3) +
  labs(title = "Workers contamination") +
  xlab("Mask wearing (%)") + ylab("Cumulative number of contaminated workers after 42 days")

gMask_Prod <- ggplot(data = SMR_Mask) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Mask, y = ProdMean * 100), size = 1) + 
  geom_segment(mapping = aes(x = Mask, xend = Mask,
                             y = (ProdMean + ProdSd) * 100,
                             yend = (ProdMean - ProdSd) * 100), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Mask, y = ProdMean * 100), size = 3) +
  labs(title = "Food production (losses due to absences)") +
  xlab("Mask wearing (%)") + ylab("Food production (%)")

g_FMMask <- ggarrange(gMask_WorkersConta, gMask_SurfacesConta, gMask_FoodConta, gMask_Prod, ncol = 1)

ggsave(filename="simulation_output/plot/g_FMMask.pdf", 
       plot = g_FMMask, 
       device = cairo_pdf, width = 297, height = 420, units = "mm")


























# >>>> checkpoint 3 <<<<< ####
## NB: check if the RData is available in the simulation_output directory
load(file = "simulation_output/output_FMAirEffect.RData")


FMAir <- list(FMAir0, FMAir600, FMAir1200, FMAir1800, FMAir2400, FMAir3000, FMAir3600, FMAir4800)


lapply(FMAir, function(x) {
  min_carcass <- tapply(x$FPS$nb_carcass, x$FPS$seed, function(y) min(y[y>0])) %>% as.vector() 
  max_carcass <- tapply(x$FPS$nb_carcass, x$FPS$seed, max) %>% as.vector()
  d1 <- data.frame(Air = rep(x$Parms_Plant$Air_renewal, length(min_carcass)),
                   Prod = min_carcass / max_carcass)
  return(d1)
}) %>% rbindlist() -> loss


SMR_Air <- data.frame(Air = c(0, 600, 1200, 1800, 2400, 3000, 3600, 4800),
                       SurfacesContaInf = sapply(FMAir, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% quantile(probs = 0.25) %>% as.vector
                       }),
                       SurfacesContaMed = sapply(FMAir, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% quantile(prob = 0.5) %>% as.vector
                       }),
                       SurfacesContaSup = sapply(FMAir, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% quantile(prob = 0.75) %>% as.vector
                       }),
                       SurfacesContaMean = sapply(FMAir, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% mean() %>% as.vector
                       }),
                       SurfacesContaSd = sapply(FMAir, function(x) {
                         f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio %>% sd() %>% as.vector
                       }),
                       FoodContaInf = sapply(FMAir, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% quantile(probs = 0.25) %>% as.vector
                       }),
                       FoodContaMed = sapply(FMAir, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% quantile(probs = 0.5) %>% as.vector
                       }),
                       FoodContaSup = sapply(FMAir, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% quantile(probs = 0.75) %>% as.vector
                       }),
                       FoodContaMean = sapply(FMAir, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% mean() %>% as.vector
                       }),
                       FoodContaSd = sapply(FMAir, function(x) {
                         f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio %>% sd() %>% as.vector
                       }),
                       CumulContaWorkersInf = sapply(FMAir, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.25, na.rm = T) %>% as.vector
                       }),
                       CumulContaWorkersMed = sapply(FMAir, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.5, na.rm = T) %>% as.vector
                       }),
                       CumulContaWorkersSup = sapply(FMAir, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(probs = 0.75, na.rm = T) %>% as.vector
                       }),
                       CumulContaWorkersMean = sapply(FMAir, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% mean %>% as.vector
                       }),
                       CumulContaWorkersSd = sapply(FMAir, function(x) {
                         f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% sd %>% as.vector
                       }),
                       ProdInf = tapply(loss$Prod, loss$Air, function(y) quantile(y, probs = 0.25)) %>% as.vector(),
                       ProdMed = tapply(loss$Prod, loss$Air, function(y) quantile(y, probs = 0.5)) %>% as.vector(),
                       ProdSup = tapply(loss$Prod, loss$Air, function(y) quantile(y, probs = 0.75)) %>% as.vector(),
                       ProdMean = tapply(loss$Prod, loss$Air, mean) %>% as.vector(),
                       ProdSd = tapply(loss$Prod, loss$Air, sd) %>% as.vector()
)


# lapply(FMMask, function(x) {
#   SurfacesConta <- f_smrzAverageSurfacesContaRatio(SS = x$SS, plant = x$MyPlant, detection = 5)$contaratio
#   FoodConta <- f_smrzAverageFoodContaRatio(FPS = x$FPS, detection = 5)$contaratio
#   d1 <- data.frame(Mask = rep(x$Parms_Workers$pMaskAcceptability[[x$Parms_Workers$MaskType]], length(SurfacesConta)),
#                    SurfacesConta = SurfacesConta,
#                    FoodConta = FoodConta)
#   return(d1)
# }) %>%
#   data.table::rbindlist() -> SMR
# 
# SMR$Mask <- as.factor(SMR$Mask)
# 
# ggplot(data = SMR) + 
#   theme(axis.ticks=element_blank(),
#         legend.position = "none",
#         panel.background=element_rect(fill="white"),
#         plot.title = element_text(face = "bold", size=10),
#         axis.title = element_text(size=10),
#         axis.text = element_text(size=10),
#         panel.grid.major.y=element_line(colour="lightgrey"),
#         panel.grid.major.x=element_line(colour="lightgrey"),
#         panel.grid.minor.y=element_line(colour="white"),
#         panel.grid.minor.x=element_line(colour="lightgrey")) +
#   geom_violin(mapping = aes(x = Mask, y = SurfacesConta))


SMR_Air <- subset(SMR_Air, ! Air %in% c(0, 2400))

gAir_SurfacesConta <- ggplot(data = SMR_Air) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Air, y = SurfacesContaMean * 100), size = 1) + 
  geom_segment(mapping = aes(x = Air, xend = Air,
                             y = (SurfacesContaMean + SurfacesContaSd) * 100,
                             yend = (SurfacesContaMean - SurfacesContaSd) * 100,), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Air, y = SurfacesContaMean * 100), size = 3) +
  labs(title = "Surface contamination") +
  xlab("Air renewal (m3/h)") + ylab("Surface contamination (%)")

# ggplot(data = subset(SMR_Air, ! Air %in% c(0,80))) +
#   theme(axis.ticks=element_blank(),
#         legend.position = "none",
#         panel.background=element_rect(fill="white"),
#         plot.title = element_text(face = "bold", size=14),
#         axis.title = element_text(size=10),
#         axis.text = element_text(size=10),
#         panel.grid.major.y=element_line(colour="lightgrey"),
#         panel.grid.major.x=element_line(colour="lightgrey"),
#         panel.grid.minor.y=element_line(colour="white"),
#         panel.grid.minor.x=element_line(colour="lightgrey")) +
#   geom_line(mapping = aes(x = Air, y = SurfacesContaMed * 100), size = 1.5, colour = "blue") +
#   geom_segment(mapping = aes(x = Air, xend = Air,
#                              y = SurfacesContaSup * 100,
#                              yend = SurfacesContaInf * 100), size = 1.5, colour = "lightblue") +
#   geom_point(mapping = aes(x = Air, y = SurfacesContaMed * 100), size = 3) +
#   labs(title = "Surface contamination") +
#   xlab("Air renewal (m3/h)") + ylab("Surface contamination (%)")

gAir_FoodConta <- ggplot(data = SMR_Air) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Air, y = FoodContaMean * 100), size = 1) + 
  geom_segment(mapping = aes(x = Air, xend = Air,
                             y = (FoodContaMean + FoodContaSd) * 100,
                             yend = (FoodContaMean - FoodContaSd) * 100), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Air, y = FoodContaMean * 100), size = 3) +
  labs(title = "Food contamination") +
  xlab("Air renewal (m3/h)") + ylab("Food contamination (%)")

gAir_WorkersConta <- ggplot(data = SMR_Air) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Air, y = CumulContaWorkersMean), size = 1) + 
  geom_segment(mapping = aes(x = Air, xend = Air,
                             y = CumulContaWorkersMean + CumulContaWorkersSd,
                             yend = CumulContaWorkersMean - CumulContaWorkersSd), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Air, y = CumulContaWorkersMean), size = 3) +
  labs(title = "Workers contamination") +
  xlab("Air renewal (m3/h)") + ylab("Cumulative number of contaminated workers\nafter 42 days")

gAir_Prod <- ggplot(data = SMR_Air) + 
  theme(axis.ticks=element_blank(),
        legend.position = "none",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_line(colour="lightgrey")) +
  geom_line(mapping = aes(x = Air, y = ProdMean * 100), size = 1) + 
  geom_segment(mapping = aes(x = Air, xend = Air,
                             y = (ProdMean + ProdSd) * 100,
                             yend = (ProdMean - ProdSd) * 100), size = 1.5, colour = "darkgray") +
  geom_point(mapping = aes(x = Air, y = ProdMean * 100), size = 3) +
  labs(title = "Food production (losses due to absences)") +
  xlab("Air renewal (m3/h)") + ylab("Food production (%)")

g_FMAir <- ggarrange(gAir_WorkersConta, gAir_SurfacesConta, gAir_FoodConta, gAir_Prod, ncol = 1)

ggsave(filename="simulation_output/plot/g_FMAir.pdf", 
       plot = g_FMAir, 
       device = cairo_pdf, width = 297, height = 420, units = "mm")




