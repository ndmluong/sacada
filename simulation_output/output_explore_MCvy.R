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
library(cowplot)
library(tidyverse)

# LOADING SIMULATED DATA ####
## MCvy 12/22 - Cross effect of mask wearing and social distancing ####
# load(file = "simulation_output/20220919_output_MCvy.RData") # archives
load(file = "simulation_output/20221201_output_MCvy.RData")

MCvy <- list(M0Cvy11, M0Cvy14, M0Cvy17, M0Cvy20,
             M25Cvy11, M25Cvy14, M25Cvy17, M25Cvy20,
             M50Cvy11, M50Cvy14, M50Cvy17, M50Cvy20,
             M80Cvy11, M80Cvy14, M80Cvy17, M80Cvy20,
             M100Cvy11, M100Cvy14, M100Cvy17, M100Cvy20)

gdata::keep(MCvy, sure = TRUE)

# FUNCTIONS #####
source("functions/functions.R")

# DATA PRETREATMENT ####
## Factors levels ####
Mask <- c(rep(0,4), rep(25,4), rep(50,4), rep(80,4), rep(100,4))
Cvy <- rep(c(11,14,17,20), 5)


## Contamination threshold ####
RNA_detection <- 5


## Summary data frame ####
SMR_MCvy <- data.frame(Mask = Mask,
                       Cvy = Cvy)
SMR_MCvy$Cvy <- as.factor(SMR_MCvy$Cvy)

### Daily surfaces contamination ####
SMR_MCvy %>%
  mutate(., SurfacesContaMean_clus1 = sapply(MCvy, function(x) {
    f_smrzAverageSurfacesContaRatio(SS = x$SS, IS = x$IS, plant = x$MyPlant, detection = RNA_detection)$contaratio %>% mean() %>% as.vector
  })) %>%
  mutate(., SurfacesContaSd_clus1 = sapply(MCvy, function(x) {
    f_smrzAverageSurfacesContaRatio(SS = x$SS, IS = x$IS, plant = x$MyPlant, detection = RNA_detection)$contaratio %>% sd() %>% as.vector
  })) -> SMR_MCvy

# Average daily surfaces contamination in case of cluster 
lapply(MCvy, function(x) {
  SurfacesContaRatio <- f_smrzAverageSurfacesContaRatio(SS = x$SS, IS = x$IS, plant = x$MyPlant, detection = RNA_detection)
  return(data.frame(seed = SurfacesContaRatio$seed,
                    Mask = rep(x$Parms_Workers$pMaskAcceptability[["Surgical mask"]], nrow(SurfacesContaRatio)),
                    Cvy = rep(x$Parms_Plant$Objects$cvy$dim.X, nrow(SurfacesContaRatio)),
                    SurfacesContaRatio = SurfacesContaRatio$contaratio))
}) %>%
  rbindlist() %>%
  mutate(., SurfacesContaRatioLog = log10(SurfacesContaRatio+1)) %>%
  mutate(., SurfacesContaRatioPercentage = SurfacesContaRatio * 100) -> AverageDailySurfacesContaRatio_clus1

AverageDailySurfacesContaRatio_clus1$Mask <- as.factor(AverageDailySurfacesContaRatio_clus1$Mask)
AverageDailySurfacesContaRatio_clus1$Cvy <- as.factor(AverageDailySurfacesContaRatio_clus1$Cvy)

# Average daily surfaces contamination in case of non-cluster 
lapply(MCvy, function(x) {
  SurfacesContaRatio <- f_smrzAverageSurfacesContaRatio(SS = x$SS, IS = x$IS, plant = x$MyPlant, cluster_filter = F, detection = RNA_detection)
  return(data.frame(seed = SurfacesContaRatio$seed,
                    Mask = rep(x$Parms_Workers$pMaskAcceptability[["Surgical mask"]], nrow(SurfacesContaRatio)),
                    Cvy = rep(x$Parms_Plant$Objects$cvy$dim.X, nrow(SurfacesContaRatio)),
                    SurfacesContaRatio = SurfacesContaRatio$contaratio))
}) %>%
  rbindlist() %>%
  mutate(., SurfacesContaRatioLog = log10(SurfacesContaRatio+1)) %>%
  mutate(., SurfacesContaRatioPercentage = SurfacesContaRatio * 100) -> AverageDailySurfacesContaRatio_clus0

AverageDailySurfacesContaRatio_clus0$Mask <- as.factor(AverageDailySurfacesContaRatio_clus0$Mask)
AverageDailySurfacesContaRatio_clus0$Cvy <- as.factor(AverageDailySurfacesContaRatio_clus0$Cvy)








# tapply(AverageDailySurfacesContaRatio$SurfacesContaRatioPercentage,
#        list(AverageDailySurfacesContaRatio$Mask, AverageDailySurfacesContaRatio$Cvy),
#        median)


### Cumulative workers contamination after 42 days ####
SMR_MCvy %>%
  mutate(., CumulContaWorkersMean = sapply(MCvy, function(x) {
    f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% mean %>% as.vector
  })) %>%
  mutate(., CumulContaWorkers_q05 = sapply(MCvy, function(x) {
    f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(., probs = 0.05) %>% as.vector
  })) %>%
  mutate(., CumulContaWorkers_q50 = sapply(MCvy, function(x) {
    f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(., probs = 0.50) %>% as.vector
  })) %>%
  mutate(., CumulContaWorkers_q95 = sapply(MCvy, function(x) {
    f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% quantile(., probs = 0.95) %>% as.vector
  })) %>%
  mutate(., CumulContaWorkers_max = sapply(MCvy, function(x) {
    f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% max() %>% as.vector
  })) %>%
  mutate(., CumulContaWorkersSd = sapply(MCvy, function(x) {
    f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)$cumul %>% sd %>% as.vector
  })) -> SMR_MCvy

lapply(MCvy, function(x) {
  CumulContaWorkers_df <- f_smrzCumulContaWorkers(IL = x$IL, IS = x$IS)
  return(data.frame(seed = CumulContaWorkers_df$seed,
                    Mask = rep(x$Parms_Workers$pMaskAcceptability[["Surgical mask"]], nrow(CumulContaWorkers_df)),
                    Cvy = rep(x$Parms_Plant$Objects$cvy$dim.X, nrow(CumulContaWorkers_df)),
                    CumulContaWorkers = CumulContaWorkers_df$cumul))
}) %>%
  rbindlist() -> CumulContaWorkers







### Daily food contamination ####
#### Summary ####
SMR_MCvy %>%
  mutate(., FoodContaMean_clus1 = sapply(MCvy, function(x) {
    f_smrzAverageFoodContaRatio(FPS = x$FPS, IS = x$IS, detection = RNA_detection)$contaratio %>% mean() %>% as.vector
  })) %>%
  mutate(., FoodContaSd_clus1 = sapply(MCvy, function(x) {
    f_smrzAverageFoodContaRatio(FPS = x$FPS, IS = x$IS, detection = RNA_detection)$contaratio %>% sd() %>% as.vector
  })) -> SMR_MCvy

## Statistical analyses
lapply(MCvy, function(x) {
  FoodContaRatio <- f_smrzAverageFoodContaRatio(FPS = x$FPS, IS = x$IS, detection = RNA_detection)
  return(data.frame(seed = FoodContaRatio$seed,
                    Mask = rep(x$Parms_Workers$pMaskAcceptability[["Surgical mask"]], nrow(FoodContaRatio)),
                    Cvy = rep(x$Parms_Plant$Objects$cvy$dim.X, nrow(FoodContaRatio)),
                    FoodContaRatio = FoodContaRatio$contaratio))
  }) %>%
  rbindlist() %>%
  mutate(., FoodContaRatioLog = log10(FoodContaRatio+1)) %>%
  mutate(., FoodContaRatioPercentage = FoodContaRatio * 100) -> AverageDailyFoodContaRatio_clus1

AverageDailyFoodContaRatio_clus1$Mask <- as.factor(AverageDailyFoodContaRatio_clus1$Mask)
AverageDailyFoodContaRatio_clus1$Cvy <- as.factor(AverageDailyFoodContaRatio_clus1$Cvy)

lapply(MCvy, function(x) {
  FoodContaRatio <- f_smrzAverageFoodContaRatio(FPS = x$FPS, IS = x$IS, cluster_filter = F, detection = RNA_detection)
  return(data.frame(seed = FoodContaRatio$seed,
                    Mask = rep(x$Parms_Workers$pMaskAcceptability[["Surgical mask"]], nrow(FoodContaRatio)),
                    Cvy = rep(x$Parms_Plant$Objects$cvy$dim.X, nrow(FoodContaRatio)),
                    FoodContaRatio = FoodContaRatio$contaratio))
}) %>%
  rbindlist() %>%
  mutate(., FoodContaRatioLog = log10(FoodContaRatio+1)) %>%
  mutate(., FoodContaRatioPercentage = FoodContaRatio * 100) -> AverageDailyFoodContaRatio_clus0

AverageDailyFoodContaRatio_clus0$Mask <- as.factor(AverageDailyFoodContaRatio_clus0$Mask)
AverageDailyFoodContaRatio_clus0$Cvy <- as.factor(AverageDailyFoodContaRatio_clus0$Cvy)




# tapply(AverageDailyFoodContaRatio$FoodContaRatioPercentage,
#        list(AverageDailyFoodContaRatio$Mask, AverageDailyFoodContaRatio$Cvy),
#        quantile, probs = c(0.95))

# aov.rnk <- aov(
#   FoodContaRatio ~ Mask * Cvy, data = AverageDailyFoodContaRatio)
# anova(aov.rnk)
# par(mfrow=c(2,2))
# plot(aov.rnk)
# 
# aov.log <- aov(
#   FoodContaRatioLog ~ Mask * Cvy, data = AverageDailyFoodContaRatio)
# anova(aov.log)
# summary(aov.log)
# par(mfrow=c(2,2))
# plot(aov.log)

### Transmission rate ####


### Production losses ####
lapply(MCvy, function(x) {
  min_carcass <- tapply(x$FPS$nb_carcass, x$FPS$seed, function(y) min(y[y>0])) %>% as.vector() 
  max_carcass <- tapply(x$FPS$nb_carcass, x$FPS$seed, max) %>% as.vector()
  d1 <- data.frame(seed = unique(x$FPS$seed),
                   Mask = rep(x$Parms_Workers$pMaskAcceptability[["Surgical mask"]], length(min_carcass)),
                   Cvy = rep(x$Parms_Plant$Objects$cvy$dim.X, length(min_carcass)),
                   Prod = min_carcass / max_carcass * 100)
  max_positive <- tapply(x$IS$Positive, x$IS$seed, max)
  cluster_vec <- max_positive[max_positive >= 25] ## cluster
  subset(d1, seed %in% names(cluster_vec))
}) %>% data.table::rbindlist() -> loss_clus1

# loss_clus1$Mask <- as.factor(loss_clus1$Mask)
loss_clus1$Cvy <- as.factor(loss_clus1$Cvy)

lapply(MCvy, function(x) {
  min_carcass <- tapply(x$FPS$nb_carcass, x$FPS$seed, function(y) min(y[y>0])) %>% as.vector() 
  max_carcass <- tapply(x$FPS$nb_carcass, x$FPS$seed, max) %>% as.vector()
  d1 <- data.frame(seed = unique(x$FPS$seed),
                   Mask = rep(x$Parms_Workers$pMaskAcceptability[["Surgical mask"]], length(min_carcass)),
                   Cvy = rep(x$Parms_Plant$Objects$cvy$dim.X, length(min_carcass)),
                   Prod = min_carcass / max_carcass * 100)
  max_positive <- tapply(x$IS$Positive, x$IS$seed, max)
  cluster_vec <- max_positive[max_positive < 25] ## cluster
  subset(d1, seed %in% names(cluster_vec))
}) %>% data.table::rbindlist() -> loss_clus0

# loss_clus0$Mask <- as.factor(loss_clus0$Mask)
loss_clus0$Cvy <- as.factor(loss_clus0$Cvy)


# SMR_MCvy %>%
#   mutate(., ProdMean = tapply(loss$Prod, loss$Cvy, mean) %>% as.vector()) %>%
#   mutate(., ProdSd = tapply(loss$Prod, loss$Cvy, sd) %>% as.vector()) -> SMR_MCvy


### ClusterProb ####
SMR_MCvy %>%
  mutate(., meanClusterProb = sapply(MCvy, function(mc) {
    f_ClusterProb(IS = mc$IS)
  })) -> SMR_MCvy


### ClusterPeak ####
# Summary metrics
# Mean
SMR_MCvy %>%
  mutate(., Peak_clus0_mean = sapply(MCvy, function(mc) {
    f_ClusterPeak(IS = mc$IS, cluster_filter = F, metrics = "mean")
  })) -> SMR_MCvy
SMR_MCvy %>%
  mutate(., Peak_clus1_mean = sapply(MCvy, function(mc) {
    f_ClusterPeak(IS = mc$IS, cluster_filter = T, metrics = "mean")
  })) -> SMR_MCvy

# Distribution metrics
# Metrics without cluster_filter
SMR_MCvy %>%
  mutate(., Peak_clus0_q05 = sapply(MCvy, function(mc) {
    f_ClusterPeak(IS = mc$IS, cluster_filter = F, metrics = "quantile", qtile = 0.05)
  })) %>%
  mutate(., Peak_clus0_q25 = sapply(MCvy, function(mc) {
    f_ClusterPeak(IS = mc$IS, cluster_filter = F, metrics = "quantile", qtile = 0.25)
  })) %>%
  mutate(., Peak_clus0_q50 = sapply(MCvy, function(mc) {
    f_ClusterPeak(IS = mc$IS, cluster_filter = F, metrics = "median")
  })) %>%
  mutate(., Peak_clus0_q75 = sapply(MCvy, function(mc) {
    f_ClusterPeak(IS = mc$IS, cluster_filter = F, metrics = "quantile", qtile = 0.75)
  })) %>%
  mutate(., Peak_clus0_q95 = sapply(MCvy, function(mc) {
    f_ClusterPeak(IS = mc$IS, cluster_filter = F, metrics = "quantile", qtile = 0.95)
  })) -> SMR_MCvy

# Metrics with cluster_filter
SMR_MCvy %>%
  mutate(., Peak_clus1_q05 = sapply(MCvy, function(mc) {
    f_ClusterPeak(IS = mc$IS, cluster_filter = T, metrics = "quantile", qtile = 0.05)
  })) %>%
  mutate(., Peak_clus1_q25 = sapply(MCvy, function(mc) {
    f_ClusterPeak(IS = mc$IS, cluster_filter = T, metrics = "quantile", qtile = 0.25)
  })) %>%
  mutate(., Peak_clus1_q50 = sapply(MCvy, function(mc) {
    f_ClusterPeak(IS = mc$IS, cluster_filter = T, metrics = "median")
  })) %>%
  mutate(., Peak_clus1_q75 = sapply(MCvy, function(mc) {
    f_ClusterPeak(IS = mc$IS, cluster_filter = T, metrics = "quantile", qtile = 0.75)
  })) %>%
  mutate(., Peak_clus1_q95 = sapply(MCvy, function(mc) {
    f_ClusterPeak(IS = mc$IS, cluster_filter = T, metrics = "quantile", qtile = 0.95)
  })) -> SMR_MCvy



### DaysBeforeCluster ####
# Summarizing metrics
SMR_MCvy %>%
  mutate(., DaysBeforeCluster = sapply(MCvy, function(mc) {
    f_DaysBeforeCluster(IS = mc$IS, metrics = "mean")
  })) %>%
  mutate(., DaysBeforeCluster_q05 = sapply(MCvy, function(mc) {
    f_DaysBeforeCluster(IS = mc$IS, metrics = "quantile", qtile = 0.05)
  })) %>%
  mutate(., DaysBeforeCluster_q25 = sapply(MCvy, function(mc) {
    f_DaysBeforeCluster(IS = mc$IS, metrics = "quantile", qtile = 0.25)
  })) %>%
  mutate(., DaysBeforeCluster_q50 = sapply(MCvy, function(mc) {
    f_DaysBeforeCluster(IS = mc$IS, metrics = "quantile", qtile = 0.5)
  }))  %>%
  mutate(., DaysBeforeCluster_q75 = sapply(MCvy, function(mc) {
    f_DaysBeforeCluster(IS = mc$IS, metrics = "quantile", qtile = 0.75)
  })) %>%
  mutate(., DaysBeforeCluster_q95 = sapply(MCvy, function(mc) {
    f_DaysBeforeCluster(IS = mc$IS, metrics = "quantile", qtile = 0.95)
  })) -> SMR_MCvy





### DaysBeforePeak ####
SMR_MCvy %>%
  mutate(., DaysBeforePeak_clus0_mean = sapply(MCvy, function(mc) {
    f_DaysBeforePeak(IS = mc$IS, cluster_filter = F, metrics = "mean")
  })) -> SMR_MCvy

SMR_MCvy %>%
  mutate(., DaysBeforePeak_clus1_mean = sapply(MCvy, function(mc) {
    f_DaysBeforePeak(IS = mc$IS, cluster_filter = T, metrics = "mean")
  })) -> SMR_MCvy




### ClusterSource ####
SMR_MCvy %>%
  mutate(., SourceInside_clus0_mean = sapply(MCvy, function(mc) {
    f_ClusterSource(IL = mc$IL, IS = mc$IS, cluster_filter = F)
  })) -> SMR_MCvy

SMR_MCvy %>%
  mutate(., SourceInside_clus1_mean = sapply(MCvy, function(mc) {
    f_ClusterSource(IL = mc$IL, IS = mc$IS, cluster_filter = T)
  })) -> SMR_MCvy


### Spatial distribution of the surface contamination
lapply(MCvy, function(mc) {
  f_surfacesOccurrences(res = mc)
}) -> spatial_conta




# PLOTS ####
## gMCvy_ClusterProb ####
gMCvy_ClusterProb <- ggplot(data = SMR_MCvy) +
  theme(axis.ticks=element_blank(),
        legend.position = "bottom",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank()) +
  geom_hline(yintercept = 0, linewidth = 1.5, alpha = 0.3) + geom_vline(xintercept = 0, linewidth = 1.5, alpha = 0.3) +
  geom_line(mapping = aes(x = Mask, y = meanClusterProb * 100, group = Cvy, colour = Cvy)) +
  geom_point(mapping = aes(x = Mask, y = meanClusterProb * 100, colour = Cvy), size = 3)  +
  scale_color_viridis(discrete = TRUE, option = "plasma", name = "Length of the conveyors (m)", begin = 0, end = 0.8) +
  scale_x_continuous(breaks = unique(Mask)) +
  xlab("Mask wearing (%)") + ylab("Cluster occurrence probability (in %)")
gMCvy_ClusterProb


## Peak_clus ####
# SMR_MCvy$Mask <- as.factor(SMR_MCvy$Mask)

g_Peak_clus <- ggplot(data = SMR_MCvy) +
  theme(axis.ticks=element_blank(),
        legend.position = "bottom",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=18),
        axis.text = element_text(size=16),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_blank()) +
  geom_vline(xintercept = -15, colour = "black", size = 1.2, alpha = 0.6) +
  geom_hline(yintercept = 0, colour = "black", size = 1.2, alpha = 0.6) +
  geom_hline(yintercept = 25, linetype = 5, colour = "red", size = 0.8, alpha = 0.5) +
  annotate("text", label = "Cluster definition threshold : 25 infected workers", x = 10, y = 27, colour = "red") +
  ## More than 25 infections 
  geom_point(mapping = aes(x = Mask, y = Peak_clus1_q50, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 4, shape = 16) +
  geom_point(mapping = aes(x = Mask, y = Peak_clus1_q25, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 2, shape = 1) +
  geom_point(mapping = aes(x = Mask, y = Peak_clus1_q75, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 2, shape = 1) +
  geom_linerange(mapping = aes(x = Mask, ymin = Peak_clus1_q25, ymax = Peak_clus1_q75, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 1.2) +
  geom_linerange(mapping = aes(x = Mask, ymin = Peak_clus1_q75, ymax = Peak_clus1_q95, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 1.2, alpha = 0.3) +
  geom_linerange(mapping = aes(x = Mask, ymin = Peak_clus1_q05, ymax = Peak_clus1_q25, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 1.2, alpha = 0.3) +
  ## Less than 25 infections
  geom_point(mapping = aes(x = Mask, y = Peak_clus0_q50, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 4, shape = 16) +
  geom_point(mapping = aes(x = Mask, y = Peak_clus0_q25, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 2, shape = 1) +
  geom_point(mapping = aes(x = Mask, y = Peak_clus0_q75, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 2, shape = 1) +
  geom_linerange(mapping = aes(x = Mask, ymin = Peak_clus0_q25, ymax = Peak_clus0_q75, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 1.2) +
  geom_linerange(mapping = aes(x = Mask, ymin = Peak_clus0_q75, ymax = Peak_clus0_q95, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 1.2, alpha = 0.3) +
  geom_linerange(mapping = aes(x = Mask, ymin = Peak_clus0_q05, ymax = Peak_clus0_q25, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 1.2, alpha = 0.3) +
  ## Annotate the cluster probability
  geom_text(aes(x = Mask, y = Peak_clus1_q95+2, label = paste("n=", meanClusterProb*100, sep = ""),
                group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 3) +
  geom_text(aes(x = Mask, y = Peak_clus0_q95+2, label = paste("n=", (1-meanClusterProb)*100, sep = ""),
                group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 3) +
  scale_x_continuous(breaks = unique(Mask)) +
  scale_y_continuous(breaks = seq(0,100,20)) +
  coord_cartesian(ylim = c(0,100), xlim = c(-12, 110)) +
  labs(title = "N = 100 simulations / scenario") +
  scale_color_viridis(discrete = TRUE, option = "plasma", name = "Length of the conveyors (m)", begin = 0, end = 0.8) +
  xlab("Mask wearing (%)") + ylab("Cluster peak values (number of infected workers)\n Median values and 0.05, 0.25, 0.75 and 0.95 quantiles")
g_Peak_clus



## DaysBeforeCluster ####
g_DaysBeforeCluster <- ggplot(data = SMR_MCvy) +
  theme(axis.ticks=element_blank(),
        legend.position = "bottom",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_blank()) +
  geom_point(mapping = aes(x = Mask, y = DaysBeforeCluster_q50, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 4, shape = 16) +
  # geom_point(mapping = aes(x = Mask, y = DaysBeforeCluster_q05, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 1) +
  geom_point(mapping = aes(x = Mask, y = DaysBeforeCluster_q25, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 2, shape = 1) +
  geom_point(mapping = aes(x = Mask, y = DaysBeforeCluster_q75, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 2, shape = 1) +
  # geom_point(mapping = aes(x = Mask, y = DaysBeforeCluster_q95, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 1) +
  geom_linerange(mapping = aes(x = Mask, ymin = DaysBeforeCluster_q25, ymax = DaysBeforeCluster_q75, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 1.2) +
  geom_linerange(mapping = aes(x = Mask, ymin = DaysBeforeCluster_q75, ymax = DaysBeforeCluster_q95, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 1.2, alpha = 0.3) +
  geom_linerange(mapping = aes(x = Mask, ymin = DaysBeforeCluster_q05, ymax = DaysBeforeCluster_q25, group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 1.2, alpha = 0.3) +
  geom_text(aes(x = Mask, y = DaysBeforeCluster_q95+1, label = paste("n=", meanClusterProb*100, sep = ""),
                group = Cvy, colour = Cvy), position = position_dodge(width = 15), size = 3) +
  scale_x_continuous(breaks = unique(Mask)) +
  scale_y_continuous(breaks = seq(0,45,10)) +
  coord_cartesian(ylim = c(0,43)) +
  labs(title = "N = 100 simulations / scenario") +
  scale_color_viridis(discrete = TRUE, option = "plasma", name = "Length of the conveyors (m)" ) +
  xlab("Mask wearing (%)") + ylab("Days before reaching 25 cases")
g_DaysBeforeCluster



## Boxplot gCumulContaWorkers ####
CumulContaWorkers$Cvy <- as.factor(CumulContaWorkers$Cvy)
gMCvy_CumulContaWorkers <- ggplot() +
  geom_jitter()
  ggboxplot(data = CumulContaWorkers, x = 'Mask', y = 'CumulContaWorkers', colour = 'Cvy') +
  theme(axis.ticks=element_blank(),
        legend.position = "bottom",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_blank()) +
  scale_colour_viridis(discrete = TRUE, option = "plasma", name = "Length of the conveyors (m)" ) +
  labs(title = "Cumulative number of infections after 42 days")
gMCvy_CumulContaWorkers


## Daily average food contamination ####
g_MCvy_DailyFoodConta_clus1 <- ggboxplot(
  data = AverageDailyFoodContaRatio_clus1, x = 'Mask', y = 'FoodContaRatioPercentage', fill = 'Cvy') +
  theme(axis.ticks=element_blank(),
        legend.position = "bottom",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_blank()) +
  coord_cartesian(ylim = c(0,0.4)) +
  scale_fill_viridis(discrete = TRUE, option = "plasma", name = "Length of the conveyors (m)", begin = 0, end = 0.8) +
  xlab("Proportion of workers wearing a mask") + ylab("Daily ratio of food contamination\nAverage values across a 42-day period (%)") +
  labs(subtitle = "Situations with more than 25 infections")

g_MCvy_DailyFoodConta_clus0 <- ggboxplot(
  data = AverageDailyFoodContaRatio_clus0, x = 'Mask', y = 'FoodContaRatioPercentage', fill = 'Cvy') +
  theme(axis.ticks=element_blank(),
        legend.position = "bottom",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_blank()) +
  coord_cartesian(ylim = c(0,0.4)) +
  scale_fill_viridis(discrete = TRUE, option = "plasma", name = "Length of the conveyors (m)", begin = 0, end = 0.8) +
  xlab("Proportion of workers wearing a mask") + ylab("Daily ratio of food contamination\nAverage values across a 42-day period (%)") +
  labs(subtitle = "Situations with less than 25 infections")

ggarrange(g_MCvy_DailyFoodConta_clus1 + rremove("xlab") + rremove("ylab"), 
          g_MCvy_DailyFoodConta_clus0 + rremove("xlab") + rremove("ylab"), 
          ncol = 2,
          common.legend = TRUE, legend = "bottom") %>%
  annotate_figure(top = grid::textGrob("Contamination of meat cuts (%)", gp = grid::gpar(cex = 2)),
                  left = grid::textGrob("Daily ratio of food contamination\nAverage values across a 42-day period (%)", rot = 90, gp = grid::gpar(cex = 1.3)),
                  bottom = grid::textGrob("Proportion of workers wearing a mask", gp = grid::gpar(cex = 1.3)))





## Daily average surfaces contamination ####
g_MCvy_DailySurfacesConta_clus1 <- ggboxplot(
  data = AverageDailySurfacesContaRatio_clus1, x = 'Mask', y = 'SurfacesContaRatioPercentage', fill = 'Cvy') +
  theme(axis.ticks=element_blank(),
        legend.position = "bottom",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_blank()) +
  coord_cartesian(ylim = c(0,50)) +
  scale_y_continuous(breaks = seq(0,50,10)) +
  scale_fill_viridis(discrete = TRUE, option = "plasma", name = "Length of the conveyors (m)", begin = 0, end = 0.8) +
  xlab("Proportion of workers wearing a mask") + ylab("Daily ratio of surfaces contamination\nAverage values across a 42-day period (%)") +
  labs(subtitle = "Situations with more than 25 infections")
g_MCvy_DailySurfacesConta_clus1

g_MCvy_DailySurfacesConta_clus0 <- ggboxplot(
  data = AverageDailySurfacesContaRatio_clus0, x = 'Mask', y = 'SurfacesContaRatioPercentage', fill = 'Cvy') +
  theme(axis.ticks=element_blank(),
        legend.position = "bottom",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_blank()) +
  coord_cartesian(ylim = c(0,50)) +
  scale_y_continuous(breaks = seq(0,50,10)) +
  scale_fill_viridis(discrete = TRUE, option = "plasma", name = "Length of the conveyors (m)", begin = 0, end = 0.8) +
  xlab("Proportion of workers wearing a mask") + ylab("Daily ratio of surfaces contamination\nAverage values across a 42-day period (%)") +
  labs(subtitle = "Situations with less than 25 infections")
g_MCvy_DailySurfacesConta_clus0

ggarrange(g_MCvy_DailySurfacesConta_clus1 + rremove("xlab") + rremove("ylab"), 
          g_MCvy_DailySurfacesConta_clus0 + rremove("xlab") + rremove("ylab"), 
          ncol = 2,
          common.legend = TRUE, legend = "bottom") %>%
  annotate_figure(top = grid::textGrob("Surfaces contamination (%)", gp = grid::gpar(cex = 2)),
                  left = grid::textGrob("Daily ratio of surfaces contamination\nAverage values across a 42-day period (%)", rot = 90, gp = grid::gpar(cex = 1.3)),
                  bottom = grid::textGrob("Proportion of workers wearing a mask", gp = grid::gpar(cex = 1.3)))





## Production losses ####
ggplot() +
  theme(axis.ticks=element_blank(),
        legend.position = "bottom",
        panel.background=element_rect(fill="white"),
        plot.title = element_text(face = "bold", size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        panel.grid.major.y=element_line(colour="lightgrey"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_line(colour="white"),
        panel.grid.minor.x=element_blank()) +
  stat_summary(data = loss_clus1, mapping = aes(x = Mask, y = Prod, group = Cvy, colour = Cvy), fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(data = loss_clus1, mapping = aes(x = Mask, y = Prod, group = Cvy, colour = Cvy), fun = mean, geom = "point", size = 2) +
  stat_summary(data = loss_clus0, mapping = aes(x = Mask, y = Prod, group = Cvy, colour = Cvy), fun = mean, geom = "line", alpha = 0.7, linewidth = 0.5, linetype = 2) +
  stat_summary(data = loss_clus0, mapping = aes(x = Mask, y = Prod, group = Cvy, colour = Cvy), fun = mean, geom = "point", size = 2, alpha = 0.7) +
  scale_colour_viridis(discrete = TRUE, option = "plasma", name = "Length of the conveyors (m)", begin = 0, end = 0.8) +
  xlab("Proportion of workers wearing a mask") + ylab("Production capacity (%)")






ggplot(data = spatial_conta[[1]]) + 
  theme_bw() +
  geom_boxplot(mapping = aes(x = S_ID, y = occurrences)) +
  ylab("occurrences if clusters") + 
  labs(title = "Mask0 Cvy11") + 
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = spatial_conta[[5]]) + 
  theme_bw() +
  geom_boxplot(mapping = aes(x = S_ID, y = occurrences)) +
  ylab("occurrences if clusters") + 
  labs(title = "Mask0 Cvy11") + 
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = spatial_conta[[3]]) + 
  geom_boxplot(mapping = aes(x = S_ID, y = occurrences)) +
  ylab("occurrences if clusters") + 
  labs(title = "Mask0 Cvy17") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = spatial_conta[[15]]) + 
  geom_boxplot(mapping = aes(x = S_ID, y = occurrences)) +
  ylab("occurrences if clusters") + 
  labs(title = "Mask80 Cvy17") +
  theme(axis.text.x = element_text(angle = 90))













ggplot(data = aa) + 
  geom_boxplot(mapping = aes(x = S_ID, y = occurrences)) +
  ylab("occurrences if clusters") + 
  theme(axis.text.x = element_text(angle = 90))



# RESULTS EXPORT ####
# write.csv(SMR_MCvy_3log, file = "simulation_output/MCVy_detection3log.csv")
# write.csv(SMR_MCvy_4log, file = "simulation_output/MCVy_detection4log.csv")
# write.csv(SMR_MCvy_5log, file = "simulation_output/MCVy_detection5log.csv")
writexl::write_xlsx(x = SMR_MCvy,
                    path = "simulation_output/SMR_MCvy_5log_bis1.xlsx")



