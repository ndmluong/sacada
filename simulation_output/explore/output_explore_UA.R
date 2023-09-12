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
load(file = "simulation_output/data/output_UA.RData")


UA <- list(UA_RNAr_min, UA_ref_2, UA_RNAr_max,
           UA_SAR_min, UA_ref_2, UA_SAR_max,
           UA_pAsymp_min, UA_ref_2, UA_pAsymp_max,
           UA_psneeze_min, UA_ref_2, UA_psneeze_max,
           UA_Vsed_A1, UA_ref_2, UA_Vsed_A2,
           UA_rS2F_min, UA_ref_2, UA_rS2F_max,
           UA_tileprop_min, UA_ref_2, UA_tileprop_max)

gdata::keep(UA, sure = TRUE)

# FUNCTIONS #####
source("functions/functions.R")

# DATA PRETREATMENT ####
## Scenario name ####
scenario_names <- c("UA_RNAr_min", "UA_RNAr_ref", "UA_RNAr_max",
                    "UA_SAR_min", "UA_SAR_ref", "UA_SAR_max",
                    "UA_pAsymp_min", "UA_pAsymp_ref", "UA_pAsymp_max",
                    "UA_psneeze_min", "UA_psneeze_ref", "UA_psneeze_max",
                    "UA_Vsed_min", "UA_Vsed_ref", "UA_Vsed_max",
                    "UA_rS2F_min", "UA_rS2F_ref", "UA_rS2F_max",
                    "UA_tileprop_min", "UA_tileprop_ref", "UA_tileprop_max")

names(UA) <- scenario_names


# RESULTATS ####
## Indicator 1: Cumulative number of contaminated workers after 42 days ####
lapply(scenario_names, function(sce) { ## for each scenario sce
  f_smrzCumulContaWorkers(IL = UA[[sce]]$IL, ## calculate the cumulative number of infections for each seed
                          IS = UA[[sce]]$IS) %>%
    mutate(., scenario = sce, .before = 1)
}) %>%
  data.table::rbindlist() %>%
  group_by(scenario) %>%
  summarise(CumulContaWorkers = mean(cumul),
            .groups = 'drop') %>%
  dplyr::arrange(scenario) %>%
  mutate(., parms = stringr::str_sub(scenario, start = 4, end = -5)) %>%
  mutate(., bound = stringr::str_sub(scenario, start = -3)) %>%
  mutate(., yboundmax = ifelse(bound == "max", CumulContaWorkers, NA)) %>%
  mutate(., yboundmin = ifelse(bound == "min", CumulContaWorkers, NA)) %>%
  mutate(., yref = ifelse(bound == "ref", CumulContaWorkers, NA)) %>%
  group_by(parms) %>%
  summarise(yref = mean(yref, na.rm = T),
            yboundmin = mean(yboundmin, na.rm = T),
            yboundmax = mean(yboundmax, na.rm = T)) -> output_CumulContaWorkers


## Indicator 2: Cluster probability (with more than > 25 cases) ####
data.frame(scenario = scenario_names,
           ClusterProb = sapply(scenario_names, function(sce) {
             f_ClusterProb(IS = UA[[sce]]$IS) * 100
           })) %>%
  dplyr::arrange(scenario) -> output_ClusterProb
rownames(output_ClusterProb) <- seq(1, nrow(output_ClusterProb))
output_ClusterProb %>%
  mutate(., parms = stringr::str_sub(scenario, start = 4, end = -5)) %>%
  mutate(., bound = stringr::str_sub(scenario, start = -3)) %>%
  mutate(., yboundmax = ifelse(bound == "max", ClusterProb, NA)) %>%
  mutate(., yboundmin = ifelse(bound == "min", ClusterProb, NA)) %>%
  mutate(., yref = ifelse(bound == "ref", ClusterProb, NA)) %>%
  group_by(parms) %>%
  summarise(yref = mean(yref, na.rm = T),
            yboundmin = mean(yboundmin, na.rm = T),
            yboundmax = mean(yboundmax, na.rm = T)) -> output_ClusterProb





## Indicator 3: Infection sources when clusters occur (percentage of indoor infection) ####
data.frame(scenario = scenario_names,
           ClusterSource = sapply(scenario_names, function(sce){
             f_ClusterSource(IL = UA[[sce]]$IL,
                             IS = UA[[sce]]$IS,
                             cluster_filter = T,
                             clusterthreshold = 25,
                             inside_source = T) * 100
           })) %>%
  dplyr::arrange(scenario) -> output_ClusterSource
rownames(output_ClusterSource) <- 1:nrow(output_ClusterSource)
output_ClusterSource %>%
  mutate(., parms = stringr::str_sub(scenario, start = 4, end = -5)) %>%
  mutate(., bound = stringr::str_sub(scenario, start = -3)) %>%
  mutate(., yboundmax = ifelse(bound == "max", ClusterSource, NA)) %>%
  mutate(., yboundmin = ifelse(bound == "min", ClusterSource, NA)) %>%
  mutate(., yref = ifelse(bound == "ref", ClusterSource, NA)) %>%
  group_by(parms) %>%
  summarise(yref = mean(yref, na.rm = T),
            yboundmin = mean(yboundmin, na.rm = T),
            yboundmax = mean(yboundmax, na.rm = T)) -> output_ClusterSource



## Indicator 4: Daily average surfaces contamination when clusters occur ####
RNA_detection <- 5

lapply(scenario_names, function(sce) { ## for each scenario sce
  f_smrzAverageSurfacesContaRatio(SS = UA[[sce]]$SS,
                                  IS = UA[[sce]]$IS,
                                  plant = UA[[sce]]$MyPlant,
                                  detection = RNA_detection,
                                  cluster_filter = T) %>%
    mutate(., scenario = sce, .before = 1)
}) %>%
  data.table::rbindlist() %>%
  group_by(scenario) %>%
  summarise(SurfacesContaRatio = mean(contaratio) * 100,
            .groups = 'drop') %>%
  dplyr::arrange(scenario) %>%
  mutate(., parms = stringr::str_sub(scenario, start = 4, end = -5)) %>%
  mutate(., bound = stringr::str_sub(scenario, start = -3)) %>%
  mutate(., yboundmax = ifelse(bound == "max", SurfacesContaRatio, NA)) %>%
  mutate(., yboundmin = ifelse(bound == "min", SurfacesContaRatio, NA)) %>%
  mutate(., yref = ifelse(bound == "ref", SurfacesContaRatio, NA)) %>%
  group_by(parms) %>%
  summarise(yref = mean(yref, na.rm = T),
            yboundmin = mean(yboundmin, na.rm = T),
            yboundmax = mean(yboundmax, na.rm = T)) -> output_SurfacesContaRatio



## Indicator 5: Daily average food contamination when clusters occur ####
RNA_detection <- 5

lapply(scenario_names, function(sce) { ## for each scenario sce
  f_smrzAverageFoodContaRatio(FPS = UA[[sce]]$FPS,
                              IS = UA[[sce]]$IS,
                              detection = RNA_detection,
                              cluster_filter = T) %>%
    mutate(., scenario = sce, .before = 1)
}) %>%
  data.table::rbindlist() %>%
  group_by(scenario) %>%
  summarise(FoodContaRatio = mean(contaratio) * 100,
            .groups = 'drop') %>%
  dplyr::arrange(scenario) %>%
  mutate(., parms = stringr::str_sub(scenario, start = 4, end = -5)) %>%
  mutate(., bound = stringr::str_sub(scenario, start = -3)) %>%
  mutate(., yboundmax = ifelse(bound == "max", FoodContaRatio, NA)) %>%
  mutate(., yboundmin = ifelse(bound == "min", FoodContaRatio, NA)) %>%
  mutate(., yref = ifelse(bound == "ref", FoodContaRatio, NA)) %>%
  group_by(parms) %>%
  summarise(yref = mean(yref, na.rm = T),
            yboundmin = mean(yboundmin, na.rm = T),
            yboundmax = mean(yboundmax, na.rm = T)) -> output_FoodContaRatio





# PLOT ####
## Generic plot function ####
f_plotUA <- function(
    df, 
    indicator_title = "",
    indicator_label ## for plot y title
) {
  ggplot(data = df) +
    theme(axis.ticks=element_blank(),
          legend.position = "right",
          legend.title.align = 0.5,
          panel.background=element_rect(fill="white"),
          plot.title = element_text(face = "bold", size=14),
          axis.title = element_text(size=14),
          axis.text = element_text(size=12),
          panel.grid.major.y=element_line(colour="lightgrey"),
          panel.grid.major.x=element_line(colour="lightgrey"),
          panel.grid.minor.y=element_blank(),
          panel.grid.minor.x=element_blank()) +
    geom_hline(yintercept = mean(df$yref),
               linetype = "dashed", linewidth = 1, colour = "#5770BE") +
    geom_segment(mapping = aes(x = parms, xend = parms,
                               y = yref, yend = yboundmin),
                 linewidth = 1.2, colour = "darkgray") +
    geom_segment(mapping = aes(x = parms, xend = parms,
                               y = yref, yend = yboundmax),
                 linewidth = 1.2, colour = "darkgray") +
    geom_segment(mapping = aes(x = parms, xend = parms,
                               y = yboundmin, yend = yboundmax),
                 linewidth = 1.2, colour = "darkgray") +
    geom_point(mapping = aes(x = parms, y = yref), colour = "#5770BE", size = 3) +
    geom_point(mapping = aes(x = parms, y = yboundmin), colour = "#00AC8C", size = 3) +
    geom_point(mapping = aes(x = parms, y = yboundmax), colour = "#E1000F", size = 3) +
    labs(title = indicator_title) + 
    xlab("Parameters") + ylab(indicator_label)
}


## Cumulative number of infections after 42 days ####
gUA_CumulContaWorkers <- f_plotUA(df = output_CumulContaWorkers,
                                  indicator_title = "Cumulative infections",
                                  indicator_label = "Cumulative number of infections after 42 days")
gUA_ClusterProb <- f_plotUA(df = output_ClusterProb,
                            indicator_title = "Cluster-occurrence probability",
                            indicator_label = "Cluster-occurrence probability (%)")
gUA_ClusterSource <- f_plotUA(df = output_ClusterSource,
                              indicator_title = "Percentage of indoor infection in cluster situations",
                              indicator_label = "Percentage of indoor infections in cluster situations (%)")
gUA_SurfacesContaRatio <- f_plotUA(df = output_SurfacesContaRatio,
                                  indicator_title = "Surface contamination in cluster situations (%)",
                                  indicator_label = "Daily ratio of surfaces contamination\nAverage values across a 42-day period (%)")
gUA_FoodContaRatio <- f_plotUA(df = output_FoodContaRatio,
                               indicator_title = "Food contamination in cluster situations (%)",
                               indicator_label = "Daily ratio of food contamination\nAverage values across a 42-day period (%)")
ggpubr::ggarrange(gUA_CumulContaWorkers,
                  gUA_ClusterProb,
                  gUA_ClusterSource,
                  gUA_SurfacesContaRatio,
                  gUA_FoodContaRatio,
                  nrow = 2, ncol = 3)
