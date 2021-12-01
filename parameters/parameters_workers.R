##### USER DEFINED PARAMETERS FOR THE WORKERS SET #####
Parms_Workers <- list(
  NWorkers = 200, ## total number of the workers during the entire process
  nContaminated_Init = 1,  ## the number of contaminated workers at the day 0
  # pContaminated = c("contaminated" = 0.1,
  #             "not contaminated" = 0.9), ## probability of infected people among the workers
  pMask = c("mask" = 0.80,
            "no mask" = 0.20), ## probability of workers wearing a mask
  Mask_Eff = c(0.9,  # Mask Efficacy for each droplet class [0 -> 1]
               0.9, 
               0.9, 
               0.9),
  pActive = list(
    W_gr1 = c("idle", "active", "active", "active", "active"),
    W_gr2 = c("active", ),
    W_gr3 = c(),
    W_gr4 = c(),
    W_gr5 = c()
  ),
  pChangeTeam = 0.05,
  # pActive = c("active" = 0.8,
  #             "idle" = 0.2), ## proportion of active workers (per day) upon the total number of employees
  pType = c("cutter1" = 0.36, ## Mallet et al. 2021
            "cutter2" = 0.47, ## Mallet et al. 2021 
            "logistic1" = 0.04, ## Gunther et al. 2020
            "logistic2" = 0.04, ## Mallet et al. 2021 
            "transverse" = 0.09),
  pTeam = c("TeamA" = 0.5, 
            "TeamB" = 0.5), 
  mandatoryWorkersType = c("transverse"),
  prev = 50/100000, ## regional prevalence
  pCommunityActivity = 0.30, ## the proportion of the workers susceptible to have community activities
  N_perCommunityActivity = 3, ## the average number of workers within a group for activity
  conta_prob_withinCommunity = 0.025 ## CHECK LITERATURE (25% every 10 days)
)
