##### PARAMETERS ASSOCIATED WITH THE WORKERS SET #####
Parms_Workers <- list(
  #### NUMBER OF WORKERS ####
  ## Total number of fixed workers (regular/temporary/subcontract)
  NWorkers = 80,
  
  ####*****Not used yet *****###
  ## Total number of exterior workers
  NWorkers_ext = 15,
  ####****************###
  
  #### TYPE OF WORKERS - TEAMS - SHIFT ####
  ## The ratio between the total number of workers over the required number of workers per working day
  r_Nt_Nperday = 1.25, ## French legislation
  ## Overall proportion for different types of workers

  pType = c("cutter1" = 0.36, ## Mallet et al. 2021
            "cutter2" = 0.47, ## Mallet et al. 2021 
            "logistic1" = 0.04, ## Gunther et al. 2020
            "logistic2" = 0.04, ## Mallet et al. 2021 
            "transverse1" = 0.045, ## Mallet et al. 2021 (total number of transverse workers = 0.09)
            "transverse2" = 0.045), ## Mallet et al. 2021 (total number of transverse workers = 0.09)
  ## The proportion of workers (cutters/logistics) in two teams A and B
  ## which alternatively change the working shift (morning/afternoon) from one week to another 
  pTeam = c("teamA" = 0.5, 
            "teamB" = 0.5),
  ## Proportion of workers (cutters/logistics) susceptible to change the teams (A/B) for a given week
  pChangeTeam = 0.05,
  
  ####*****Not used yet *****###
  ## Worker types with important mission having high probabilities to be present at work 
  ## despite their possible contamination/symptoms
  mandatoryWorkersType = c("transverse1"),
  ###*******************###
  
  #### COMMUNITY ACTIVITIES ####
  ## Proportion of the workers susceptible to have community activities
  pCommunityActivity = 0.50, 
  ## Average number of workers within a commune for activity
  N_perCommunityActivity = 5, 
  
  ## Secondary Attack Rate (SAR) depending on the Variants of Concern (VoC (original, alpha, delta, omicron))
  SAR_sim = "unvaccinated_delta", ## the VoC and SAR in use for simulation
  
  SAR = c("unvaccinated_original" = 0.17, # (Fung et al. 2021) [14%-21%]
          "unvaccinated_alpha" = 0.17, # (Fung et al. 2021) [14%-21%]
          "unvaccinated_delta" = 0.38, # [24%-53%] (Singanayagam et al. 2021) 
          "unvaccinated_omicron" = 0.57, # (Lyngse et al. 2021)
          "vaccinated_original" = NA,
          "vaccinated_alpha" = NA,
          "vaccinated_delta" = 0.25, # [18%-33%]	(Singanayagam et al. 2021)
          "vaccinated_omicron" = 0.375), # (Lyngse et al. 2021)
  
  ## the probability to get infected daily due to community activities = SAR / total number of infectious duration (2+8=10 days, see parameters below)
  daily_SAR = c("unvaccinated_original" = 0.017, # (Fung et al. 2021) [14%-21%]
                "unvaccinated_alpha" = 0.017, # (Fung et al. 2021) [14%-21%]
                "unvaccinated_delta" = 0.038, # [24%-53%] (Singanayagam et al. 2021) 
                "unvaccinated_omicron" = 0.057, # (Lyngse et al. 2021)
                "vaccinated_original" = NA,
                "vaccinated_alpha" = NA,
                "vaccinated_delta" = 0.025, # [18%-33%]	(Singanayagam et al. 2021)
                "vaccinated_omicron" = 0.0375), # (Lyngse et al. 2021)
  
  #### MASKS ####
  ## Type of the mask worn by the workers in use for simulation by default: "Surgical mask"
  MaskType = "Surgical mask",
  ## Acceptability by the workers for the different types of mask (proportion of the workers wearing it)
  pMaskAcceptability = c("Surgical mask" = 0.8,
                         "FFP2" = 0.6),
  
  ## Mask efficiency for different types of mask
  Mask_Eff = c("Surgical mask" = 0.9,
               "FFP2" = 0.95),
  
  
  #### EPIDEMIOLOGY SCENARIO ####
  ## The number of contaminated workers at the day 0
  nContaminated_Init = 5,
  ## Regional prevalence ##
  prev = 50/100000,
  
  ## Contamination day (counter expressed in day)
  InfectedDay = 1, ## first day of the course of infections
  
  LatentPhaseDuration = 3, ## Latent phase of presymptomatic period (range [0-8]) (Zhao et al. 2021)
  InfectiousDay = 4, ## InfectedDay + LatentPhaseDuration
  
  PresympDuration = 2, ## xx [1-4] (Byrne et al. 2020)
  SymptomDay = 6, ## InfectiousDay + PresympDuration
  
  SympDuration = 8, ## yy Infectious phase after presymptomatic period for symptomatic people (range [5-11]) (Byrne et al. 2020; Kampen et al. 2021)
  AsympDuration = 8, ## zz Infectious phase after presymptomatic period for asymptomatic people (range [5-11]) (Byrne et al. 2020)
  
  NonInfectiousDay = 14, ## End of the infectious period for (a)symptomatic people (SymptomDay + yy) or (SymptomDay + zz) 
  NonInfectiousDaySymp = 14,
  NonInfectiousDayAsymp = 14,
  
  NonInfectiousDuration = 4, ## 4 days from the non-infectious state (but still positive) to recovered status
  
  RecoveredDay = 18, ## the day at which the person becomes negative: NonInfectiousDay + NonInfectiousDuration
  
  ## Proportion of asymptomatic
  pAsymptom = 0.335, ## [0.18-0.475] (Alene et al. 2021; Ma et al. 2021; Sah et al. 2021)
  
  ## Probability of absence for symptomatic workers
  pPresenceSymp = 24/140, ## (Mallet 2021)
  AbsenceDuration = 7 ## (assumption)
)

# (Optional) Parameters with unchanged values that can be saved in global environment 
NWorkers <<- Parms_Workers$NWorkers
Mask_Eff <<- Parms_Workers$Mask_Eff[[Parms_Workers$MaskType]]