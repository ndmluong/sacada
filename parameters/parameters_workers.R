##### USER DEFINED PARAMETERS FOR THE WORKERS SET #####
Parms_Workers <- list(
  #### NUMBER OF WORKERS ####
  ## Total number of fixed workers (regular/temporary/subcontract)
  NWorkers = 100, 
  ## Total number of exterior workers
  NWorkers_ext = 15,
  
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
  ## Worker types with important mission having high probabilities to be present at work 
  ## despite their possible contamination/symptoms
  mandatoryWorkersType = c("transverse1"),
  
  #### COMMUNITY ACTIVITIES ####
  ## Proportion of the workers susceptible to have community activities
  pCommunityActivity = 0.30, 
  ## Average number of workers within a commune for activity
  N_perCommunityActivity = 3, 
  ## Probability of contamination within a commune for activity
  conta_prob_withinCommunity = 0.025, ## CHECK LITERATURE DATA (average daily value of 25% every 10 days)
  
  #### MASKS ####
  ## Type of the mask worn by the workers (by default: "Surgical mask")
  MaskType = "Surgical mask",
  ## Acceptability by the workers for the different types of mask (proportion of the workers wearing it)
  pMaskAcceptability = c("Surgical mask" = 0.8,
                         "FFP2" = 0.60),
  ## Mask efficiency for different droplet size classes [0 -> 1]
  Mask_Eff = c(0.9,  
               0.9,
               0.9, 
               0.9),

  #### EPIDEMIOLOGY SCENARIO ####
  ## The number of contaminated workers at the day 0
  nContaminated_Init = 30,
  ## Regional prevalence ##
  prev = 50/100000,
  
  ## Contamination counter (expressed in day)
  InfectedDay = 1,
  InfectiousDay = 3,
  SymptomDay = 5,
  NonInfectiousDay = 12,
  ContaEndDay = 15,
  
  ## Probability to develop symptoms
  pSymptom = 0.60
)
