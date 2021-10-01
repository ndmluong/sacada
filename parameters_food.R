##### USER DEFINED PARAMETERS FOR FOOD PORTIONS #####
Parms_Food <- list(
  NPortions = 100,
  ## Initial proportion
  PortionTypeProb = c(0.20, ## possible contact / non contaminated (PC-NC)
                      0.05, ## possible contact / contaminated (PC-C)
                      0.70, ## no possible contact (NPC)
                      0.05) ## loss portion (Loss)
)