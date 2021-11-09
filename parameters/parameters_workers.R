##### USER DEFINED PARAMETERS FOR THE WORKERS SET #####
Parms_Workers <- list(
  NWorkers = 20, ## total number of the workers during the entire process
  pContaminated = c("contaminated" = 0.1,
              "not contaminated" = 0.9), ## probability of infected people among the workers
  pMask = c("mask" = 0.80,
            "no mask" = 0.20), ## probability of workers wearing a mask
  pActive = c("active" = 0.8,
              "idle" = 0.2), ## proportion of active workers (per day) upon the total number of employees
  pType = c("cutter" = 0.8,
            "logistic1" = 0.05,
            "logistic2" = 0.05,
            "administrative" = 0.1)
)
