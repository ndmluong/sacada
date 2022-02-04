##### USER DEFINED PARAMETERS FOR THE WORKERS SET #####
Parms_Conta <- list(
  ## the Variants of Concern (original, alpha, delta, omicron)
  VoC = "delta", 
  
  # The parameters of the triangular distribution used to describe variability of viral load depending on the VoC
  RNA_dist = list(delta = c(1.6, 8.6, 10.4),
                  original = c(0.6, 7.6, 9.4),
                  alpha = c(1.3, 8.3, 10.1),
                  omicron = c(1.6, 8.6, 10.4)),
  
  ## Dose-response parameters
  # DRM1: Watanabe dose-response function
  DRM1_r = 0.00246,
  
  ## Ratio between the number of RNA copies and infectious virions in aerosol
  RNA_virion_ratio = 100
)
