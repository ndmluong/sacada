##### USER DEFINED PARAMETERS FOR THE WORKERS SET #####
Parms_Conta <- list(
  ## the Variants of Concern (original, alpha, delta, omicron)
  VoC = "delta", 
  
  # The parameters of the triangular distribution used to describe variability of viral load depending on the VoC
  RNA_dist = list(delta = c("min" = 1.6, "mode" = 8.6, "max" = 10.4),
                  original = c("min" = 0.6, "mode" = 7.6, "max" = 9.4),
                  alpha = c("min" = 1.3, "mode" = 8.3, "max" = 10.1),
                  omicron = c("min" = 1.6, "mode" = 8.6, "max" = 10.4)),
  
  ## Dose-response parameters
  # DRM1: Watanabe dose-response function
  DRM1_r = 0.00246,
  
  ## Ratio between the number of RNA copies and infectious virions in aerosol
  RNA_virion_ratio = 100
)
