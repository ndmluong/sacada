##### USER DEFINED PARAMETERS FOR SURFACES #####
Parms_Surfaces <- list(
  # contamination distribution between inert and meat surfaces
  inert_prop = 1/3, # assumption
  
  ## the number of meat portions exposed in each surface unit
  nFP_expo = 10, # assumption
  
  # transfer rate between inert and meat surfaces
  transfer_S2F = 0.99, # Duret et al., 2017
  transfer_F2S = 0.28, # Duret et al., 2017
  
  # positive threshold for inert and meat contamination (expressed in number of RNA copies)
  pos_threshold = c("3log" = 10^3,
                    "4log" = 10^4,
                    "5log" = 10^5,
                    "6log" = 10^6,
                    "7log" = 10^7,
                    "8log" = 10^8,
                    "9log" = 10^9) # assumption, taking into account the number
)
