#### Parameters used in the air module ####

Parms_Air <- list(
  
  Droplet_class = c(0.8, # (µm) Midpoint diameters of each droplet class (Buonanno et al. 2020) (respiration)
                    1.8,
                    3.5,
                    5.5),
  d_Vol = 4/3*3.14159*(1e-06*c(0.8, # (ml) Droplet Volume
            1.8,
            3.5,
            5.5)/2)^3*1e6,
  

  
  # Droplet_class = c(1, # (µm) Midpoint diameters of each droplet class (Buonanno et al. 2020) (respiration)
  #                   5,
  #                   10,
  #                   
  #                   20
  #                    
  #                   
  # (m-3) - Concentration of droplets of each class during respiratory activiies Morawska et al., 2009, kenedy et al.2020 teble2
                          #d0.8  d1.8   d3.5    d5.5
  Cd_exp =  1e6*rbind( c(0.236, 0.068, 0.007, 0.011), # Voiced counting
                        c(0.751, 0.139, 0.139, 0.059), # Unmodulated Vocalization
                        c(0.084, 0.009, 0.003, 0.002), # Breathing (insp noze, exp mouth) details in Morawska
                        c(0.567, 0.093, 0.012, 0.006)), # Cough
  
  # (m3/min) Respiration rate Adams (1993)
  RespRate = c(0.49,  # Resting
               0.54,  # Standing
               1.38,  # Light excercise
               2.35,  # Moderate exercise
               3.3)/60   # Heavy  exercise
  
  )


