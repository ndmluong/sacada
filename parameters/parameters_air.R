#### Parameters used in the air module ####

Parms_Air <- list(
  
  Droplet_class = c(0.8, # (µm) Midpoint diameters of each droplet class (Buonanno et al. 2020) (respiration)
                    1.8,
                    3.5,
                    5.5, 
                    20, 
                    100),
  d_Vol = 4/3*3.14159*(1e-06*c(0.8, # (ml) Droplet Volume
            1.8,
            3.5,
            5.5, 
            20, 
            100)/2)^3*1e6,
  

  Vol_sneeze = 0.002, ## (in m3) (volume of air) Duguid 1946
  Vol_cough = 0.002, ## (in m3) 20-fold smaller than sneeze (assumption)
  
  # (m-3) - Concentration of droplets of each class during respiratory activiies Morawska et al., 2009, kenedy et al.2020 teble2
  #d0.8  d1.8   d3.5    d5.5   d 20   d 100
  Cd_exp =  1e6 * rbind(c(0.236, 0.068, 0.007, 0.011, 0, 0), # Voiced counting
                        c(0.751, 0.139, 0.139, 0.059, 0, 0), # Unmodulated Vocalization
                        c(0.084, 0.009, 0.003, 0.002, 0, 0), # Breathing (insp noze, exp mouth) details in Morawska
                        c(0.567, 0.093, 0.012, 0.006, 0.1, 0.1)),# Cough (not used here, see below Cd_cough)
  
  Cd_sneeze = 1e6* c(0, 13, 80, 175, 230, 489), # # (m-3) sneeze ## Duguid 1946 / 2L air
  Cd_cough = 1e6* c(0, 0.025, 0.145, 0.48, 2.51, 6.65), # # (m-3) sneeze ## Duguid 1946 / 2L
  
  # (m3/min) Respiration rate Adams (1993)
  RespRate = c(0.49,  # Resting
               0.54,  # Standing
               1.38,  # Light exercise
               2.35,  # Moderate exercise
               3.3)/60,   # Heavy  exercise
  
  ## sneezing probability depending on status
  p_cough = c("infectious" = 2/60, ## freq per minutes (Birrong et al. 2006)
               "symptomatic" = 47/60,
               "asymptomatic" = 2/60),
  
  ## sneezing probability depending on status
  p_sneeze = c("infectious" = 0.002, ## freq per minutes
               "symptomatic" = 1/60,
               "asymptomatic" = 0.08),
  
  # equal probability of other three activities: talking or talking loud or breathing
  p_talk = 1/3, 
  
  # probability of settling on surfaces
  # source, adjacent surface, ...,...
  p_zone = c("ctr" = 0.2,
             "adj1" = 0.05,
             "adj2" = 0.03,
             "adj3" = 0.015),
  
  p_zone_air = c("ctr"= 1,
                 "adj1" = 0.11,
                 "adj2" = 0.045,
                 "adj3" = 0.015)
)


