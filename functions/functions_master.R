##### OPTIMIZED f_Who_is() to sum up who is where, how is he and is he wearing a mask at A GIVEN TIME INDEX #####
f_Who_is <- function(
  ## INPUT
  # To sum up who is where, how is he and is he wearing a mask at A GIVEN TIME INDEX
  SubW, ## (subset of MyWorkers) THE WORKERS ASSOCIATED WITH ONLY THE GIVEN TIME INDEX
  prm_plant,
  ind ## the given time index
  ## OUTPUT
  
  
){
  ## Total number of workers
  NWorkers <- length(unique(SubW$W_ID))
  
  #### Spaces 
  ## Convert all workspace locations "WS..." into "Cutting room" (prm_plant$label)
  SubW$location[str_starts(SubW$location,"WS")] <- prm_plant$label
  
  ## Extract the name of every spaces including the cutting room
  Spaces_label <<- c(prm_plant$label,
                    as.vector(unlist(lapply(Parms_Plant$Spaces, FUN = function(x) return(x$label)))))
  
  ## Total number of spaces including the cutting room
  NSpaces <- length(Spaces_label)
  
  ## Define the matrices showing the presence/absence (T/F) of every workers inside every rooms
  Cont_mask <- matrix(FALSE, nrow = NWorkers, ncol = NSpaces) %>% `colnames<-`(., Spaces_label)
  Cont_no_mask <- matrix(FALSE, nrow = NWorkers, ncol = NSpaces) %>% `colnames<-`(., Spaces_label)
  Non_Cont_mask <- matrix(FALSE, nrow = NWorkers, ncol = NSpaces) %>% `colnames<-`(., Spaces_label)
  Non_Cont_no_mask <- matrix(FALSE, nrow = NWorkers, ncol = NSpaces) %>% `colnames<-`(., Spaces_label)
  
  
  ## Update the presences/absences matrices for every spaces 
  # Contaminated - mask
  sapply(Spaces_label, FUN = function(x) {
    Cont_mask[,x] <- SubW$W_status %in% c("infectious", "symptomatic", "asymptomatic") &
      SubW$location == x & SubW$W_mask=="mask"
  }) %>% `rownames<-`(.,unique(SubW$W_ID)) -> Cont_mask
  
  # Contaminated - no mask
  sapply(Spaces_label, FUN = function(x) { ## for each space
    Cont_no_mask[,x] <- SubW$W_status %in% c("infectious", "symptomatic", "asymptomatic") &
      SubW$location == x & SubW$W_mask=="no mask"
  }) %>% `rownames<-`(.,unique(SubW$W_ID)) -> Cont_no_mask
  
  # Non contaminated - mask
  Non_Cont_mask <- sapply(Spaces_label, FUN = function(x) { ## for each space
    Non_Cont_mask[,x] <- SubW$W_status %in% c("susceptible", "infected", "non-infectious") &
      SubW$location == x & SubW$W_mask=="mask"
  }) %>% `rownames<-`(.,unique(SubW$W_ID)) -> Non_Cont_mask
  
  # Non contaminated - no mask
  Non_Cont_no_mask <- sapply(Spaces_label, FUN = function(x) { ## for each space
    Non_Cont_no_mask[,x] <- SubW$W_status %in% c("susceptible", "infected", "non-infectious") &
      SubW$location == x & SubW$W_mask=="no mask"
  }) %>% `rownames<-`(.,unique(SubW$W_ID)) -> Non_Cont_no_mask
  
  ## ID of the workers in each room depending on their contamination and mask wearing status
  apply(Cont_mask, 2, FUN = function(x) {
    if (length(which(x==T)) > 0) return(names(which(x==T))) else return(NA)
  }) -> C_M_ID
  
  apply(Cont_no_mask, 2, FUN = function(x) {
    if (length(which(x==T)) > 0) return(names(which(x==T))) else return(NA)
  }) -> C_NM_ID
  
  apply(Non_Cont_mask, 2, FUN = function(x) {
    if (length(which(x==T)) > 0) return(names(which(x==T))) else return(NA)
  }) -> NC_M_ID
  
  apply(Non_Cont_no_mask, 2, FUN = function(x) {
    if (length(which(x==T)) > 0) return(names(which(x==T))) else return(NA)
  }) -> NC_NM_ID
  
  lapply(Spaces_label, FUN = function(x) {
    return(list(C_M_ID[[x]], C_NM_ID[[x]], NC_M_ID[[x]], NC_NM_ID[[x]]))
  }) %>% `names<-`(.,unique(Spaces_label))-> Who

  symptomatic = SubW$W_status == "symptomatic"

  return(list(Cont_mask = Cont_mask,
              Cont_no_mask = Cont_no_mask,
              Non_Cont_mask = Non_Cont_mask,
              Non_Cont_no_mask = Non_Cont_no_mask,
              Who = Who,
              symptomatic = symptomatic))

}


##### f_Sneeze #####
f_Sneeze <- function(
  ## INTPUT
  SubW, ## MyWorkers associated with ONE GIVEN TIME INDEX,
  SubS, ## MySurfaces associated with ONE GIVEN TIME INDEX
  Rooms_label,
  t_ind,
  Rooms, ## one of the output of the function f_Who_is
  prm_air,
  prm_time,
  P_drop_conta
  ## OUTPUT
  ## 1. to_aeorosol: nb droplet from sneeze to aerosol
  ## 2. d_expo_sneeze: nb droplet from the sneezing worker to the other agents (workers, surfaces, meat)
) {
  
  SubW$location[str_starts(SubW$location,"WS")] <- "Cutting Room"
  
  ## OUTPUT: initialisaiton d_expo_sneeze
  d_expo <- matrix(0, nrow = nrow(SubW), ncol = ncol(Method_calc))
  rownames(d_expo) <- sort(unique(SubW$W_ID))
  
  ## 3th - OUTPUT
  m2_drop <- matrix(0, nrow = nrow(SubS), ncol = ncol(Method_calc))
  
  ## Efficacité du masque qui ne change pas au cours du temps
  mask <- SubW$W_mask
  
  eff <- (mask == "mask") * (1-prm_air$Mask_Eff) + (mask == "no mask")
  eff[is.na(eff)] <- 0
  names(eff) <- unique(SubW$W_ID)
  
  To_aerosol <- matrix(0, nrow = length(Rooms_label), ncol = ncol(Method_calc))
  
  for (i in 1:length(Rooms)) { ## for each Rooms 
    ## extract the contaminated masked workers (Cont_mask, including 3 status) ...[[1]]
    Cont_mask_ID <- Rooms[[i]][[1]]
    Cont_no_mask_ID <- Rooms[[i]][[2]]

    ## their corresponding status (asymptomatic, symptomatic, infectious)
    status_mask <- SubW$W_status[SubW$W_ID %in% Cont_mask_ID] ########
    status_no_mask <- SubW$W_status[SubW$W_ID %in% Cont_no_mask_ID]

    ## sneeze: indicates if each worker prensent in the room and infectious sneezes or not (value 1/0)
    if (length(status_mask) > 0) {
      sapply(status_mask, FUN = function(x) {
        prob <- prm_air$p_sneeze[[x]]
        return(   min(rbinom(1, prm_time$Step, prob = prob), 1)   )
      }) %>% as.vector() -> sneeze_mask
    } else {sneeze_mask <- 0}
    
    if (length(status_no_mask)) {
      sapply(status_no_mask, FUN = function(x) {
        prob <- prm_air$p_sneeze[[x]] 
        return( min(rbinom(1, prm_time$Step, prob = prob), 1))
      }) %>% as.vector() ->  sneeze_no_mask
    } else {sneeze_no_mask <- 0}

    To_aerosol[i,] = sum(sneeze_mask) * (1 - prm_air$Mask_Eff) * prm_air$Cd_sneeze * (Method_calc[i,]==T) * prm_air$Vol_sneeze +
      sum(sneeze_no_mask) * prm_air$Cd_sneeze * (Method_calc[i,]==T) * prm_air$Vol_sneeze
    

    ## Falling droplets (number emitted when sneezing)
    nd_Source = prm_air$Cd_sneeze * (Method_calc[i,]==F) * prm_air$Vol_sneeze

    # ID of the sneezing worker(s) wearing a mask
    if (sum(sneeze_mask)> 0) { #
      Source_ID <- Cont_mask_ID[sneeze_mask == 1]
      ## their corresponding X, Y coordinates
      Source_X <- SubW$coordX[SubW$W_ID %in% Source_ID]
      Source_Y <- SubW$coordY[SubW$W_ID %in% Source_ID]
    
      W_susceptible <- subset(SubW, location == Spaces_label[i] & W_status == "susceptible")
    
      p_zone <- numeric(nrow(SubS))

      for (S in 1:length(Source_ID)){ ## for each source worker
        ## distance between close workers
        DX = abs(Source_X[S]-W_susceptible$coordX)
        DY = abs(Source_Y[S]-W_susceptible$coordY)
        # if (max(c(DX,DY)<=1) {}
        
        print("DX")
        print(DX)
        
        print("DY")
        print(DY)
        
        p_zone_air = (DY <= 1 & DX <= 1) * 0.11 +
          ((DX==2 & DY<=1)|(DY==2 & DX<=1)) * 0.035 +
          ((DY==3 & DX==0)|(DX==3 & DY==0)|(DY==2 & DX==2)) * 0.015
        
        print("p_zone_air_mask")
        print(p_zone_air)
        
        resp <- runif(nrow(W_susceptible),prm_air$RespRate[3],prm_air$RespRate[5]) #[m3/min]
        
        tsed = 2/Vsed/60 # [minutes] # man_height = 2 [m]  hypothesis (source of droplet emission )
        tsed[tsed >= 5] <- 5

        ## number of droplets exposed to each susceptible worker (unit: # droplets)
        d_expo[W_susceptible$W_ID, ] = d_expo[W_susceptible$W_ID,] +
          eff[W_susceptible$W_ID] * (p_zone_air * resp / 2) %*% (P_drop_conta[Source_ID[S],] * nd_Source * (1-prm_air$Mask_Eff) * tsed)  # 2 corresponds to 2m3, the air volume around the worker
        
        if (names(Rooms[i]) == "Cutting Room") {
          ## distance between close surfaces
          DX = abs(Source_X[S] - SubS$coordX)
          DY = abs(Source_Y[S] - SubS$coordY)
          
          p_zone <- numeric(nrow(SubS))
          
          p_zone = (DY <= 1 & DX <= 1) * 0.05 +
            ((DX==2 & DY<=1)|(DY==2 & DX<=1)) * 0.03 +
            ((DY==3 & DX==0)|(DX==3 & DY==0)|(DY==2 & DX==2)) * 0.015
        
          
          m2_drop <- m2_drop + p_zone %*% t(P_drop_conta[Source_ID[S],] * nd_Source) * (1-prm_air$Mask_Eff)
          
        }
      
      }
    }
    
    
    # ID of the sneezing worker(s) without mask
    if (sum(sneeze_no_mask)> 0) { #
      Source_ID <- Cont_no_mask_ID[sneeze_no_mask == 1]
      ## their corresponding X, Y coordinates
      Source_X <- SubW$coordX[SubW$W_ID %in% Source_ID]
      Source_Y <- SubW$coordY[SubW$W_ID %in% Source_ID]
      
      W_susceptible <- subset(SubW, location == Spaces_label[i] & W_status == "susceptible")
      
      for (S in 1:length(Source_ID)){ ## for each source worker
        DX = abs(Source_X[S]-W_susceptible$coordX)
        DY = abs(Source_Y[S]-W_susceptible$coordY)
      
        
        p_zone_air = (DY <= 1 & DX <= 1) * 0.11 +
          ((DX==2 & DY<=1)|(DY==2 & DX<=1)) * 0.035 +
          ((DY==3 & DX==0)|(DX==3 & DY==0)|(DY==2 & DX==2)) * 0.015
        

        
        resp <- runif(nrow(W_susceptible),prm_air$RespRate[3],prm_air$RespRate[5]) #[m3/min]
        
        tsed = 2/Vsed/60 # [minutes] # man_height = 2 [m]  hypothesis (source of droplet emission )
        tsed[tsed >= 5] <- 5
        
        ## number of droplets exposed to each susceptible worker (unit: # droplets)
        d_expo[W_susceptible$W_ID, ] = d_expo[W_susceptible$W_ID,] +
          eff[W_susceptible$W_ID] * (p_zone_air * resp / 2) %*% (P_drop_conta[Source_ID[S],] * nd_Source * tsed)  # 2 corresponds to 2m3, the air volume around the worker
        
        if (names(Rooms[i]) == "Cutting Room") {
          ## distance between close surfaces
          DX = abs(Source_X[S] - SubS$coordX)
          DY = abs(Source_Y[S] - SubS$coordY)
          
          p_zone <- numeric(nrow(SubS))
          
          p_zone = (DY <= 1 & DX <= 1) * 0.05 +
            ((DX==2 & DY<=1)|(DY==2 & DX<=1)) * 0.03 +
            ((DY==3 & DX==0)|(DX==3 & DY==0)|(DY==2 & DX==2)) * 0.015
          
          m2_drop <- m2_drop + p_zone %*% t(P_drop_conta[Source_ID[S],] * nd_Source) * (1-prm_air$Mask_Eff)
        }
      } ## end boucle for source S
    } # end if sum_sneeze
  }
   ## output: d_expo, 
  # return(list(To_aerosol = To_aerosol,
  #             d_expo_sneeze = d_expo))
  return(list(To_aerosol = To_aerosol,
              d_expo_sneeze = d_expo,
              m2_drop = m2_drop))
}


##### MASTER f_Module_Master() aims at dealing with the different modules Air, Surface, Transfers ... ##### 
# 1 - Determination of what is happening at time ind, who is doing what at this moment, e.g. who is in the cutting room without mask, who is touching a surface, who is talking,... 
# 2 - The tranfers between the different modules at time ind are calculated, e.g., sedimentation, inhalation, die-off.... 
# 3 - The Balances in the different module are calculated to determine the aerosol concentration or the quantity of virus at the time ind+1  
f_Module_Master <- function (
  MyAir, # /!\ MyAir (WHICH WILL BE USED/CHANGED FURTHER INSED THE FUNCTION) 
  # WAS ADDED HERE AS A FUNCTION INPUT BECAUSE IT SHOULD NOT BE A GLOBAL VARIABLE
  MyWorkers, ## ADDED
  indi_viral_load, # ADDED, numeric vector (log10 copies/ml) individual viral load for all workers if they are in the infectiousness period (status "infectious", "symptomatic", "asymptomatic")
  prm_plant,
  prm_air,
  prm_time,
  prm_workers,
  ind_min,
  ind_max,
  seed = NULL, ## added for simulation purposes
  ...
) {
  if (!is.null(seed)) {set.seed(seed)} ## added for simulation purposes
  
  # Surfaces of the room (m2)
  S_rooms <- c(prm_plant$dim.X * prm_plant$dim.Y, # surface of the cutting room
               unname(unlist(lapply(prm_plant$Spaces, function(x) return(x$dim.X * x$dim.Y)))))
  
  # Volume of the room (m3) 
  V_rooms <- c(prm_plant$dim.X * prm_plant$dim.Y * prm_plant$dim.Z, # volume of the cutting room
               unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$dim.X * x$dim.Y * x$dim.Z)))))
  
  N_rooms <- length(V_rooms)
  
  # Airflow rate of air renewal (m3/s) 
  # CAREFULL IF CHANGE UNITS (m3/h into m3/s) IN THE PARMS_PLANT (CURRENTLY IN m3/h)
  V_renew <- c(prm_plant$Air_renewal,
               unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$Air_renewal)))))/3600 
  
  ## The cumulative number in different classes of droplets exposed to each worker
  W_droplet_Expos_cumul = matrix(0, prm_workers$NWorkers, length(prm_air$Droplet_class))
  
  # Respiration rate of each employee ! Wrong assumption !!! the respiration rate is random evry time step (ce n'est pas grave Steven)
  resp <- runif(prm_workers$NWorkers,prm_air$RespRate[3],prm_air$RespRate[5]) #[m3/min]
  # Droplet contamination probability as function as volume and individual viral load
  # Can be moved out of this function !! 
  P_drop_conta =  (10^indi_viral_load) %*% t(prm_air$d_Vol)
  rownames(P_drop_conta) <- unique(MyWorkers$W_ID)

  ########## day time loop ######################
  for (ind in ind_min:ind_max)#max(MyAir$t_ind))### A modifier !!!!!! #### à mettre par jour
  {
    # ind=73
    # ind=1
    # print(ind)
    
    # The number of droplets in each class at the time index ind
    Cd = MyAir[MyAir$t_ind==(ind), 2:(1+length(prm_air$Droplet_class))] # OK (cf. output of f_initAir() if needed)
    
    # 1 - What is happening at time ind ? [[1]] cont_mask; [[2]] cont_no_mask; [[3]] no_cont_mask; [[4]] no_cont_no_mask, [[5]] Who, 
    W_Loc_N_mask <- f_Who_is(subset(MyWorkers,t_ind == ind), prm_plant, prm_workers$NWorkers)
    
    
    # p_emission <- sample(c(1,2,3,4), size = prm_workers$NWorkers, prob= c(0.25,0.25,0.25,0.25), replace = T)
    
    # Respiration activity
    # of non symtomatic  operators:  probability talking , talking loud, just breathing 
    p_emission <- sample(c(1,2,3), size = prm_workers$NWorkers, prob= c(prm_air$p_other,prm_air$p_other,prm_air$p_other), replace = T)
    
    ###### identify who ? 
    
    # 2 - TRANSFERS ----------------------------------------------------------
    # 2.1 FROM AIR to SURFACES : Droplet sedimentation of the surfaces
    dsed = S_rooms%*%Vsed*60    # (m3/min)
    #  ----------------------------------------------------------------------
    # 2.2 FROM AIR to WORKERS # !!!!ATTENTION, FAUTE ici le masque n'absorbe pas les gouttes :::)
    Resp_inh = ((W_Loc_N_mask[[1]]+W_Loc_N_mask[[3]])*0.1 + (W_Loc_N_mask[[2]]+W_Loc_N_mask[[4]]))*resp
    
    # list[[7]] for each room, number of droplets inhaled in each droplet class per operators [d / min]
    W_droplet_Expos <- lapply(seq(1:N_rooms), 
                              function (x) return(Resp_inh[,x]*t(matrix(rep(as.matrix(Cd[x,]),prm_workers$NWorkers),ncol = prm_workers$NWorkers))))
    # total Number of droplet inhaled in each rooms for each droplet classes --> Air module balance equation
    dinhale <- t(matrix(unlist(lapply(seq(1:N_rooms), 
                                      function (x) return(colSums(W_droplet_Expos[[x]])))),ncol = N_rooms))
    
    # Cumulative (during the day) exposition of all workers to all classes --> dose response model at the end of the day
    W_droplet_Expos_cumul = W_droplet_Expos_cumul+Reduce("+",W_droplet_Expos)*prm_time$Step

    #  ----------------------------------------------------------------------      
    # 2.3 FROM WORKERS to AIR #

    # total number of droplets exhaled per employee (All employee)
    dTot_exh_all_drops <- prm_air$Cd_exp[p_emission,] * resp  # [d/min]
    # total number of contaminated droplets exhaled per employee (All employee)
    # !!! calculus is done even for non infectious workers (it was just easier to do it this way)
    dTot_exh <- dTot_exh_all_drops * P_drop_conta
    
    # number of contaminated drolets exhaled of all contaminated employees in each room per droplet classes 
    # Only one copie ARN in each droplet
    # Conversion fromt copie RNA to infectious viru is done in the daily_contamination.R
    # Employees with masks
    N_CMR = (1-prm_workers$Mask_Eff)*t(matrix(unlist(lapply(seq(1:N_rooms), 
                                                            function (x) return(colSums(W_Loc_N_mask[[1]][,x]*dTot_exh)))),nrow = length (prm_air$Droplet_class)))
    # Employees without masks
    N_CNoMR = t(matrix(unlist(lapply(seq(1:N_rooms), 
                                     function (x) return(colSums(W_Loc_N_mask[[2]][,x]*dTot_exh)))),nrow = length (prm_air$Droplet_class)))
    
    dexhale <-N_CMR+N_CNoMR # number of contaminated droplets exhaled and aerosolized  [d]
    #  ----------------------------------------------------------------------
    # 2.4 Airflow rate of air renewal m3/min
    V_renew <- c(prm_plant$Air_renewal,
                 unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$Air_renewal)))))/60
    #  ----------------------------------------------------------------------
    
    ## EVENTS : SNEEZE + COUGH
    Out_sneeze <- f_Sneeze(SubW = subset(MyWorkers, t_ind == ind), Rooms_label = Spaces_label, t_ind = ind, Rooms = W_Loc_N_mask$Who, prm_air = prm_air, prm_time = prm_time)
    # Out_cough ....
    
    
    # 2.5 Balance equation of emissions (sick workers) and absortion (sedimentation, inhalation, exhaust air)
    MyAir[MyAir$t_ind==ind+1,2:(1+length(prm_air$Droplet_class))] = Cd+
      (dexhale*Method_calc + Out_sneeze$To_aerosol  -  dinhale-(V_renew + dsed)*Cd)*prm_time$Step/V_rooms # [d / m3]
    
    ##### 
    ###MySurfaces = dsed + fallindrop(sneenze) + fallingdrop(cough)
    
    
    # MyAir[MyAir$t_ind==ind,2:(1+length(prm_air$Droplet_class))] <- MyAir[MyAir$t_ind==(ind-1),2:(1+length(prm_air$Droplet_class))] + 
    #                                                     f_dCd(MyAir[MyAir$t_ind==(ind-1),2:(1+length(prm_air$Droplet_class))],
    #                                                           S_rooms,V_rooms,prm_time,MyWorkers,prm_plant,prm_workers, ind)
    #  
    # print(W_droplet_Expos_cumul)
  }
  ########## End of day time loop ######################
  
  return(list(MyAir,W_droplet_Expos_cumul))
}
