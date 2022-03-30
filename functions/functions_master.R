##### f_Who_is() - At a given time index: Who ? Where ? Status ? Mask?  #####
f_Who_is <- function(
  ## Function to look for every workers at the different locations of the plant  
  ## depending on their sanitary status and mask wearing attributes at a given time index 
  ## INPUT
  SubW, ## (subset of MyWorkers) The workers attributes associated with one given time index
  prm_plant
  ## OUTPUT
){
  #### Spaces 
  ## Convert all workspace locations "WS..." into "Cutting room" (prm_plant$label)
  SubW$location[str_starts(SubW$location,"WS")] <- prm_plant$label
  
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
    return(list(C_M_ID = C_M_ID[[x]],
                C_NM_ID = C_NM_ID[[x]],
                NC_M_ID = NC_M_ID[[x]],
                NC_NM_ID = NC_NM_ID[[x]]))
  }) %>% `names<-`(.,unique(Spaces_label))-> Rooms

  symptomatic = SubW$W_status == "symptomatic"

  return(list(Cont_mask = Cont_mask,
              Cont_no_mask = Cont_no_mask,
              Non_Cont_mask = Non_Cont_mask,
              Non_Cont_no_mask = Non_Cont_no_mask,
              Rooms = Rooms,
              symptomatic = symptomatic))

}


##### f_Sneeze #####
f_Sneeze <- function(
  ## INTPUT
  SubW, ## (subset of) MyWorkers associated with ONE GIVEN TIME INDEX,
  SubS, ## (subset of) MySurfaces associated with ONE GIVEN TIME INDEX
  Rooms, ## One of the outputs of the function f_Who_is() (check for details)
  prm_air,
  prm_time
  ## OUTPUT: (list of 3 elements)
  ## 1. $To_aeorosol (numeric matrix): number of droplets transfered from the sneezing worker(s) to aerosol
  ## 2. $d_expo (numeric matrix): number of droplets from the sneezing worker(s) to other near susceptible worker(s)
  ## 3. $m2_drop: number of droplets settling from the sneezing worker(s) to other near coordinates
) {
  ## Convert all workspace locations "WS..." into "Cutting room" (prm_plant$label)
  SubW$location[str_starts(SubW$location,"WS")] <- "Cutting Room"
  
  ## Initializing the output formats
  To_aerosol <- matrix(0, nrow = length(Spaces_label), ncol = ncol(Method_calc))
  d_expo <- matrix(0, nrow = nrow(SubW), ncol = ncol(Method_calc)) %>% `rownames<-`(.,sort(unique(SubW$W_ID)))
  m2_drop <- matrix(0, nrow = nrow(SubS), ncol = ncol(Method_calc)) %>% `rownames<-`(.,sort(unique(SubS$S_ID)))
  
  ## Mask efficiency depending the wearing mask attribute of each worker (SubW$W_mask)
  eff <- (SubW$W_mask == "mask") * (1-prm_air$Mask_Eff) + (SubW$W_mask == "no mask")
  
  eff[is.na(eff)] <- 0 # assign 0 to efficiency if NA (e.g. for non active worker(s))
  names(eff) <- sort(unique(SubW$W_ID))
  
  ## Calculate the sedimentation time for every rooms
  tsed = 2/Vsed/60 # [minutes] # man_height = 2 [m]  hypothesis (source of droplet emission )
  tsed[tsed >= 5] <- 5
  
  ## SNEEZING EVENTS IN EACH ROOM (LOOP) ---- 
  for (i in 1:length(Rooms)) { ## for each Rooms 
    ## extract the IDs of the contaminated masked and not-masked workers
    Cont_mask_ID <- Rooms[[i]][[1]] ## ([[1]]: Cont_mask_ID : check f_Who_is for details)
    Cont_no_mask_ID <- Rooms[[i]][[2]] ## ([[2]]: Cont_no_mask_ID : check f_Who_is for details)
    
    ## extract their corresponding status (asymptomatic, symptomatic, infectious)
    status_mask <- SubW$W_status[SubW$W_ID %in% Cont_mask_ID] 
    status_no_mask <- SubW$W_status[SubW$W_ID %in% Cont_no_mask_ID]
    
    # The number of falling/settling droplets per sneezing worker depending on the droplet size class and air characteristics
    nd_Source = prm_air$Cd_sneeze * (Method_calc[i,]==F) * prm_air$Vol_sneeze
    
    # Look for all other susceptible worker(s) in the same space (room) i
    W_susceptible <- subset(SubW, location == Spaces_label[i] & W_status == "susceptible")
    
    ## TRANSFERT FROM SNEEZING WORKERS TO AEROSOL ----
    ## Sneezing events by masked contaminated worker(s)
    if (length(status_mask) > 0) {  # if there is any contaminated worker (masked) present in the current room, 
      sapply(status_mask, FUN = function(x) { # for each one,
        prob <- prm_air$p_sneeze[[x]] # determine if the worker sneezes or not with a probability depending on their status
        return(   min(rbinom(1, prm_time$Step, prob = prob), 1)   )
      }) %>% as.vector() -> sneeze_mask # sneeze_mask : vector of values 0/1 (same length as the number of considering workers)
      
      # Cumulative number of contaminated droplets going to aerosol for each droplet size class of the above masked sneezing workers
      # while taking into account the individual variability in the viral load emitted
      To_aerosol[i,] = To_aerosol[i,] + 
        colSums(sneeze_mask * (1 - prm_air$Mask_Eff) * P_drop_conta[Cont_mask_ID,] * prm_air$Cd_sneeze) * (Method_calc[i,]==T) * prm_air$Vol_sneeze
    } else {sneeze_mask <- 0} # otherwise, if there is contaminated worker, assign 0 value
    
    ## Sneezing events by masked contaminated worker(s)
    if (length(status_no_mask)) {  # if there is any contaminated worker (non-masked) present in the current room,
      sapply(status_no_mask, FUN = function(x) { # for each one,
        prob <- prm_air$p_sneeze[[x]] # determine if the worker sneezes or not with a probability depending on their status
        return( min(rbinom(1, prm_time$Step, prob = prob), 1))
      }) %>% as.vector() ->  sneeze_no_mask # sneeze_no_mask : vector of values 0/1 (same length as the number of considering workers)
      
      # Cumulative number of contaminated droplets going to aerosol for each droplet size class of the above masked sneezing workers
      # while taking into account the individual variability in the viral load emitted
      To_aerosol[i,] = To_aerosol[i,] + 
        colSums(sneeze_no_mask * P_drop_conta[Cont_mask_ID,] * prm_air$Cd_sneeze) * (Method_calc[i,]==T) * prm_air$Vol_sneeze
      
    } else {sneeze_no_mask <- 0} # otherwise, if there is contaminated worker, assign 0 value
    
    ## TRANSFERT FROM SNEEZING WORKERS TO OTHER NEARBY WORKERS ----
    ## Exposition of the near susceptible workers and locations due to sneezing events by (source) MASKED worker(s)
    if (length(status_mask) > 0) { # if there is any sneezing masked worker
      # Extract their IDs and X,Y coordinates
      Source_ID <- Cont_mask_ID[sneeze_mask == 1] # IDs
      Source_X <- SubW$coordX[SubW$W_ID %in% Source_ID] # X coordinates
      Source_Y <- SubW$coordY[SubW$W_ID %in% Source_ID] # Y coordinates

      for (S in 1:length(Source_ID)) { ## for each source (sneezing worker)  S
        # Distance between the near susceptible workers and the source (sneezing worker) S
        DX = abs(Source_X[S]-W_susceptible$coordX)
        DY = abs(Source_Y[S]-W_susceptible$coordY)
        
        # Calculate the fraction of droplets exposed to each susceptible worker depending on the distance between him and the source
        p_zone_air = (DY <= 1 & DX <= 1) * prm_air$p_zone_air[["adj1"]] + # 1st (nearest) adjacent cases (check parameters_air.R)
          ((DX==2 & DY<=1)|(DY==2 & DX<=1)) * prm_air$p_zone_air[["adj2"]] + # 2nd adjacent cases
          ((DY==3 & DX==0)|(DX==3 & DY==0)|(DY==2 & DX==2)) * prm_air$p_zone_air[["adj3"]] # 3rd (farthest) adjacent cases
        
        # Calculate the respiration rates of all considering susceptible workers (from "light exercise" to "heavy exercise")
        resp <- runif(nrow(W_susceptible), prm_air$RespRate[3], prm_air$RespRate[5]) #[m3/min]
        
        ## The total number of droplets exposed to each susceptible worker (unit: # droplets) (# 2 corresponds to 2m3 of the air volume around the worker)
        d_expo[W_susceptible$W_ID, ] = d_expo[W_susceptible$W_ID,] +
          eff[W_susceptible$W_ID] * (p_zone_air * resp / 2) %*% (P_drop_conta[Source_ID[S],] * nd_Source * (1-prm_air$Mask_Eff) * tsed) 
        
        ## Droplets settling events exclusively in the cutting room
        if (names(Rooms[i]) == "Cutting Room") {
          ## distance between the current source S and the near locations (coordinates)
          DX = abs(Source_X[S] - SubS$coordX)
          DY = abs(Source_Y[S] - SubS$coordY)
          
          # Calculate the fraction of droplets settling on near coordinates depending on the distances with the source
          p_zone = (DY <= 1 & DX <= 1) * prm_air$p_zone[["adj1"]] + # 1st nearest cases (check parameters_air.R)
            ((DX==2 & DY<=1)|(DY==2 & DX<=1)) * prm_air$p_zone[["adj2"]] + # 2nd adjacent cases 
            ((DY==3 & DX==0)|(DX==3 & DY==0)|(DY==2 & DX==2)) * prm_air$p_zone[["adj3"]] # 3rd (farthest) adjacent cases
          
          ## The total number of droplets settling on the near locations with mask efficiency
          m2_drop <- m2_drop + p_zone %*% t(P_drop_conta[Source_ID[S],] * nd_Source) * (1-prm_air$Mask_Eff)
        } # end if "Droplet settling in Cutting Room
        
      } # end loop "each source worker"
    } # end MASKED sneezing workers
    
    ## Exposition of the near susceptible workers and locations due to sneezing events by (source) NON-MASKED worker(s)
    if (sum(sneeze_no_mask)> 0) { # if there is any sneezing masked worker
      # Extract their IDs and X,Y coordinates
      Source_ID <- Cont_no_mask_ID[sneeze_no_mask == 1] # IDs
      Source_X <- SubW$coordX[SubW$W_ID %in% Source_ID] # X coordinates
      Source_Y <- SubW$coordY[SubW$W_ID %in% Source_ID] # Y coordinates
      
      for (S in 1:length(Source_ID)) { ## for each source (sneezing worker)  S
        # Distance between the near susceptible workers and the source (sneezing worker) S
        DX = abs(Source_X[S]-W_susceptible$coordX)
        DY = abs(Source_Y[S]-W_susceptible$coordY)
        
        # Calculate the fraction of droplets exposed to each susceptible worker depending on the distance between him and the source
        p_zone_air = (DY <= 1 & DX <= 1) * prm_air$p_zone_air[["adj1"]] + # 1st (nearest) adjacent cases (check parameters_air.R)
          ((DX==2 & DY<=1)|(DY==2 & DX<=1)) * prm_air$p_zone_air[["adj2"]] + # 2nd adjacent cases
          ((DY==3 & DX==0)|(DX==3 & DY==0)|(DY==2 & DX==2)) * prm_air$p_zone_air[["adj3"]] # 3rd (farthest) adjacent cases
        
        # Calculate the respiration rates of all considering susceptible workers (from "light exercise" to "heavy exercise")
        resp <- runif(nrow(W_susceptible), prm_air$RespRate[3], prm_air$RespRate[5]) #[m3/min]
        
        ## The total number of droplets exposed to each susceptible worker (unit: # droplets) (# 2 corresponds to 2m3 of the air volume around the worker)
        d_expo[W_susceptible$W_ID, ] = d_expo[W_susceptible$W_ID,] +
          eff[W_susceptible$W_ID] * (p_zone_air * resp / 2) %*% (P_drop_conta[Source_ID[S],] * nd_Source * tsed) 
        
        ## Droplets settling events exclusively in the cutting room
        if (names(Rooms[i]) == "Cutting Room") {
          ## distance between the current source S and the near locations (coordinates)
          DX = abs(Source_X[S] - SubS$coordX)
          DY = abs(Source_Y[S] - SubS$coordY)
          
          # Calculate the fraction of droplets settling on near coordinates depending on the distances with the source
          p_zone = (DY <= 1 & DX <= 1) * prm_air$p_zone[["adj1"]] + # 1st nearest cases (check parameters_air.R)
            ((DX==2 & DY<=1)|(DY==2 & DX<=1)) * prm_air$p_zone[["adj2"]] + # 2nd adjacent cases 
            ((DY==3 & DX==0)|(DX==3 & DY==0)|(DY==2 & DX==2)) * prm_air$p_zone[["adj3"]] # 3rd (farthest) adjacent cases
          
          ## The total number of droplets settling on the near locations (droplets coming from non-masked sneezing workers)
          m2_drop <- m2_drop + p_zone %*% t(P_drop_conta[Source_ID[S],] * nd_Source)
        } # end if "Droplet settling in Cutting Room
        
      } # end loop "each source workers"
    } # end NON-MASKED sneezing workers
    
  } # end loop Rooms 

  return(list(To_aerosol = To_aerosol,
              d_expo = d_expo,
              m2_drop = m2_drop))
}




##### MASTER f_Module_Master() aims at dealing with the different modules Air, Surface, Transfers ... ##### 
# 1 - Determination of what is happening at time ind, who is doing what at this moment, e.g. who is in the cutting room without mask, who is touching a surface, who is talking,... 
# 2 - The tranfers between the different modules at time ind are calculated, e.g., sedimentation, inhalation, die-off.... 
# 3 - The Balances in the different module are calculated to determine the aerosol concentration or the quantity of virus at the time ind+1  
f_Module_Master <- function (
  MyAir, ## (object MyAir)
  W, ## (object MyWorkers)
  S, ## (bbject MySurfaces)
  prm_plant,
  prm_air,
  prm_time,
  prm_workers,
  ind_min,
  ind_max,
  seed = NULL, ## added for simulation purposes
  ## Some objects in global environment will be used
  ## - S_rooms, V_rooms, NWorkers, N_rooms, V_renew (check the function f_initAir),
  ## - P_drop_conta (check the function f_indi_viral_load)
  ...
) {
  ## Simulation seed
  if (!is.null(seed)) {set.seed(seed)}
  
  ## The cumulative number in different classes of droplets exposed to each worker
  W_droplet_Expos_cumul <- matrix(0, nrow=NWorkers, ncol=length(prm_air$Droplet_class)) %>% `rownames<-`(.,sort(unique(W$W_ID)))
  
  ## Day loop - For each time step ----
  for (ind in ind_min:ind_max) # for each time index of the day
  {
    # The number of droplets in each class at the time index ind
    Cd = MyAir[MyAir$t_ind==(ind), 2:(1+length(prm_air$Droplet_class))]
    
    # 1. WHAT IS HAPPENING AT THIS TIME INDEX ? ----
    # 1.1 Who ? Where ? Masked ? -----
    WhoIs <- f_Who_is(SubW = subset(W, t_ind == ind), prm_plant = prm_plant) # WhoIs[[1]] cont_mask; [[2]] cont_no_mask; [[3]] no_cont_mask; [[4]] no_cont_no_mask, [[5]] Rooms, 
    
    # 1.2 Talking, talking loud, breathing ----
    # Respiration rate of each worker which is time step dependent (from light to heavy exercise) (cf. parameters_air.R)
    resp <- runif(NWorkers, prm_air$RespRate[3], prm_air$RespRate[5]) #[m3/min]
    
    # Emission by workers due to other activities such as talking , talking loud or breathing 
    p_emission <- sample(c(1,2,3), size = NWorkers, prob=c(prm_air$p_talk, prm_air$p_talk, prm_air$p_talk), replace = T)
    
    # 1.3 SNEEZES ----
    Sneeze <- f_Sneeze(SubW = subset(W, t_ind == ind), Rooms = WhoIs$Rooms, SubS = subset(S, t_ind == ind), prm_air = prm_air, prm_time = prm_time)
    
    # 1.4 COUGHS ----
    # Out_cough ....
    
    # 2 - TRANSFERS ----------------------------------------------------------
    # 2. From air to surfaces (sedimentation) ----
    ## Droplet sedimentation of the surfaces 
    dsed = S_rooms %*% Vsed * 60    # (m3/min)

    # 2.2 FROM AIR to WORKERS ---- 
    # [[1]] cont_mask; [[2]] cont_no_mask; [[3]] no_cont_mask; [[4]] no_cont_no_mask
    Resp_inh = ((WhoIs[[1]] + WhoIs[[3]]) * (1 - prm_air$Mask_Eff) + (WhoIs[[2]] + WhoIs[[4]])) * resp
    
    # list[[7]] for each room, number of droplets inhaled in each droplet class per workers [d / min]
    W_droplet_Expos <- lapply(seq(1:N_rooms), 
                              function(x) return(Resp_inh[,x] * t(matrix(rep(as.matrix(Cd[x,]), NWorkers), ncol = NWorkers))))
    
    # Total number of droplets inhaled in each rooms for each droplet classes --> Air module balance equation
    dinhale <- t(matrix(unlist(lapply(seq(1:N_rooms), 
                                      function (x) return(colSums(W_droplet_Expos[[x]])))), ncol = N_rooms))
    
    # Cumulative (during the day) exposition of all workers to all classes --> dose response model at the end of the day
    W_droplet_Expos_cumul = W_droplet_Expos_cumul + Reduce("+",W_droplet_Expos) * Step

    # 2.3 FROM WORKERS to AIR ----
    # total number of droplets exhaled per employee (All employee)
    dTot_exh_all_drops <- prm_air$Cd_exp[p_emission,] * resp  # [d/min]
    
    # Total number of CONTAMINATED droplets exhaled per employee (All employees) (The calculation is done even for non infectious workers)
    # N.B.: P_drop_conta (global variable) : the number of RNA copies per droplets, already taking into account the individual variability (check the function f_indi_viral_load)
    dTot_exh <- dTot_exh_all_drops * P_drop_conta
    
    # Total number of contaminated droplets exhaled of all contaminated workers in each room for each droplet class 
    # (Conversion from #RNAcopies to #infectious virus is done in "daily_contamination.R")
    # Employees with masks
    N_CMR = (1-prm_air$Mask_Eff) * t(matrix(unlist(lapply(seq(1:N_rooms), 
                                                              function (x) return(colSums(WhoIs[[1]][,x]*dTot_exh)))),nrow = length (prm_air$Droplet_class)))
    # Employees without masks
    N_CNoMR = t(matrix(unlist(lapply(seq(1:N_rooms), 
                                     function (x) return(colSums(WhoIs[[2]][,x]*dTot_exh)))),nrow = length (prm_air$Droplet_class)))
    
    # Total number of contaminated droplets exhaled and aerosolized  [d]
    dexhale <- N_CMR + N_CNoMR 

    # 2.4 AIR RENEWAL ----
    ## Volume of the renewal air (m3/min) (V_renew in global environment)
    # V_renew <- c(prm_plant$Air_renewal,
    #              unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$Air_renewal)))))/60
    

    
    
    # 2.5 AEROSOL - EMISSION/ABSORPTION BALANCE EQUATION ----
    MyAir[MyAir$t_ind==ind+1, 2:(1+length(prm_air$Droplet_class))] = Cd +
      (dexhale*Method_calc + Sneeze$To_aerosol - dinhale - (V_renew + dsed) * Cd) * Step / V_rooms # [d / m3]
    
    ##### 
    ###MySurfaces = dsed + fallindrop(sneenze) + fallingdrop(cough)
    
    
  }
  
  return(list(MyAir = MyAir,
              Expocum = W_droplet_Expos_cumul))
}
