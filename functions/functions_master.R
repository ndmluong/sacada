##### f_Who_is() to sum up who is where, how is he and is he wearing a mask at A GIVEN TIME INDEX #####
f_Who_is <- function(
  # To sum up who is where, how is he and is he wearing a mask at A GIVEN TIME INDEX
  Sub_MyWorkers, ## THE WORKERS ASSOCIATED WITH ONLY THE GIVEN TIME INDEX
  prm_plant,
  NWorkers,
  ind ## the given time index
){
  # For the test
  # ind=536
  # Sub_MyWorkers <- subset(MyWorkers,t_ind == ind)
  # NWorkers = prm_workers$NWorkers
  # Number of contaminated and non contaminated workers in the rooms
  i=1
  
  ## Convert all workspace locations "WS..." into "Cutting room" (prm_plant$label)
  Sub_MyWorkers$location[str_starts(Sub_MyWorkers$location,"WS")] <- prm_plant$label
  
  Rooms_label <- c(prm_plant$label)
  
  ### --> /!\ MODIFICATION PROPOSITION (automatised)
  # The total number of the room = nb of spaces declared in prm_plant + cutting room
  NRooms <- length(prm_plant$Spaces) + 1 
  # Define the matrices showing the presence/absence (T/F) of every workers inside every rooms
  Cont_mask <- matrix(FALSE, NWorkers, NRooms) # size = NWORKER,NROOMS, TRUE/FALSE
  Cont_no_mask <- matrix(FALSE, NWorkers, NRooms) # size = NWORKER,NROOMS, TRUE/FALSE
  Non_Cont_mask <- matrix(FALSE, NWorkers, NRooms) # size = NWORKER,NROOMS, TRUE/FALSE
  Non_Cont_no_mask <- matrix(FALSE, NWorkers, NRooms) # size = NWORKER,NROOMS, TRUE/FALSE
  ###
  
  Cont_mask[,1] <- Sub_MyWorkers$W_status %in% c("infectious", "symptomatic", "asymptomatic") &
    Sub_MyWorkers$location == prm_plant$label & Sub_MyWorkers$W_mask=="mask"
  
  Cont_no_mask[,1]  =Sub_MyWorkers$W_status %in% c("infectious", "symptomatic", "asymptomatic") &
    Sub_MyWorkers$location == prm_plant$label & Sub_MyWorkers$W_mask=="no mask"
  
  Non_Cont_mask[,1]  = Sub_MyWorkers$W_status %in% c("susceptible", "infected", "non-infectious") &
    Sub_MyWorkers$location == prm_plant$label & Sub_MyWorkers$W_mask=="mask"
  
  Non_Cont_no_mask[,1]  = Sub_MyWorkers$W_status %in% c("susceptible", "infected", "non-infectious") &
    Sub_MyWorkers$location == prm_plant$label & Sub_MyWorkers$W_mask=="no mask"

  ### For the other rooms
  for (x in prm_plant$Spaces) {
    Cont_mask[,i+1] =  Sub_MyWorkers$W_status %in% c("infectious", "symptomatic", "asymptomatic") &
      Sub_MyWorkers$location==x$label & Sub_MyWorkers$W_mask=="mask"
    
    Cont_no_mask[,i+1] =  Sub_MyWorkers$W_status %in% c("infectious", "symptomatic", "asymptomatic") &
      Sub_MyWorkers$location==x$label & Sub_MyWorkers$W_mask=="no mask"
    
    Non_Cont_mask[,i+1] =  Sub_MyWorkers$W_status %in% c("susceptible", "infected", "non-infectious") &
      Sub_MyWorkers$location==x$label & Sub_MyWorkers$W_mask=="mask"
    
    Non_Cont_no_mask[,i+1] =  Sub_MyWorkers$W_status %in% c("susceptible", "infected", "non-infectious") &
      Sub_MyWorkers$location==x$label & Sub_MyWorkers$W_mask=="no mask"
    
    i=i+1
    
    Rooms_label <- c(Rooms_label, x$label)
  }
   symptomatic=Sub_MyWorkers$W_status =="symptomatic"
  # 
  
  Rooms <- vector("list", length = NRooms)
  
  for (i in 1:NRooms) {
    if (length(which(Cont_mask[,i])) > 0) {
      Cont_mask_ID <- paste("W", str_pad(which(Cont_mask[,i]), width = 3, pad = 0), sep="")
    } else Cont_mask_ID <- NA
    
    if (length(which(Cont_no_mask[,i])) > 0) {
      Cont_no_mask_ID <- paste("W", str_pad(which(Cont_no_mask[,i]), width = 3, pad = 0), sep="")
    } else Cont_no_mask_ID <- NA
    
    if (length(which(Non_Cont_mask[,i])) > 0) {
      Non_Cont_mask_ID <- paste("W", str_pad(which(Non_Cont_mask[,i]), width = 3, pad = 0), sep="")
    } else Non_Cont_mask_ID <- NA
    
    if (length(which(Non_Cont_no_mask[,i])) > 0) {
      Non_Cont_no_mask_ID <- paste("W", str_pad(which(Non_Cont_no_mask[,i]), width = 3, pad = 0), sep="")
    } else Non_Cont_no_mask_ID <- NA
    
    Rooms[[i]] <- list(Cont_mask_ID, Cont_no_mask_ID, Non_Cont_mask_ID, Non_Cont_no_mask_ID)
  }

  return(list(Cont_mask = Cont_mask,
              Cont_no_mask = Cont_no_mask,
              Non_Cont_mask = Non_Cont_mask,
              Non_Cont_no_mask = Non_Cont_no_mask,
              Rooms = Rooms,
              symptomatic = symptomatic))
  
}


f_Sneeze <- function(
  ## INTPUT
  Rooms, ## one of the output of the function f_Who_is
  MyWorkers,
  Rooms_label,
  t_ind,
  prm_air,
  prm_time,
  ## OUTPUT
  ## 1. nb droplet from sneeze to aerosol
  ## 2. nb droplet from the sneezing worker to the other agents (workers, surfaces, meat)
) {
  
  W <- subset(MyWorkers, t_ind == t_ind)
  
  for (i in length(Rooms)) { ## for each Rooms 
    
    ## extract the infectious masked workers (Cont_mask) ...[[1]]
    Cont_mask_ID <- Rooms[[i]][[1]]
    
    ## their corresponding status
    status <- W$W_status[W$w_ID %in% Cont_mask_ID]
    
    ## sneeze: indicates if each worker prensent in the room and infectious sneezes or not (value 1/0)
    sapply(status, FUN = function(x) {
      prob <- Parms_Air$p_sneeze[x] * prm_time$Step
      sneeze <- rbinom(1, 1, prob = prob)
      return(sneeze)
    }) -> sneeze
    
    #aerosols droplet nuclei
    To_aerosol[i] = sum(sneeze) * prm_air$Cd_sneeze * (Method_calc[i,]==T) * prm_air$Vol_sneeze
  
    ##Falling droplets (number emitted when sneezing)
    nd_Source = prm_air$Cd_sneeze * (Method_calc[i,]==F) * prm_air$Vol_sneeze
        # ID of the sneezing worker(s)
    Source_ID <- Cont_mask_ID[sneeze == 1]

    # Surfaces_ID
    
    ## their corresponding X, Y coordinates
    Source_X <- W$coordX[W$W_ID %in% Source_ID]
    Source_Y <- W$coordY[W$W_ID %in% Source_ID]
    
    W_susceptible <- subset(W, location == Rooms_label[i] & W_status == "susceptible")
    
    d_expo <- matrix(0, nrow = nrow(W_susceptible),
                     ncol = ncol(Method_calc))
    
    for (S in 1:length(Source_ID)){
      DX = abs(Source_X[S]-W_susceptible$coordX)
      DY = abs(Source_Y[S]-W_susceptible$coordY)
      # if (max(c(DX,DY)<=1) {}
      
      p_zone_air = (DY <= 1 & DX <= 1) * 0.11 + 
      ((DX==2 & DY<=1)|(DY==2 & DX<=1)) * 0.035 + 
      ((DY==3 & DX==0)|(DX==3 & DY==0)|(DY==2 & DX==2)) * 0.015
      
      resp <- runif(nrow(W_susceptible),prm_air$RespRate[3],prm_air$RespRate[5]) #[m3/min]
      
      tsed = 2/Vsed/60 # [minutes] # man_height = 2 [m]  hypothesis (source of droplet emission )
      tsed[tsed >= 5] <- 5
      
      d_expo = d_expo +
        (p_zone_air * resp / 2) %*% (nd_Source * tsed)  # 2 corresponds to 2m3, the air volume around the worker
      
    }
    # 
    # (DY <= 1 & DX <= 1) * 0.05 + 
    #   ((DX=2 & DY<=1)|(DY=2 & DX<=1)) * 0.03 + 
    #   ((DY=3 & DX=0)|(DX=3 & DY=0)|(DY=2 & DX=2)) * 0.015
    
    
    }
   ## output: d_expo, 
  return(d_expo)
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

  ########## day time loop ######################
  for (ind in ind_min:ind_max)#max(MyAir$t_ind))### A modifier !!!!!! #### à mettre par jour
  {
    # ind=73
    # ind=1
    # print(ind)
    
    # The number of droplets in each class at the time index ind
    Cd = MyAir[MyAir$t_ind==(ind), 2:(1+length(prm_air$Droplet_class))] # OK (cf. output of f_initAir() if needed)
    
    # 1 - What is happening at time ind ? [[1]] cont_mask; [[2]] cont_no_mask; [[3]] no_cont_mask; [[4]] no_cont_no_mask
    W_Loc_N_mask <- f_Who_is(subset(MyWorkers,t_ind == ind), prm_plant, prm_workers$NWorkers)
    
    
    # p_emission <- sample(c(1,2,3,4), size = prm_workers$NWorkers, prob= c(0.25,0.25,0.25,0.25), replace = T)
    
    # Respiration activity
    # of non symtomatic  operators:  probability talking , talking loud, just breathing, coughing at time ind 
    p_emission <- sample(c(1,2,3,4), size = prm_workers$NWorkers, prob= c(prm_air$p_other,prm_air$p_other,prm_air$p_other,prm_air$p_cough), replace = T)
    
    # of symtomatic  operators:  coughing all the time ind 
    p_emission[W_Loc_N_mask[[5]]] <- 4
    
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
    # 2.5 Balance equation of emissions (sick workers) and absortion (sedimentation, inhalation, exhaust air)
    MyAir[MyAir$t_ind==ind+1,2:(1+length(prm_air$Droplet_class))] = Cd+
      (dexhale*Method_calc -  dinhale-(V_renew + dsed)*Cd)*prm_time$Step/V_rooms # [d / m3]
    
    
    # MyAir[MyAir$t_ind==ind,2:(1+length(prm_air$Droplet_class))] <- MyAir[MyAir$t_ind==(ind-1),2:(1+length(prm_air$Droplet_class))] + 
    #                                                     f_dCd(MyAir[MyAir$t_ind==(ind-1),2:(1+length(prm_air$Droplet_class))],
    #                                                           S_rooms,V_rooms,prm_time,MyWorkers,prm_plant,prm_workers, ind)
    #  
    # print(W_droplet_Expos_cumul)
  }
  ########## End of day time loop ######################
  
  return(list(MyAir,W_droplet_Expos_cumul))
}


f_Which_is <- function(

) {
  
}