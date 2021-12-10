library(ggplot2)
#### FOR THE TESTS !!!!!! 
prm_plant = Parms_Plant
prm_air = Parms_Air
prm_time = Parms_Time
prm_workers = Parms_Workers
MyAir <- f_initAir(prm = Parms_Plant, prm_time = Parms_Time, prm_air = Parms_Air)

AIR_ID <- c(prm_plant$label,
            unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$label)))))

MyWorkers <- subset(MyWorkers,Week<2)
MyWorkers <- dplyr::arrange(MyWorkers, t_ind, W_ID)

MyWorkers$W_location <- rep(sample(AIR_ID,
                  size = prm_workers$NWorkers, replace = T,
                  prob = c(0.8,0.025,0.025,0.05,0.025,0.025,0.05)),2016)
# Pour le test, les opérateurs arrivent vers 6h, prennent une pause à 12h, travaillent jusqu' à 22h
MyWorkers$W_location[MyWorkers$Hour<6]='Home'
MyWorkers$W_location[MyWorkers$Hour>12 & MyWorkers$Hour<15]='Home'
MyWorkers$W_location[MyWorkers$Hour>22]='Home'
MyWorkers$W_location[MyWorkers$Day>5]='Home'

status<-c("contaminated","not contaminated")
MyWorkers$W_status <- rep(sample(status,
                               size = Parms_Workers$NWorkers, replace = T,
                               prob = c(0.1,0.9)) ,  2016)
status<-c("mask","no mask")
MyWorkers$W_mask<- rep(sample(status,
                             size = Parms_Workers$NWorkers, replace = T,
                             prob = c(0.5,0.05)), 2016)

Method_calc <<- f_Air_Criteria_Calc(Parms_Plant,Parms_Air)
MyAir <- f_initAir(prm = Parms_Plant, prm_time = Parms_Time, prm_air = Parms_Air)

MyAir[MyAir$t_ind==0,2:(1+length(prm_air$Droplet_class))] = matrix(0,7,6)*(Method_calc)
MyAir <- subset(MyAir,t_ind<2016)


ind = 1
#########################################



Who_is <- function(Sub_MyWorkers,prm_plant,NWorkers,ind){
# To sum up who is where, how is he and is he wearing a mask
   # For the test
  # ind=536
  # Sub_MyWorkers <-subset(MyWorkers,t_ind == ind)
  # NWorkers = prm_workers$NWorkers
   # Number of contaminated and non contaminated workers in the rooms
  i=1
  
  # Who is in which room, weering a mask or not
  Cont_mask <- matrix(FALSE,NWorkers,7) # size = NWORKER,NROOMS, TRUE/FALSE 
  Cont_nomask <- matrix(FALSE,NWorkers,7) # size = NWORKER,NROOMS, TRUE/FALSE
  Non_Cont_mask <- matrix(FALSE,NWorkers,7) # size = NWORKER,NROOMS, TRUE/FALSE
  Non_Cont_no_mask <- matrix(FALSE,NWorkers,7) # size = NWORKER,NROOMS, TRUE/FALSE
  ### For the cutting room
  Cont_mask[,1] <- Sub_MyWorkers$W_status =="contaminated" & 
    Sub_MyWorkers$W_location == prm_plant$label & Sub_MyWorkers$W_mask=="mask"

  Cont_nomask[,1]  =Sub_MyWorkers$W_status =="contaminated" &
    Sub_MyWorkers$W_location == prm_plant$label & Sub_MyWorkers$W_mask=="no mask"

  Non_Cont_mask[,1]  = Sub_MyWorkers$W_status =="not contaminated" &
    Sub_MyWorkers$W_location == prm_plant$label & Sub_MyWorkers$W_mask=="mask"

  Non_Cont_no_mask[,1]  = Sub_MyWorkers$W_status =="not contaminated" &
    Sub_MyWorkers$W_location == prm_plant$label & Sub_MyWorkers$W_mask=="no mask"
  ### For the other rooms
  for(x in prm_plant$Spaces){
    Cont_mask[,i+1] =  Sub_MyWorkers$W_status =="contaminated" & 
      Sub_MyWorkers$W_location==x$label & Sub_MyWorkers$W_mask=="mask"
    Cont_nomask[,i+1] =  Sub_MyWorkers$W_status =="contaminated" &
      Sub_MyWorkers$W_location==x$label & Sub_MyWorkers$W_mask=="no mask"
    Non_Cont_mask[,i+1] =  Sub_MyWorkers$W_status =="not contaminated" &
      Sub_MyWorkers$W_location==x$label & Sub_MyWorkers$W_mask=="mask"
    Non_Cont_no_mask[,i+1] =  Sub_MyWorkers$W_status =="not contaminated" &
      Sub_MyWorkers$W_location==x$label & Sub_MyWorkers$W_mask=="no mask"
    i=i+1
  }
  return(list(Cont_mask,Cont_nomask,Non_Cont_mask,Non_Cont_no_mask))
}

##### MASTER ##### 
 # This function aims at dealing with the different modules Air, Surface, Transfers ... 
 ########################
 
 # 1 - Determination of what is happening at time ind, who is doing what at this moment, e.g. who is in the cutting room without mask, who is touching a surface, who is talking,... 
 # 2 - The tranfers between the different modules at time ind are calculated, e.g., sedimentation, inhalation, die-off.... 
 # 3 - The Balances in the different module are calculated to determine the aerosol concentration or the quantity of virus at the time ind+1  
 
f_Module_Master <- function (prm_plant, prm_air, prm_time, prm_workers){
 # Surfaces of the room (m2)
 S_rooms <- c(prm_plant$dim.X*prm_plant$dim.Y,
              unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$dim.X*x$dim.Y)))))
 
 # Volume of the room (m3)
 V_rooms <- c(prm_plant$dim.X*prm_plant$dim.Y*prm_plant$dim.Z,
              unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$dim.X*x$dim.Y*x$dim.Z)))))
 N_rooms <- length(V_rooms)
 # Airflow rate of air renewal m3/s
 V_renew <- c(prm_plant$Air_renewal,
              unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$Air_renewal)))))/3600
 W_droplet_Expos_cumul = matrix(NA,prm_workers$NWorkers,length(prm_air$Droplet_class))
 
 # Respiration rate of each employee ! Wrong assumption !!! the respiration rate is random evry time step
 resp <- runif(prm_workers$NWorkers,prm_air$RespRate[3],prm_air$RespRate[5]) #[m3/min]
 
 ########## day time loop ######################  
    for (ind in 1:max(MyAir$t_ind))### A modifier !!!!!! #### à mettre par jour
{
      # ind=73
      # ind=1
      print(ind)
      Cd = MyAir[MyAir$t_ind==(ind-1),2:(1+length(prm_air$Droplet_class))]
      # probability talking , talking loud, just breathing, coughing at time ind 
      p_emission <- sample(c(1,2,3,4), size = prm_workers$NWorkers, prob= c(0.25,0.25,0.25,0.25), replace = T)
      
      # 1 - What is happening at time ind ? [[1]] cont_mask; [[2]] cont_no_mask; [[3]] no_cont_mask; [[4]] no_cont_no_mask
      W_Loc_N_mask <- Who_is(subset(MyWorkers,t_ind == ind),prm_plant,prm_workers$NWorkers)
      
      # 2 - TRANSFERS ----------------------------------------------------------
      # 2.1 FROM AIR to SURFACES : Droplet sedimention of the surfaces
      dsed = S_rooms%*%Vsed*60    # (m3/min)
      #  ----------------------------------------------------------------------
      # 2.2 FROM AIR to WORKERS # !!!!ATTENTION, FAUTE ici le masque n'absorbe pas les gouttes :::)
      Resp_inh = ((W_Loc_N_mask[[1]]+W_Loc_N_mask[[3]])*0.1 + (W_Loc_N_mask[[2]]+W_Loc_N_mask[[4]]))*resp
      
      # list[[7]] for each room, number of droplets inhaled in each droplet class per operators
      W_droplet_Expos <- lapply(seq(1:N_rooms), 
      function (x) return(Resp_inh[,x]*t(matrix(rep(as.matrix(Cd[x,]),prm_workers$NWorkers),ncol = prm_workers$NWorkers))))
      # total Number of droplet inhaled in each rooms for each droplet classes --> Air module balance equation
      dinhale <- t(matrix(unlist(lapply(seq(1:N_rooms), 
                        function (x) return(colSums(W_droplet_Expos[[x]])))),ncol = N_rooms))

      # Cumulative (during the day) exposition of all workers to all classes --> dose response model at the end of the day
      W_droplet_Expos_cumul = W_droplet_Expos_cumul+Reduce("+",W_droplet_Expos) 
      #  ----------------------------------------------------------------------      
      # 2.3 FROM WORKERS to AIR #
      # # probability talking , talking loud, just breathing, coughing at time ind 
      # p_emission <- sample(c(1,2,3,4), size = prm_workers$NWorkers, prob= c(0.2,0.1,0.6,0.1), replace = T)
      # Number of contaminated with mask in rooms
      # total number of droplets exhaled per employee (All employee)
      dTot_exh <- prm_air$Cd_exp[p_emission,] * resp    # [d/min]
      # number of drolets exhaled of all contaminated employees in each room per droplet classes 
      # Employees with masks
      N_CMR = (1-c(0.9,0.9,prm_workers$Mask_Eff))*t(matrix(unlist(lapply(seq(1:N_rooms), 
              function (x) return(colSums(W_Loc_N_mask[[1]][,x]*dTot_exh)))),nrow = length (prm_air$Droplet_class)))
      # Employees without masks
      N_CNoMR = t(matrix(unlist(lapply(seq(1:N_rooms), 
                                     function (x) return(colSums(W_Loc_N_mask[[2]][,x]*dTot_exh)))),nrow = length (prm_air$Droplet_class)))
      
      dexhale <-N_CMR+N_CNoMR # number of droplets exhaled and aerosolized  [d]
      #  ----------------------------------------------------------------------
      # 2.4 Airflow rate of air renewal m3/min
      V_renew <- c(prm_plant$Air_renewal,
                   unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$Air_renewal)))))/60
      #  ----------------------------------------------------------------------
      
      MyAir[MyAir$t_ind==ind,2:(1+length(prm_air$Droplet_class))] = MyAir[MyAir$t_ind==(ind-1),2:(1+length(prm_air$Droplet_class))]+
        (dexhale*Method_calc -  dinhale-(V_renew + dsed)*Cd)*prm_time$Step/V_rooms
      
      
      # MyAir[MyAir$t_ind==ind,2:(1+length(prm_air$Droplet_class))] <- MyAir[MyAir$t_ind==(ind-1),2:(1+length(prm_air$Droplet_class))] + 
      #                                                     f_dCd(MyAir[MyAir$t_ind==(ind-1),2:(1+length(prm_air$Droplet_class))],
      #                                                           S_rooms,V_rooms,prm_time,MyWorkers,prm_plant,prm_workers, ind)
      #  
    }
 return(MyAir)
########## End of day time loop ######################
 }
MyAir <- f_Module_Master(prm_plant, prm_air, prm_time, prm_workers)
 # plot on same grid, each series colored differently -- 
 # good if the series have same scale
 MyAir[MyAir$AIR_ID=='Arrival gate',2:6]<-0
 ggplot(MyAir, aes(t_ind,d01)) + geom_point(aes(colour = AIR_ID))
 ggplot(MyAir, aes(t_ind,d02)) + geom_point(aes(colour = AIR_ID))
 ggplot(MyAir, aes(t_ind,d03)) + geom_point(aes(colour = AIR_ID))
 ggplot(MyAir, aes(t_ind,d04)) + geom_point(aes(colour = AIR_ID))
 
 
 # Number of contaminated employees in the rooms
  # Number of non-contaminated employees in the rooms 
 


 
 



Delta_Cd < -f_AirModuleCalc(V_rooms,V_renew,S_rooms)

Cd[i+1] = Cd[i]+Delta_Cd*dt/V_rooms # Number of droplets (m-3)


