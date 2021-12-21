library(ggplot2)
library(stringr)
library(plotly)
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
                  prob = c(0.7,0.05,0.05,0.05,0.05,0.05,0.05)),2016)
# Pour le test, les opérateurs arrivent vers 6h, prennent une pause à 12h, travaillent jusqu' à 22h
MyWorkers$W_location[MyWorkers$Hour<6]='Home'
MyWorkers$W_location[MyWorkers$Hour>12 & MyWorkers$Hour<15]='Home'
MyWorkers$W_location[MyWorkers$Hour>22]='Home'
MyWorkers$W_location[MyWorkers$Day>5]='Home'

MyWorkers$W_location[is.na(MyWorkers$W_location)]<-"Home"

status<-c("contaminated","not contaminated")
MyWorkers$W_status <- rep(sample(status,
                               size = Parms_Workers$NWorkers, replace = T,
                               prob = c(0.02,0.98)) ,  2016*8+1)
status<-c("mask","no mask")
MyWorkers$W_mask<- rep(sample(status,
                             size = Parms_Workers$NWorkers, replace = T,
                             prob = c(0.5,0.05)), 2016)

Method_calc <<- f_Air_Criteria_Calc(Parms_Plant,Parms_Air)
MyAir <- f_initAir(prm = Parms_Plant, prm_time = Parms_Time, prm_air = Parms_Air)

MyAir[MyAir$t_ind==0,2:(1+length(prm_air$Droplet_class))] = matrix(0,7,4)*(Method_calc)
 MyAir <- subset(MyAir,t_ind<1440)


ind = 1
#########################################



f_Who_is <- function(Sub_MyWorkers,prm_plant,NWorkers,ind){
# To sum up who is where, how is he and is he wearing a mask
   # For the test
  # ind=536
  # Sub_MyWorkers <-subset(MyWorkers,t_ind == ind)
  # NWorkers = prm_workers$NWorkers
   # Number of contaminated and non contaminated workers in the rooms
  i=1

  Sub_MyWorkers$W_location[str_starts(Sub_MyWorkers$W_location,"WS")]<-prm_plant$label
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
 
f_Module_Master <- function (prm_plant, prm_air, prm_time, prm_workers, ind_min, ind_max){
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
 W_droplet_Expos_cumul = matrix(0,prm_workers$NWorkers,length(prm_air$Droplet_class))
 
 # Respiration rate of each employee ! Wrong assumption !!! the respiration rate is random evry time step
 resp <- runif(prm_workers$NWorkers,prm_air$RespRate[3],prm_air$RespRate[5]) #[m3/min]
 
 ########## day time loop ######################  
    for (ind in ind_min:ind_max)#max(MyAir$t_ind))### A modifier !!!!!! #### à mettre par jour
{
       # ind=73
      # ind=1
      # print(ind)
      Cd = MyAir[MyAir$t_ind==(ind),2:(1+length(prm_air$Droplet_class))]
      # probability talking , talking loud, just breathing, coughing at time ind 
      p_emission <- sample(c(1,2,3,4), size = prm_workers$NWorkers, prob= c(0.25,0.25,0.25,0.25), replace = T)
      
      # 1 - What is happening at time ind ? [[1]] cont_mask; [[2]] cont_no_mask; [[3]] no_cont_mask; [[4]] no_cont_no_mask
      W_Loc_N_mask <- f_Who_is(subset(MyWorkers,t_ind == ind),prm_plant,prm_workers$NWorkers)
      
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
      N_CMR = (1-prm_workers$Mask_Eff)*t(matrix(unlist(lapply(seq(1:N_rooms), 
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
      
      MyAir[MyAir$t_ind==ind+1,2:(1+length(prm_air$Droplet_class))] = Cd+
        (dexhale*Method_calc -  dinhale-(V_renew + dsed)*Cd)*prm_time$Step/V_rooms
      
      
      # MyAir[MyAir$t_ind==ind,2:(1+length(prm_air$Droplet_class))] <- MyAir[MyAir$t_ind==(ind-1),2:(1+length(prm_air$Droplet_class))] + 
      #                                                     f_dCd(MyAir[MyAir$t_ind==(ind-1),2:(1+length(prm_air$Droplet_class))],
      #                                                           S_rooms,V_rooms,prm_time,MyWorkers,prm_plant,prm_workers, ind)
      #  
    # print(W_droplet_Expos_cumul)
      }
 return(list(MyAir,W_droplet_Expos_cumul))
########## End of day time loop ######################
}
  Dose_tot<-matrix(0,prm_workers$NWorkers,max(MyWorkers$Day))
N_contaminated=0
# for (Day in unique(MyWorkers$Day-1) ){
MyWorkers$W_location[is.na(MyWorkers$W_location)]<-"home"
for (Day in 1:57 ){
  
  Dose_per_class1 <- matrix(0,100,1)
  Dose_per_class2 <- matrix(0,100,1)
  Dose_per_class3 <- matrix(0,100,1)
  Dose_per_class4 <- matrix(0,100,1)
  
  print(Day)
  ind_min <- 24*60/Parms_Time$Step*(Day-1)
  ind_max <- 24*60/Parms_Time$Step*(Day)-1
  
  N_contaminated[Day] = sum (MyWorkers$W_status[MyWorkers$t_ind==ind_min] =="contaminated")
  print(N_contaminated[Day])
  OUT <- f_Module_Master(prm_plant, prm_air, prm_time, prm_workers,ind_min,ind_max)
  
  MyAir <- OUT[[1]]
  Expocum <-OUT[[2]]
  Viral_Load <-1e10
  P=Viral_Load*prm_air$d_Vol
  
  ### VERIFIER LA DOSE PER CLASS: Parametre size en particulier
  Dose_per_class1[Expocum[,1]>0] <- rbinom(n = sum(Expocum[,1]>0),size= round(Expocum[Expocum[,1]>0,1]), prob = P[1])
  Dose_per_class2[Expocum[,2]>0] <- rbinom(n = sum(Expocum[,2]>0),size= round(Expocum[Expocum[,2]>0,2]), prob = P[2])
  Dose_per_class3[Expocum[,3]>0] <- rbinom(n = sum(Expocum[,3]>0),size= round(Expocum[Expocum[,3]>0,3]), prob = P[3])
  Dose_per_class4[Expocum[,4]>0] <- rbinom(n = sum(Expocum[,4]>0),size= round(Expocum[Expocum[,4]>0,4]), prob = P[4])
  
  Dose_tot[,Day]=Dose_per_class1+Dose_per_class2+Dose_per_class3+Dose_per_class4
  # Dose_per_class <- lapply(1:length(prm_air$Droplet_class), function (x) 
  #   return(rbinom(n = sum(Expocum[,x]>0),size= round(Expocum[Expocum[,x]>0,x]), prob = P[x])))
  # Dose_tot[,Day] <- Reduce("+",Dose_per_class[[1:4]])
  
  Risk=0
  Risk_W=0
  New_sicks=0  
  New_sicks_W=0
  
  Risk = 1-exp(-Dose_tot[,Day]/50)
  Risk_W=unique(MyWorkers$W_ID[Risk>0])
  New_sicks <- rbinom(n = sum(Risk>0), size = 1, prob = Risk[Risk>0])
  New_sicks_W <- Risk_W[New_sicks>0]
  
  print(New_sicks_W)
  
  MyWorkers$W_status[MyWorkers$W_ID==New_sicks_W & MyWorkers$t_ind>ind_max] <-"contaminated"
  print(ind_max)
}


# plot on same grid, each series colored differently -- 
 # good if the series have same scale
MyAir[MyAir$AIR_ID=='Waste area',2:6]<-0

 MyAir[MyAir$AIR_ID=='Arrival gate',2:6]<-0
 MyAir[MyAir$AIR_ID=='Entry hall',2:6]<-0
 MyAir[MyAir$AIR_ID=='Cooling area',2:6]<-0
 
 
 ggplot() + 
   geom_point(MyAir, mapping=aes(x=t_ind, y=d01), colour="red") +
   geom_point(MyAir, mapping=aes(x=t_ind, y=d02), colour="blue") + 
   geom_point(MyAir, mapping=aes(x=t_ind, y=d03), colour="green") + 
   geom_point(MyAir, mapping=aes(x=t_ind, y=d04), colour="orange") + 
   
      facet_grid(AIR_ID ~ .)
 
 ggplot(subset(MyAir,AIR_ID=="Cutting Room")) + 
   geom_point( mapping=aes(x=t_ind, y=d01), colour="red") +
   geom_point( mapping=aes(x=t_ind, y=d02), colour="blue") + 
   geom_point( mapping=aes(x=t_ind, y=d03), colour="green") + 
   geom_point( mapping=aes(x=t_ind, y=d04), colour="orange") -> g1
 
 ggplot(subset(MyAir,AIR_ID=="Cooling area")) + 
   geom_point( mapping=aes(x=t_ind, y=d01), colour="red") +
   geom_point( mapping=aes(x=t_ind, y=d02), colour="blue") + 
   geom_point( mapping=aes(x=t_ind, y=d03), colour="green") + 
   geom_point( mapping=aes(x=t_ind, y=d04), colour="orange") -> g2
 
 
ggplotly(g1)
ggplotly(g2)
 ggplot(subset(MyAir,AIR_ID=="Cutting Room"  & t_ind>250 & t_ind<400)) + 
   geom_line( mapping=aes(x=t_ind, y=d01), colour="red") +
   geom_line( mapping=aes(x=t_ind, y=d02), colour="blue") + 
   geom_line( mapping=aes(x=t_ind, y=d03), colour="green") + 
   geom_line( mapping=aes(x=t_ind, y=d04), colour="orange")   
   
   
 ggplot(MyAir, aes(t_ind,d01)) + geom_point(aes(colour = AIR_ID))
 
 ggplot(MyAir, aes(t_ind,d02)) + geom_point(aes(colour = AIR_ID))
 ggplot(MyAir, aes(t_ind,d03)) + geom_point(aes(colour = AIR_ID))
 ggplot(MyAir, aes(t_ind,d04)) + geom_point(aes(colour = AIR_ID))
 
 
 # Number of contaminated employees in the rooms
  # Number of non-contaminated employees in the rooms 
 


 
 



Delta_Cd < -f_AirModuleCalc(V_rooms,V_renew,S_rooms)

Cd[i+1] = Cd[i]+Delta_Cd*dt/V_rooms # Number of droplets (m-3)


