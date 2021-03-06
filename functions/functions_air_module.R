library(ggplot2)
#### FOR THE TESTS !!!!!! 
prm_plant = Parms_Plant
prm_air = Parms_Air
prm_time = Parms_Time
prm_workers = Parms_Workers
AIR_ID <- c(prm_plant$label,
            unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$label)))))

MyWorkers <- subset(MyWorkers,Week<2)
MyWorkers <- dplyr::arrange(MyWorkers, t_ind, W_ID)

MyWorkers$W_location <- rep(sample(AIR_ID,
                  size = prm_workers$NWorkers, replace = T,
                  prob = c(0.8,0.025,0.05,0.025,0.025,0.025,0.05)),2016)
# Pour le test, les op�rateurs arrivent vers 6h, prennent une pause � 12h, travaillent jusqu' � 22h
MyWorkers$W_location[MyWorkers$Hour<6]=NA
MyWorkers$W_location[MyWorkers$Hour>12 & MyWorkers$Hour<15]=NA
MyWorkers$W_location[MyWorkers$Hour>22]=NA
MyWorkers$W_location[MyWorkers$Day>5]=NA

status<-c("contaminated","not contaminated")
MyWorkers$W_status <- rep(sample(status,
                               size = Parms_Workers$NWorkers, replace = T,
                               prob = c(0.5,0.5)) ,  2016)
status<-c("mask","no mask")
MyWorkers$W_mask<- rep(sample(status,
                             size = Parms_Workers$NWorkers, replace = T,
                             prob = c(0.5,0.5)), 2016)

Method_calc <<- f_Air_Criteria_Calc(Parms_Plant,Parms_Air)
MyAir <- f_initAir(prm = Parms_Plant, prm_time = Parms_Time, prm_air = Parms_Air)

MyAir[MyAir$t_ind==0,2:(1+length(prm_air$Droplet_class))] = matrix(0,7,6)*(Method_calc)
MyAir <- subset(MyAir,t_ind<2016)


ind = 500
#########################################

# Number of contaminated and non containayted workers in the rooms at time ind
NW_inRoom <- function(MyWorkers,prm_plant,ind){
  # Number of contaminated and non contaminated workers in the rooms
  i=1
  Cont_W =c(nrow(subset(MyWorkers,W_status =="contaminated" & t_ind == ind & W_location == prm_plant$label))
            ,rep(0,length(prm_plant$Spaces)))
  Non_Cont_W =c(nrow(subset(MyWorkers,W_status =="not contaminated" & t_ind == ind & W_location == prm_plant$label))
                ,rep(0,length(prm_plant$Spaces)))
  
  for(x in prm_plant$Spaces){
    Cont_W[i+1] =  nrow(subset(MyWorkers,W_status =="contaminated" & t_ind == ind & W_location==x$label))
    Non_Cont_W[i+1] =  nrow(subset(MyWorkers,W_status =="not contaminated" & t_ind == ind & W_location==x$label))
    i=i+1
  }
  return(list(Cont_W,Non_Cont_W))
}


Who_is <- function(Sub_MyWorkers,prm_plant,NWorkers,ind){
# To sum up who is where, how is he and is he wearing a mask
   # For the test
  ind=536
  Sub_MyWorkers <-subset(MyWorkers,t_ind == ind)
  NWorkers = prm_workers$NWorkers
   # Number of contaminated and non contaminated workers in the rooms
  i=1
  Cont_mask <- matrix(NA,NWorkers,7) # NWORKER NROOMS 
  Cont_nomask <- matrix(NA,NWorkers,7)
  Non_Cont_mask <- matrix(NA,NWorkers,7)
  Non_Cont_no_mask <- matrix(NA,NWorkers,7)
  
  Cont_mask[,1] <- Sub_MyWorkers$W_status =="contaminated" & Sub_MyWorkers$t_ind == ind & 
    Sub_MyWorkers$W_location == prm_plant$label & Sub_MyWorkers$W_mask=="mask"

  Cont_nomask[,1]  =Sub_MyWorkers$W_status =="contaminated" & Sub_MyWorkers$t_ind == ind &
    Sub_MyWorkers$W_location == prm_plant$label & Sub_MyWorkers$W_mask=="no mask"

  Non_Cont_mask[,1]  = Sub_MyWorkers$W_status =="not contaminated" & Sub_MyWorkers$t_ind == ind &
    Sub_MyWorkers$W_location == prm_plant$label & Sub_MyWorkers$W_mask=="mask"

  Non_Cont_no_mask[,1]  = Sub_MyWorkers$W_status =="not contaminated" & Sub_MyWorkers$t_ind == ind &
    Sub_MyWorkers$W_location == prm_plant$label & Sub_MyWorkers$W_mask=="no mask"
  
  for(x in prm_plant$Spaces){
    Cont_mask[,i+1] =  Sub_MyWorkers$W_status =="contaminated" & Sub_MyWorkers$t_ind == ind & 
      Sub_MyWorkers$W_location==x$label & Sub_MyWorkers$W_mask=="mask"
    Cont_nomask[,i+1] =  Sub_MyWorkers$W_status =="contaminated" & Sub_MyWorkers$t_ind == ind &
      Sub_MyWorkers$W_location==x$label & Sub_MyWorkers$W_mask=="no mask"
    Non_Cont_mask[,i+1] =  Sub_MyWorkers$W_status =="not contaminated" & Sub_MyWorkers$t_ind == ind &
      Sub_MyWorkers$W_location==x$label & Sub_MyWorkers$W_mask=="mask"
    Non_Cont_no_mask[,i+1] =  Sub_MyWorkers$W_status =="not contaminated" & Sub_MyWorkers$t_ind == ind &
      Sub_MyWorkers$W_location==x$label & Sub_MyWorkers$W_mask=="no mask"
    i=i+1
  }
  return(list(Cont_mask,Cont_nomask,Non_Cont_mask,Non_Cont_no_mask))
}



 f_dCd<- function (Cd,S_rooms,V_rooms,prm_time,MyWorkers,prm_plant,prm_workers,ind){
  # Calculate the droplet concentration variation in the air of the rooms at time step dt
  # Only contaminated droplet are modeled
   # Impact of air renewal on the droplet concentration in the air, expressed as a dilution  (Y=1-V_renew/V_rooms)
  # dAir_renewal = matrix(rep(V_rooms-V_renew*prm_time$Step *60,ncol(Vsed)),ncol = ncol(Vsed)) # (m3)
   
  # Droplet sedimention of the surfaces
  dsed = S_rooms%*%Vsed*60    # (m3/min)
  # contaminated droplets emission by contaminated workers  
  NEmployees <- NW_inRoom(MyWorkers,prm_plant,ind) # NEmployees[[1]] (contaminated (sick&symptomatique)) NEmployees[[3]] (non contaminated)
  
  dexhale <- as.matrix(NEmployees[[1]])%*%prm_air$Cd_exp[1,] * prm_air$RespRate[3]     # [d/m3/min]
  # contaminated droplets absortion by contaminated workers  
  dinhale <- matrix(rep(as.matrix(NEmployees[[2]]+NEmployees[[1]]) %*% prm_air$RespRate[3],ncol(Vsed)),ncol = ncol(Vsed))  # m3/min


  return((dexhale*Method_calc - (V_renew + dsed + dinhale)*Cd)*prm_time$Step/V_rooms)

 }
 

# function f_Sedimentation(...)
# 
# return(Ved)

for (ind in 1:max(MyAir$t_ind)){### A modifier !!!!!! #### � mettre par jour
 print(ind)
  
  
  # W_condition  <- Who_is(subset(MyWorkers,t_ind == ind),prm_plant,prm_workers$NWorkers, ind)
  
  
  
  MyAir[MyAir$t_ind==ind,2:(1+length(prm_air$Droplet_class))] <- MyAir[MyAir$t_ind==(ind-1),2:(1+length(prm_air$Droplet_class))] + 
                                                      f_dCd(MyAir[MyAir$t_ind==(ind-1),2:(1+length(prm_air$Droplet_class))],
                                                            S_rooms,V_rooms,prm_time,MyWorkers,prm_plant,prm_workers, ind)
   
}
 
 # plot on same grid, each series colored differently -- 
 # good if the series have same scale
 ggplot(MyAir, aes(t_ind,d01)) + geom_point(aes(colour = AIR_ID))
 ggplot(MyAir, aes(t_ind,d02)) + geom_point(aes(colour = AIR_ID))
 ggplot(MyAir, aes(t_ind,d03)) + geom_point(aes(colour = AIR_ID))
 ggplot(MyAir, aes(t_ind,d04)) + geom_point(aes(colour = AIR_ID))
 
 
 # Number of contaminated employees in the rooms
  # Number of non-contaminated employees in the rooms 
 

 # Surfaces of the room (m2)
 S_rooms <- c(prm_plant$dim.X*prm_plant$dim.Y,
              unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$dim.X*x$dim.Y)))))
 
 # Volume of the room (m3)
 V_rooms <- c(prm_plant$dim.X*prm_plant$dim.Y*prm_plant$dim.Z,
              unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$dim.X*x$dim.Y*x$dim.Z)))))
 # Airflow rate of air renewal m3/s
 V_renew <- c(prm_plant$Air_renewal,
              unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$Air_renewal)))))/60
 
 



Delta_Cd < -f_AirModuleCalc(V_rooms,V_renew,S_rooms)

Cd[i+1] = Cd[i]+Delta_Cd*dt/V_rooms # Number of droplets (m-3)


