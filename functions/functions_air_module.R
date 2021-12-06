library(ggplot2)
#### FOR THE TESTS !!!!!! 
prm_plant = Parms_Plant
prm_air = Parms_Air
prm_time = Parms_Time
AIR_ID <- c(prm_plant$label,
            unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$label)))))

MyWorkers$W_location <- sample(AIR_ID,
                  size = Parms_Workers$NWorkers, replace = T,
                  prob = c(0.7,0.025,0.025,0.05,0.05,0.05,0.1))

status<-c("contaminated","not contaminated")
MyWorkers$W_status <- sample(status,
                               size = Parms_Workers$NWorkers, replace = T,
                               prob = c(0.3,0.7))
MyAir[MyAir$t_ind==0,2:(1+length(prm_air$Droplet_class))] = matrix(1e3,7,6)*Method_calc
Method_calc <<- f_Air_Criteria_Calc(Parms_Plant,Parms_Air)


ind = 1
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


 f_dCd<- function (Cd,S_rooms,V_rooms,prm_time,MyWorkers,prm_plant,ind){
  # Calaculate the droplet concentration variation in the air of the rooms at time step dt
  # Only contaminated droplet are modeled
   # Impact of air renewal on the droplet concentration in the air, expressed as a dilution  (Y=1-V_renew/V_rooms)
  # dAir_renewal = matrix(rep(V_rooms-V_renew*prm_time$Step *60,ncol(Vsed)),ncol = ncol(Vsed)) # (m3)
   
  # Droplet sedimention of the surfaces
  dsed = S_rooms%*%Vsed*60    # (m3/min)
  # contaminated droplets emission by contaminated workers  
  NEmployees <- NW_inRoom(MyWorkers,prm_plant,ind) # NEmployees[[1]] (contaminated (sick&symptomatique)) NEmployees[[3]] (non contaminated)
  
  dexhale <- as.matrix(NEmployees[[1]])%*%prm_air$Cd_exp[1,] * prm_air$RespRate[3]/60     # [nv/m3/min]
  # contaminated droplets absortion by contaminated workers  
  dinhale <- matrix(rep(as.matrix(NEmployees[[2]]+NEmployees[[1]]) %*% prm_air$RespRate[3]/60,ncol(Vsed)),ncol = ncol(Vsed))  # m3/min

  # min?_loop(min in 0 :dt :  1440){ 
  #   ind <- f_convertTime()
  
  return((dexhale*Method_calc - (V_renew + dsed + dinhale)*Cd)*prm_time$Step/V_rooms)

 }
 

# function f_Sedimentation(...)
# 
# return(Ved)

for (ind in 1:(1440/prm_time$Step)){
 print(ind)
  
  MyAir[MyAir$t_ind==ind,2:(1+length(prm_air$Droplet_class))] <- MyAir[MyAir$t_ind==(ind-1),2:(1+length(prm_air$Droplet_class))] + 
                                                      f_dCd(MyAir[MyAir$t_ind==(ind-1),2:(1+length(prm_air$Droplet_class))],
                                                            S_rooms,V_rooms,prm_time,MyWorkers,prm_plant,ind)
   

  
}
 
 # plot on same grid, each series colored differently -- 
 # good if the series have same scale
 ggplot(MyAir[1:(288*7),], aes(t_ind,d03)) + geom_line(aes(colour = AIR_ID))
 
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
              unname(unlist(lapply(prm_plant$Spaces, function (x) return(x$Air_renewal)))))/3600
 
 



Delta_Cd < -f_AirModuleCalc(V_rooms,V_renew,S_rooms)

Cd[i+1] = Cd[i]+Delta_Cd*dt/V_rooms # Number of droplets (m-3)



txt1 <- "global variable"
my_function <- function(...) {
  # txt = "fantastic"
  paste("R is", txt1)
  print(V_renew)
  print(V_rooms)
  
}

my_function()

txt # print txt 

