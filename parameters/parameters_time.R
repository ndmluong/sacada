##### PARAMETERS FOR THE PROCESS #####
Parms_Time <- list(
  NDays = 21, ## total number of days during the entire process
  Step = 5, ## time step (in minutes)
  
  ## Parameters associated with the daily cutting process
  time_cut_begin = c("H"=5,"M"=0), ## begin of the daily cutting process 5:00 AM
  time_cut_end = c("H"=21,"M"=0) ## end of the daily cutting process 21:00 AM
)

# (Optional) Parameters with unchanged values that can be saved in global environment 
Step <<- Parms_Time$Step