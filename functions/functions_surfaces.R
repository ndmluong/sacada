##### f_initSurfaces #####
f_initSurfaces <- function(
  ## Function allowing to initialize (at the beginning of every day) the agent surfaces involving in the SARS-CoV-2 infection
  #### INPUT
  P, ## (matrix) the matrix corresponding to the details of each plant tile (output of the function f_createPlant())
  prm_plant, ## (list) the parameters associated with the timetable (check the script "parameters_plant.R")
  prm_time, ## (list) the parameters associated with the timetable (check the script "parameters_time.R")
  day ## the considered day
  #### OUTPUT
  ## S (dataframe): the initialised workers
  ##  - $S_ID (character/string): the ID of each surface unit ("W001","W002",...)
  ##  - $coordX (integer): the X coordinate of the surface agent
  ##  - $coordY (integer): the Y coordinate of the surface agent
  ##  - $location (character): the location of the surface agent
  ##  - $S_Nv (numeric): viral quantity (log CFU)
) {
  #### BEGIN OF FUNCTION
  writeLines(paste("\n***** Initializing surfaces (beginning of the day ", day, ") *****", sep=""))
  
  ## The surfaces involving in the infection (objects)
  sapply(prm_plant$Objects, function(x) return(x$label)) %>%
    as.vector() -> S_selected
  
  ## unique ID of each surface tile
  expand.grid(str_pad(1:nrow(P), width = 2, pad = "0"), ## X coordinates crossed with the Y coordinates
              str_pad(1:ncol(P), width = 2, pad = "0") ## Y coordinates
              ) %>%
    as.data.frame(.) %>%
    `colnames<-`(., c("coordX", "coordY")) %>%
    apply(., 1, function(id) { ## create ID with the format S_xx_yy
      paste("S", id["coordX"], id["coordY"], sep = "_")}) %>%
    sort(.) -> unique_ID
  
  ## All the time indices associated with the given day
  seq(f_convertTime("time2ind", dt=prm_time$Step, D=day, H=0, M=0),
      f_convertTime("time2ind", dt=prm_time$Step, D=day+1, H=0, M=0) - 1) -> unique_t_ind
  
  ## preparing the initialized data frame for all surfaces at the given day
  expand.grid(unique_ID, unique_t_ind) %>% ## combine the time indices and the surfaces IDs
    as.data.frame(.) %>%
    `colnames<-`(., c("S_ID", "t_ind")) %>% 
    dplyr::arrange(., t_ind, S_ID) %>%
    dplyr::mutate(.,
                  coordX = as.numeric(str_sub(S_ID, start = 3, end = 4)), ## X coordinates from the ID
                  coordY = as.numeric(str_sub(S_ID, start = 6, end = 7)) ## Y coordinates from the ID)
                  ) -> S
  
  S %>% apply(., 1, function(a) {
      P[ as.numeric(a["coordX"]) , as.numeric(a["coordY"])]
      }) -> location
  
  S %>%
    tibble::add_column(location = location,
                       RNA_load = 0, ## the viral load (number of RNA copies) present on the surfaces
                       ) %>%
    ## retain only the surfaces involving in the SARS-CoV-2 transmission
    dplyr::filter(., location %in% S_selected) -> S
  
  S_ID <<- sort(unique(S$S_ID))
    
  return(S)
  #### END OF FUNCTION
}


##### f_transfers #####
f_transfers <- function(
    ## Function to calculate the transfers between the inert surfaces and food portions at a given time point
  S, ## data frame (MySurfaces) with all time indices
  FP, ## 
  m2_tiles_ti, ## named vector (same length as the total number of considered inert surfaces)
  ti, ## time index
  prm_surfaces
  ## OUTPUT
  ## list of S and FP updated
) {
  
  ## Check if there is any food portions which are present at the indices ti and ti+1 (in the middle of the processing cut)
  ## if it is the case: by default, the RNA load on these portions are the same from the time ti to ti+1, regardless their coordinates
  FP_ID_2ti <- sort(intersect(FP$FP_ID[FP$t_ind == ti],                       
                              FP$FP_ID[FP$t_ind == ti+1]))
  
  FP$RNA_load[FP$t_ind == ti+1 & FP$FP_ID %in% FP_ID_2ti] = FP$RNA_load[FP$t_ind == ti & FP$FP_ID %in% FP_ID_2ti]
  
  ## for each tile Z in S_ID
  for (Z in as.vector(S_ID)) {
    ## TRANSFER FROM SURFACES TO FOOD PORTIONS
    FP_ID_expoZ_ti <- sort(as.vector(sample(FP$FP_ID[FP$coords_ID == Z & FP$t_ind == ti],
                                            size = min(prm_surfaces$nFP_expo,
                                                       length(FP$FP_ID[FP$coords_ID == Z & FP$t_ind == ti])))))
    FP_ID_expoZ_tip1 <- sort(as.vector(sample(FP$FP_ID[FP$coords_ID == Z & FP$t_ind == ti+1],
                                              size = min(prm_surfaces$nFP_expo,
                                                         length(FP$FP_ID[FP$coords_ID == Z & FP$t_ind == ti+1])))))

    S2F <- (m2_tiles_ti[[Z]] * prm_surfaces$inert_prop) * prm_surfaces$transfer_S2F / prm_surfaces$nFP_expo

    FP$RNA_load[FP$t_ind == ti+1 & FP$coords_ID == Z & FP$FP_ID %in% FP_ID_expoZ_tip1] =
      FP$RNA_load[FP$t_ind == ti & FP$FP_ID %in% FP_ID_expoZ_tip1] +
      S2F

    ## TRANSFER FROM FOOD PORTIONS TO SURFACES
    F2S <- m2_tiles_ti[[Z]] * (1-prm_surfaces$inert_prop) * prm_surfaces$transfer_F2S

    S$RNA_load[S$S_ID == Z & S$t_ind == ti+1] = S$RNA_load[S$S_ID == Z & S$t_ind == ti] + F2S

    FP$RNA_load[FP$t_ind == ti+1 & FP$FP_ID %in% FP_ID_expoZ_ti] =
      FP$RNA_load[FP$t_ind == ti & FP$FP_ID %in% FP_ID_expoZ_ti] +
      m2_tiles_ti[[Z]] * (1-prm_surfaces$inert_prop) * (1 - prm_surfaces$transfer_F2S) / prm_surfaces$nFP_expo
  }

  return(list(S = S,
              FP = FP))
}

# ##### f_tiles2surfaces #####
# f_tiles2surfaces <- function(
#     S,
#     m2_tiles_ti,
#     ti
# ) {
#   for (Z in as.vector(S_ID)) {
#     S$RNA_load[S$S_ID == Z & S$t_ind == ti + 1] = S$RNA_load[S$S_ID == Z & S$t_ind == ti] + m2_tiles_ti[[Z]]
#   }
#   return(S)
# }
