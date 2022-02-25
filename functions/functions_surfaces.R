f_initSurfaces <- function(
  ## Function allowing to initialize (EVERY DAY) the agent surfaces at the beginning of the day
  #### INPUT
  P, ## (matrix) the matrix corresponding to the details of each plant tile (output of the function f_createPlant())
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
  ## total number of surface Agents
  NS <- nrow(P) * ncol(P)
  
  ## unique ID of each surface tile
  expand.grid(str_pad(1:nrow(P), width = 2, pad = "0"), ## X coordinates
              str_pad(1:ncol(P), width = 2, pad = "0") ## Y coordinates
              ) %>%
    as.data.frame(.) %>%
    `colnames<-`(., c("coordX", "coordY")) %>%
    apply(., 1, function(id) {
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
    tibble::add_column(.,
                       location = location,
                       viral_load = 0, ## the viral load (virion / infectious virus) present on the portion
                       ) -> S
  
  return(S)
  #### END OF FUNCTION
}
