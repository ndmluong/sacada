##### en cours: f_initFood() FUNCTION TO INITIALISE A SET OF FOOD PORTIONS #####
f_initFood <- function(
  ## Function allowing to initialize a set of food portions at a given day
  ## depending on food parameters provided as input argument.
  #### INPUT
  prm_food, ## (list) parameters associated with all attributes of the food portions (check Parms_Food.R)
  prm_time, ## (list) parameters associated with the simulation time (check Parms_Time.R)
  day ## the considered day
  #### OUTPUT
  ## FP (dataframe): the initialized food portions
  ##  - $FP_ID (character/string): the ID of each food portion (e.g.: "B_01_025_0001","FP0002",...)
  ##  - $coordX (integer): coordinates of the food portions in the plant (initialized as NA)
  ##  - $coordY (integer): coordinates of the food portions in the plant (initialized as NA)
  ##  - $location (character): location of the food portions ("Arrival gate", "Waste area"...) (initialized as NA)
  ##  - $viral_load (numeric): total viral quantity (virions) present on the food portions (initialized as NA)
) {
  #### BEGIN OF FUNCTION
  ## the type of meat
  meat <- prm_food$meat
  
  ### unique ID of each meat portion
  expand.grid(str_pad(day, width = 2, pad = "0"), # day number
              str_pad(1:prm_food$Ncarcass_daily[meat], width = 3, pad = "0"), # carcass number
              str_pad(1:(prm_food$Ncarcass_daily[meat] / prm_food$CSU[meat]), width = 3, pad = "0")) %>% # pieces number (per carcass)
    as.data.frame(.) %>%
    `colnames<-`(., c("day", "carcass", "portion")) %>%
    apply(., 1, function(id) {
      paste(id["day"], id["carcass"], id["portion"], sep = "_")}) %>%
    paste(str_sub(Parms_Food$meat, 1, 1) %>% str_to_upper(.), ., sep = "_") %>%
    sort(.) -> unique_ID
  
  ## All the time indices associated with the given day
  seq(f_convertTime("time2ind", dt=prm_time$Step, D=day, H=0, M=0),
      f_convertTime("time2ind", dt=prm_time$Step, D=day+1, H=0, M=0) - 1) -> unique_t_ind
  
  ## preparing the initialized data frame for all food portions at the given day
  expand.grid(unique_ID, unique_t_ind) %>% ## combine the time indices and the food portion IDs
    as.data.frame(.) %>%
    `colnames<-`(., c("FP_ID", "t_ind")) %>%
    arrange(., t_ind, FP_ID) %>% ## arrange by time indices then portion IDs
    tibble::add_column(coordX = NA, ## X coordinates of the portion
                       coordY = NA, ## Y coordinates of the portion
                       location = NA, ## location name ("office", "conveyor1", "conveyor2"...)
                       contact = NA, ## (logical) indicates if the portion could be in contact with other surfaces or not
                       viral_load = NA  ## the viral load (virion / infectious virus) present on the portion
                       ) -> FP
  
  return(FP)
  #### END OF FUNCTION
}
