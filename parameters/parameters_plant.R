##### USER DEFINED PARAMETERS FOR THE PLANT #####
Parms_Plant <- list(
  #### Plant
  dim.X = 26, ## (numeric - m) length of the processing plants (meter)
  dim.Y = 21, ## (numeric - m) width of the processing plants (meter)
  dim.Z = 5,  ## (numeric - m) height of the room 
  label = "Cutting Room",
  Air_Cond_Type = "AHU", #Air Handing Unit
  Air_renewal = 2400, # TO MODIFY !!!!!!! (m3/h) (2400 m3/h for 40 persons approximately)
  AirflowRate = 90000, # 30Vol/h TO MODIFY !!!!!!! (m3/h)
  
  ######### SPACES (begin)
  Spaces = list(
    ## Entry hall
    entry = list( 
      label = "Entry hall",
      dim.X = 5, # dimensions (meter)
      dim.Y = 4, # dimensions (meter)
      dim.Z = 3, # dimensions (meter)
      pos.X = 0, # (numeric from 0 to 1) Position of regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
      pos.Y = 1, # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: bottom, 0.5: middle, 1:top)
      intdoor.side = "bottom",
      extdoor.side = "left",
      Air_Cond_Type = "CMV", # Controlled Mechanical Ventilation
      Air_renewal = 90, # TO MODIFY !!!!!!! (m3/h)
      AirflowRate = 90 # TO MODIFY !!!!!!! (m3/h)
    )
    ,
    ## WC
    wc = list( 
      label = "W.C.",
      dim.X = 2, # dimensions (meter)
      dim.Y = 2, # dimensions (meter)
      dim.Z = 2.5, # dimensions (meter)
      pos.X = 1, # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
      pos.Y = 1, # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: bottom, 0.5: middle, 1:top)
      intdoor.side = "left",
      extdoor.side = NA, 
      Air_Cond_Type = "CMV", # Controlled Mecanical Ventilation 
      Air_renewal = 30, #  (m3/h)
      AirflowRate = 30 #   (m3/h)
    )
    ,
    ## Cooling area
    cool = list( 
      label = "Cooling area",
      dim.X = 5, # dimensions (meter)
      dim.Y = 5, # dimensions (meter)
      dim.Z = 3, # dimensions (meter)
      pos.X = 1, # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
      pos.Y = 0, # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: bottom, 0.5: middle, 1:top)
      intdoor.side = "left",
      extdoor.side = NA,
      Air_Cond_Type = "DEC", # Direct Expansion Coil
      Air_renewal = 120, # TO MODIFY !!!!!!! (m3/h)
      AirflowRate = 500 # TO MODIFY !!!!!!! (m3/h)
    )
    ,
    ## Gate (arrival of meat products)
    gate = list(
      label = "Arrival gate",
      dim.X = 2, # dimensions (meter)
      dim.Y = 2, # dimensions (meter)
      dim.Z = 3, # dimensions (meter)
      pos.X = 0, # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
      pos.Y = 0.4, # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: bottom, 0.5: middle, 1:top)
      intdoor.side = "right",
      extdoor.side = "left",
      Air_Cond_Type = "DEC", # Direct Expansion Coil
      Air_renewal = 30, # TO MODIFY !!!!!!! (m3/h)
      AirflowRate = 200 # TO MODIFY !!!!!!! (m3/h)
    )
    ,
    ## Waste area
    waste = list(
      label = "Waste area",
      dim.X = 2, # dimensions
      dim.Y = 3, # dimensions
      dim.Z = 3, # dimensions (meter)
      pos.X = 0.7, # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
      pos.Y = 1, # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: bottom, 0.5: middle, 1:top)
      intdoor.side = "bottom",
      extdoor.side = "top",
      Air_Cond_Type = "DEC", # Direct Expansion Coil
      Air_renewal = 120, # TO MODIFY !!!!!!! (m3/h)
      AirflowRate = 200 # TO MODIFY !!!!!!! (m3/h)
    )
    ,
    ## Office
    office = list(
      label = "Office",
      dim.X = 5, # dimensions
      dim.Y = 4, # dimensions
      dim.Z = 3, # dimensions (meter)
      pos.X = 1, # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
      pos.Y = 0.5, # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: bottom, 0.5: middle, 1:top)
      intdoor.side = "left",
      extdoor.side = NA,
      Air_Cond_Type = "CMV", # Controlled Mecanical Ventilation
      Air_renewal = 60, # TO MODIFY !!!!!!! (m3/h)
      AirflowRate = 60 # TO MODIFY !!!!!!! (m3/h)

    )
    ####### NOTE /!\ 
    ####### To add new spaces, uncomment and copy the following code section (INCLUDING THE FIRST COMMA). All fields are mandatory
    ##############################################################################################################################
    # ,
    # ## Other spaces
    # myspacename = list(
    #   label = ".....",
    #   dim.X = , # dimensions
    #   dim.Y = , # dimensions
    #   pos.X = , # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
    #   pos.Y = , # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: bottom, 0.5: middle, 1:top)
    #   intdoor.side = "top",
    #   extdoor.side = "bottom"
    # )
    ##############################################################################################################################
  ),
  ######### SPACES (end)
  
  
  
  
  ######### OBJECTS (begin)
  Objects = list(
    ## conveyor treadmill
    cvy = list(
      label = "Conveyor1",
      dim.X = 12, # dimensions
      dim.Y = 1, # dimensions
      pos.X = 0.2, # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
      pos.Y = 0.45 # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: bottom, 0.5: middle, 1:top)
    )
    ,
    ## New object
    cvy2 = list(
      label = "Conveyor2",
      dim.X = 10, # dimensions
      dim.Y = 1, # dimensions
      pos.X = 0.25, # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
      pos.Y = 0.25 # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: bottom, 0.5: middle, 1:top)
    )
    ,
    ## 'Eplucheuse'
    epm1 = list(
      label = "Equipment 1",
      dim.X = 1, # dimensions
      dim.Y = 1, # dimensions
      pos.X = 0.75, # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
      pos.Y = 0.35 # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: bottom, 0.5: middle, 1:top)
    )
    
    ####### NOTE /!\ 
    ####### To add new objects, uncomment and copy the following code section (INCLUDING THE FIRST COMMA) into the previous listing. All fields are mandatory
    ##############################################################################################################################
    # ,
    # ## New object
    # myobjectid = list(
    #   label = "......",
    #   dim.X = , # dimensions
    #   dim.Y = , # dimensions
    #   pos.X = , # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
    #   pos.Y =  # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: left, 0.5: middle, 1:right)
    # )
    ##############################################################################################################################
    
  )
  ######### OBJECTS (end)
)

# (Optional) Parameters with unchanged values that can be saved in global environment 
# Names (labels) of all spaces including the cutting room
Spaces_label <<- c(Parms_Plant$label,
                   as.vector(unlist(lapply(Parms_Plant$Spaces, FUN = function(x) return(x$label)))))