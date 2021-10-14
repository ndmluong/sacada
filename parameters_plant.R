##### USER DEFINED PARAMETERS FOR THE PLANT #####
Parms_Plant <- list(
  #### Plant
  dim.X = 52, ## (numeric) length of the processing plants (meter)
  dim.Y = 30, ## (numeric) width of the processing plants (meter)
  
  ######### SPACES (begin)
  Spaces = list(
    ## Entry hall
    entry = list( 
      label = "Entry hall",
      dim.X = 5, # dimensions including the border wall
      dim.Y = 5, # dimensions including the border wall
      pos.X = 0, # (numeric from 0 to 1) Position of regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
      pos.Y = 0, # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: bottom, 0.5: middle, 1:top)
      intdoor.side = "top",
      extdoor.side = "left"
    )
    ,
    ## WC
    wc = list( 
      label = "W.C.",
      dim.X = 4, # dimensions including the border wall
      dim.Y = 4, # dimensions including the border wall
      pos.X = 1, # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
      pos.Y = 0, # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: bottom, 0.5: middle, 1:top)
      intdoor.side = "left",
      extdoor.side = "left"
    )
    ,
    ## Cooling area
    cool = list( 
      label = "Cooling area",
      dim.X = 6, # dimensions including the border wall
      dim.Y = 12, # dimensions including the border wall
      pos.X = 1, # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
      pos.Y = 0.4, # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: bottom, 0.5: middle, 1:top)
      intdoor.side = "left",
      extdoor.side = "left"
    )
    ,
    ## Gate (arrival of meat products)
    gate = list(
      label = "Arrival gate",
      dim.X = 6, # dimensions including the border wall
      dim.Y = 4, # dimensions including the border wall
      pos.X = 0, # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
      pos.Y = 0.4, # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: bottom, 0.5: middle, 1:top)
      intdoor.side = "right",
      extdoor.side = "left"
    )
    ,
    ## Waste area
    waste = list(
      label = "Waste area",
      dim.X = 4, # dimensions including the border wall
      dim.Y = 7, # dimensions including the border wall
      pos.X = 0.7, # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
      pos.Y = 0, # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: bottom, 0.5: middle, 1:top)
      intdoor.side = "top",
      extdoor.side = "bottom"
    )
    ####### NOTE /!\ 
    ####### To add new spaces, uncomment and copy the following code section (INCLUDING THE FIRST COMMA). All fields are mandatory
    ##############################################################################################################################
    # ,
    # ## Other spaces
    # myspacename = list(
    #   label = ".....",
    #   dim.X = , # dimensions including the border wall
    #   dim.Y = , # dimensions including the border wall
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
      label = "Conveyor",
      dim.X = 20, # dimensions including the border 
      dim.Y = 3, # dimensions including the border 
      pos.X = 0.2, # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
      pos.Y = 0.4 # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: left, 0.5: middle, 1:right)
    )
    ,
    ## 'Eplucheuse'
    epm1 = list(
      label = "Equipment 1",
      dim.X = 2, # dimensions including the border 
      dim.Y = 2, # dimensions including the border 
      pos.X = 0.8, # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
      pos.Y = 0.5 # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: left, 0.5: middle, 1:right)
    )
    
    ####### NOTE /!\ 
    ####### To add new objects, uncomment and copy the following code section (INCLUDING THE FIRST COMMA) into the previous listing. All fields are mandatory
    ##############################################################################################################################
    # ,
    # ## New object
    # myobjectid = list(
    #   label = "......",
    #   dim.X = , # dimensions including the border 
    #   dim.Y = , # dimensions including the border 
    #   pos.X = , # (numeric from 0 to 1) Position regarding the X axis of the plant (0: left, 0.5: middle, 1:right)
    #   pos.Y =  # (numeric from 0 to 1) Position regarding the Y axis of the plant (0: left, 0.5: middle, 1:right)
    # )
    ##############################################################################################################################
    
  )
  ######### OBJECTS (end)
)


