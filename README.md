SACADA
========

## Contents
- [General presentation](#general-presentation)
- [Updates](#updates)
  * [2021-10-13](#2021-10-13)
  * [2021-10-08](#2021-10-08)

## General presentation
Agent-based model (ABM) for modelling the SARS-CoV-2 transmission in food facilities

## Updates

#### 2021-10-18


********************************************************************************

#### 2021-10-13

* **Model parameters**
  * **Food portions**:
    * Probability of the different types of food portions [*modification*]: only two probabilities to take into account: the portions with possible contact and the loss portion.
    
* **Functions**
  * **f_initWorkers()** [*modification*]: output data frame **W** of the function with an additional column 
    * **$W_active** (logical T/F) indicating that a worker is in the working phase or not. This column is initialized as NA.
    * **t** (integer) time index. Initialized at 0
  * **f_initFood()** [*modification*]:output data frame with additional and modified columns 
    * **$FP_contact** logical (T/F) (possible contact or not)
    * **$FP_loss** logical T/F (possible contact or not)

********************************************************************************

#### 2021-10-08

* **Functions**
  * **f_addObject()** [*modification*]: function to add an object inside the plant. Additional argument 'label': label of the added object, assigned automatically for the tiles corresponding to the objects (e.g.: "conveyor", "table", "equipment"...)
  
  * **f_generateWorkspace()** [*new*]: function to create automatically workspaces along the conveyor (1x1m tables on the one and the other side of the conveyor). The workers will be positioned afterwards on the left-hand side of the tables.
  
********************************************************************************
