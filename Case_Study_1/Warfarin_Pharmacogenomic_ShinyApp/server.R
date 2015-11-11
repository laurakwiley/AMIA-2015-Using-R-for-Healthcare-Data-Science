
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output){
    library(dplyr)
    library(tidyr)
    
    input_model <- eventReactive(input$calc, {
      data.frame(Age = input$age,
                 Race = input$race,
                 VKORC1 = input$vkorc1,
                 CYP2C9 = input$cyp2c9,
                 On_Amiodarone = input$amiodarone,
                 On_Enzyme_Inducers = input$enzyme_inducers)
    })
    
    output$selectedvalues <- renderTable({input_model() %>% 
        gather(key = Variable, value = Selection)})
    
    
    output$warfarindose <- renderText({
      warfarin_model <- structure(list(term = c("intercept", "age_decades", "vkorc1_1639_ag", "vkorc1_1639_aa", "vkorc1_1639_unknown", "cyp2c9_1_2", "cyp2c9_1_3", "cyp2c9_2_2", "cyp2c9_2_3", "cyp2c9_3_3", "asian", "african_american", "missing_or_mixed_race", "amiodarone", "enzyme_inducers"), estimate = c(8.28092953788836, -0.283786900016807, -0.800068515352432, -1.56451454024173, -0.595825280765066, -0.465304224113711, -0.892056417892715, -1.08355935593238, -1.86263474384314, -2.49990281796991, -0.686065650288828, -0.0769652407947399, -0.341619674379834, -0.668291885082912, 0.539804214460087)), row.names = c(NA, -15L), class = "data.frame", .Names = c("term", "estimate"))
      
      predicted_dose <- input_model() %>% 
        mutate(intercept = 1,
               age_decades = as.numeric(substr(Age, 1, 1)),
               vkorc1_1639_ag = ifelse(VKORC1 == "G/A", 1, 0),
               vkorc1_1639_aa = ifelse(VKORC1 == "A/A", 1, 0),
               vkorc1_1639_unknown = ifelse(VKORC1 == "Unknown", 1, 0),
               cyp2c9_1_2 = ifelse(CYP2C9 == "*1/*2", 1, 0),
               cyp2c9_1_3 = ifelse(CYP2C9 == "*1/*3", 1, 0),
               cyp2c9_2_2 = ifelse(CYP2C9 == "*2/*2", 1, 0), 
               cyp2c9_2_3 = ifelse(CYP2C9 == "*2/*3", 1, 0), 
               cyp2c9_3_3 = ifelse(CYP2C9 == "*3/*3", 1, 0), 
               asian = ifelse(Race == "Asian", 1, 0),
               african_american = ifelse(Race == "African American", 1, 0),
               missing_or_mixed_race = ifelse(Race == "Other or Unknown", 1, 0), 
               amiodarone = ifelse(On_Amiodarone, 1, 0),
               enzyme_inducers = ifelse(On_Enzyme_Inducers, 1, 0)) %>% 
        select(-c(Age:On_Enzyme_Inducers)) %>% 
        gather(key = term, value = value) %>% 
        mutate(term = as.character(term)) %>% 
        inner_join(warfarin_model) %>% 
        mutate(weighted = value * estimate) %>% 
        summarise(round(sum(weighted)^2))
      
      
      paste0("Based on the values entered, the predicted warfarin dose is: ", predicted_dose, "mg per week, or ~", round(predicted_dose/7),"mg per day.")
    })
  }
)
