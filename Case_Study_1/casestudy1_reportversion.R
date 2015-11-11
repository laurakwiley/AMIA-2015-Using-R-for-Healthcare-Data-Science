## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)


libraries <- c("readr", "knitr", "DT", "purrr", "lubridate", "stringr", "tidyr", "ggplot2", "broom", "dplyr","magrittr","shiny", "cowplot","ggfortify")
libs_to_install <- libraries[!(libraries %in% installed.packages()[,"Package"])]
if(length(libs_to_install)>0){install.packages(libs_to_install)}

library(magrittr)
library(knitr)
library(readr)
library(DT)
library(purrr)
library(lubridate)
library(stringr)
library(broom)
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggfortify)
library(dplyr)
library(shiny)

## ----read_data-----------------------------------------------------------
iwpc_data <- read.delim(file = "iwpc_data_7_3_09_revised3.txt") %>% tbl_df()

## ----view_original_data, echo = FALSE------------------------------------
iwpc_data %>% 
  group_by(Project.Site) %>% 
  sample_n(1) %>% 
  datatable(rownames = FALSE, options = list(columnDefs = list(list(className = "dt-center", targets = c(0:21))),paging = FALSE, scrollX = TRUE, scrollY = '300px', bFilter = FALSE))

## ----view_data_types-----------------------------------------------------
iwpc_data %>% 
  map(~class(.x)) %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(Variable_Name = rownames(.), Variable_Type = V1) %>% 
  select(Variable_Name, Variable_Type) %>% 
  datatable(rownames = FALSE, options = list(paging = FALSE, bFilter = FALSE, info = FALSE), extensions = 'FixedHeader')

## ----rename_columns------------------------------------------------------
iwpc_data %<>% 
  rename(subject_id = PharmGKB.Subject.ID,
         sample_id = PharmGKB.Sample.ID,
         project_site = Project.Site,
         gender = Gender,
         race_reported = Race..Reported.,
         race_omb = Race..OMB.,
         ethnicity_reported = Ethnicity..Reported.,
         ethnicitiy_omb = Ethnicity..OMB.,
         age = Age,
         height = Height..cm.,
         weight = Weight..kg.,
         indication = Indication.for.Warfarin.Treatment,
         comorbidities = Comorbidities,
         medications = Medications,
         target_inr = Target.INR,
         target_inr_estimated = Estimated.Target.INR.Range.Based.on.Indication,
         reached_stable_dose = Subject.Reached.Stable.Dose.of.Warfarin,
         therapeutic_warfarin_dose = Therapeutic.Dose.of.Warfarin,
         inr_on_warfarin = INR.on.Reported.Therapeutic.Dose.of.Warfarin,
         smoker = Current.Smoker,
         cyp2c9_consensus = CYP2C9.consensus,
         vkorc1_1639_consensus = VKORC1..1639.consensus)

## ----distinct_target_inr_estimated---------------------------------------
iwpc_data %>% 
  count(target_inr_estimated) %>% 
  datatable(rownames = FALSE, colnames = c("Target INR", "N"),  options = list(order = list(1, "dsc"),  paging = FALSE, bFilter = FALSE, info = FALSE), extensions = 'FixedHeader')

## ----fix_target_inr_estimated--------------------------------------------
iwpc_data %<>% 
  mutate(target_inr_estimated = as.character(target_inr_estimated)) %>% 
  mutate(target_inr_estimated = ifelse(target_inr_estimated == "3-Feb",
                                       yes = "2-3", 
                                       no = ifelse(target_inr_estimated == "4-Mar",
                                                   yes = "3-4", 
                                                   no = target_inr_estimated)))

## ----distinct_target_inr_estimated_post_mod------------------------------
iwpc_data %>% 
  count(target_inr_estimated) %>% 
  datatable(rownames = FALSE, colnames = c("Target INR", "N"),  options = list(order = list(1, "dsc"),  paging = FALSE, bFilter = FALSE, info = FALSE), extensions = 'FixedHeader')

## ----age_look------------------------------------------------------------
iwpc_data %>% 
  count(age) %>% 
  datatable(rownames = FALSE, colnames = c("Age", "N"),  options = list(order = list(1, "dsc"),  paging = FALSE, bFilter = FALSE, info = FALSE), extensions = 'FixedHeader')

## ----age_fix_excel-------------------------------------------------------
iwpc_data %<>% 
  mutate(age = as.character(age)) %>% 
  mutate(age = ifelse(age == "19-Oct", 
                      yes = "10 - 19", no = age))

## ----age_look_postfix, echo = FALSE--------------------------------------
iwpc_data %>% 
  count(age) %>% 
  datatable(rownames = FALSE, colnames = c("Age", "N"),  options = list(order = list(1, "dsc"),  paging = FALSE, bFilter = FALSE, info = FALSE), extensions = 'FixedHeader')

## ----process_dummy_age---------------------------------------------------
iwpc_data %>% 
  count(age, 
        substr(age,1,1), 
        as.numeric(substr(age,1,1))) %>% 
  datatable(rownames = FALSE, colnames = c("Age", "Substring of Age", "Numeric Version of Substring", "N"),  options = list(order = list(3, "dsc"),  paging = FALSE, bFilter = FALSE, info = FALSE), extensions = 'FixedHeader')

iwpc_data %<>% 
  mutate(age_decades = as.numeric(substr(age,1,1)))

## ----vkorc1_look---------------------------------------------------------
iwpc_data %>% 
  count(vkorc1_1639_consensus) %>% 
  datatable(rownames = FALSE, colnames = c("VKORC1 Genotype", "N"),  options = list(order = list(1, "dsc"),  paging = FALSE, bFilter = FALSE, info = FALSE), extensions = 'FixedHeader')

## ----vkorc1_1, echo = TRUE, eval=FALSE-----------------------------------
## iwpc_data %>%
##   mutate(vkorc1_1639_ag = ifelse(str_detect(vkorc1_1639_consensus,"A/G"),
##                                  yes = 1, no = 0),
##          vkorc1_1639_aa = ifelse(str_detect(vkorc1_1639_consensus, "A/A"),
##                                  yes = 1,no = 0),
##          vkorc1_1639_unknown = ifelse(is.na(vkorc1_1639_consensus),
##                                       yes = 1,no = 0)) %>%
##   count(vkorc1_1639_consensus,vkorc1_1639_ag,vkorc1_1639_aa,vkorc1_1639_unknown) %>%
##   datatable(colnames = c("VKORC1 1639","VKORC1 A/G","VKORC1 A/A","VKORC1 Unknown","N"), rownames = FALSE, options = list(pageLength = 12, bFilter = FALSE, info = FALSE, paging = FALSE))

## ----vkorc1_mutate-------------------------------------------------------
iwpc_data %<>% 
  mutate(vkorc1_1639_ag = ifelse(is.na(vkorc1_1639_consensus) | 
                                   !str_detect(vkorc1_1639_consensus,"A/G"),
                                 yes = 0,  no = 1),
         vkorc1_1639_aa = ifelse(is.na(vkorc1_1639_consensus) | 
                                   !str_detect(vkorc1_1639_consensus, "A/A"),
                                 yes = 0, no = 1),
         vkorc1_1639_unknown = ifelse(is.na(vkorc1_1639_consensus),
                                      yes = 1, no = 0))

## ----vkorc1_check, echo=FALSE--------------------------------------------
iwpc_data %>% 
  count(vkorc1_1639_consensus,vkorc1_1639_ag,vkorc1_1639_aa,vkorc1_1639_unknown) %>% 
  datatable(colnames = c("VKORC1 1639","VKORC1 A/G","VKORC1 A/A","VKORC1 Unknown","N"), rownames = FALSE, options = list(pageLength = 12, bFilter = FALSE, info = FALSE, paging = FALSE))

## ----cyp2c9_look---------------------------------------------------------
iwpc_data %>% 
  count(cyp2c9_consensus) %>% 
  datatable(rownames = FALSE, colnames = c("CYP2C9 Genotype", "N"),  options = list(order = list(1, "dsc"),  paging = FALSE, bFilter = FALSE, info = FALSE), extensions = 'FixedHeader')

## ----fix_cycp2c9---------------------------------------------------------
iwpc_data %<>% 
  mutate(cyp2c9_1_2 = ifelse(is.na(cyp2c9_consensus) |
                               !str_detect(cyp2c9_consensus,"\\*1/\\*2"),
                             yes = 0, no = 1),
         cyp2c9_1_3 = ifelse(is.na(cyp2c9_consensus) |
                               !str_detect(cyp2c9_consensus,"\\*1/\\*3"),
                             yes = 0, no = 1),
         cyp2c9_2_2 = ifelse(is.na(cyp2c9_consensus) |
                               !str_detect(cyp2c9_consensus,"\\*2/\\*2"),
                             yes = 0, no = 1),
         cyp2c9_2_3 = ifelse(is.na(cyp2c9_consensus) |
                               !str_detect(cyp2c9_consensus,"\\*2/\\*3"),
                             yes = 0, no = 1),
         cyp2c9_3_3 = ifelse(is.na(cyp2c9_consensus) |
                               !str_detect(cyp2c9_consensus,"\\*3/\\*3"),
                             yes = 0, no = 1),
         cyp2c9_unknown = ifelse(is.na(cyp2c9_consensus),
                                 yes = 1,no = 0))

## ----cyp2c9_1, echo=FALSE------------------------------------------------
iwpc_data %>% 
  count(cyp2c9_consensus, cyp2c9_1_2, cyp2c9_1_3,cyp2c9_2_2,cyp2c9_2_3,cyp2c9_3_3,cyp2c9_unknown) %>% 
  datatable(colnames = c("CYP2C9","*1/*2","*1/*3","*2/*2","*2/*3","*3/*3","Unknown","N"), rownames = FALSE, options = list(order = list(7, "dsc"), paging = FALSE, bFilter = FALSE, info = FALSE), extensions = 'FixedHeader')

## ----race_look-----------------------------------------------------------
iwpc_data %>% 
  count(race_omb) %>% 
  datatable(rownames = FALSE, colnames = c("Race", "N"),  options = list(order = list(1, "dsc"),  paging = FALSE, bFilter = FALSE, info = FALSE), extensions = 'FixedHeader')

## ----race_1, echo=TRUE---------------------------------------------------
iwpc_data %<>% 
  mutate(asian = ifelse(str_detect(race_omb, "Asian"),
                        yes = 1,
                        no = 0),
         african_american = ifelse(str_detect(race_omb, "Black or African American"),
                                   yes = 1, 
                                   no = 0),
         missing_or_mixed_race = ifelse(str_detect(race_omb, "Unknown"),
                                        yes = 1,
                                        no = 0))

## ----race_2--------------------------------------------------------------
iwpc_data %>% 
  count(race_omb, asian, african_american, missing_or_mixed_race) %>% 
  datatable(colnames = c("Race OMB","Asian","African American","Missing/Mixed Race","N"), rownames = FALSE, options = list(pageLength = 12, bFilter = FALSE, info = FALSE, paging = FALSE))

## ----medication_look-----------------------------------------------------
iwpc_data %>% 
  count(medications) %>% 
  datatable(rownames = FALSE, colnames = c("Medications", "N"),  options = list(order = list(1, "dsc"),  paging = FALSE, bFilter = FALSE, info = FALSE, scrollY = '300px'))

## ----amiodarone_count----------------------------------------------------
iwpc_data %>% 
  filter(str_detect(medications, "amiodarone")) %>% 
  count(medications) %>% 
  datatable(rownames = FALSE, colnames = c("Medications", "N"),  options = list(order = list(1, "dsc"),  paging = FALSE, bFilter = FALSE, info = FALSE, scrollY = '300px'))

## ----amiodarone_regex_count----------------------------------------------
iwpc_data %>% filter(str_detect(medications, "amiodarone")) %>% count()
iwpc_data %>% filter(str_detect(medications, "(^|;)[a-z ]*amiodarone[a-z ]*($|;)")) %>% count()

## ----amiodarone_snippet--------------------------------------------------
iwpc_data %>% 
  mutate(amiodarone_text = str_extract(medications, "(^|;)[a-z ]*amiodarone[a-z ]*($|;)")) %>% 
  count(amiodarone_text) %>% 
  datatable(rownames = FALSE, colnames = c("Amiodarone_Snippet", "N"),  options = list(order = list(1, "dsc"),  paging = FALSE, bFilter = FALSE, info = FALSE), extensions = 'FixedHeader')

## ----amiodarone_snippet_regex_builder------------------------------------
iwpc_data %>% 
  mutate(amiodarone_text = str_extract(medications, "(^|;)[a-z ]*amiodarone[a-z ]*($|;)"), 
         amiodarone_bool = ifelse( !is.na(medications) & str_detect(medications, "(?<!not? )amiodarone"), 
                                   yes = 1, 
                                   no = 0)) %>% 
  count(amiodarone_text, amiodarone_bool) %>% 
  datatable(rownames = FALSE, colnames = c("Amiodarone_Snippet", "Amiodarone_Detector", "N"),  options = list(order = list(1, "dsc"),  paging = FALSE, bFilter = FALSE, info = FALSE), extensions = 'FixedHeader')

## ----fix_amiodarone------------------------------------------------------
iwpc_data %<>% 
  mutate(amiodarone = ifelse( !is.na(medications) & str_detect(medications, "(?<!not? )amiodarone"), 
                              yes = 1, 
                              no = 0))

## ----enxzyme_inducers_breakdown------------------------------------------
iwpc_data %<>% 
  mutate(carbamazepine = ifelse(!is.na(medications) & str_detect(medications,"(?<!not )carbamazepine"), yes = 1, no = 0),
         phenytoin = ifelse(!is.na(medications) & str_detect(medications,"(?<!not )phenytoin"),yes = 1,no = 0),
         rifampin = ifelse(!is.na(medications) & str_detect(medications,"(?<!not )rifampin"),yes = 1,no = 0),
         rifampicin = ifelse(!is.na(medications) & str_detect(medications,"(?<!not )rifampicin"),yes = 1,no = 0))

## ----enzyme_inducers_combined--------------------------------------------
iwpc_data %<>% 
  mutate(enzyme_inducers = ifelse((carbamazepine + phenytoin + rifampin + rifampicin) > 0, yes = 1, no = 0))

## ----check_enzymes, echo=FALSE-------------------------------------------
iwpc_data %>% 
  count(carbamazepine, phenytoin, rifampin, rifampicin, enzyme_inducers) %>% 
  datatable(rownames = FALSE, colnames = c("Carbamazepine", "Phenytoin", "Rifampin", "Rifampicin", "Enzyme_Inducers", "N"),  options = list(order = list(5, "dsc"),  paging = FALSE, bFilter = FALSE, info = FALSE), extensions = 'FixedHeader')

## ----plot_warfarin_dose--------------------------------------------------
iwpc_data %>% 
  ggplot(aes(x = 1, y = therapeutic_warfarin_dose)) + geom_boxplot()

## ----plot_sqrt_warfarin_dose---------------------------------------------
iwpc_data %>% 
  ggplot(aes(x = 1, y = sqrt(therapeutic_warfarin_dose))) + geom_boxplot()

## ----fix warfarin_dose---------------------------------------------------
iwpc_data %<>% mutate(sqrt_warfarin_dose = sqrt(therapeutic_warfarin_dose))

## ----model---------------------------------------------------------------
iwpc_data %>% 
  lm(formula = sqrt_warfarin_dose ~ age_decades + vkorc1_1639_ag + vkorc1_1639_aa + vkorc1_1639_unknown + cyp2c9_1_2 + cyp2c9_1_3 + cyp2c9_2_2 + cyp2c9_2_3 + cyp2c9_3_3 + cyp2c9_unknown + asian + african_american + missing_or_mixed_race + amiodarone + enzyme_inducers)

iwpc_data %>% 
  lm(formula = sqrt_warfarin_dose ~ age_decades + vkorc1_1639_ag + vkorc1_1639_aa + vkorc1_1639_unknown + cyp2c9_1_2 + cyp2c9_1_3 + cyp2c9_2_2 + cyp2c9_2_3 + cyp2c9_3_3 + cyp2c9_unknown + asian + african_american + missing_or_mixed_race + amiodarone + enzyme_inducers) %>% 
  summary()

## ----model_and_broom, eval = FALSE---------------------------------------
## model <-  iwpc_data %>% lm(formula = sqrt_warfarin_dose ~ age_decades + vkorc1_1639_ag + vkorc1_1639_aa + vkorc1_1639_unknown + cyp2c9_1_2 + cyp2c9_1_3 + cyp2c9_2_2 + cyp2c9_2_3 + cyp2c9_3_3 + cyp2c9_unknown + asian + african_american + missing_or_mixed_race + amiodarone + enzyme_inducers)
## 
## warfarin_pharmacogenomic_model <- tidy(model)
## warfarin_pharmacogenomic_model

## ----tidy_output, echo = FALSE-------------------------------------------
model <-  iwpc_data %>% lm(formula = sqrt_warfarin_dose ~ age_decades + vkorc1_1639_ag + vkorc1_1639_aa + vkorc1_1639_unknown + cyp2c9_1_2 + cyp2c9_1_3 + cyp2c9_2_2 + cyp2c9_2_3 + cyp2c9_3_3 + cyp2c9_unknown + asian + african_american + missing_or_mixed_race + amiodarone + enzyme_inducers)
warfarin_pharmacogenomic_model <- tidy(model)


tidy(model) %>% 
  datatable(rownames = FALSE, options = list(paging = FALSE, bFilter = FALSE, info = FALSE, columnDefs = list(list(className = "dt-center", targets = c(0:4)))), extensions = 'FixedHeader')

## ----glance_model--------------------------------------------------------
glance(model) %>% 
  datatable(options = list(paging = FALSE, bFilter = FALSE, info = FALSE, scrollX = TRUE, columnDefs = list(list(className = "dt-center", targets = c(0:11)))))

## ----model_forest,  fig.align='center'-----------------------------------
warfarin_pharmacogenomic_model %>% 
  filter(term != "(Intercept)") %>% 
  mutate(variable = factor(term, levels = rev(c("age_decades", "asian","african_american","missing_or_mixed_race", "amiodaron","enzyme_inducers","vkorc1_1639_unkown","vkorc1_1639_ag","vkorc1_1639_aa","cyp2c9_unknown", "cyp2c9_1_2","cyp2c9_1_3","cyp2c9_2_2","cyp2c9_2_3","cyp2c9_3_3")))) %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) + 
  coord_flip()

## ----model_fit, fig.height=10, fig.align='center'------------------------
autoplot(model)

## ----variable_plots, fig.height=11, message=FALSE,  fig.align='center'----
warfarin_dose <- iwpc_data %>% 
  ggplot(aes(x = therapeutic_warfarin_dose)) + geom_histogram() + 
  xlab("Weekly Warfarin Dose") + ylab("Count")

age <- iwpc_data %>% 
  ggplot(aes(x = age)) + geom_histogram() + 
  xlab("Age") + ylab("Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

race <- iwpc_data %>% 
  ggplot(aes(x = race_omb)) + geom_histogram() +
  xlab("Race") + ylab("Count") +
  scale_x_discrete(breaks = c("Asian", "Black or African American", "White", "Unknown"), labels = c("Asian", "Black", "White", "Unk."))

vkorc1 <- iwpc_data %>% 
  ggplot(aes(x = vkorc1_1639_consensus)) + geom_histogram() +
  xlab("VKORC1 Genotype") + ylab("Count")

cyp2c9 <- iwpc_data %>% 
  ggplot(aes(x = cyp2c9_consensus)) + geom_histogram() +
  xlab("CYP2C9 Genotype") + ylab("Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggdraw() +
  draw_plot(warfarin_dose, x = 0, y = 0.75, width = 1, height = 0.25) +
  draw_plot(age, x = 0, y = 0.5, width = 1, height = 0.25) +
  draw_plot(cyp2c9, x = 0, y = 0.25, width = 1, height = 0.25) +
  draw_plot(vkorc1, x = 0, y = 0, width = 0.5, height = 0.25) +
  draw_plot(race, x = 0.5, y = 0, width = 0.5, height = 0.25)

## ----run_shiny_app, eval = TRUE-----------------------------------------
runGist("cafba2c579b6922c4956")

## ----shiny_app, eval = FALSE, echo = TRUE--------------------------------
## shinyApp(
## 
##   ui = pageWithSidebar(
##     headerPanel("Warfarin Pharmagenomic Dose Predictor"),
##     sidebarPanel(selectInput("age", "Age in Decades:", choices = c("10-19","20-29","30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")),
##                  radioButtons("race", "Race:", choices = c( "Other or Unknown", "Asian", "African American", "White")),
##                  radioButtons("vkorc1", "VKORC1 Genotype:", choices = c("Unknown", "G/G", "G/A", "A/A")),
##                  radioButtons("cyp2c9", "CYP2C9 Genotype:", choices = c("Unknown", "*1/*2", "*1/*3", "*2/*2", "*2/*3", "*3/*3","Other")),
##                  checkboxInput('amiodarone', "Taking Amiodarone", FALSE),
##                  checkboxInput("enzyme_inducers", "Taking an Enzyme Inducer (rifampin, carbamazepine, phenytoin or rifampicin)", FALSE),
##                  actionButton("calc","Calculate")
##                  ),
##     mainPanel(
##       strong(em("THIS IS A PROGRAMMING EXAMPLE ONLY - DO NOT USE FOR PATIENT CARE!")),
##       br(),br(),
##       strong(em("IF YOU HAVE QUESTIONS ABOUT YOUR WARFARIN DOSE, PLEASE CONTACT YOUR DOCTOR.")),
##       br(),br(),
##       p("You Selected: "),
##       tableOutput("selectedvalues"),
##       br(),br(),
##       textOutput("warfarindose")
##     )
##   ),
## 
##   server = function(input, output){
##     library(dplyr)
##     library(tidyr)
## 
##     input_model <- eventReactive(input$calc, {
##       data.frame(Age = input$age,
##                  Race = input$race,
##                  VKORC1 = input$vkorc1,
##                  CYP2C9 = input$cyp2c9,
##                  On_Amiodarone = input$amiodarone,
##                  On_Enzyme_Inducers = input$enzyme_inducers)
##     })
## 
##     output$selectedvalues <- renderTable({input_model() %>%
##         gather(key = Variable, value = Selection)})
## 
## 
## 
##     output$warfarindose <- renderText({
##           warfarin_model <- structure(list(term = c("intercept", "age_decades", "vkorc1_1639_ag", "vkorc1_1639_aa", "vkorc1_1639_unknown", "cyp2c9_1_2", "cyp2c9_1_3", "cyp2c9_2_2", "cyp2c9_2_3", "cyp2c9_3_3", "asian", "african_american", "missing_or_mixed_race", "amiodarone", "enzyme_inducers"), estimate = c(8.28092953788836, -0.283786900016807, -0.800068515352432, -1.56451454024173, -0.595825280765066, -0.465304224113711, -0.892056417892715, -1.08355935593238, -1.86263474384314, -2.49990281796991, -0.686065650288828, -0.0769652407947399, -0.341619674379834, -0.668291885082912, 0.539804214460087)), row.names = c(NA, -15L), class = "data.frame", .Names = c("term", "estimate"))
## 
##     predicted_dose <- input_model() %>%
##       mutate(intercept = 1,
##              age_decades = as.numeric(substr(Age, 1, 1)),
##              vkorc1_1639_ag = ifelse(VKORC1 == "G/A", 1, 0),
##              vkorc1_1639_aa = ifelse(VKORC1 == "A/A", 1, 0),
##              vkorc1_1639_unknown = ifelse(VKORC1 == "Unknown", 1, 0),
##              cyp2c9_1_2 = ifelse(CYP2C9 == "*1/*2", 1, 0),
##              cyp2c9_1_3 = ifelse(CYP2C9 == "*1/*3", 1, 0),
##              cyp2c9_2_2 = ifelse(CYP2C9 == "*2/*2", 1, 0),
##              cyp2c9_2_3 = ifelse(CYP2C9 == "*2/*3", 1, 0),
##              cyp2c9_3_3 = ifelse(CYP2C9 == "*3/*3", 1, 0),
##              asian = ifelse(Race == "Asian", 1, 0),
##              african_american = ifelse(Race == "African American", 1, 0),
##              missing_or_mixed_race = ifelse(Race == "Other or Unknown", 1, 0),
##              amiodarone = ifelse(On_Amiodarone, 1, 0),
##              enzyme_inducers = ifelse(On_Enzyme_Inducers, 1, 0)) %>%
##       select(-c(Age:On_Enzyme_Inducers)) %>%
##       gather(key = term, value = value) %>%
##       mutate(term = as.character(term)) %>%
##       inner_join(warfarin_model) %>%
##       mutate(weighted = value * estimate) %>%
##       summarise(round(sum(weighted)^2))
## 
## 
##       paste0("Based on the values entered, the predicted warfarin dose is: ", predicted_dose, "mg per week, or ~", round(predicted_dose/7),"mg per day.")
##     })
##   },
## 
##   options = list(height = 1000)
## )

