## SET UP ---------------------------------------------------------------------
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

## ----Read and View Data----------------------------
iwpc_data <- read.delim(file = "iwpc_data_7_3_09_revised3.txt") %>% tbl_df()

iwpc_data %>% 
  View

iwpc_data %>% 
  group_by(Project.Site) %>% 
  sample_n(1) %>% 
  View

## ----Data Types-------------------------------
iwpc_data %>% 
  map(~class(.x)) %>% 
  t() %>% 
  as.data.frame() %>% 
  View

## ----Rename Columns-----------------------------
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

## ----Excel: INR Goal Range Problem-------------
iwpc_data %>% 
  count(target_inr_estimated) %>% View

iwpc_data %>% 
  mutate(target_inr_estimated = as.character(target_inr_estimated)) %>% 
  mutate(target_inr_estimated = ifelse(target_inr_estimated == "3-Feb",
                                       yes = "2-3", 
                                       no = ifelse(target_inr_estimated == "4-Mar",
                                                   yes = "3-4", 
                                                   no = target_inr_estimated))) %>% 
  count(target_inr_estimated) %>% 
  View

iwpc_data %<>% 
  mutate(target_inr_estimated = as.character(target_inr_estimated)) %>% 
  mutate(target_inr_estimated = ifelse(target_inr_estimated == "3-Feb",
                                       yes = "2-3", 
                                       no = ifelse(target_inr_estimated == "4-Mar",
                                                   yes = "3-4", 
                                                   no = target_inr_estimated)))


## ----Fix Age------------------------------------------------------------
iwpc_data %>% 
  count(age) %>% 
  View

iwpc_data %>% 
  mutate(age = as.character(age)) %>% 
  mutate(age = ifelse(age == "19-Oct", 
                      yes = "10 - 19", no = age)) %>% 
  count(age) %>% 
  View

iwpc_data %<>% 
  mutate(age = as.character(age)) %>% 
  mutate(age = ifelse(age == "19-Oct", 
                      yes = "10 - 19", no = age))


## ----Dumy Age---------------------------------------------------
iwpc_data %>% 
  count(age, 
        substr(age,1,1), 
        as.numeric(substr(age,1,1))) %>% 
 View

iwpc_data %<>% 
  mutate(age_decades = as.numeric(substr(age,1,1)))

## ----VKORC1---------------------------------------------------------
iwpc_data %>% 
  count(vkorc1_1639_consensus) %>% 
  View()

iwpc_data %>%
  mutate(vkorc1_1639_ag = ifelse(str_detect(vkorc1_1639_consensus,"A/G"),
                                 yes = 1, no = 0),
         vkorc1_1639_aa = ifelse(str_detect(vkorc1_1639_consensus, "A/A"),
                                 yes = 1,no = 0),
         vkorc1_1639_unknown = ifelse(is.na(vkorc1_1639_consensus),
                                      yes = 1,no = 0)) %>%
  count(vkorc1_1639_consensus,vkorc1_1639_ag,vkorc1_1639_aa,vkorc1_1639_unknown) %>%
  View()

## ----VKORC1 Reverse Mutate-------------------------------------------------------
iwpc_data %>% 
  mutate(vkorc1_1639_ag = ifelse(is.na(vkorc1_1639_consensus) | 
                                   !str_detect(vkorc1_1639_consensus,"A/G"),
                                 yes = 0,  no = 1),
         vkorc1_1639_aa = ifelse(is.na(vkorc1_1639_consensus) | 
                                   !str_detect(vkorc1_1639_consensus, "A/A"),
                                 yes = 0, no = 1),
         vkorc1_1639_unknown = ifelse(is.na(vkorc1_1639_consensus),
                                      yes = 1, no = 0)) %>% 
  count(vkorc1_1639_consensus,vkorc1_1639_ag,vkorc1_1639_aa,vkorc1_1639_unknown) %>%
  View()

iwpc_data %<>% 
  mutate(vkorc1_1639_ag = ifelse(is.na(vkorc1_1639_consensus) | 
                                   !str_detect(vkorc1_1639_consensus,"A/G"),
                                 yes = 0,  no = 1),
         vkorc1_1639_aa = ifelse(is.na(vkorc1_1639_consensus) | 
                                   !str_detect(vkorc1_1639_consensus, "A/A"),
                                 yes = 0, no = 1),
         vkorc1_1639_unknown = ifelse(is.na(vkorc1_1639_consensus),
                                      yes = 1, no = 0))

## ----CYP2C9---------------------------------------------------------
iwpc_data %>% 
  count(cyp2c9_consensus) %>% 
  View

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

iwpc_data %>% 
  count(cyp2c9_consensus, cyp2c9_1_2, cyp2c9_1_3,cyp2c9_2_2,cyp2c9_2_3,cyp2c9_3_3,cyp2c9_unknown) %>% 
  View()


## ----Race-----------------------------------------------------------
iwpc_data %>% 
  count(race_omb) %>% 
  View()

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

iwpc_data %>% 
  count(race_omb, asian, african_american, missing_or_mixed_race) %>% 
  View

## ----Medications-----------------------------------------------------
iwpc_data %>% 
  count(medications) %>% 
  View

## ----Amiodarone----------------------------------------------------
iwpc_data %>% 
  filter(str_detect(medications, "amiodarone")) %>% 
  count(medications) %>% 
  View()

## ----Amiodarone New Regex----------------------------------------------
iwpc_data %>% filter(str_detect(medications, "amiodarone")) %>% count()
iwpc_data %>% filter(str_detect(medications, "(^|;)[a-z ]*amiodarone[a-z ]*($|;)")) %>% count()

iwpc_data %>% 
  mutate(amiodarone_text = str_extract(medications, "(^|;)[a-z ]*amiodarone[a-z ]*($|;)")) %>% 
  count(amiodarone_text) %>% 
  View()

iwpc_data %>% 
  mutate(amiodarone_text = str_extract(medications, "(^|;)[a-z ]*amiodarone[a-z ]*($|;)"), 
         amiodarone_bool = ifelse( !is.na(medications) & str_detect(medications, "(?<!not? )amiodarone"), 
                                   yes = 1, 
                                   no = 0)) %>% 
  count(amiodarone_text, amiodarone_bool) %>% 
  View()

iwpc_data %<>% 
  mutate(amiodarone = ifelse( !is.na(medications) & str_detect(medications, "(?<!not? )amiodarone"), 
                              yes = 1, 
                              no = 0))

## ----Enzyme Inducers------------------------------------------
iwpc_data %<>% 
  mutate(carbamazepine = ifelse(!is.na(medications) & str_detect(medications,"(?<!not )carbamazepine"), yes = 1, no = 0),
         phenytoin = ifelse(!is.na(medications) & str_detect(medications,"(?<!not )phenytoin"),yes = 1,no = 0),
         rifampin = ifelse(!is.na(medications) & str_detect(medications,"(?<!not )rifampin"),yes = 1,no = 0),
         rifampicin = ifelse(!is.na(medications) & str_detect(medications,"(?<!not )rifampicin"),yes = 1,no = 0))


iwpc_data %>% 
  mutate(enzyme_inducers = ifelse((carbamazepine + phenytoin + rifampin + rifampicin) > 0, yes = 1, no = 0)) %>% 
  count(carbamazepine, phenytoin, rifampin, rifampicin, enzyme_inducers) %>% 
  View

iwpc_data %<>% 
  mutate(enzyme_inducers = ifelse((carbamazepine + phenytoin + rifampin + rifampicin) > 0, yes = 1, no = 0)) 

## ----Plot Warfarin Dose--------------------------------------------------
iwpc_data %>% 
  ggplot(aes(x = 1, y = therapeutic_warfarin_dose)) + geom_boxplot()

## ----Plor SQRT Warfarin Dose---------------------------------------------
iwpc_data %>% 
  ggplot(aes(x = 1, y = sqrt(therapeutic_warfarin_dose))) + geom_boxplot()

## ----Fix Warfarin Dose---------------------------------------------------
iwpc_data %<>% mutate(sqrt_warfarin_dose = sqrt(therapeutic_warfarin_dose))

## ----Linear Model---------------------------------------------------------------

model <-  iwpc_data %>% 
  lm(formula = sqrt_warfarin_dose ~ age_decades + vkorc1_1639_ag + vkorc1_1639_aa + vkorc1_1639_unknown + cyp2c9_1_2 + cyp2c9_1_3 + cyp2c9_2_2 + cyp2c9_2_3 + cyp2c9_3_3 + cyp2c9_unknown + asian + african_american + missing_or_mixed_race + amiodarone + enzyme_inducers)

warfarin_pharmacogenomic_model <- tidy(model)

warfarin_pharmacogenomic_model %>% View

glance(model) %>% 
  View

## ---- Forest Plot-----------------------------------
warfarin_pharmacogenomic_model %>% 
  filter(term != "(Intercept)") %>% 
  mutate(variable = factor(term, levels = rev(c("age_decades", "asian","african_american","missing_or_mixed_race", "amiodaron","enzyme_inducers","vkorc1_1639_unkown","vkorc1_1639_ag","vkorc1_1639_aa","cyp2c9_unknown", "cyp2c9_1_2","cyp2c9_1_3","cyp2c9_2_2","cyp2c9_2_3","cyp2c9_3_3")))) %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) + 
  coord_flip()

## ----Plot Model Fit------------------------
## From library(ggally)
autoplot(model)

## ----Variable Plots----
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

## From library cowplot
ggdraw() +
  draw_plot(warfarin_dose, x = 0, y = 0.75, width = 1, height = 0.25) +
  draw_plot(age, x = 0, y = 0.5, width = 1, height = 0.25) +
  draw_plot(cyp2c9, x = 0, y = 0.25, width = 1, height = 0.25) +
  draw_plot(vkorc1, x = 0, y = 0, width = 0.5, height = 0.25) +
  draw_plot(race, x = 0.5, y = 0, width = 0.5, height = 0.25)

## ----Shiny App-----------------------------------------
runGist("cafba2c579b6922c4956")

## ----Shiny Code--------------------------------
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

