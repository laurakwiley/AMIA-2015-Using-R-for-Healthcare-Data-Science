libraries <- c("readr", "knitr", "DT", "purrr", "lubridate", "stringr", "tidyr", "ggplot2", "broom", "tidyr","magrittr")
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
library(dplyr)

iwpc_data <- read.delim(file = "iwpc_data_7_3_09_revised3.txt") %>% tbl_df()

iwpc_data %>% 
  group_by(Project.Site) %>% 
  sample_n(1) %>% 
  datatable(rownames = FALSE, options = list(pageLength = 3, scrollX = TRUE))

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

## Caculated / mutated fields

### SQRT of Stable Dose
iwpc_data %>% count(reached_stable_dose)
iwpc_data %<>% filter(reached_stable_dose!=0)

iwpc_data %>% count(therapeutic_warfarin_dose)
iwpc_data %>% ggplot(aes(x=1,y = therapeutic_warfarin_dose)) + geom_boxplot()
iwpc_data %>% ggplot(aes(x=1,y = sqrt(therapeutic_warfarin_dose))) + geom_boxplot()

iwpc_data %<>% mutate(sqrt_warfarin_dose = sqrt(therapeutic_warfarin_dose))
### race
iwpc_data %>% count(race_omb)
iwpc_data %>% 
  mutate(asian = ifelse(str_detect(race_omb, "Asian"),yes = 1,no = 0),
         african_american = ifelse(str_detect(race_omb, "Black or African American"),yes = 1,no = 0),
         missing_or_mixed_race = ifelse(str_detect(race_omb, "Unknown"),yes = 1,no = 0)) %>% 
  count(race_omb,asian,african_american,missing_or_mixed_race)
iwpc_data %<>% 
  mutate(asian = ifelse(str_detect(race_omb, "Asian"),yes = 1,no = 0),
         african_american = ifelse(str_detect(race_omb, "Black or African American"),yes = 1,no = 0),
         missing_or_mixed_race = ifelse(str_detect(race_omb, "Unknown"),yes = 1,no = 0))

## VKORC1

iwpc_data %>% count(vkorc1_1639_consensus)
iwpc_data %>% 
  mutate(vkorc1_1639_ag = ifelse(str_detect(vkorc1_1639_consensus,"A/G"),yes = 1,no = 0),
         vkorc1_1639_aa = ifelse(str_detect(vkorc1_1639_consensus, "A/A"),yes = 1,no = 0),
         vkorc1_1639_unknown = ifelse(is.na(vkorc1_1639_consensus),yes = 1,no = 0)) %>% 
  count(vkorc1_1639_consensus,vkorc1_1639_ag,vkorc1_1639_aa,vkorc1_1639_unknown)
iwpc_data %>% 
  mutate(vkorc1_1639_ag = ifelse(is.na(vkorc1_1639_consensus)|!str_detect(vkorc1_1639_consensus,"A/G"),yes = 0,no = 1),
         vkorc1_1639_aa = ifelse(is.na(vkorc1_1639_consensus)|!str_detect(vkorc1_1639_consensus, "A/A"),yes = 0,no = 1),
         vkorc1_1639_unknown = ifelse(is.na(vkorc1_1639_consensus),yes = 1,no = 0)) %>% 
  count(vkorc1_1639_consensus,vkorc1_1639_ag,vkorc1_1639_aa,vkorc1_1639_unknown)
iwpc_data %<>% 
  mutate(vkorc1_1639_ag = ifelse(is.na(vkorc1_1639_consensus)|!str_detect(vkorc1_1639_consensus,"A/G"),yes = 0,no = 1),
         vkorc1_1639_aa = ifelse(is.na(vkorc1_1639_consensus)|!str_detect(vkorc1_1639_consensus, "A/A"),yes = 0,no = 1),
         vkorc1_1639_unknown = ifelse(is.na(vkorc1_1639_consensus),yes = 1,no = 0))

## CYP2C9

iwpc_data %>% count(cyp2c9_consensus)
iwpc_data %>% 
  mutate(cyp2c9_1_2 = ifelse(is.na(cyp2c9_consensus)|!str_detect(cyp2c9_consensus,"\\*1/\\*2"),yes = 0,no = 1),
         cyp2c9_1_3 = ifelse(is.na(cyp2c9_consensus)|!str_detect(cyp2c9_consensus,"\\*1/\\*3"),yes = 0,no = 1),
         cyp2c9_2_2 = ifelse(is.na(cyp2c9_consensus)|!str_detect(cyp2c9_consensus,"\\*2/\\*2"),yes = 0,no = 1),
         cyp2c9_2_3 = ifelse(is.na(cyp2c9_consensus)|!str_detect(cyp2c9_consensus,"\\*2/\\*3"),yes = 0,no = 1),
         cyp2c9_3_3 = ifelse(is.na(cyp2c9_consensus)|!str_detect(cyp2c9_consensus,"\\*3/\\*3"),yes = 0,no = 1),
         cyp2c9_unknown = ifelse(is.na(cyp2c9_consensus),yes = 1,no = 0)) %>% 
  count(cyp2c9_consensus, cyp2c9_1_2, cyp2c9_1_3,cyp2c9_2_2,cyp2c9_2_3,cyp2c9_3_3,cyp2c9_unknown)
iwpc_data %<>% 
  mutate(cyp2c9_1_2 = ifelse(is.na(cyp2c9_consensus)|!str_detect(cyp2c9_consensus,"\\*1/\\*2"),yes = 0,no = 1),
         cyp2c9_1_3 = ifelse(is.na(cyp2c9_consensus)|!str_detect(cyp2c9_consensus,"\\*1/\\*3"),yes = 0,no = 1),
         cyp2c9_2_2 = ifelse(is.na(cyp2c9_consensus)|!str_detect(cyp2c9_consensus,"\\*2/\\*2"),yes = 0,no = 1),
         cyp2c9_2_3 = ifelse(is.na(cyp2c9_consensus)|!str_detect(cyp2c9_consensus,"\\*2/\\*3"),yes = 0,no = 1),
         cyp2c9_3_3 = ifelse(is.na(cyp2c9_consensus)|!str_detect(cyp2c9_consensus,"\\*3/\\*3"),yes = 0,no = 1),
         cyp2c9_unknown = ifelse(is.na(cyp2c9_consensus),yes = 1,no = 0))

## Text fields:
iwpc_data %<>% mutate(medications = tolower(medications))

### Amiodarone
iwpc_data %>% filter(str_detect(medications, "amiodarone")) %>% count(medications)
iwpc_data %>% filter(str_detect(medications, "amiodarone")) %>% count()

iwpc_data %>% filter(str_detect(medications, "(^|;)[a-z ]*amiodarone")) %>% count()
iwpc_data %>% mutate(medications = str_extract(medications, "(^|;)[a-z ]*amiodarone")) %>% count(medications)
iwpc_data %>% mutate(medications = str_extract(medications, "(^|;)[a-z ]*amiodarone"),amiodarone_test = str_detect(medications, "(; amiodarone)|(^[a-z ]*amiodarone)")) %>% count(medications,amiodarone_test)
iwpc_data %>% mutate(medications_temp = str_extract(medications, "(^|;)[a-z ]*amiodarone"), amiodarone_test = str_detect(medications, "(; amiodarone)|(^[a-z ]*amiodarone)"), test = ifelse(is.na(medications)|!str_detect(medications, "(; amiodarone)|(^[a-z ]*amiodarone)"),yes = 0,no = 1)) %>% count(medications_temp,amiodarone_test,test)


iwpc_data %<>% mutate(amiodarone = ifelse(is.na(medications)|!str_detect(medications, "(; amiodarone)|(^[a-z ]*amiodarone)"),yes = 0,no = 1))

### Enzyme inducers

#### Carbamazepine
iwpc_data %>% filter(str_detect(medications,"carbamazepine")) %>% count()
iwpc_data %>% filter(str_detect(medications,"(^|;)[a-z ]*carbamazepine")) %>% count()
iwpc_data %>% mutate(medications_temp = str_extract(medications,"(^|;)[a-z ]*carbamazepine")) %>% count(medications_temp)
iwpc_data %<>% mutate(carbamazepine = ifelse(is.na(medications)|!str_detect(medications,"(^|;)[a-z ]*carbamazepine"),yes = 0,no = 1))

#### Phenytoin
iwpc_data %>% filter(str_detect(medications,"phenytoin")) %>% count()
iwpc_data %>% filter(str_detect(medications,"(^|;)[a-z ]*phenytoin")) %>% count()
iwpc_data %>% mutate(medications_temp = str_extract(medications,"(^|;)[a-z ]*phenytoin")) %>% count(medications_temp)
iwpc_data %<>% mutate(phenytoin = ifelse(is.na(medications)|!str_detect(medications,"(^|;)[a-z ]*phenytoin"),yes = 0,no = 1))

#### rifampin
iwpc_data %>% filter(str_detect(medications,"rifampin")) %>% count()
iwpc_data %>% filter(str_detect(medications,"(^|;)[a-z ]*rifampin")) %>% count()
iwpc_data %>% mutate(medications_temp = str_extract(medications,"(^|;)[a-z ]*rifampin")) %>% count(medications_temp)
iwpc_data %<>% mutate(rifampin = ifelse(is.na(medications)|!str_detect(medications,"(^|;)[a-z ]*rifampin"),yes = 0,no = 1))

#### rifampicin
iwpc_data %>% filter(str_detect(medications,"rifampicin")) %>% count()
iwpc_data %>% filter(str_detect(medications,"(^|;)[a-z ]*rifampicin")) %>% count()
iwpc_data %>% mutate(medications_temp = str_extract(medications,"(^|;)[a-z ]*rifampicin")) %>% count(medications_temp)
iwpc_data %<>% mutate(rifampicin = ifelse(is.na(medications)|!str_detect(medications,"(^|;)[a-z ]*rifampicin"),yes = 0,no = 1))

#### enzyme
iwpc_data %>% mutate(enzyme_enducer = ifelse((carbamazepine+phenytoin+rifampin+rifampicin)>0,yes = 1,no = 0)) %>% count(carbamazepine,phenytoin,rifampin,rifampicin,enzyme_enducer)

iwpc_data %<>% mutate(enzyme_enducer = ifelse((carbamazepine+phenytoin+rifampin+rifampicin)>0,yes = 1,no = 0))

### Age
iwpc_data %>% count(age)
iwpc_data %>% count(age, substr(age,1,1), as.numeric(substr(age,1,1)))
iwpc_data %>% mutate(age_decades = as.numeric(substr(age,1,1)))

##

