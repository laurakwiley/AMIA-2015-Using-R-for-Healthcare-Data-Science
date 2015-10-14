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
  select(subject_id = PharmGKB.Subject.ID,
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
         diabetes = Diabetes,
         chf_cardiomyopathy = Congestive.Heart.Failure.and.or.Cardiomyopathy,
         valve_replacement = Valve.Replacement,
         medications = Medications,
         aspirin = Aspirin,
         acetaminophen = Acetaminophen.or.Paracetamol..Tylenol.,
         acetaminophen_high_dose = Was.Dose.of.Acetaminophen.or.Paracetamol..Tylenol...1300mg.day,
         simvastatin = Simvastatin..Zocor.,
         atorvastatin = Atorvastatin..Lipitor.,
         fluvastatin = Fluvastatin..Lescol.,
         lovastatin = Lovastatin..Mevacor.,
         pravastatin = Pravastatin..Pravachol.,
         rosuvastatin = Rosuvastatin..Crestor.,
         cerivastatin = Cerivastatin..Baycol.,
         amiodarone = Amiodarone..Cordarone.,
         carbamazepine = Carbamazepine..Tegretol.,
         phenytoin = Phenytoin..Dilantin.,
         rifampin = Rifampin.or.Rifampicin,
         sulfonamids = Sulfonamide.Antibiotics,
         macrolides = Macrolide.Antibiotics,
         azoles = Anti.fungal.Azoles,
         supplements = Herbal.Medications..Vitamins..Supplements,
         target_inr = Target.INR,
         target_inr_estimated = Estimated.Target.INR.Range.Based.on.Indication,
         reached_stable_dose = Subject.Reached.Stable.Dose.of.Warfarin,
         therapeutic_warfarin_dose = Therapeutic.Dose.of.Warfarin,
         inr_on_warfarin = INR.on.Reported.Therapeutic.Dose.of.Warfarin,
         smoker = Current.Smoker,
         cyp2c9_genotypes = Cyp2C9.genotypes,
         cyp2c9_star2_qc = Genotyped.QC.Cyp2C9.2,
         cyp2c9_star3_qc = Genotyped.QC.Cyp2C9.3,
         cyp2c9_diplotype = Combined.QC.CYP2C9,
         cyp2c9_consensus = CYP2C9.consensus,
         vkorc1_1639_preqc = VKORC1.genotype...1639.G.A..3673...chr16.31015190..rs9923231..C.T,
         vkorc1_1639_qc = VKORC1.QC.genotype...1639.G.A..3673...chr16.31015190..rs9923231..C.T,
         vkorc1_1639_consensus = VKORC1..1639.consensus,
         vkorc1_497_preqc = VKORC1.genotype..497T.G..5808...chr16.31013055..rs2884737..A.C,
         vkorc1_497_qc = VKORC1.QC.genotype..497T.G..5808...chr16.31013055..rs2884737..A.C,
         vkorc1_497_consensus = VKORC1.497.consensus,
         vkorc1_1173_preqc = VKORC1.genotype..1173.C.T.6484...chr16.31012379..rs9934438..A.G,
         vkorc1_1173_qc = VKORC1.QC.genotype..1173.C.T.6484...chr16.31012379..rs9934438..A.G,
         vkorc1_1173_consensus = VKORC1.1173.consensus,
         vkorc1_1542_preqc = VKORC1.genotype..1542G.C..6853...chr16.31012010..rs8050894..C.G,
         vkorc1_1542_qc = VKORC1.QC.genotype..1542G.C..6853...chr16.31012010..rs8050894..C.G,
         vkorc1_1542_consensus = VKORC1.1542.consensus,
         vkorc1_3730_preqc = VKORC1.genotype..3730.G.A..9041...chr16.31009822..rs7294...A.G,
         vkorc1_3730_qc = VKORC1.QC.genotype..3730.G.A..9041...chr16.31009822..rs7294...A.G,
         vkorc1_3730_consensus = VKORC1.3730.consensus,
         vkorc1_2255_preqc = VKORC1.genotype..2255C.T..7566...chr16.31011297..rs2359612..A.G,
         vkorc1_2255_qc = VKORC1.QC.genotype..2255C.T..7566...chr16.31011297..rs2359612..A.G,
         vkorc1_2255_consensus = VKORC1.2255.consensus,
         vkorc1_4451_preqc = VKORC1.genotype...4451.C.A..861...Chr16.31018002..rs17880887..A.C,
         vkorc1_4451_qc = VKORC1.QC.genotype...4451.C.A..861...Chr16.31018002..rs17880887..A.C,
         vkorc1_4451_consensus = VKORC1..4451.consensus,
         comments = Comments.regarding.Project.Site.Dataset)

working_dataset <- iwpc_data %>% select(subject_id,sample_id,project_site,gender,age,race_omb,height,weight,indication,comorbidities, medications,target_inr,target_inr_estimated,reached_stable_dose,therapeutic_warfarin_dose,inr_on_warfarin,smoker,cyp2c9_genotypes,cyp2c9_star2_qc,cyp2c9_star3_qc,cyp2c9_diplotype,cyp2c9_consensus,vkorc1_1639_preqc,vkorc1_1639_qc,vkorc1_1639_consensus)

## Caculated Fields
working_dataset %>% 
  mutate(body_surface_area = (0.007184 * (height^0.725)*(weight^0.425)))
