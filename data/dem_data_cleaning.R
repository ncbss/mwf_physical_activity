#----------------------------#
# Loading packages        ####
#----------------------------#
library(tidyverse)
library(openxlsx)

## ------------------------------------------------------- ##
#                       Loading datasets                    #
## ------------------------------------------------------- ##

# Loading data
cogmob_dem <- read.xlsx("cogmob_demographics_30April2021.xlsx")
rvci_dem <- read.xlsx("rvci_demographics_30April2021.xlsx")
subjects <- read_table("subjects_list.txt", col_names = "id")

## Adding pase data
rvci_pase <- read.xlsx("Cassio_PASE & Demographics_Notes_20211025.xlsx") %>%
  select(1,4) %>% 
  rename(id = 1, pase = 2)

cogmob_pase <- read.xlsx("Cogmob Physical and Questinonaire DATA_2018-01-30.xlsx") %>%
  filter(timepoint == 0) %>% 
  select(1,7) %>% 
  rename(id = 1, pase = 2) %>% 
  mutate(id = str_replace_all(id, c("MCI" = "FALLERS2_")))

## ------------------------------------------------------- ##
#                       Structural data                     #
## ------------------------------------------------------- ##

# FreeSurfer data
## Loading thickness data
all_aparc_thickness_lh <- read.xlsx("all_aseg_parc.xlsx", sheet = "aparc_thickness_lh")
all_aparc_thickness_rh <- read.xlsx("all_aseg_parc.xlsx", sheet = "aparc_thickness_rh")

## Loading aseg volumetric data 
all_aseg_stats <- read.xlsx("all_aseg_parc.xlsx", sheet = "aseg_stats")

#------------------------------------------------------------------#
#                        Data management                        ####
#------------------------------------------------------------------#

## Renaming 1st column to merge later
all_aparc_thickness_lh <- rename(all_aparc_thickness_lh, id = lh.aparc.thickness)
all_aparc_thickness_rh <- rename(all_aparc_thickness_rh, id = rh.aparc.thickness)

## Cleaning up
all_aparc_thickness <- left_join(all_aparc_thickness_lh, all_aparc_thickness_rh, by = "id") %>% 
  rename_with(~(gsub("-", "_", .x, fixed = TRUE))) %>% 
  rename(lh_mean_thickness = lh_MeanThickness_thickness,
         rh_mean_thickness = rh_MeanThickness_thickness) %>% 
  mutate(mean_thickness = (rh_mean_thickness + lh_mean_thickness)/2)

# Volumetric data
## Renaming variables
all_aseg_stats <- all_aseg_stats %>% 
  rename(id = "Measure:volume",
         lh_wm_vol = lhCerebralWhiteMatterVol,
         rh_wm_vol = rhCerebralWhiteMatterVol,
         cerebral_wm_vol = CerebralWhiteMatterVol,
         lh_cortex_vol = lhCortexVol,
         rh_cortex_vol = rhCortexVol,
         sub_cort_gm_vol = SubCortGrayVol,
         total_gm_vol = TotalGrayVol) %>% 
  rename_with(~(gsub("-", "_", .x, fixed = TRUE))) %>%
  rename_all(tolower) %>% 
  select(id, 
         lh_wm_vol, rh_wm_vol, cerebral_wm_vol,
         lh_cortex_vol, rh_cortex_vol, sub_cort_gm_vol, total_gm_vol,
         left_hippocampus, right_hippocampus, 
         left_lateral_ventricle, right_lateral_ventricle,
         left_inf_lat_vent, right_inf_lat_vent)

# Transforming volumetric data to cm3
vol_cm3 <- function(x, na.rm = FALSE) x / 1000

all_aseg_stats <- all_aseg_stats %>% 
  mutate_at(c("lh_wm_vol", 
               "rh_wm_vol", 
               "cerebral_wm_vol",
               "lh_cortex_vol", 
               "rh_cortex_vol", 
               "sub_cort_gm_vol",
               "left_hippocampus",
               "right_hippocampus",
               "total_gm_vol",
               "left_lateral_ventricle",
               "right_lateral_ventricle",
               "left_inf_lat_vent",
               "right_inf_lat_vent"), vol_cm3, na.rm = FALSE) %>% 
  rename_with(~paste(., sep = "_", "cm3"), 2:14)

# Cleaning datasets
## RVCI
rvci_dem_clean <- rvci_dem %>% 
  rename(age = "Age.at.Enrollment", height = "Height.(cm)", weight = "Weight.(kg)", moca = Total.MoCA, mmse = Total.MMSE) %>% 
  rename_all(tolower) %>%
  rename_with(~(gsub(".", "_", .x, fixed = TRUE))) %>% 
  rename_with(~(gsub(",", "_", .x, fixed = TRUE))) %>% 
  rename_with(~(gsub("/", "_", .x, fixed = TRUE))) %>% 
  rename_with(~(gsub("(", "_", .x, fixed = TRUE))) %>% 
  rename_with(~(gsub(">", "", .x, fixed = TRUE))) %>% 
  rename_with(~(gsub(")", "", .x, fixed = TRUE))) %>% 
  rename_with(~(gsub("__", "_", .x, fixed = TRUE))) %>% 
  mutate(sex = str_replace_all(sex, c("M" = "male", "F" = "female")))

rvci_dem_clean <- rvci_dem_clean %>% 
  select(id, age, sex, education, moca, mmse, height, weight, bmi, meters_walked, overall_fall_risk_score,
         fci_1_arthritis,
         fci_2_osteoporosis,
         fci_3_asthma,
         fci_4_copd_ards_or_emphysema,
         fci_5_angina, 
         fci_6_congestive_heart_failure_or_heart_disease,
         fci_7_heart_attack_myocardial_infarct,
         fci_8_neurological_disease,
         fci_9_stroke_or_tia,
         fci_10_peripheral_vascular_disease,
         fci_11_diabetes_type_i_and_ii,
         fci_12_upper_gastrointestinal_disease,
         fci_13_depression,                                
        fci_14_anxiety_or_panic_disorders,
        fci_15_visual_impairment,
        fci_16_hearing_impairment,
        fci_17_degenerative_disc_disease,
        fci_18_obesity_and_or_body_mass_index_30,
        fci_19_thyroid_disease,
        fci_20_cancer,
        fci_21_hypertension,
        fci_total)

## CogMob
cogmob_dem_clean <- cogmob_dem %>%
  rename(height = "Final.Height", weight = "Final.Weight", moca = Total.MoCA, mmse = Total.MMSE) %>% 
  mutate(bmi = weight/((height/100)^2)) %>% 
  rename_all(tolower) %>% 
  rename_with(~(gsub(".", "_", .x, fixed = TRUE))) %>% 
  rename_with(~(gsub(",", "_", .x, fixed = TRUE))) %>% 
  rename_with(~(gsub("/", "_", .x, fixed = TRUE))) %>% 
  rename_with(~(gsub("(", "_", .x, fixed = TRUE))) %>% 
  rename_with(~(gsub(">", "", .x, fixed = TRUE))) %>% 
  rename_with(~(gsub(")", "", .x, fixed = TRUE))) %>% 
  rename_with(~(gsub("__", "_", .x, fixed = TRUE))) %>% 
  mutate(id = str_replace_all(id, c("MCI" = "FALLERS2_")))

cogmob_dem_clean <- cogmob_dem_clean %>% 
  select(id, age, sex, education, moca, mmse, height, weight, bmi, meters_walked, overall_fall_risk_score,
         fci_1_arthritis,
         fci_2_osteoporosis,
         fci_3_asthma,
         fci_4_copd_ards_or_emphysema,
         fci_5_angina, 
         fci_6_congestive_heart_failure_or_heart_disease,
         fci_7_heart_attack_myocardial_infarct,
         fci_8_neurological_disease,
         fci_9_stroke_or_tia,
         fci_10_peripheral_vascular_disease,
         fci_11_diabetes_type_i_and_ii,
         fci_12_upper_gastrointestinal_disease,
         fci_13_depression,                                
         fci_14_anxiety_or_panic_disorders,
         fci_15_visual_impairment,
         fci_16_hearing_impairment,
         fci_17_degenerative_disc_disease,
         fci_18_obesity_and_or_body_mass_index_30,
         fci_19_thyroid_disease,
         fci_20_cancer,
         fci_21_hypertension,
         fci_total)

# Merging all
## Adding PASE data
cogmob_dem_clean <- left_join(cogmob_dem_clean, cogmob_pase)
rvci_dem_clean <- left_join(rvci_dem_clean, rvci_pase)

## Merging all
all_data_demographics <- rbind(cogmob_dem_clean, rvci_dem_clean)
all_data_demographics <- left_join(subjects, all_data_demographics, by = "id") %>% 
  left_join(., all_aparc_thickness_lh, by = "id") %>% 
  left_join(., all_aparc_thickness_rh, by = "id") %>% 
  left_join(., all_aseg_stats, by = "id")

## Recoding education and mutating FCI variables to character
all_data_demographics <- all_data_demographics %>% 
  mutate(education_r = str_replace_all(education, c("trades or professional certificate or diploma \\(CEGEP in Quebec\\)" = "trades or professional certificate",
                                                    "Less than grade 9" = "high school or less",
                                                    "high school certificate or diploma" = "high school or less",
                                                    "grades 9-13, without certificate or diploma" = "high school or less",
                                                    "some university certificate or diploma" = "some university")))

## ------------------------------------------------------- ##
#        Removing participant FALLERS2_228 = RVCI_027       #
#        Removing RVCI_016_V1 (first baseline assessment)   #
## ------------------------------------------------------- ##
all_data_demographics <- all_data_demographics %>% 
  filter(id != "FALLERS2_228") %>% 
  filter(id != "RVCI_016")

# Saving dataset  
write.xlsx(all_data_demographics, "all_data_demographics.xlsx")
