#------------------------------------------------------------------#
# Loading packages                                              ####
#------------------------------------------------------------------#
library(tidyverse)
library(openxlsx)

#------------------------------------------------------------------#
# Loading data                                                  ####
#------------------------------------------------------------------#

# Demographics data
all_data_demographics <- read.xlsx("./data/all_data_demographics.xlsx")

# MWF data
cogmob_mwf <- read.xlsx("./data/cogmob_mwf_all_wm.xlsx") # Needs to be separate from rvci due to IDs having different lenghts
rvci_mwf <- read.xlsx("./data/rvci_mwf_all_wm.xlsx")

# WMH and Fazekas score data
rvci_wmh <- read.xlsx("./data/rvci_wmh_volume.xlsx" )
cogmob_wmh <- read.xlsx("./data/cogmob_wmh_volume.xlsx")

#------------------------------------------------------------------#
# Data management                                               ####
#------------------------------------------------------------------#

# Myelin data ####
## Cleaning up
### MWF
cogmob_mwf <- cogmob_mwf %>% 
  separate(ID_ROI, c("id","roi"), sep = 12) # Cleaning up ROI and study id

rvci_mwf <- rvci_mwf %>%
  separate(ID_ROI, c("id","roi"), sep = 8) # Cleaning up ROI and study id

## Merging datasets
all_mwf_data_raw <- rbind(cogmob_mwf, rvci_mwf) %>% 
  mutate(roi = str_replace_all(roi, c("_ROI_" = "", "_M" = "M","_wm" = "_WM"))) %>% 
  mutate(roi = str_replace_all(roi, c("JLF_" = "", "_F" = "F","_O" = "O", "_P" = "P"))) %>% 
  rename("volume" = "Volume.(voxels)") %>% 
  rename_all(tolower)

## Transposing roi mwf data from long to wide
mwf_wide_mean <- all_mwf_data_raw %>% 
  pivot_wider(id_cols = id, names_from = roi, values_from = mwf_mean)
colnames(mwf_wide_mean)[2:ncol(mwf_wide_mean)] <- paste(colnames(mwf_wide_mean)[2:ncol(mwf_wide_mean)], "mean", sep = "_")

mwf_wide_sd <- all_mwf_data_raw %>% 
  pivot_wider(id_cols = id, names_from = roi, values_from = mwf_sd)
colnames(mwf_wide_sd)[2:ncol(mwf_wide_sd)]  <- paste(colnames(mwf_wide_sd) [2:ncol(mwf_wide_sd)] , "sd", sep = "_")

mwf_wide_vol <- all_mwf_data_raw %>% 
  pivot_wider(id_cols = id, names_from = roi, values_from = volume)
colnames(mwf_wide_vol)[2:ncol(mwf_wide_vol)]  <- paste(colnames(mwf_wide_vol) [2:ncol(mwf_wide_vol)] , "vol", sep = "_")

# Merging clean datasets
all_mwf_data_clean <- left_join(mwf_wide_mean, mwf_wide_sd, by = "id") %>% 
  left_join(., mwf_wide_vol, by = "id")

#------------------------------------------------------------------#
# Merging final gait, MWF data, and wmh                          ####
#------------------------------------------------------------------#

# Merging gait,  myelin and structural final datasets ####
# eicv data — computed separately to increase N
rvci_eicv <- read.xlsx("./data/rvci_eicv.xlsx") %>% 
  rename(id = ID) 

cogmob_eicv <- read.xlsx("./data/cogmob_eicv.xlsx") %>% 
  rename(id = ID)

all_eicv <- rbind(cogmob_eicv, rvci_eicv) %>% 
  mutate(eicv_cm3 = eicv/1000)

# Fazekas score
cogmob_wmh <- cogmob_wmh %>% 
  rename(id = ID, wmh = Total.Vol, fazekas_score = Fazekas.Score) %>% 
  mutate(id = str_replace(id, "Cogmob2_", "FALLERS2_"))

rvci_wmh <- rvci_wmh %>% 
  rename(id = ID, wmh = Total.Vol, fazekas_score = Fazekas.from.Baseline.MRI)

all_wmh <- rbind(cogmob_wmh, rvci_wmh) %>% 
  rename_all(tolower) %>% 
  mutate(wmh_cm3 = wmh/1000) %>% 
  select(id, wmh, wmh_cm3, fazekas_score)

# Merging
all_data_clean <- left_join(all_data_demographics, all_wmh, by="id") %>% 
  left_join(., all_mwf_data_clean, by = "id") %>% 
  left_join(., all_eicv, by = "id")

# Checking observations for duplicates
tableone::CreateTableOne(data = all_data_clean, "id")

# Saving 
write.xlsx(all_data_clean, "all_data_clean.xlsx")


summary(all_data_clean)
