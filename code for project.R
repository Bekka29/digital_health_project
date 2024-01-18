pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse   # data management and visualization
)

ocd_data_raw <- import("~/bekkys deggendorf docs 2022/Third semester/digital health WS23/R_INTRO/ocd patient dataset.csv")
head(ocd_data_raw, n = 3)
janitor::clean_names(ocd_data_raw) #trying to clean the column names
colnames(ocd_data_raw)
#manually renaming the columns:
colnames(ocd_data_raw) <- c("age","gender","ethnicity","marital_status","education","prev_diag","family_hist","obs_type","comp_type","y_bocs_obs","y_bocs_comp","depr_diag","anx_ diag","medication")
colnames(ocd_data_raw) # checking for change
#exploring dataset datatypes
typeof(ocd_data_raw) 
typeof(ocd_data_raw$family_hist)
typeof(ocd_data_raw$y_bocs_comp) 

install.packages("readxl")
library(readxl)
patient_id <- import("~/bekkys deggendorf docs 2022/Third semester/digital health WS23/patient_id.xlsx") 
ocd_dataset <- cbind(ocd_data_raw, patient_id)
colnames(ocd_dataset)
colnames(ocd_dataset) <- c("age","gender","ethnicity","marital_status","education","prev_diag","family_hist","obs_type","comp_type","y_bocs_obs","y_bocs_comp","depr_diag","anx_diag","medication","patient_id") 

ocd_dataset[,c("age","gender","ethnicity","marital_status","education","prev_diag","family_hist","obs_type","comp_type","y_bocs_obs","y_bocs_comp","depr_diag","anx_diag","medication","patient_id")]
ocd_dataset[,c("patient_id","age","gender","ethnicity","marital_status","education","prev_diag","family_hist","obs_type","comp_type","y_bocs_obs","y_bocs_comp","depr_diag","anx_diag","medication")]



