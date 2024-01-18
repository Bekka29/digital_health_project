pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse   # data management and visualization
)

#importing the data and editing datatypes 
ocd_dataset <- import(here('data','ocd_dataset.csv'))
library(readr)
ocd_dataset <- read_csv(here("data","ocd_dataset.csv"), 
                        col_types = cols(`Patient ID` = col_character(), `Age` = col_integer(), `Gender` = col_factor(levels = c("Female", "Male")), 
                                         `Ethnicity` = col_factor(levels = c("African", "Asian", "Caucasian", "Hispanic")), `Marital Status` = col_factor(levels = c("Single", "Married", "Divorced")), 
                                         `Education Level` = col_factor(levels = c("High School","Some College", "College Degree","Graduate Degree")), `OCD Diagnosis Date` = col_date(format = "%m/%d/%Y"), 
                                         `Duration of Symptoms (months)` = col_skip(), 
                                         `Previous Diagnoses` = col_factor(levels = c("None","GAD", "MDD", "Panic Disorder","PTSD")), `Family History of OCD` = col_factor(levels = c("Yes", "No")), 
                                         `Obsession Type` = col_factor(levels = c("Contamination","Harm-related", "Hoarding", "Religious","Symmetry")), 
                                         `Compulsion Type` = col_factor(levels = c("Checking", "Counting", "Ordering", "Praying","Washing")), 
                                         `Y-BOCS Score (Obsessions)` = col_integer(),`Y-BOCS Score (Compulsions)` = col_integer(), `Depression Diagnosis` = col_factor(levels = c("Yes","No")), 
                                         `Anxiety Diagnosis` = col_factor(levels = c("Yes","No")), `Medications` = col_factor(levels = c("None", "Benzodiazepine", "SNRI", "SSRI"))))

head(ocd_dataset)
colnames(ocd_dataset)
#manually renaming the columns:
colnames(ocd_dataset) <- c("patient_id", "age_years","gender","ethnicity","marital_status","education","diag_date","prev_diag","family_hist","obs_type","comp_type","y_bocs_obs","y_bocs_comp","depr_diag","anx_ diag","medication")
head(ocd_dataset)
View(ocd_dataset)
#data exploration
summary(ocd_dataset['age_years'])
tabyl(ocd_dataset$'ethnicity')
tabyl(ocd_dataset$'gender')
clean_ocd_dataset <- ocd_dataset %>%
  clean_names()
glimpse(clean_ocd_dataset)
tabyl(ocd_dataset$'ethnicity')
tabyl(ocd_dataset$'ethnicity')



clean_names()