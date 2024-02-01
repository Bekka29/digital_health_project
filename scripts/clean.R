#install packages----------------------------------
pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,   # data management and visualization
  
  )


#import the data and edit datatypes---------------- 
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
#manually rename the columns
colnames(ocd_dataset) <- c("patient_id", "age_years","gender","ethnicity","marital_status","education","diag_date","prev_diag","family_hist","obs_type","comp_type","y_bocs_obs","y_bocs_comp","depr_diag","anx_ diag","medication")
head(ocd_dataset)
#data exploration-----------------------------
View(ocd_dataset)
summary(ocd_dataset['age_years'])
tabyl(ocd_dataset$'gender')

#data transformation--------------------------

#create column groups to change type after mutation

cols_log=c("family_hist", "depr_diag","anx_diag")
cols_fac=c("gender","ethnicity","marital_status","education","prev_diag","obs_type","comp_type","medication")
cols_int=c("age_years","y_bocs_obs","y_bocs_comp")
cols_date=c("diag_date")

ocd_dataset <- ocd_dataset |>
  
  ###
  clean_names() |>
  
  ###
  # rename(date_onset = onset_date,
  #        date_report = date_of_report,
  #        district_res = adm3_name_res,
  #        district_det = adm3_name_det)  |>


mutate(
  across(cols_log, ~ as.logical(.x == "Yes")),
  across(2:8, tolower),
  across(10:11, tolower), 
  medication = tolower(medication),
  age_cat = age_categories(age_years,
                           lower = 15,
                           upper = 70,
                           by = 5),
  # Create categories for disorder severity
  obs_cat = dplyr::case_when(
      y_bocs_obs <= 19                   ~ "mild",
      y_bocs_obs > 19 & y_bocs_obs <= 29 ~ "moderate",
      y_bocs_obs > 29                    ~ "severe"
    ),
  comp_cat = dplyr::case_when(
    y_bocs_comp <= 19                    ~ "mild",
    y_bocs_comp > 19 & y_bocs_comp <= 29 ~ "moderate",
    y_bocs_comp > 29                     ~ "severe"),
  
  #cats_fac = c("obs_cat", "comp_cat"),
  # ocd_dataset$comp_cat <- as.factor(ocd_dataset$comp_cat),
  # ocd_dataset$obs_cat <- as.factor (ocd_dataset$obs_cat),
  # ocd_dataset$obs
  #comp_cat <- as.factor(ocd_dataset$comp_cat),
  
  across(cols_fac, as.factor),
  
  across(cols_int, as.integer),
  
  across(cols_date, as.Date),
 
)  

#convert new columns into factors
ocd_dataset$comp_cat <- as.factor(ocd_dataset$comp_cat)

ocd_dataset$obs_cat <- as.factor (ocd_dataset$obs_cat)

  
#ocd_dataset%>% tabyl(age_cat)
#ocd_dataset%>% tabyl(obs_cat)

#ocd_dataset%>% tabyl(comp_cat)
  
  
  
  glimpse(ocd_dataset)

#View(ocd_dataset)
  
  







