#Install Packages----------------------------------

pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways 
  dplyr,      # enables data manipulation
  epikit,     # age_categories() function
  forcats,    # enables handling of categorical variables
  readr,      # reads rectangular data like csv
  styler,     # formats code according to style guide and maintains style consistency
  tidyverse,  # data management and visualization
  janitor     # data cleaning and tables
  )


#Import Data and Edit datatypes---------------------

ocd_dataset <- read_csv(
                        here("ocd_dataset.csv"),
                        #name_repair = "universal",
                           col_types = cols(`Patient ID` = col_character(),
                                            `Age` = col_integer(),
                                            `Gender` = col_factor(levels = c("Female", "Male")),
                                            `Ethnicity` = col_factor(levels = c("African", "Asian", "Caucasian", "Hispanic")),
                                            `Marital Status` = col_factor(levels = c("Single", "Married", "Divorced")),
                                            `Education Level` = col_factor(levels = c("High School","Some College", "College Degree","Graduate Degree")),
                                            `OCD Diagnosis Date` = col_date(format = "%m/%d/%Y"),
                                            `Duration of Symptoms (months)` = col_skip(),
                                            `Previous Diagnoses` = col_skip(),          
                                            `Family History of OCD` = col_factor(levels = c("Yes", "No")),
                                            `Obsession Type` = col_factor(levels = c("Contamination","Harm-related", "Hoarding", "Religious","Symmetry")),
                                            `Compulsion Type` = col_factor(levels = c("Checking", "Counting", "Ordering", "Praying","Washing")),
                                            `Y-BOCS Score (Obsessions)` = col_integer(),
                                            `Y-BOCS Score (Compulsions)` = col_integer(),
                                            `Depression Diagnosis` = col_factor(levels = c("Yes","No")),
                                            `Anxiety Diagnosis` = col_factor(levels = c("Yes","No")),
                                            `Medications` = col_factor(levels = c("None", "Benzodiazepine", "SNRI", "SSRI"))
                                           ),
)

  

#Data Transformation-----------------------------
  
  
#Create column groups to change type after mutation

 cols_log=c( "family_hist", "depr_diag","anx_diag")
 cols_fac=c("gender","ethnicity","marital_status","education","obs_type","comps_type","medications")
 cols_int=c("age_years","y_bocs_obs","y_bocs_comps")
 cols_date=c("diag_date")
  
clean_ocd_dataset <- ocd_dataset  %>%
  clean_names() %>%    

  
#View(clean_ocd_dataset)

#Rename the columns
rename(age_years = age,
    education = education_level,
    diag_date = ocd_diagnosis_date,
    family_hist= family_history_of_ocd,
    obs_type = obsession_type,
    comps_type = compulsion_type,
    y_bocs_obs = y_bocs_score_obsessions,
    y_bocs_comps = y_bocs_score_compulsions,
    depr_diag =  depression_diagnosis,
    anx_diag = anxiety_diagnosis)  %>%          

#Mutate the data types and create new categories  
mutate(
    across(cols_log, ~ as.logical(.x == "Yes")),
    
    #Create age categories
    age_cat = age_categories(age_years,
                           lower = 15,
                           upper = 70,
                           by = 5),
    #Create categories for disorder severity
    obs_cat = case_when(
      y_bocs_obs <= 7                    ~ "subclinical",
      y_bocs_obs > 7  & y_bocs_obs <= 15 ~ "mild",
      y_bocs_obs > 15 & y_bocs_obs <= 23 ~ "moderate",
      y_bocs_obs > 23 & y_bocs_obs <= 31 ~ "severe",
      y_bocs_obs > 31                    ~ "extreme"
     ),
    comp_cat = case_when(
      y_bocs_comps <= 7                      ~ "subclinical",
      y_bocs_comps > 7  & y_bocs_comps <= 15 ~ "mild",
      y_bocs_comps > 15 & y_bocs_comps <= 23 ~ "moderate",
      y_bocs_comps > 23 & y_bocs_comps <= 31 ~ "severe",
      y_bocs_comps > 31                      ~ "extreme"
      ),


    across(cols_fac, as.factor),

    across(cols_int, as.integer),

    across(cols_date, as.Date),

     )

#Convert new columns into factors
   clean_ocd_dataset$comp_cat <- as.factor(clean_ocd_dataset$comp_cat)
   clean_ocd_dataset$obs_cat <- as.factor (clean_ocd_dataset$obs_cat)


 glimpse(clean_ocd_dataset)


   







