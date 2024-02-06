#Install Packages----------------------------------

pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways 
  dplyr,      # deployer
  epikit,     # age_categories() function
  forcats,    # function
  readr,      # 
  styler,     #
  tidyverse,   # data management and visualization
  janitor    # data cleaning and tables
  )


#Import Data and Edit datatypes---------------------

ocd_dataset <- read_csv(
                        here("data","ocd_dataset.csv"),
                        #name_repair = "universal",
                           col_types = cols(`Patient ID` = col_character(),
                                            `Age` = col_integer(),
                                            `Gender` = col_factor(levels = c("Female", "Male")),
                                            `Ethnicity` = col_factor(levels = c("African", "Asian", "Caucasian", "Hispanic")),
                                            `Marital Status` = col_factor(levels = c("Single", "Married", "Divorced")),
                                            `Education Level` = col_factor(levels = c("High School","Some College", "College Degree","Graduate Degree")),
                                            `OCD Diagnosis Date` = col_date(format = "%m/%d/%Y"),
                                            `Duration of Symptoms (months)` = col_skip(),
                                            `Previous Diagnoses` = col_skip(),           #col_factor(levels = c("None","GAD", "MDD", "Panic Disorder","PTSD")),
                                            `Family History of OCD` = col_skip(), #col_factor(levels = c("Yes", "No")),
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

 cols_log=c( "depr_diag","anx_diag")
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
    obs_type = obsession_type,
    comps_type = compulsion_type,
    y_bocs_obs = y_bocs_score_obsessions,
    y_bocs_comps = y_bocs_score_compulsions,
    depr_diag =  depression_diagnosis,
    anx_diag = anxiety_diagnosis)  %>%          

#Mutate the data types and create new categories  
mutate(
    across(cols_log, ~ as.logical(.x == "yes")),
    
    #Create age categories
    age_cat = age_categories(age_years,
                           lower = 15,
                           upper = 70,
                           by = 5),
    #Create categories for disorder severity
    obs_cat = case_when(
      y_bocs_obs <= 19                   ~ "mild",
      y_bocs_obs > 19 & y_bocs_obs <= 29 ~ "moderate",
      y_bocs_obs > 29                    ~ "severe"
     ),
    comp_cat = case_when(
      y_bocs_comps <= 19                    ~ "mild",
      y_bocs_comps > 19 & y_bocs_comps <= 29 ~ "moderate",
      y_bocs_comps > 29                     ~ "severe"),


    across(cols_fac, as.factor),

    across(cols_int, as.integer),

    across(cols_date, as.Date),

     )

#Convert new columns into factors
   clean_ocd_dataset$comp_cat <- as.factor(clean_ocd_dataset$comp_cat)
   clean_ocd_dataset$obs_cat <- as.factor (clean_ocd_dataset$obs_cat)


 glimpse(clean_ocd_dataset)


   







