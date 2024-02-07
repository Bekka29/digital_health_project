#Install Packages-------------------------
pacman::p_load(
  ggplot,      # plot charts
  ggpubr,      # textual tables
  gridExtra    # high level functions for grid graphic objects
  
)


source("scripts/clean.R")


#Time to analyze data!

#Grouping data------------------------------------------

#Grouping the data by education
ocd_by_education <- clean_ocd_dataset %>%
  group_by(education) %>%
  tally()
ggplot(ocd_by_education, aes(x=education, y=n))+ geom_col()

#Grouping data by education and other variables
##
ocd_by_education <- clean_ocd_dataset %>%
  group_by(education, obs_cat) %>%
  tally()
ggplot(ocd_by_education, aes(x=education, y=n, fill=obs_cat))+ geom_col()

##
ocd_by_education <- clean_ocd_dataset %>%
  group_by(education, comp_cat) %>%
  tally()
ggplot(ocd_by_education, aes(x=education, y=n, fill=comp_cat))+ geom_col()

##   
ocd_by_education <- clean_ocd_dataset %>%
  group_by(education, anx_diag) %>%
  tally()
ggplot(ocd_by_education, aes(x=education, y=n, fill=anx_diag))+ geom_col()

##
ocd_by_education <- clean_ocd_dataset %>%
  group_by(education, depr_diag) %>%
  tally()
ggplot(ocd_by_education, aes(x=education, y=n, fill=depr_diag))+ geom_col()

#Grouping data by gender
ocd_by_gender <- clean_ocd_dataset %>%
  group_by(gender) %>%
  tally()
ggplot(ocd_by_gender, aes(x=gender, y=n))+ geom_col()

#grouping data by gender and other variables
##
ocd_by_gender <- clean_ocd_dataset %>%
  group_by(gender, obs_cat) %>%
  tally()
ggplot(ocd_by_gender, aes(x=gender, y=n, fill=obs_cat))+ geom_col()

##
ocd_by_gender <- clean_ocd_dataset %>%
  group_by(gender, comp_cat) %>%
  tally()
ggplot(ocd_by_gender, aes(x=gender, y=n, fill=comp_cat))+ geom_col()

##
ocd_by_gender <- clean_ocd_dataset %>%
  group_by(gender, anx_diag) %>%
  tally()
ggplot(ocd_by_gender, aes(x=gender, y=n, fill=anx_diag))+ geom_col() 

##
ocd_by_gender <- clean_ocd_dataset %>%
  group_by(gender, depr_diag) %>%
  tally()
ggplot(ocd_by_gender, aes(x=gender, y=n, fill=depr_diag))+ geom_col()

#Grouping by ethnicity  
ocd_by_ethnicity <- clean_ocd_dataset %>%
  group_by(ethnicity, obs_cat) %>%
  tally()
ggplot(ocd_by_ethnicity, aes(x=ethnicity, y=n, fill=obs_cat))+ geom_col()

#Grouping by ethnicity and other variables
ocd_by_ethnicity <- clean_ocd_dataset %>%
  group_by(ethnicity, age_cat) %>%
  tally()
ggplot(ocd_by_ethnicity, aes(x=ethnicity, y=n, fill=age_cat))+ geom_col()

#Un-group grouped data
clean_ocd_dataset          %>%
  group_by(education, obs_cat) %>%
  tally()              %>%
  ungroup(education)

clean_ocd_dataset          %>%
  group_by(gender, obs_cat) %>%
  tally()              %>%
  ungroup(gender)

clean_ocd_dataset          %>%
  group_by(ethnicity, obs_cat) %>%
  tally()              %>%
  ungroup(ethnicity)

# Summary statistics on grouped data----------------

# Summarizing mean, max and min age for ethnicity
clean_ocd_dataset %>% 
  group_by(ethnicity) %>% 
  summarise(
    n_cases  = n(),
    mean_age = mean(age_years),
    max_age  = max(age_years),
    min_age  = min(age_years),
  )  

#summarizing mean, max and min values for gender
clean_ocd_dataset %>% 
  group_by(ethnicity) %>% 
  summarise(
    n_cases  = n(),
    mean_age = mean(age_years),
    max_age  = max(age_years),
    min_age  = min(age_years),
    n_males    = sum(gender == "male"))

#summarizing marital status
clean_ocd_dataset %>%
  count(marital_status)

#summarizing age categories by gender
clean_ocd_dataset %>%
  tabyl(age_cat, gender)%>%
  adorn_totals(where = c("row"))


#Create text table for gender and age variables 
age_table <- clean_ocd_dataset %>%
  tabyl(age_cat, gender) %>%                      # cross-tabulate counts of two columns
  adorn_totals(where = "both") %>%                # add a total row
  adorn_percentages(denominator = "col") %>%      # convert to proportions with column denominator
  adorn_pct_formatting()  %>%                     # convert proportions to percents
  adorn_ns(position = "front") %>%                # display as: "count (percent)"
  adorn_title(                                     # adjust titles
    row_name = "Age Category",
    col_name = "Gender")

plot(tableGrob(age_table))

#Create a box plot for YBOCS scores
boxplot(clean_ocd_dataset$y_bocs_obs)

##
boxplot(clean_ocd_dataset$y_bocs_obs,
        main = "y-bocs score",
        xlab =  "score",
        ylab = "y_bocs_score",
        col = "orange",
        border = "brown")

##
y_bocs_obs <- clean_ocd_dataset$y_bocs_obs
y_bocs_comps <- clean_ocd_dataset$y_bocs_comps
boxplot(y_bocs_obs, y_bocs_comps,
        main = "Y-BOCS for Obsession and Compulsion",
        at = c(1, 4),
        names = c("y_bocs_obs", "y_bocs_comps"),
        las = 2,
        col = c("orange", "red"),
        border = "brown",
        horizontal = TRUE)



