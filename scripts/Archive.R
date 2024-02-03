pacman::p_load(
  ggplot,      # plot charts
  ggpubr,       # textual tables
  gridExtra
  
)


source("scripts/clean.R")


#Time to plot data!---------------


#hist(ocd_dataset$age_years)
#plot(ocd_dataset$ethnicity,ocd_dataset$depr_diag)

# typeof(ocd_dataset$age_cat)
# 
# class(ocd_dataset$age_cat)
# 
# table(ocd_dataset$age_cat)
# 
# agedist <- table(ocd_dataset$age_cat)
# barplot(agedist)
# ethnic <- table(ocd_dataset$ethnicity)
# barplot(ethnic)

#Grouping data------------------------------------------

#grouping the data by education
ocd_by_education <- clean_ocd_dataset %>%
  group_by(education) %>%
  tally()
ggplot(ocd_by_education, aes(x=education, y=n))+ geom_col()

# #grouping data by education and obsession, compulsion category and diagnosis 
ocd_by_education <- clean_ocd_dataset %>%
  group_by(education, obs_cat) %>%
  tally()
ggplot(ocd_by_education, aes(x=education, y=n, fill=obs_cat))+ geom_col()

ocd_by_education <- clean_ocd_dataset %>%
  group_by(education, comp_cat) %>%
  tally()
ggplot(ocd_by_education, aes(x=education, y=n, fill=comp_cat))+ geom_col()
#   
ocd_by_education <- clean_ocd_dataset %>%
  group_by(education, anx_diag) %>%
  tally()
ggplot(ocd_by_education, aes(x=education, y=n, fill=anx_diag))+ geom_col()

ocd_by_education <- clean_ocd_dataset %>%
  group_by(education, depr_diag) %>%
  tally()
ggplot(ocd_by_education, aes(x=education, y=n, fill=depr_diag))+ geom_col()

#grouping data by gender
#grouping data by gender vs obsession and compulsion category
ocd_by_gender <- clean_ocd_dataset %>%
  group_by(gender) %>%
  tally()
ggplot(ocd_by_gender, aes(x=gender, y=n))+ geom_col()

ocd_by_gender <- clean_ocd_dataset %>%
  group_by(gender, obs_cat) %>%
  tally()
ggplot(ocd_by_gender, aes(x=gender, y=n, fill=obs_cat))+ geom_col()

ocd_by_gender <- clean_ocd_dataset %>%
  group_by(gender, comp_cat) %>%
  tally()
ggplot(ocd_by_gender, aes(x=gender, y=n, fill=comp_cat))+ geom_col()

ocd_by_gender <- clean_ocd_dataset %>%
  group_by(gender, anx_diag) %>%
  tally()
ggplot(ocd_by_gender, aes(x=gender, y=n, fill=anx_diag))+ geom_col() 

ocd_by_gender <- clean_ocd_dataset %>%
  group_by(gender, depr_diag) %>%
  tally()
ggplot(ocd_by_gender, aes(x=gender, y=n, fill=depr_diag))+ geom_col()

#grouping by ethnicity  
ocd_by_ethnicity <- clean_ocd_dataset %>%
  group_by(ethnicity, obs_cat) %>%
  tally()
ggplot(ocd_by_ethnicity, aes(x=ethnicity, y=n, fill=obs_cat))+ geom_col()

ocd_by_ethnicity <- clean_ocd_dataset %>%
  group_by(ethnicity, age_cat) %>%
  tally()
ggplot(ocd_by_ethnicity, aes(x=ethnicity, y=n, fill=age_cat))+ geom_col()

#un-group grouped data
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

# Summary statistics on grouped data
# summarizing mean, max and min age for ethnicity
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



clean_ocd_dataset %>%   
  tabyl(age_cat, gender) %>%                     # cross-tabulate counts of two columns
  adorn_totals(where = "both") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%   # convert to proportions with column denominator
  adorn_pct_formatting()  %>%                  # convert proportions to percents
  adorn_ns(position = "front") %>%             # display as: "count (percent)"
  adorn_title(                                 # adjust titles
    row_name = "Age Category",
    col_name = "Gender")

#group buy age
# age_classes <- data |> 
# group_by(
#   age_class = ifelse(adult, "adult", "child")) |> 
#   tally(sort = T)

# ggplot(age_classes, aes(x=age_class, y=n)) + geom_col()





#create text table for gender and age variables 
age_table <- clean_ocd_dataset %>%
  tabyl(age_cat, gender) %>%
  adorn_totals(where = "both") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting()  %>%
  adorn_ns(position = "front") %>%
  adorn_title(
    row_name = "Age Category",
    col_name = "Gender")

# ggtexttable(age_table)
plot(tableGrob(age_table))
