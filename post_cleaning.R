---
  title: "post-stra-cleaning"
output: html_document
---
  

library(haven)
library(tidyverse)
library(labelled)
set.seed(8996)
# Read in the raw data.
setwd("/Users/aochenhu/Desktop/STA 304 FInal")
raw_data_post <- read_csv("gss.csv")


# Add the labels
raw_data_post <- labelled::to_factor(raw_data_post)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data_post <- 
  raw_data_post %>% 
  select(age,
         sex, 
         province, 
         education, 
         occupation,
         citizenship_status)


#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

reduced_data_post$age <- as.numeric(reduced_data_post$age)


reduced_data_post <-
  reduced_data_post %>%
  mutate(new_emp = 
           ifelse(occupation == "Sales and service occupations" | occupation == "Trades, transport and equipment operators and related oc..." | occupation == "Business, finance, and administration occupations" | occupation == "Management occupations" | occupation == "Trades, transport and equipment operators and related oc..." | occupation == "Natural and applied sciences and related occupations" | occupation == "Occupations in education, law and social, community and ..." | occupation == "Natural resources, agriculture and related production oc..." | occupation == "Occupations in art, culture, recreation and sport" | occupation == "Health occupations" | occupation == "Occupations in manufacturing and utilities", "employed","unemployed"))

reduced_data_post <- na.omit(reduced_data_post)

reduced_data_post <- reduced_data_post %>% 
  mutate(age_grp = 
           ifelse(age <= 25, "25 or less",
                  ifelse(age > 25 & age <= 40, "26 to 40",
                         ifelse(age > 40 & age <= 55, "41 to 55",
                                ifelse(age > 55 & age <= 70, "56 to 70", "above 70")))))

reduced_data_post <-
  reduced_data_post %>% 
  mutate(gender = 
           ifelse(sex == "Female", "Female", "Male"))

reduced_data_post <-
  reduced_data_post %>%
  mutate(new_educ = 
           ifelse(education == "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)" | education == "College, CEGEP or other non-university certificate or di...", "university degree",
                  ifelse(education == "High school diploma or a high school equivalency certificate", "high school graduate", "under high school")))


reduced_data_post <- reduced_data_post %>% 
  select(province, gender, new_educ, new_emp, age_grp)

reduced_data_post <- 
  reduced_data_post %>%
  count(gender, province, new_educ, age_grp, new_emp) %>%
  group_by(gender, province, new_educ, new_emp, age_grp) 

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data_post, "census_data.csv")


