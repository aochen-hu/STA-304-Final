---
  title: "Final-survey data cleaning"
output: html_document
---
  
library(haven)
library(tidyverse)
library(dplyr)
set.seed(8996)
setwd("/Users/aochenhu/Desktop/STA 304 FInal")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data_voterstudy <- read_dta("2019 Canadian Election Study - Online Survey v1.0.dta")
# Add the labels
raw_data_voterstudy <- labelled::to_factor(raw_data_voterstudy)
# Just keep some variables
reduced_data_voterstudy <- 
  raw_data_voterstudy %>% 
  select(cps19_votechoice,
         cps19_province,
         cps19_yob,
         cps19_v_likely,
         cps19_fed_gov_sat,
         cps19_citizenship,
         cps19_gender,
         cps19_education,
         cps19_employment)

reduced_data_voterstudy<-
  reduced_data_voterstudy %>%
  mutate(vote_liberal = 
           ifelse(cps19_votechoice=="Liberal Party", 1, 0))


reduced_data_voterstudy <- reduced_data_voterstudy %>% 
  mutate(age_grp = 
           ifelse(as.numeric(as.character(cps19_yob)) >= 1985, "25 or less",
                  ifelse(as.numeric(as.character(cps19_yob)) < 1985 & as.numeric(as.character(cps19_yob)) >= 1970, "26 to 40",
                         ifelse(as.numeric(as.character(cps19_yob)) < 1970 & as.numeric(as.character(cps19_yob)) >= 1955, "41 to 55",
                                ifelse(as.numeric(as.character(cps19_yob)) < 1955 & as.numeric(as.character(cps19_yob)) >= 1940, "56 to 70", "above 70")))))

reduced_data_voterstudy <-
  reduced_data_voterstudy %>% 
  mutate(gender = 
           ifelse(cps19_gender == "A woman", "Female", "Male"))

reduced_data_voterstudy <-
  reduced_data_voterstudy %>% 
  mutate(province = cps19_province)


reduced_data_voterstudy <- na.omit(reduced_data_voterstudy)

reduced_data_voterstudy<-
  reduced_data_voterstudy %>%
  mutate(new_emp = 
           ifelse(cps19_employment == "Working for pay full-time" | cps19_employment == "Working for pay part-time" | cps19_employment == "Self employed (with or without employees)","employed", ifelse(cps19_employment == "Unemployed/ looking for work", "unemployed", "not in labor force")))

reduced_data_voterstudy<-
  reduced_data_voterstudy %>%
  mutate(new_educ = 
           ifelse(cps19_education == "Bachelor's degree" | cps19_education == "Master's degree" | cps19_education == "Professional degree or doctorate", "university degree", ifelse(cps19_education == "Completed secondary/ high school" | cps19_education == "Some university", "high school graduate", "under high school")))

reduced_data_voterstudy<-
  reduced_data_voterstudy %>%
  mutate(new_citizen = 
           ifelse(cps19_citizenship == "Canadian citizen", 
                  "canadian_citizen", ifelse(cps19_citizenship == "Permanent resident", 
                                             "p_r", "other")))

reduced_data_voterstudy <- reduced_data_voterstudy %>% 
  select(gender, province, vote_liberal, new_emp, new_educ, age_grp)

write_csv(reduced_data_voterstudy, "survey_data.csv")


