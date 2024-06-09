getwd()
setwd("E:/R Jaman Sir")

install.packages("tidyverse")
library(tidyverse)
library(dplyr)

install.packages("forcats")
library(forcats)

data_import = read.csv(file.choose())
attach(data_import)
str(data_import)
names(data_import)
dim(data_import)

dim(data_import[!complete.cases(data_import),])

unique(updated_data$employment_status)

updated_data = data_import %>%
  na.omit() %>%
  mutate(age_category = case_when(
    data_import$Age < 18 ~ "Young",
    data_import$Age >= 18 & data_import$Age < 35 ~ "Adult",
    data_import$Age >= 35 & data_import$Age < 55 ~ "Middle Age",
    data_import$Age >= 55 ~ "Old")) %>%
  mutate(income_category = case_when(
    data_import$Total_income < 20000 ~ "Low",
    data_import$Total_income >= 20000 & data_import$Total_income < 35000 ~ "Medium Low",
    data_import$Total_income >= 35000 & data_import$Total_income < 50000 ~ "Medium",
    data_import$Total_income >= 50000 & data_import$Total_income < 70000 ~ "Medium High",
    data_import$Total_income >= 70000 & data_import$Total_income < 90000 ~ "High",
    data_import$Total_income >= 90000 ~ "Very High")) %>%
  mutate(Education_type = fct_recode(Education_type, "Secondary or secondary special" = "Secondary / secondary special"))%>%
  mutate(Family_status = fct_recode(Family_status, "Single or not married" = "Single / not married"))%>%
  mutate(Housing_type = fct_recode(Housing_type, "House or apartment" = "House / apartment")) %>%
  
                               
  mutate(education_type = fct_collapse(updated_data$Education_type,
                                       "Higher Education" = c("Higher education", "Incomplete higher", "Academic degree"),
                                       "Secondary Education" = c("Secondary or secondary special", "Lower secondary"))) %>%
  
  mutate(family_status = fct_collapse(updated_data$Family_status,
                                      "Married" = c("Civil marriage", "Married"),
                                      "Single" = "Single or not married",
                                      "Separated" = "Separated",
                                      "Widowed" = "Widow")) %>%
  mutate(employment_status = fct_collapse(updated_data$Income_type,
                                          "Employed" = c("Working", "Commercial associate", "State servant"),
                                          "Pensioner" = "Pensioner",
                                          "Student" = "Student")) %>%
  mutate(housing_type = fct_collapse(updated_data$Housing_type,
                                "Owned" = c("House or apartment", "Co-op apartment"),
                                "Rented" = c("Rented apartment", "Municipal apartment"),
                                "With parents" = "With parents",
                                "Other" = "Office apartment")) %>%
  
  
  data = data_import %>%
  na.omit() %>%
  mutate(age_category = case_when(
    Age < 30 ~ "Young",
    Age >= 30 & Age < 50 ~ "Adult",
    Age >= 50 & Age < 65 ~ "Middle Age",
    Age >= 65 ~ "Old"
  )) %>%
  mutate(income_category = case_when(
    Total_income < 20000 ~ "Low",
    Total_income >= 20000 & Total_income < 35000 ~ "Medium Low",
    Total_income >= 35000 & Total_income < 50000 ~ "Medium",
    Total_income >= 50000 & Total_income < 70000 ~ "Medium High",
    Total_income >= 70000 & Total_income < 90000 ~ "High",
    Total_income >= 90000 ~ "Very High"
  )) %>%
  mutate(education_type = fct_collapse(data_import$Education_type,
                                       "Higher Education" = c("Higher education", "Incomplete higher", "Academic degree"),
                                       "Secondary Education" = c("Secondary / secondary special", "Lower secondary"))) %>%
  
  mutate(family_type = fct_collapse(data_import$Family_status,
                                      "Married" = c("Civil marriage", "Married"),
                                      "Single" = "Single / not married",
                                      "Separated" = "Separated",
                                      "Widowed" = "Widow")) %>%
  mutate(employment_status = fct_collapse(data_import$Income_type,
                                          "Employed" = c("Working", "Commercial associate", "State servant"),
                                          "Pensioner" = "Pensioner",
                                          "Student" = "Student")) %>%
  mutate(housing_type = fct_collapse(data_import$Housing_type,
                                     "Owned" = c("House / apartment", "Co-op apartment"),
                                     "Rented" = c("Rented apartment", "Municipal apartment"),
                                     "With parents" = "With parents",
                                     "Other" = "Office apartment")) %>%
  mutate(
    family_type = as_factor(family_type),
    housing_type = as_factor(housing_type),
    education_type = as_factor(education_type)
  ) %>%
  average_income_by_income <- data %>%
  group_by(income_category) %>%
  summarize(average_income = mean(Total_income, na.rm = TRUE)) %>%
  average_income_by_age <- data %>%
  group_by(age_category) %>%
  summarize(average_incomeage = mean(Total_income, na.rm = TRUE)) %>%
  average_income_by_family <- data %>%
  group_by(family_type) %>%
  summarize(average_incomefamily = mean(Total_income, na.rm = TRUE))
 
  
  
names(updated_data)
unique(updated_data$income_category)
unique(data_import$Education_type)
unique(updated_data$education_type)
unique(data_import$Family_status)
unique(updated_data$family_status)
unique(data_import$Income_type)
unique(updated_data$employment_status)
unique(data_import$Housing_type)
unique(updated_data$housing_type)


 
  
names(updated_data)

dim(updated_data[!complete.cases(updated_data),])
