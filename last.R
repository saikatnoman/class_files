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

updated_data = na.omit(data_import) %>%
  mutate(
    age_category = case_when(
      Age < 18 ~ "Young",
      Age >= 18 & Age < 35 ~ "Adult",
      Age >= 35 & Age < 55 ~ "Middle Age",
      Age >= 55 ~ "Old"
    ),
    income_category = case_when(
      Total_income < 20000 ~ "Low",
      Total_income >= 20000 & Total_income < 35000 ~ "Medium Low",
      Total_income >= 35000 & Total_income < 50000 ~ "Medium",
      Total_income >= 50000 & Total_income < 70000 ~ "Medium High",
      Total_income >= 70000 & Total_income < 90000 ~ "High",
      Total_income >= 90000 ~ "Very High"
    )
  ) %>%
  mutate(
    Education_type = fct_recode(Education_type, "Secondary or secondary special" = "Secondary / secondary special"),
    Family_status = fct_recode(Family_status, "Single or not married" = "Single / not married"),
    Housing_type = fct_recode(Housing_type, "House or apartment" = "House / apartment")
  ) %>%
  mutate(
    education_type = fct_collapse(Education_type,
                                  "Higher Education" = c("Higher education", "Incomplete higher", "Academic degree"),
                                  "Secondary Education" = c("Secondary or secondary special", "Lower secondary")),
    
    family_status = fct_collapse(Family_status,
                                 "Married" = c("Civil marriage", "Married"),
                                 "Single" = "Single or not married",
                                 "Separated" = "Separated",
                                 "Widowed" = "Widow"),
    employment_status = fct_collapse(Income_type,
                                     "Employed" = c("Working", "Commercial associate", "State servant"),
                                     "Pensioner" = "Pensioner",
                                     "Student" = "Student"),
    housing_type = fct_collapse(Housing_type,
                                "Owned" = c("House or apartment", "Co-op apartment"),
                                "Rented" = c("Rented apartment", "Municipal apartment"),
                                "With parents" = "With parents",
                                "Other" = "Office apartment"))%>%
  mutate_at(vars(education_type, family_status, housing_type), as.factor)%>%

group_by(income_category) %>%
  summarize(avg_income_by_income_category = mean(Total_income, na.rm = TRUE), .groups = "drop") %>%
  group_by(age_category) %>%
  summarize(avg_income_by_age_category = mean(Total_income, na.rm = TRUE), .groups = "drop") %>%
  group_by(family_type) %>%
  summarize(avg_income_by_family_status = mean(Total_income, na.rm = TRUE), .groups = "drop")

str(data_import)
str(updated_data)
