getwd()
setwd("E:/R Jaman Sir")


install.packages("tidyverse")
library(tidyverse)
library(dplyr)

table1
table2
table3
table4a
table4b


table1 %>%
  mutate(percent_cases = cases/population*100)

long_data = table4b %>%
  pivot_longer (cols = "1999":"2000",
                values_to = "value",
                names_to = "year")
              
wide_data = long_data %>%
  pivot_wider (id_cols = "country",
                names_from = "year",
                values_from = "value")

long_data = table4b %>%
  pivot_longer (cols = "1999":"2000",
                values_to = "value",
                names_to = "year") %>%
  arrange(desc(year))


data("iris")
data_iris=iris

group_result = data_iris %>%
  group_by(Species)%>%
  summarise(across(where(is.numeric),~ mean(., na.rm = TRUE)))

data("warpbreaks")
data_warp=warpbreaks

group_warp = data_warp %>%
  group_by(wool, tension) %>%
  summarise(across(where(is.numeric), ~ var(., na.rm = TRUE)))

group_warp_1 = data_warp %>%
  group_by(wool, tension) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)))

table1

data_count = table1 %>%
  count(year, wt = cases)

data_3 = table4a %>%
  gather ("1999","2000", key = "year", value = "cases")
data_4 = table4b %>%
  gather ("1999","2000", key = "year", value = "population")
data_5 = left_join (data_3, data_4)

data_6 = table2 %>%
  spread(key = type, value =count)

data_wide_2 = long_data %>%
  spread(key = year, value = value)