getwd()

#Question - 1

install.packages("ggplot2")
library(ggplot2)

data("diamonds")
data_diamonds=diamonds
head(data_diamonds,15)

#(a)

unique(data_diamonds$cut)
unique(data_diamonds$color)
unique(data_diamonds$clarity)

(b)

price_means = colMeans(data_diamonds["price"])
depth_means = colMeans(data_diamonds["depth"])
x_means = colMeans(data_diamonds["x"])

(c)

tapply(data_diamonds$price, data_diamonds$cut, mean)

(d)

tapply(data_diamonds$price, list(data_diamonds$cut,data_diamonds$color), mean)

#Question - 2
install.packages("pacman")
library(pacman)
pacman::p_load(dplyr,tidyverse)

(a)

filter_data= data_diamonds %>%
  dplyr::filter(data_diamonds$cut=="Very Good")

(b)

filter_data_2 = data_diamonds %>%
  dplyr::filter(data_diamonds$cut=="Very Good" 
                & data_diamonds$cut=="Good"
                & data_diamonds$cut=="Premium")

(c)

filter_data_3= data_diamonds %>%
  dplyr::filter(data_diamonds$price>=335)

(d)

filter_data_3= data_diamonds %>%
  dplyr::filter(data_diamonds$price>=330 & data_diamonds$price <=335)

#Question - 03

(a)

tidymean_price = data_diamonds %>% 
  group_by(data_diamonds$cut) %>%
  summarise(tdprice_mean=mean(data_diamonds$price))

(b)

tidymean_price_1 = data_diamonds %>% 
  group_by(data_diamonds$color) %>%
  summarise(tdprice_mean=mean(data_diamonds$price))
            
            
(c)

tidymean_price_2 = data_diamonds %>% 
  group_by(data_diamonds$cut) %>%
  summarise(tdprice_mean=mean(data_diamonds$price),
            tdprice_var=var(data_diamonds$price),
            tdprice_max=max(data_diamonds$price),
            tdprice_min=min(data_diamonds$price))

(d)

group_result = data_diamonds %>%
  group_by(cut)%>%
  summarise(across(where(is.numeric),~ mean(., na.rm = TRUE)))

#Question - 5

(a)

subset_cu_tprice = data_diamonds %>%
  dplyr::select(c(cut,price))


subset_color_price = data_diamonds %>%
  dplyr::select(c(color,price))

subset_clarity_price = data_diamonds %>%
  dplyr::select(c(clarity,price))

(b)

wide_data_1 = data_diamonds %>%
  pivot_wider (id_cols = "id",
               names_from = "cut",
               values_from = "price")
