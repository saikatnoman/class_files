getwd()
setwd("E:/R Jaman Sir")



list_data = list(name=c("x","y","z"), age=c(20,30,40))
list_frame = data.frame(list_data)
apply(list_frame[-1],2,mean)
apply(list_frame[-1],2,sum)

list_data_1=list(name=c("a","b","c"), age=c(10,20,30))
list_frame_1=data.frame(list_data_1)
apply(list_frame_1[-1],2,sum)
apply(list_frame_1[-1],2,mean)


install.packages("pacman")
library(pacman)
pacman::p_load(dplyr,tidyverse)


data("iris")
data_iris=iris
head(data_iris,5)

unique(data_iris$Species)

colnames(data_iris)

mean_Sepal_length =mean(data_iris$Sepal.Length)

tapply(data_iris$Sepal.Length, data_iris$Species, mean)

var_sepal_length = var(data_iris$Sepal.Length)


#tidyverse starts here

group_data = data_iris %>% 
  group_by(data_iris$Species) %>%
  summarise(mean_sl=mean(data_iris$Sepal.Length),
            mean_sw=mean(data_iris$Sepal.Width),
            mean_pl=mean(data_iris$Petal.Length),
            mean_pw=mean(data_iris$Petal.Width),
            var_sl=var(data_iris$Sepal.Length),
            var_sw=var(data_iris$Sepal.Width),
            var_pl=var(data_iris$Petal.Length),
            var_pw=var(data_iris$Petal.Width),
            sum_sl=sum(data_iris$Sepal.Length),
            sum_sw=sum(data_iris$Sepal.Width),
            sum_pl=sum(data_iris$Petal.Length),
            sum_pw=sum(data_iris$Petal.Width),)



data_5 = data.frame(group_data)
write.csv(data_5, file = "data_5.csv", row.names = FALSE)


subset_iris = data_iris %>%
  dplyr::select(c(Sepal.Length, Petal.Length, Species))

filtere_data= data_iris %>%
  dplyr::filter(data_iris$Sepal.Length>=5.0)
  
multi_filtering = data_iris %>%
  dplyr::filter(data_iris$Sepal.Length>= 5.0 & data_iris$Petal.Length >=3.0)

filtering_2 = data_iris %>%
  dplyr::filter(between(data_iris$Sepal.Length,4,5))

dropping_column = data_iris%>%
  dplyr::select(-Sepal.Length)

drop_column = data_iris%>%
  dplyr::select(-c(Petal.Length,Sepal.Width))

data_4 = data_iris %>%
  mutate(ratio_1 = data_iris$Sepal.Length/data_iris$Petal.Length,
         ratio_2 = data_iris$Sepal.Width/data_iris$Petal.Width)
