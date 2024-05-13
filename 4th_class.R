getwd()

setwd("E:/R Jaman Sir")

z = matrix(c(4,7,3,8,9,2), nrow = 3)

x = matrix (c(1,2,3,4,5,6,7,8,9), nrow = 3, ncol = 3)




#Altenatively

row_means = rowMeans(x)
row_sum = rowSums(x)
col_sum = colSums(x)
col_means = colMeans(x)  

y = matrix (c(1,2,3,4,5,6,7,8,9), nrow = 3, ncol = 3)
 data_1 = data.frame(y)
 colnames(data_1) = c("Column 1", "Column 20", "Column 3")
  
 row_means1 = rowMeans(data_1)
 row_sum1 = rowSums(data_1)
 col_sum1 = colSums(data_1)
 col_means1 = colMeans(data_1)
 
 apply(data_1,2,sum)
 apply(data_1,2,mean)
 apply(data_1,2,max)
 apply(data_1,2,min) 
 
 apply(data_1,1,sum)
 apply(data_1,1,mean)
 apply(data_1,1,max)
 apply(data_1,1,min)  
 
 
 data(warpbreaks)

 data_2 = warpbreaks
 
 head(data_2, 10)

 unique(data_2$tension) 
unique(data_2$breaks) 
unique(data_2$wool)

tapply(data_2$breaks, data_2$tension, sum)
tapply(data_2$breaks, data_2$tension, mean)


tapply(data_2$breaks, data_2$wool, sum)
tapply(data_2$breaks, data_2$wool, mean)

tapply (data_2$breaks, data_2[-1], sum)
tapply (data_2$breaks, data_2[-1], mean)4

library(pacman)
pacman::p_load(dplyr,tidyverse)
