

getwd()
setwd("E:/R Jaman Sir")



install.packages("pacman")
library(pacman)

pacman::p_load(car, caret, tseries, ggplot2, MASS, psych)

data = read.csv(file.choose())

cleaned_data = data

write.csv(cleaned_data, file = "cleaned_file.csv", row.names = TRUE)

x = 1:10
y = 20:30

z = x+y

y > 28

x = c(2,3,4,5)
y = length(x)

q = c("saikat","tithy","noman")
p = length(q)
