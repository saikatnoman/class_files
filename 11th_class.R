getwd()
setwd("E:/R Jaman Sir")

install.packages("tidyverse")
library(tidyverse)
library(dplyr)

install.packages("forcats")
library(forcats)
install.packages('ggplot2')
library(ggplot2)
install.packages('ggdark')
library(ggdark)


data_import = read.csv(file.choose())
attach(data_import)
names(data_import)
dim(data_import)
str(data_import)

updated_data = data_import %>%
  select (-c(longitude,latitude,website,phoneNumber,city,country,state.1,yearFounded,stateCode)) %>%
  mutate(rank = cut(rank, 
                  breaks = c(0, 100, 200, 300, 500), 
                  labels = c('High', 'Medium High', 'Medium', 'Low'))) 

histo_plot_1 = ggplot(data = updated_data, aes(x = studentFacultyRatio)) +
  geom_histogram(aes(y = ..density..), col = 'red', fill = 'red') +
  geom_density(color = 'blue')+
  #facet_wrap(.~rank_cat, nrow = 2, ncol =2) +
  labs(x = "Student-Teacher Ratio", y = "Density", title = 'Histogram of Student-Teacher Ratio')+
  theme(axis.text.x=element_text(angle = 0, size = 8, color = 'black'), axis.text.y=element_text(angle=360, size = 8, color = 'black'))+
  theme(text=element_text(size = 9, color = 'black'))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  xlim(c(0, 30))+
  theme(legend.position = 'none')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=,7 ,color = 'white'))+
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = 'darkblue'))+
  theme(strip.text.x = element_text(size=8, color = 'green'))  +
  theme_bw()


histo_plot_2 = ggplot(data = updated_data, aes(x = studentFacultyRatio)) +
  geom_histogram(col = 'blue', fill = 'blue', binwidth = 1) +
  labs(x = "Student Faculty Ratio", y = "Frequency", title = 'Histogram of Student-Teacher Ratio')+
  theme(axis.text.x=element_text(angle = 0, size = 8, color = 'black'), axis.text.y=element_text(angle=360, size = 8, color = 'black'))+
  theme(text=element_text(size = 9, color = 'black'))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  xlim(c(0, 30))+
  theme(legend.position = 'none')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=,7 ,color = 'white'))+
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = 'black'))+
  theme(strip.text.x = element_text(size=8, color = 'green'))+
  annotate("text",x = 27,y = 42,label = paste("Mean =", round(mean(updated_data$studentFacultyRatio),2)),
           col = "black",size = 3)+
  annotate("text",x = 27, y = 40,label = paste("Median =", median(updated_data$studentFacultyRatio)),
           col = "black",size = 3)

histo_plot_3 = ggplot(data = updated_data, aes(x = studentFacultyRatio)) +
  geom_histogram(col = 'firebrick', fill = 'firebrick', binwidth = 1) +
  #geom_density(color ='green')+
  #facet_wrap(.~rank_cat, nrow = 2, ncol =2) +
  labs(x = "Student Faculty Ratio", y = "Frequency", title = 'Histogram of Student-Teacher Ratio')+
  dark_theme_gray()+
  theme(axis.text.x=element_text(angle = 0, size = , color = 'white'), axis.text.y=element_text(angle=360, size = 8, color = 'white'))+
  theme(text=element_text(size = 9, color = 'white'))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  xlim(c(0, 30))+
  theme(legend.position = 'none')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=,7 ,color = 'white'))+
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = 'white'))+
  theme(strip.text.x = element_text(size=8, color = 'green'))+
  annotate("text",x = 27,y = 42,label = paste("Mean =", round(mean(updated_data$studentFacultyRatio),2)),
           col = "yellow",size = 3)+
  annotate("text",x = 27, y = 40,label = paste("Median =", median(updated_data$studentFacultyRatio)),
           col = "yellow",size = 3)


histo_plot_4 = ggplot(data = updated_data, aes(x = updated_data$studentFacultyRatio)) +
  geom_histogram(aes(y = ..density..),col = 'blue', fill = 'blue', binwidth = 1) +
  geom_density(color ='red')+
  #facet_wrap(.~rank_cat, nrow = 2, ncol =2) +
  labs(x = "Student Faculty Ratio", y = "Density", title = 'Histogram of Student-Teacher Ratio')+
  dark_theme_gray()+
  theme(axis.text.x=element_text(angle = 0, size = , color = 'white'), axis.text.y=element_text(angle=360, size = 8, color = 'white'))+
  theme(text=element_text(size = 9, color = 'white'))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  xlim(c(0, 30))+
  ylim(c(0,0.11))+
  theme(legend.position = 'none')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=,7 ,color = 'white'))+
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = 'white'))+
  theme(strip.text.x = element_text(size=8, color = 'green'))+
  annotate("text",x = 27,y = 0.08,label = paste("Mean =", round(mean(updated_data$studentFacultyRatio),2)),
           col = "yellow",size = 3)+
  annotate("text",x = 27, y = 0.075,label = paste("Median =", median(updated_data$studentFacultyRatio)),
           col = "yellow",size = 3)




mean <- updated_data%>% 
  group_by(rank)%>%
  summarize(mean_val=mean(studentFacultyRatio))
median <- updated_data%>%
  group_by(rank)%>%
  summarize(median_val=median(studentFacultyRatio))

histo_plot_5 = ggplot(data = updated_data, aes(x = updated_data$studentFacultyRatio)) +
  geom_histogram(aes(y = ..density..),col = 'red', fill = 'red', binwidth = 1) +
  #geom_density(color = 'red')+
  facet_wrap(.~rank, nrow = 2, ncol =2) +
  labs(x = "Student-Teacher Ratio", y = "Density", title = 'Histogram of Student-Teacher Ratio by University Ranking')+
  dark_theme_gray()+
  scale_x_continuous(limits = c(0,30),expand = c(0,0))+
  scale_y_continuous(limits = c(0,0.13),expand = c(0,0))+
  theme(axis.text.x=element_text(angle = 0, size = 7, color = 'white'), axis.text.y=element_text(angle=360, size = 7, color = 'white'))+
  theme(text=element_text(size = 9, color = 'cyan'))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(legend.position = 'none')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=,7 ,color = 'cyan'))+
  theme(plot.title = element_text(hjust = 0.5, size = 10, color = 'cyan'))+
  theme(strip.text.x = element_text(size=8, color = 'cyan'))+
  geom_vline(data= mean, aes(xintercept = mean_val), color = 'green')+
  geom_vline(data= median, aes(xintercept = median_val), color = 'cyan')+
  annotate("text",x = 27,y = 0.11,label = 'Mean',
           col = "green",size = 3)+
  annotate("text",x = 27, y = 0.10,label = 'Median',
           col = 'cyan',size = 3)+
  theme(panel.spacing = unit(0.4, "cm"))

