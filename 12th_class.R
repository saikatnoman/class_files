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


head(data_import)

# what is box plot: median values 

box_plot_1 = ggplot(data = data_import, aes(x = campusSetting, y = medianBaseSalary, fill = campusSetting))+
  geom_boxplot(outlier.color="red",width = 0.4, notch = FALSE, outlier.size = 0.8)+
  stat_summary(fun = mean, geom = 'point', col = 'green', fill = 'green', size = 1)+
  labs(x = 'Regions', y = 'Median Base Salary')+
  #dark_theme_gray()+
  scale_y_continuous(limits=c(60000, 180000), expand = c(0,0))+
  theme(axis.text.x=element_text(angle = 25, size = 8), axis.text.y=element_text(angle=90, size = 8))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  labs(title = 'Boxplot of Median Base Salary of Different Regions', x = 'Regions', y = 'Median Base Salary')+
  theme(text = element_text(size = 7.5, color = 'blue'))+
  theme(legend.position = 'none')+
  theme(plot.title = element_text(hjust = 0.5, size = 8, color = 'darkblue'))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=,7 ,color = 'black'))

box_plot_1

#------------- Save the plot 
ggsave(box_plot_1, width = 4, height = 5, dpi = 500, filename = "Box_plot_111.png") 
#----------------- Box plot with Notch shape 

box_plot_2 = ggplot(data = data_import, aes(x = region, y = medianBaseSalary, color = region))+
  geom_boxplot(outlier.color="red",width = 0.4, notch = TRUE, outlier.size = 0.70 )+
  labs(x = 'Regions', y = 'Median Base Salary')+
  stat_summary(fun = mean, geom = 'point', col = 'green', fill = 'green', size = 1)+
  theme(axis.text.x=element_text(angle = 25, size = 8, color = 'white'), axis.text.y=element_text(angle=90, size = 8, color = 'white'))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  scale_y_continuous(limits=c(70000, 180000))+
  labs(title = 'Boxplot of Median Base Salary of Different Regions', x = 'Regions', y = 'Median Base Salary')+
  theme(text = element_text(size = 7.5, color = 'green'))+
  theme(legend.position = 'none')+
  theme(plot.title = element_text(hjust = 0.5, size = 8, color = 'white'))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=,7 ,color = 'black'))

box_plot_1_1

ggsave(box_plot_1_1, width = 4, height = 5, dpi = 500, filename = "Box_plot_1_123.png") 

#------------- Single Box plot_2
box_plot_3 = ggplot(data = data_import, aes(x = region, y = medianBaseSalary, fill = region))+
  geom_boxplot( width = 0.4)+
  labs(x = 'Regions', y = 'Median Base Salary')+
  stat_summary(fun = mean, geom = 'point', col = 'green', fill = 'green', size = 1)+
  theme(axis.text.x=element_text(angle = 25, size = 8), axis.text.y=element_text(angle=90, size = 8))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  scale_y_continuous(limits=c(70000, 180000))+
  labs(title = 'Boxplot of Median Base Salary of Different Regions', x = 'Regions', y = 'Median Base Salary')+
  theme(text = element_text(size = 7.5, color = 'blue'))+
  scale_fill_brewer(palette="Dark2")+
  theme(legend.position = 'none')+
  theme(plot.title = element_text(hjust = 0.5, size = 8, color = 'darkblue'))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=,7 ,color = 'black'))

box_plot_2

ggsave(box_plot_2, width = 4, height = 5, dpi = 500, filename = "Box_plot_2.png") 

#---------------------- Box plot with Notch shape and fill by region

box_plot_4 = ggplot(data = data_import, aes(x = region, y = medianBaseSalary, fill = region))+
  geom_boxplot( width = 0.4, notch = TRUE, outlier.colour = 'red', outlier.size = .8)+
  labs(x = 'Regions', y = 'Median Base Salary')+
  stat_summary(fun = mean, geom = 'point', col = 'green', fill = 'green', size = 1)+
  theme(axis.text.x=element_text(angle = 25, size = 8), axis.text.y=element_text(angle=90, size = 8))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  scale_y_continuous(limits=c(70000, 180000))+
  labs(title = 'Boxplot of Median Base Salary of Different Regions', x = 'Regions', y = 'Median Base Salary')+
  theme(text = element_text(size = 7.5, color = 'blue'))+
  scale_fill_brewer(palette="Dark2")+
  theme(legend.position = 'none')+
  theme(plot.title = element_text(hjust = 0.5, size = 8, color = 'darkblue'))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=,7 ,color = 'black'))

box_plot_2_1

ggsave(box_plot_2_1, width = 4, height = 5, dpi = 500, filename = "Box_plot_2_1.png") 

#------------Grouped Box plot_1: grouped by Campus Setting 


box_plot_5 = ggplot(data = data_import, aes(x = region, y = medianBaseSalary, color = 'red', fill = 'black'))+
  geom_boxplot(width = 0.4, outlier.colour = 'red', outlier.size = .7)+
  labs(x = 'Regions', y = 'Median Base Salary')+
  facet_grid(.~campusSetting)+
  stat_summary(fun = mean, geom = 'point', col = 'green', fill = 'green', size = 1)+
  theme(axis.text.x=element_text(angle = 25, size = 6.5), axis.text.y=element_text(angle=90, size = 6.5))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  scale_y_continuous(limits=c(70000, 180000))+
  labs(title = 'Boxplot of Median Base Salary of Different Regions by Campus Seeting', x = 'Regions', y = 'Median Base Salary')+
  theme(text = element_text(size = 7.5, color = 'blue'))+
  scale_fill_brewer(palette="Dark2")+
  theme(legend.position = 'none')+
  theme(plot.title = element_text(hjust = 0.5, size = 8, color = 'darkblue'))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=9 ,color = 'black'))

box_plot_3

ggsave(box_plot_3, width = 6, height = 4, dpi = 500, filename = "Box_plot_3.png") 

#-----------------------Grouped Box Plot---------

box_plot_6 = ggplot(data = data_import, aes(x = region, y = medianBaseSalary, fill = region))+
  geom_boxplot(width = 0.4, outlier.colour = 'red', outlier.size = 0.7)+
  labs(x = 'Regions', y = 'Median Base Salary')+
  facet_grid(.~campusSetting)+
  stat_summary(fun = mean, geom = 'point', col = 'green', fill = 'green', size = 1)+
  theme(axis.text.x=element_text(angle = 25, size = 6.5), axis.text.y=element_text(angle=90, size = 6.5))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  scale_y_continuous(limits=c(70000, 180000))+
  labs(title = 'Boxplot of Median Base Salary of Different Regions by Campus Seeting', x = 'Regions', y = 'Median Base Salary')+
  theme(text = element_text(size = 7.5, color = 'blue'))+
  scale_fill_brewer(palette="Dark2")+
  theme(legend.position = 'none')+
  theme(plot.title = element_text(hjust = 0.5, size = 8, color = 'darkblue'))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=9 ,color = 'black'))

box_plot_4

ggsave(box_plot_4, width = 6, height = 4, dpi = 500, filename = "Box_plot_4.png") 

#----------------Grouped_box_plot_5 
#--------------- creat rank category of university 
data_import$rank = cut(data_import$rank, breaks =  c(0,100,200,300,500), labels = c('High', 'Medium High', 'Medium', 'Low'))
#----------------grouping variable is rank_cat
data_import$region <- as.factor(data_import$region)
str(data_import)
dim(updated_data[!complete.cases(updated_data),])


updated_data = data_import %>%
  select (-c(longitude,latitude,website,phoneNumber,city,country,state.1,yearFounded,stateCode)) %>%
  na.omit()

data_subset <- updated_data %>% sample_n(100)

box_plot_7 = ggplot(data = data_subset, aes(x = region, y = medianBaseSalary))+
  geom_boxplot( aes(color = region, fill = 'NA'), outlier.color="red", width = 0.4)+
  labs(x = 'Regions', y = 'Median Base Salary (000)')+
  facet_wrap(.~rank, nrow = 2, ncol = 2)+
  stat_summary(fun = mean, geom = 'point', col = 'green', fill = 'green', size = 1)+
  theme(axis.text.x=element_text(angle = 25, size = 10), axis.text.y=element_text(angle=90, size = 10))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  scale_y_continuous(limits=c(70, 180), expand = c(0,0))+
  labs(title = 'Boxplot of Median Base Salary of Different Regions by University Ranking', x = 'Regions', y = 'Median Base Salary(000)')+
  theme(text = element_text(size = 11, color = 'blue'))+
  scale_fill_brewer(palette="Dark2")+
  theme(legend.position = 'none')+
  theme(plot.title = element_text(hjust = 0.5, size = 12, color = 'darkblue'))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=8 ,color = 'black'))


data_clean <- data_import %>%
  filter(!is.na(medianBaseSalary) & is.finite(medianBaseSalary)) %>%
  mutate(
    region = as.factor(region),
    rank = as.factor(rank)
  )

box_plot_7 = ggplot(data = data_clean, aes(x = region, y = medianBaseSalary))+
  geom_boxplot( aes(color = region, fill = NA), outlier.color="red", width = 0.4)+
  labs(
    title = 'Boxplot of Median Base Salary of Different Regions by University Ranking',
    x = 'Regions',
    y = 'Median Base Salary (000)'
  ) +
  facet_wrap(~rank, nrow = 2, ncol = 2) +
  stat_summary(fun = mean, geom = 'point', col = 'green', fill = 'green', size = 1) +
  theme(
    axis.text.x = element_text(angle = 25, size = 10),
    axis.text.y = element_text(angle = 90, size = 10),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    text = element_text(size = 11, color = 'blue'),
    legend.position = 'none',
    plot.title = element_text(hjust = 0.5, size = 12, color = 'darkblue'),
    legend.title = element_blank(),
    legend.key.size = unit(0.25, "cm"),
    legend.text = element_text(size = 8, color = 'black')
  ) +
  scale_y_continuous(limits = c(70, 180), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Dark2")
