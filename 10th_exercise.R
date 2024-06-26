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

data_import$campusSetting = data_import$campusSetting %>%
  fct_collapse("Rural" = c ("Suburban","Rural"))
unique(data_import$campusSetting)

data_import$collegeType = data_import$collegeType %>%
  fct_recode(Private = "Private not-for-profit")
unique(data_import$collegeType)

data_import$carnegieClassification = data_import$carnegieClassification %>%
  fct_collapse("Doctoral" = c("Doctoral Universities: Very High Research Activity", "Doctoral Universities: High Research Activity", "Doctoral/Professional Universities"),
               "Masters" = c("Masters Colleges & Universities: Larger Programs", "Masters Colleges & Universities: Small Programs", "Masters Colleges & Universities: Medium Programs"),
               "Bachelor" = c("Baccalaureate Colleges: Arts & Sciences Focus","Baccalaureate Colleges: Diverse Fields"),
               "Other" = c("Special Focus Four-Year: Engineering Schools","Special Focus Four-Year: Arts, Music & Design Schools","Special Focus Four-Year: Other Technology-Related Schools"))
unique(data_import$carnegieClassification)

unique(data_import$rank)


updated_data = data_import %>%
  select (-c(longitude,latitude,website,phoneNumber,city,country,state.1,yearFounded,stateCode)) %>%
  mutate(rank = cut(rank, 
                    breaks = c(0, 100, 200, 300, 500), 
                    labels = c('High', 'Medium High', 'Medium', 'Low'))) %>%
 # mutate(campusSetting = as_factor(campusSetting),
  #campusSetting = as.numeric(campusSetting)) %>%
  na.omit()
  


names(updated_data)
unique(updated_data$rank)
unique(updated_data$campusSetting)
unique(updated_data$collegeType)
str(updated_data)

value_counts = table(updated_data$collegeType)

dim(updated_data[!complete.cases(updated_data),])

bar_chart_1 = ggplot(updated_data, aes(x = collegeType))+
  geom_bar(aes(fill = collegeType), position = "dodge", width  =0.30)+
  labs(x = "", y = "Frequency", title = "Bar Plot by CollegeType")+
  scale_y_continuous(limits = c(0,280))+
  scale_fill_manual(values = c ('dark blue','red'))+
  theme(axis.text.x = element_text(angle = 25, size = 10.5, colour = 'blue'),axis.text.y = element_text(angle = 90, size = 10.5))+
  theme(text = element_text(colour = 'blue', size = 10))+
  theme(legend.position = 'right')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.30,"cm"))+
  theme(legend.text = element_text(size = 8, colour = 'blue'))+
  theme(plot.title = element_text(hjust = 0.5,size = 12, color = 'darkblue'))

ggsave(bar_chart_1, width = 4, height = 4, dpi = 800, filename ="Bar Plot.png")

bar_chart_2 = ggplot(updated_data, aes(x = collegeType))+
  geom_bar(aes(fill = region), position  = 'dodge', width = 0.50)+
  labs(x = '', y = 'Frequency', title = 'Bar Plot of Region')+
  scale_y_continuous(limits = c(0,135))+
  scale_fill_manual(values = c('blue', 'dark green', 'orange', 'red'))+
  theme(axis.text.x=element_text(angle = 10, size = 6.5, color = 'blue'), axis.text.y=element_text(angle=90, size = 6.5))+
  theme(text=element_text(color = 'blue', size = 7))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  theme(legend.position = 'right')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=6, color = 'blue'))+
  theme(plot.title = element_text(hjust = 0.5, size = 8, color = 'darkblue')) 

 

bar_chart_3 = updated_data %>% ggplot (aes(x = region, color = campusSetting))+
  geom_bar(aes(fill = campusSetting),width = 0.30, position = position_stack(vjust = 0.50))+
  #geom_text(size = 5, position = position_stack(vjust = 0.5))+
  labs(x = 'Region', y = 'Frequency', title = 'Staked Bar Plot')+
  scale_y_continuous(limits = c(0,200), expand = c(0,0))+
  theme(axis.text.x=element_text(angle = 25, size = 10, color = 'blue'), axis.text.y=element_text(angle=90, size = 10))+
  theme(text=element_text(size = 11))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(legend.position = 'right')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=7, color = 'darkblue'))+
  theme(plot.title = element_text(hjust = 0.5, size = 14, color = 'darkblue')) 

bar_chart_4 = ggplot(updated_data, aes(x = region, color = campusSetting))+
  geom_bar(aes(fill = campusSetting), position = position_stack(reverse = TRUE), width = 0.30)+
  labs(x = '', y = 'Frequency', title = 'Horizontal Stacked Bar')+
  scale_y_continuous(limits = c(0,170))+
  theme(axis.text.x=element_text(angle = 0, size = 7, color = 'black'), axis.text.y=element_text(angle=360, size = 7, color = 'blue'))+
  theme(text=element_text(color = 'blue', size = 7))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  coord_flip()+
  theme(legend.position = 'right')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=6, color = 'black'))+
  theme(plot.title = element_text(hjust = 0.5, size = 8, color = 'darkblue')) 


bar_chart_5 = ggplot(updated_data, aes(x = region))+
  geom_bar(aes(fill = rank), stat = 'count', position = position_stack(reverse = TRUE), width = 0.30)+
  labs(x = '', y = 'Frequency', title = 'Stacked Bar by Ranking of University')+
  scale_y_continuous(limits = c(0,200))+
  scale_fill_manual(values = c('dark green', 'orange', 'darkblue','red'))+
  theme(axis.text.x=element_text(angle = 0, size = 7, color = 'black'), axis.text.y=element_text(angle=360, size = 7, color = 'blue'))+
  theme(text=element_text(color = 'blue', size = 7))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  theme(legend.position = 'Bottom')+
  coord_flip()+
  theme(legend.position = 'right')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=6, color = 'black'))+
  theme(plot.title = element_text(hjust = 0.5, size = 8, color = 'darkblue')) 

bar_chart_6 = ggplot(updated_data, aes(x = rank))+
  geom_bar(aes(fill = rank), stat = 'count', position = position_stack(reverse = TRUE), width = 0.30)+
  labs(x = '', y = 'Frequency', title = 'Grouped Bar Plot by Regions')+
  scale_y_continuous(limits = c(0,60))+
  facet_wrap(.~region, nrow = 2, ncol = 2)+
  scale_fill_manual(values = c('darkblue', 'dark green', 'red', 'orange'))+
  #dark_theme_gray()+
  theme(axis.text.x=element_text(angle = 20, size = 6, color = 'blue'), axis.text.y=element_text(angle=360, size = 6, color = 'black'))+
  theme(text=element_text(size = 8, color = 'blue'))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  #coord_flip()+
  theme(legend.position = 'none')+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=,7 ,color = 'black'))+
  theme(plot.title = element_text(hjust = 0.5, size = 8, color = 'darkblue'))+
  theme(strip.text.x = element_text(size=7, color = 'blue'))

