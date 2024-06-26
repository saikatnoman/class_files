getwd()
setwd("D:")

install.packages("tidyverse")
library(tidyverse)
library(dplyr)

install.packages("forcats")
library(forcats)
install.packages('ggplot2')
library(ggplot2)
install.packages('ggdark')
library(ggdark)
library(ggthemes)
library(scales)

data_import = read.csv(file.choose())
attach(data_import)
names(data_import)
dim(data_import)
str(data_import)

updated_data <- data_import %>%
  mutate(
    Gender = case_when(
      Gender == 0 ~ "male",
      Gender == 1 ~ "female",
      TRUE ~ as.character(Gender)
    ),
    Education_type = fct_recode(Education_type, "Secondary Education" = "Secondary / secondary special"),
    Family_status = fct_collapse(Family_status,
                                 "Married" = c("Civil marriage", "Married"),
                                 "Unmarried" = "Single / not married",
                                 "Separated" = "Separated",
                                 "Widowed" = "Widow")
  )

data_import %>%
  summarise(
    max_value = max(Total_income, na.rm = TRUE),
    min_value = min(Total_income, na.rm = TRUE),
  )

box_plot_4 = ggplot(data = updated_data, aes(x = Gender, y = Total_income, fill = Gender))+
  geom_boxplot( width = 0.4, notch = TRUE, outlier.colour = 'red', outlier.size = .8)+
  labs(x = 'Gender', y = 'Median Income')+
  stat_summary(fun = mean, geom = 'point', col = 'green', fill = 'green', size = 1)+
  theme(axis.text.x=element_text(angle = 25, size = 8), axis.text.y=element_text(angle=90, size = 8))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  scale_y_continuous(limits=c(22000, 1579000))+
  labs(title = 'Boxplot of Median Income by Gender', x = 'Gender', y = 'Median Income')+
  theme(text = element_text(size = 7.5, color = 'blue'))+
  scale_fill_brewer(palette="Dark2")+
  theme(legend.position = 'none')+
  theme(plot.title = element_text(hjust = 0.5, size = 8, color = 'darkblue'))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=,7 ,color = 'black'))

box_plot_24 <- ggplot(updated_data, aes(x = Gender, y = Total_income, fill = Gender)) +
  geom_boxplot(
    width = 0.6, 
    notch = TRUE, 
    outlier.shape = 21,
    outlier.fill = "red",
    outlier.alpha = 0.7,
    outlier.size = 2
  ) +
  stat_summary(
    fun = mean, 
    geom = "point", 
    shape = 23, 
    size = 3, 
    fill = "yellow", 
    color = "darkgreen"
  ) +
  scale_y_continuous(
    limits = c(22000, 1579000),
    labels = scales::dollar_format(scale = 1e-3, suffix = "K")
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribution of Income by Gender",
    subtitle = "Boxplot with mean values",
    x = "Gender",
    y = "Annual Income",
    caption = "Mean values shown as yellow diamonds"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "navy"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "darkblue"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 0),
    axis.text.y = element_text(angle = 0),
    legend.position = "none",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(20, 20, 20, 20)
  )

box_plot_66 = ggplot(data = updated_data, aes(x = Education_type, y = Total_income, fill = Education_type))+
  geom_boxplot(width = 0.4, outlier.colour = 'red', outlier.size = 0.7)+
  labs(x = 'Education Type', y = 'Median Income')+
  facet_grid(.~Gender)+
  stat_summary(fun = mean, geom = 'point', col = 'green', fill = 'green', size = 1)+
  theme(axis.text.x=element_text(angle = 25, size = 6.5), axis.text.y=element_text(angle=90, size = 6.5))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  scale_y_continuous(limits=c(22000, 1579000))+
  labs(title = 'Boxplot of Median Income of Different Education Type by Gender', x = 'Education Type', y = 'Median Income')+
  theme(text = element_text(size = 7.5, color = 'blue'))+
  scale_fill_brewer(palette="Dark2")+
  theme(legend.position = 'none')+
  theme(plot.title = element_text(hjust = 0.5, size = 8, color = 'darkblue'))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=9 ,color = 'black'))

box_plot_665 <- ggplot(data = updated_data, aes(x = Education_type, y = Total_income, fill = Education_type)) +
  geom_boxplot(
    width = 0.6, 
    outlier.colour = 'red', 
    outlier.size = 0.7,
    outlier.alpha = 0.7
  ) +
  stat_summary(
    fun = mean, 
    geom = 'point', 
    shape = 23,
    color = 'darkgreen', 
    fill = 'green', 
    size = 2
  ) +
  facet_grid(. ~ Gender, scales = "free_x", space = "free_x") +
  scale_y_continuous(
    limits = c(22000, 1579000),
    labels = label_number(scale = 1e-3, suffix = "K"),
    breaks = seq(0, 1579000, by = 200000)
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = 'Distribution of Income by Education Type and Gender',
    x = 'Education Type',
    y = 'Annual Income',
    caption = "Mean values shown as green diamonds"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = 'darkblue'),
    axis.title = element_text(size = 10, face = "bold", color = 'black'),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    legend.position = 'none',
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    plot.margin = margin(20, 20, 20, 20),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30"),
    panel.spacing = unit(2, "lines"),  # Increase space between facets
    strip.background = element_rect(fill = "lightblue", color = "black", size = 1),  # Add background to facet labels
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add border to each facet
  )


box_plot_67 = ggplot(data = updated_data, aes(x = Gender, y = Total_income, fill = Gender))+
  geom_boxplot(width = 0.4, outlier.colour = 'red', outlier.size = 0.7)+
  labs(x = 'Education Type', y = 'Median Income')+
  facet_wrap(.~Family_status, nrow = 2, ncol = 2)+
  stat_summary(fun = mean, geom = 'point', col = 'green', fill = 'green', size = 1)+
  theme(axis.text.x=element_text(angle = 25, size = 6.5), axis.text.y=element_text(angle=90, size = 6.5))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  scale_y_continuous(limits=c(22000, 1579000))+
  labs(title = 'Boxplot of Median Income of different Gender by Family Satus', x = 'Gender', y = 'Median Income')+
  theme(text = element_text(size = 7.5, color = 'blue'))+
  scale_fill_brewer(palette="Dark2")+
  theme(legend.position = 'none')+
  theme(plot.title = element_text(hjust = 0.5, size = 8, color = 'darkblue'))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.25, "cm"))+
  theme(legend.text = element_text(size=9 ,color = 'black'))

box_plot_677 <- ggplot(data = updated_data, aes(x = Gender, y = Total_income, fill = Gender)) +
  geom_boxplot(
    width = 0.6, 
    outlier.colour = 'red', 
    outlier.size = 0.7,
    outlier.alpha = 0.7
  ) +
  stat_summary(
    fun = mean, 
    geom = 'point', 
    shape = 23,
    color = 'darkgreen', 
    fill = 'green', 
    size = 2
  ) +
  facet_wrap(. ~ Family_status, nrow = 2, ncol = 2, scales = "free_y") +
  scale_y_continuous(
    limits = c(22000, 1579000),
    labels = label_number(scale = 1e-3, suffix = "K"),
    breaks = seq(0, 1579000, by = 200000)
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = 'Distribution of Income by Gender and Family Status',
    x = 'Gender',
    y = 'Annual Income',
    caption = "Mean values shown as green diamonds"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = 'darkblue'),
    axis.title = element_text(size = 10, face = "bold", color = 'black'),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 8),
    legend.position = 'none',
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    plot.margin = margin(20, 20, 20, 20),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30"),
    panel.spacing = unit(1.5, "lines"),
    strip.background = element_rect(fill = "lightblue", color = "black", size = 1),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )
