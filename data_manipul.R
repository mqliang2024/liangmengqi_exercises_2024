#
#Author: liangmengqi
#Copyright   Copyright 2024-liangmengqi
#Email:mqliang2023@mail.ustc.edu.cn
#
#Date:2024-03-23
#
#Script Name:liangmengqi_homework03_2024
#
#Script Description:Using a data frame as an example, write a short code to illustrate some functions or packages for data processing.
#
#
#SETUP ----------------------------------------------
#import and save data
#data file import/output 
emp.data<- data.frame( #Creating data frame    
  name = c("Raman","Rafia","Himanshu","jasmine","Yash"),    
  salary = c(623.3,915.2,611.0,729.0,843.25),     
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11","2015-03-27")),  
  dept = c("Operations","IT","HR","IT","Finance"),    
  stringsAsFactors = FALSE    
)  

# save the dataframe as a csv file
install.packages("csv")
library("csv") 
write.csv(emp.data, file = "D:/data-driven-ecology/liangmengqi_exercises_2024/employee.csv", # save in a file
           )  
csv_data <- read.csv("D:/data-driven-ecology/liangmengqi_exercises_2024/employee.csv")
print(csv_data) 

#check data structure
print(is.data.frame(csv_data)) 

#check whether a column or row has missing data
any(is.na(csv_data[1, ]))
any(is.na(csv_data[1, ]))

#extract values from a column or select/add a column
max_sal<- max(csv_data$salary) # Getting the maximum
print(max_sal)

#transform a wider table to a long format
download.file("tinyurl.com/dcmac2017dec/data/surveys_wide.csv",
              dest="D:/data-driven-ecology/liangmengqi_exercises_2024/surveys_wide.csv")
library(tidyverse)
surveys_wide <- read.csv("D:/data-driven-ecology/liangmengqi_exercises_2024/surveys_wide.csv")
surveys_long <- surveys_wide %>%
  gather(key = species_abbrev, value = count, -(month:plot_id))

# inspect the data structure
str(surveys_long)

#reverse the operation
spread(surveys_long,key=species_abbrev,value=count)

#visualize the data
surveys_complete <- surveys_wide %>%
  filter(         
         !is.na(DM),           # remove missing DM
         !is.na(RM),           # remove missing RM
        ) 
write_csv(surveys_complete, "D:/data-driven-ecology/liangmengqi_exercises_2024/surveys_complete.csv")
# Plotting scatter plot
surveys_complete <- read_csv("D:/data-driven-ecology/liangmengqi_exercises_2024/surveys_complete.csv")
ggplot(data = surveys_complete)
ggplot(data = surveys_complete, 
       aes(x = DM, y = RM)) # define aes
picture <- ggplot(data = surveys_complete, 
       aes(x = DM, y = RM)) +
  geom_point() # dot plots

picture + theme_bw() +  
  theme(panel.grid = element_blank())  

ggsave("D:/data-driven-ecology/liangmengqi_exercises_2024/survey_complete_plot.png", picture, width = 15, height = 10)
