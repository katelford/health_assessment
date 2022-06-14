library(tidyverse)
library(dplyr)
library(GGally)    
library(ggiraph)
library(ggiraphExtra) 
library(broom)
library(car)
library(lubridate)
library(rstatix)
library(knitr) 

jasmine_angle_data=read.csv('Data/Sounders_Health_Assessment_Jasmine.csv',
                            col.names=c("AoD_J","offset_J","AoD_K","offset_K","julian","ID","year"))

head(jasmine_angle_data)
class(jasmine_angle_data$AoD_J)


# paired t-test
### jasmine_angle_data%>%levene_test(AoD_J~AoD_K) popping up with error

t.test(jasmine_angle_data$AoD_J~jasmine_angle_data$AoD_K, paired=T)







