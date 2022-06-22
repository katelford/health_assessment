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

# EMG comment - it's bad practice to just load libraries that you don't use

jasmine_angle_data=read.csv('Data/Sounders_Health_Assessment_Jasmine.csv',
                            col.names=c("AoD_J","offset_J","AoD_K","offset_K","julian","ID","year"))

head(jasmine_angle_data)
class(jasmine_angle_data$AoD_J)


# paired t-test
### jasmine_angle_data%>%levene_test(AoD_J~AoD_K) popping up with error

t.test(jasmine_angle_data$AoD_J~jasmine_angle_data$AoD_K, paired=T)

# EMG's code
# I looked up t.test in the help file and read the part about "If paired is TRUE" under the Details section
# This made me realize you need to specify x and y explicitly (rather than using the formulaic method with ~)
t.test(x=jasmine_angle_data$AoD_J, y=jasmine_angle_data$AoD_K, paired=T)
# p-value is not significant which is good - there is not a significant difference in means
# between the Jasmine and Kira

cor(x=jasmine_angle_data$AoD_J, y=jasmine_angle_data$AoD_K, use="complete.obs")
# it's a nice correlation, tho I suppose you'd be hoping for higher.
# this value may improve with more measurements (higher sample size)

plot(x=jasmine_angle_data$AoD_J, y=jasmine_angle_data$AoD_K)
# scatter plot shows good agreement btwn analysts






