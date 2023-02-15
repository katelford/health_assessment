library(tidyverse)
library(dplyr)
library(GGally)    


compare_angle_data=read.csv('Data/Updated.csv',
                            col.names=c("AoD_K","ratioA", "ratio","julian_day","offset",
                                        "ID","year","AoD_J","bradford", "month"))%>%
  filter(!(ID=="NA"))

compare_angle_data$ID=as.factor(compare_angle_data$ID)
compare_angle_data$year=as.factor(compare_angle_data$year)
compare_angle_data$month=as.factor(compare_angle_data$month)
compare_angle_data$bradford=as.factor(compare_angle_data$bradford)

head(compare_angle_data)
class(compare_angle_data$AoD_J)
class(compare_angle_data$bradford)

filter(compare_angle_data, !is.na(AoD_J) | AoD_J!="", !is.na(AoD_K) | AoD_K!="", !is.na(bradford) | bradford!="")


# paired t-test
### jasmine_angle_data%>%levene_test(AoD_J~AoD_K) popping up with error

# t.test(compare_angle_data$AoD_J~compare_angle_data$AoD_K, paired=T)



# EMG's code
# I looked up t.test in the help file and read the part about "If paired is TRUE" under the Details section
# This made me realize you need to specify x and y explicitly (rather than using the formulaic method with ~)
t.test(x=compare_angle_data$AoD_J, y=compare_angle_data$AoD_K, paired=T)
# p-value is not significant which is good - there is not a significant difference in means
# between the Jasmine and Kira

cor(x=compare_angle_data$AoD_J, y=compare_angle_data$AoD_K, use="complete.obs")
# it's a nice correlation, tho I suppose you'd be hoping for higher.
# this value may improve with more measurements (higher sample size)

plot(x=compare_angle_data$AoD_J, y=compare_angle_data$AoD_K)
# scatter plot shows good agreement btwn analysts


### Bradford
ggplot(data = compare_angle_data)+
  geom_point(aes(x=AoD_K, y=bradford))+
  facet_wrap("ID")

anovaBrad=aov(AoD_K~bradford, data=compare_angle_data)
summary(anovaBrad)

