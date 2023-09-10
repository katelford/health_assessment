## RUN ON START

library(tidyverse)
library(dplyr)
library(GGally)    
library(ggiraph)
library(ggiraphExtra) 
library(lme4)
library(sjPlot)
library(ggplot2)

angle_data=read.csv('Data/Updated.csv',
                    col.names=c("depression_angle","julian_day","offset","ID","year", "AoD_J","bradford", "month"))%>%
  filter(!depression_angle=="NA") #,!year=="2021")

angle_data$ID=as.factor(angle_data$ID)
angle_data$year=as.factor(angle_data$year)
angle_data$month=as.factor(angle_data$month)
head(angle_data)
summary(angle_data)

low_offset_data=angle_data%>%
  filter(offset<3)

low_offset_data$depression_angle=as.numeric(low_offset_data$depression_angle)
class(low_offset_data$depression_angle)

grey_annual_change=low_offset_data%>%
  group_by(ID, year) %>%
  summarize(first_sight=min(julian_day),
            last_sight=max(julian_day)) %>%
  ungroup()%>%
  left_join(.,low_offset_data%>%select(ID, julian_day, depression_angle), 
            by=c("ID"="ID", "first_sight"="julian_day"))%>%
  rename(first_sight_depression_angle=depression_angle)%>%
  left_join(.,low_offset_data%>%select(ID, julian_day, depression_angle), 
            by=c("ID"="ID", "last_sight"="julian_day"))%>%
  rename(last_sight_depression_angle=depression_angle)%>%
  group_by(ID, year, first_sight, last_sight)%>%
  summarize(first_sight_depression_angle=mean(first_sight_depression_angle), 
            last_sight_depression_angle=mean(last_sight_depression_angle))%>%
  ungroup()%>%
  group_by(ID, year) %>%
  mutate(sight_period_days=last_sight-first_sight,
         diff_depression_angle=last_sight_depression_angle-first_sight_depression_angle)

## END HERE


## what I originally tried

plot_lm=data.frame(Predicted_value=predict(mixed_low_offset),
                     Observed_value=low_offset_data$depression_angle)

ggplot(plot_lm, aes(x=Predicted_value, y=Observed_value))+
  geom_point()+
  geom_abline(intercept=0, slope=1, colour="red")+
  ylab("Measured Body Condition Angle")+
  xlab("Predicted Body Condition Angle")+
  theme_bw()





# website I used for the plots below. I think they went into way more detail than I need, but I'm not
# a 100% on all John D. was asking for

https://www.azandisresearch.com/2022/12/31/visualize-mixed-effect-regressions-in-r-with-ggplot2/
  
  

#### MEM plots
# all data
mixed_ext=lmer(depression_angle~julian_day  + (1|year) + (1|ID) + (1|offset), data=angle_data)
summary(mixed_ext)
anova(mixed_ext)

all=angle_data%>%
  mutate(fit.m=predict(mixed_ext, re.form=NA),
         fit.c=predict(mixed_ext, re.form=NULL))

ggplot(aes(x=julian_day, y=depression_angle), data=all) +
  geom_point(pch=16, col="grey") +
  geom_line(aes(y=fit.m), col=1, size=2)


# low offset data
mixed_low_offset=lmer(depression_angle~julian_day + (1|year)+ (1|ID) + (1|offset), data=low_offset_data)
summary(mixed_low_offset)
anova(mixed_low_offset)

low=low_offset_data%>%
  mutate(fit.m=predict(mixed_low_offset, re.form=NA),
         fit.c=predict(mixed_low_offset, re.form=NULL))

ggplot(aes(x=julian_day, y=depression_angle), data=low) +
  geom_point(pch=16, col="grey") +
  geom_line(aes(y=fit.m), col=1, size=2)
















