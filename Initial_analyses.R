library(tidyverse)
library(dplyr)
library(GGally)    
library(ggiraph)
library(ggiraphExtra) 
library(broom)
library(car)
library(lubridate)

angle_data=read.csv('Data/Sounder_angles_1990_2020.csv',
                    skip=1, col.names=c("ID","category","date","julian_day","year","offset","depression_angle"))

head(angle_data)
summary(angle_data)


# filter out offsets >1
low_offset_data=angle_data%>%
  filter(offset<2)%>%
  mutate(ID=as.factor(ID),
         year=as.factor(year),
         offset=as.factor(offset))

summary(low_offset_data)
head(low_offset_data)
dim(low_offset_data)



#offsets=lm(depression_angle~offset, data=low_offset_data)
#summary(offsets)
#print(low_offset_data$offset)


# plot julian day vs angle by year for 22
data_22=low_offset_data%>%
  filter(ID==22,
         year%in%c(2006,2010,2015,2018,2019,2020))

ggplot(data_22, aes(x=julian_day, y=depression_angle))+
  geom_point()+
  facet_wrap(~year, nrow=3)+
  geom_smooth(method=lm)+
  theme_bw()+
  xlab("Julian Day")+
  ylab("Depression Angle")
ggsave(filename="Figures/6_years_of_22.png", height=7, width=5)

  
# ANOVA
sounder_anova=aov(depression_angle~julian_day+ID, data=low_offset_data)
summary(sounder_anova) 

lm_sounder=lm(depression_angle~ID+julian_day+year, data=low_offset_data)
best_model=step(lm_sounder)
summary(best_model)


# ERIN'S DF WITH SLIGHT MODS

# Create a data frame for each individual whale in each year that contains the first date sighted, 
# the last date sighted, the total sighting period that year (in days), and the difference in 
# depression angle between the first and last sighting of the year
grey_annual_change=low_offset_data%>%
  group_by(ID, year) %>%
  summarize(first_sight=min(julian_day),
            last_sight=max(julian_day)) %>%   # groups by ID, then year and adds 2 new columns
  ungroup()%>%
  left_join(.,low_offset_data%>%select(ID, julian_day, depression_angle), 
            by=c("ID"="ID", "first_sight"="julian_day"))%>%
  rename(first_sight_depression_angle=depression_angle)%>%
  left_join(.,low_offset_data%>%select(ID, julian_day, depression_angle), 
            by=c("ID"="ID", "last_sight"="julian_day"))%>%
  rename(last_sight_depression_angle=depression_angle)%>%
  # Deal with multiple pictures (and thus depression angles) collected on same day by taking the mean
  group_by(ID, year, first_sight, last_sight)%>%
  summarize(first_sight_depression_angle=mean(first_sight_depression_angle), 
            last_sight_depression_angle=mean(last_sight_depression_angle))%>%
  ungroup()%>%
  # Calculate sighting period and change in depression angle for each ID and each year
  group_by(ID, year) %>%
  mutate(sight_period_days=last_sight-first_sight,
         diff_depression_angle=last_sight_depression_angle-first_sight_depression_angle)


# PLOT diff in sighting vs angle coloured by year, facetwrap by ID
ggplot(data=grey_annual_change)+
  geom_point(aes(x=sight_period_days, y=diff_depression_angle, color=year)) +
  geom_smooth(aes(x=sight_period_days, y=diff_depression_angle), method="lm") +
  facet_wrap(~ID)+
  theme_bw() +
  scale_color_discrete(name="whale ID") +
  ylab("Difference in depression angle between\nfirst and last sighting of the season") + xlab("Sighting period (days)")
ggsave(filename="Figures/change_in_angle_over_sight_period_bby_ID.png", height=8, width=12)

# PLOT diff in sighting vs angle coloured by ID, facetwrap by year
ggplot(data=grey_annual_change)+
  geom_point(aes(x=sight_period_days, y=diff_depression_angle, color=ID)) +
  geom_smooth(aes(x=sight_period_days, y=diff_depression_angle), method="lm") +
  facet_wrap(~year)+
  theme_bw() +
  scale_color_discrete(name="whale ID") +
  ylab("Difference in depression angle between\nfirst and last sighting of the season") + xlab("Sighting period (days)")
ggsave(filename="Figures/change_in_angle_over_sight_period_by_year.png", height=8, width=12)


# lm
lm_diff_angle_vs_sight_period = lm(diff_depression_angle~sight_period_days, data=grey_annual_change)
summary(lm_diff_angle_vs_sight_period)

lm_diff_angle_vs_sight_period_id_year = lm(diff_depression_angle ~ sight_period_days + ID + year, data=grey_annual_change)
summary(lm_diff_angle_vs_sight_period_id_year)




  
  
  
  
