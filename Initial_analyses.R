#### RUN UPON OPEN STARTING HERE

library(tidyverse)
library(dplyr)
library(GGally)    
library(ggiraph)
library(ggiraphExtra) 

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

#### ENDING HERE

summary(low_offset_data)
head(low_offset_data)
dim(low_offset_data)





## plot of all measurements for all years
ggplot(low_offset_data, aes(x=julian_day, y=depression_angle, colour=year, shape=ID))+
  geom_point()+
  geom_smooth(method=lm, SE=F)+
  theme_bw()+
  xlab("Julian Day")+
  ylab("Depression Angle")
ggsave(filename="Figures/6_years_of_22.png", height=7, width=5)


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






##### 08/28/2022 analyses

### Length of time in sound when minimal benefit

head(low_offset_data)
range(low_offset_data$depression_angle)


ggplot(data=low_offset_data)+
  geom_boxplot(aes(y=ID, 
                   x=depression_angle))
ggsave(filename="figures/range_angle_boxplot.png", width=6, height=4.5)

low_offset_data %>%
  count(ID)


# remove IDs 185 and 365 for low sighting history
benefit=low_offset_data

benefit=benefit%>%
  filter(!(ID == "185"),!(ID=="356"))

ggplot(data=benefit)+
  geom_boxplot(aes(y=ID, 
                   x=depression_angle))
ggsave(filename="figures/filtered_range_angle_boxplot.png", width=6, height=4.5)


# narrowed down to rough average spread to "minimal benefit" sightings
filtered_grey_annual_change=low_offset_data%>%
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
  

# removing outliers
filtered_grey_annual_change=filtered_grey_annual_change%>%
  filter(diff_depression_angle>-1.75,
         diff_depression_angle<2)

# plot number of days in sound vs angle
ggplot(filtered_grey_annual_change)+
  geom_point(aes(x=sight_period_days,
             y=diff_depression_angle,
             colour=ID))+
  stat_smooth(method = "lm", 
              se=F,
              aes(x=sight_period_days,
                  y=diff_depression_angle,
                  colour=ID))+
  theme_bw()
ggsave(filename="Figures/Stage_2/BENEFIT_1_change_in_angle_over_sight_period_by_ID.png", height=10, width=12)

ggplot(filtered_grey_annual_change)+
  geom_point(aes(y=sight_period_days,
                 x=diff_depression_angle,
                 colour=ID))+
  stat_smooth(method = "lm", 
              se=F,
              aes(y=sight_period_days,
                  x=diff_depression_angle,
                  colour=ID))+
  theme_bw()
ggsave(filename="Figures/Stage_2/BENEFIT_2_change_in_angle_over_sight_period_by_ID.png", height=12, width=10)

##  turns out, it does seem to depend on the individual! Individuals 21, 531, and 383 hung around the sound longer 
##  in hopes of better foraging, but ultimately failed. Individuals 22, 44, 49, and 56 found better foraging success
##  the longer they stayed.
  

### graph, difference against year colour by ID, graph sight period against year colour by ID




### Decline in condition with higher competition?

## look at comparison of no animals in the sound that year and a comparison using cumulative 
## animal days in sound that year
    # check with john about survey efforts and importance of sighting periods


year_w_ID_count=filtered_grey_annual_change %>%
  group_by(year)%>%
  summarise(number_of_IDs=n_distinct(ID))



ggplot(data=year_w_ID_count)+
  geom_point(aes(x=year,
                 y=number_of_IDs))

summary(year_w_ID_count$number_of_IDs)
# 3rd quartile is 7 individuals and above

high_year_w_ID_count=year_w_ID_count%>%
  filter(number_of_IDs>=7)


high_comp_years=filtered_grey_annual_change%>%
  left_join(high_year_w_ID_count, by="year")%>%
  select(year,diff_depression_angle,number_of_IDs,ID)%>%
  filter(!is.na(number_of_IDs))
  
                                ## somehow plot it? each ID's average by year, coloured by no.IDs that year?



ggplot(data=high_comp_years)+
  geom_point(aes(x=number_of_IDs,
             y=diff_depression_angle,     #why can't it find this?
             colour=year))


### mixed effects model
library(lme4)

mem=lmer(diff_depression_angle~year+(1|ID), data=filtered_grey_annual_change)
summary(mem)

anova(mem)
    ## I think this means ID does not have a significant effect

 ### good website https://stats.oarc.ucla.edu/other/mult-pkg/introduction-to-linear-mixed-models/

  ## look into more model transformations and results + p value



### temporal patterns of sounders entering and leaving the sound

# using grey_annual_change for df since it already contains columns: ID, year, first_sight, last_sight

# wrapped by ID
time_in_sound_ID=ggplot(data=grey_annual_change)+
  geom_segment(aes(x=first_sight,
                   xend=last_sight,
                   y=first_sight_depression_angle,
                   yend=last_sight_depression_angle,
                   colour=year))+
  facet_wrap(~ID)+
  ylab("Body Condition Angle")+
  xlab("Julian Day")
ggsave(filename="Figures/Stage_3/time_in_sound_ID.png", height=6, width=10)

# wrapped by year
change_year=grey_annual_change%>%
  filter(!year%in%c(1990,1995,1998,2000,2004,2005))

time_in_sound_year=ggplot(data=change_year)+
  geom_segment(aes(x=first_sight,
                   xend=last_sight,
                   y=first_sight_depression_angle,
                   yend=last_sight_depression_angle,
                   colour=ID))+
  facet_wrap(~year)+
  ylab("Body Condition Angle")+
  xlab("Julian Day")
ggsave(filename="Figures/Stage_3/time_in_sound_year.png", height=6, width=10)







