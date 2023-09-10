#### RUN UPON OPEN STARTING HERE

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


### remove low count IDs
low_ID_data=angle_data%>%
  filter(!ID=="185",!ID=="356", !ID=="44", !ID=="56", !ID=="531", offset<3, !depression_angle=="NA")


#### ENDING HERE




## plot of all measurements for all years
ggplot(angle_data, aes(x=julian_day, y=depression_angle, colour=year))+ #, shape=ID))+
  geom_point()+
  geom_smooth(method=lm, SE=F)+
  theme_bw()+
  xlab("Julian Day")+
  ylab("Depression Angle")
ggsave(filename="Figures/all_year_scatter.png", height=7, width=5)

#scatterplot of all points
ggplot(low_offset_data, aes(x=julian_day, y=depression_angle))+
  geom_point()+
  geom_smooth(method="lm")+
  xlab("Julian Day")+
  ylab("Body Condition Angle")+
  theme_bw()
ggsave(filename="Figures/all_angles_julian_lowoffset.png", height=5, width=6)

#scatterplot of 22
angle_22=low_offset_data%>%
  filter(ID=="22",!year=="1990",!year=="1998",!year=="2003",!year=="2005",!year=="2009")

ggplot(angle_22, aes(x=julian_day, y=depression_angle))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~year)+
  xlab("Julian Day")+
  ylab("Body Condition Angle")+
  theme_bw()
ggsave(filename="Figures/22_since_1990.png", height=5, width=7)

#offsets=lm(depression_angle~offset, data=low_offset_data)
#summary(offsets)
#print(low_offset_data$offset)


# plot julian day vs angle by year and by ID

ggplot(angle_data, aes(x=julian_day, y=depression_angle, colour=ID))+
  geom_point()+
  facet_wrap(~year, nrow=5)+
  geom_smooth(method=lm)+
  theme_bw()+
  xlab("Julian Day")+
  ylab("Body Condition Angle")
ggsave(filename="Figures/angle_day_year.png", height=7, width=7)

ggplot(low_offset_data, aes(x=julian_day, y=AoD_J, colour=year))+
  geom_point()+
  facet_wrap(~ID, nrow=3)+
  geom_smooth(method=lm)+
  theme_bw()+
  xlab("Julian Day")+
  ylab("Body Condition Angle")
ggsave(filename="Figures/angle_day_ID.png", height=5, width=7)

  
# ANOVA
sounder_anova=aov(depression_angle~julian_day+ID, data=angle_data)
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
  scale_color_discrete(name="Whale ID") +
  ylab("Difference in depression angle between\nfirst and last sighting of the season") + xlab("Sighting period (days)")
ggsave(filename="Figures/change_in_angle_over_sight_period_by_ID.png", height=8, width=12)

  # plot but only 22
annual_change_22 = grey_annual_change %>%
  filter(ID==22)
  
  
ggplot(data=annual_change_22)+
  geom_point(aes(x=sight_period_days, y=diff_depression_angle, color=year)) +
  geom_smooth(aes(x=sight_period_days, y=diff_depression_angle), method="lm") +
  facet_wrap(~ID)+
  theme_bw() +
  scale_color_discrete(name="whale ID") +
  ylab("Difference in depression angle between\nfirst and last sighting of the season") + xlab("Sighting period (days)")
ggsave(filename="Figures/22_change_in_angle_over_sight_period.png", height=6, width=5)

# PLOT diff in sighting vs angle coloured by ID, facetwrap by year
change=grey_annual_change%>%
  filter(!year==1990,!year==1992,!year==1998,!year==2002,!year==2005,!year==2007,!year==2008,!year==1991,!year==2000,!year==2004)

ggplot(data=change)+
  geom_point(aes(x=sight_period_days, y=diff_depression_angle, color=ID)) +
  geom_smooth(aes(x=sight_period_days, y=diff_depression_angle), method="lm") +
  facet_wrap(~year)+
  theme_bw() +
  scale_color_discrete(name="whale ID") +
  ylab("Difference in depression angle between\nfirst and last sighting of the season") + xlab("Sighting period (days)")
ggsave(filename="Figures/change_in_angle_over_sight_period_by_year.png", height=8, width=12)

  # plot but only 2009-2013
filtered_annual_change = grey_annual_change %>%
  filter(year%in%c(year == 2010,2009,2011,2012,2013))

ggplot(data=filtered_annual_change)+
  geom_point(aes(x=sight_period_days, y=diff_depression_angle, color=ID)) +
  geom_smooth(aes(x=sight_period_days, y=diff_depression_angle), method="lm") +
  facet_wrap(~year)+
  theme_bw() +
  scale_color_discrete(name="whale ID") +
  ylab("Difference in depression angle between\nfirst and last sighting of the season") + xlab("Sighting period (days)")
ggsave(filename="Figures/filtered_change_in_angle_over_sight_period_by_year.png", height=3, width=5)


# lm
lm_diff_angle_vs_sight_period = lm(diff_depression_angle~sight_period_days, data=grey_annual_change)
summary(lm_diff_angle_vs_sight_period)

lm_diff_angle_vs_sight_period_id_year = lm(diff_depression_angle ~ sight_period_days + ID + year, data=grey_annual_change)
summary(lm_diff_angle_vs_sight_period_id_year)






##### 08/28/2022 analyses

### Length of time in sound when minimal benefit

head(angle_data)
range(angle_data$depression_angle)

angle_data=angle_data%>%
  filter(!(ID=="NA"))

ggplot(data=angle_data)+
  geom_boxplot(aes(y=ID, 
                   x=depression_angle))+
  theme_bw()+
  xlab("Body Condition Angle")
ggsave(filename="figures/range_angle_boxplot.png", width=6, height=4.5)

ggplot(data=low_offset_data)+
  geom_boxplot(aes(y=ID, 
                   x=depression_angle))+
  theme_bw()+
  xlab("Body Condition Angle")

angle_data %>%
  count(ID)


# remove IDs 185 and 365 for low sighting history
benefit=angle_data%>%
  filter(!(ID == "185"),!(ID=="356"),!(ID=="NA"))

ggplot(data=benefit)+
  geom_boxplot(aes(y=ID, 
                   x=depression_angle))
ggsave(filename="figures/filtered_range_angle_boxplot.png", width=6, height=4.5)


# narrowed down to rough average spread to "minimal benefit" sightings
filtered_grey_annual_change=angle_data%>%
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
  geom_point(aes(x=sight_period_days, y=diff_depression_angle, colour=ID))+
  stat_smooth(method = "lm", se=F, 
              aes(x=sight_period_days, y=diff_depression_angle, colour=ID))+
  xlab("Sighting Period (Days)")+
  ylab("Difference in first and last BC angle of the season")
  theme_bw()
ggsave(filename="Figures/Stage_2/BENEFIT_1_change_in_angle_over_sight_period_by_ID.png", height=10, width=12)

ggplot(filtered_grey_annual_change)+
  geom_point(aes(y=sight_period_days,x=diff_depression_angle,colour=ID))+
  stat_smooth(method = "lm", se=F,
              aes(y=sight_period_days,x=diff_depression_angle,colour=ID))+
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


year_w_ID_count=grey_annual_change %>%
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
             y=diff_depression_angle,     
             colour=year))


### mixed effects model
mem=lmer(diff_depression_angle~year+(1|ID), data=filtered_grey_annual_change)
summary(mem)
vcov(mem)

anova(mem)
    ## I think this means ID does not have a significant effect

 ### good website https://stats.oarc.ucla.edu/other/mult-pkg/introduction-to-linear-mixed-models/

  ## look into more model transformations and results + p value



### temporal patterns of sounders entering and leaving the sound

# using grey_annual_change for df since it already contains columns: ID, year, first_sight, last_sight

# wrapped by ID
grey_annual_change%>%
  filter(!(ID==185))

time_in_sound_ID=ggplot(data=grey_annual_change)+
  geom_segment(aes(x=first_sight,
                   xend=last_sight,
                   y=first_sight_depression_angle,
                   yend=last_sight_depression_angle,
                   colour=year))+
  facet_wrap(~ID)+
  ylab("Body Condition Angle")+
  xlab("Julian Day")+
  theme_bw()
ggsave(filename="Figures/Stage_3/time_in_sound_ID.png", height=6, width=10)

  # only 22
time_22 = grey_annual_change %>%
  filter(ID==22)

ggplot(data=time_22)+
  geom_segment(aes(x=first_sight,
                   xend=last_sight,
                   y=first_sight_depression_angle,
                   yend=last_sight_depression_angle,
                   colour=year))+
  facet_wrap(~ID)+
  ylab("Body Condition Angle")+
  xlab("Julian Day")+
  theme_bw()
ggsave(filename="Figures/Stage_3/time_22.png", height=4, width=6)

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
  xlab("Julian Day")+
  theme_bw()
ggsave(filename="Figures/Stage_3/time_in_sound_year.png", height=6, width=10)








### general count and graphs
ggplot(angle_data)+
  geom_bar(aes(x=year, fill=ID))+
  xlab("Year")+
  ylab("Measurements")+
  theme_bw()
ggsave(filename="Figures/Stage_3/count_year.png", height=6, width=10)

ggplot(angle_data)+
  geom_bar(aes(x=ID))+
  xlab("ID")+
  ylab("Measurements")+
  theme_bw()
ggsave(filename="Figures/Stage_3/count_ID.png", height=3, width=5)

count_month=angle_data%>%
  filter(!month=="NA")

ggplot(count_month)+
  geom_bar(aes(x=month))+
  xlab("Month")+
  ylab("Count")+
  theme_bw()
ggsave(filename="Figures/Stage_3/count_month.png", height=4, width=5)

ggplot(data=angle_data)+
  geom_boxplot(aes(y=ID, x=depression_angle))+
  theme_bw()+
  xlab("Body Condition Angle")
ggsave(filename="figures/all_angles_boxplot.png", width=6, height=4.5)

ggplot(data=low_offset_data)+
  geom_boxplot(aes(y=ID, x=depression_angle))+
  theme_bw()+
  xlab("Body Condition Angle")
ggsave(filename="figures/low_offset_boxplot.png", width=6, height=4.5)

year_data=angle_data%>%
  filter(!year%in%c(1990,1991,1992,1998,2000,2002,2003,2004,2005,2007,2008))

ggplot(angle_data, aes(x=julian_day, y=depression_angle, colour=ID))+
  geom_point()+
  facet_wrap(~year, nrow=4)+
  theme_bw()+
  xlab("Julian Day")+
  ylab("Body Condition Angle")
ggsave(filename="figures/filtered_year.png", width=8, height=6)

ggplot(low_ID_data, aes(x=julian_day, y=depression_angle))+
  geom_point(aes(x=julian_day, y=depression_angle))+
  stat_smooth(method = "lm", se=F, 
              aes(x=julian_day, y=depression_angle))+
  xlab("Julian Day")+
  ylab("Body Condition Angle")+
  theme_bw()
ggsave(filename="Figures/all_angles_julian_filteredID.png", height=5, width=6)


plot_lm=data.frame(Predicted_value=predict(mixed_low_ID),
                   Observed_value=low_ID_data$depression_angle)

#plot_lm=data.frame(Predicted_value=predict(lm_angle_vs_day),
                   #Observed_value=angle_data$depression_angle)


ggplot(plot_lm, aes(x=Predicted_value, y=Observed_value))+
  geom_point()+
  geom_abline(intercept=0, slope=1, colour="red")+
  ylab("Measured Body Condition Angle")+
  xlab("Predicted Body Condition Angle")+
  theme_bw()
ggsave(filename="Figures/predicted_measurements_filteredID.png", height=5, width=7)


grey_annual_change=grey_annual_change%>%
  filter(!diff_depression_angle=="NA")
plot_change=data.frame(Predicted_value=predict(mixed_annual_change),
                   Observed_value=grey_annual_change$diff_depression_angle)

ggplot(plot_change, aes(x=Predicted_value, y=Observed_value, colour=grey_annual_change$ID))+
  geom_point()+
  geom_abline(intercept=0, slope=1, colour="red")+
  ylab("Measured Annual Change in Body Condition Angle")+
  xlab("Predicted Annual Change in Body Condition Angle")+
  labs(colour="ID")+
  theme_bw()
ggsave(filename="Figures/predicted_annual_change_in_measurements.png", height=5, width=7)






### linear regression
lm_angle_vs_day = lm(depression_angle~julian_day, data=angle_data)
summary(lm_angle_vs_day)

lm_angle_vs_day_low_offset = lm(depression_angle~julian_day, data=low_offset_data)
summary(lm_angle_vs_day_low_offset)

lm_angle_vs_day_low_ID = lm(depression_angle~julian_day, data=low_ID_data)
summary(lm_angle_vs_day_low_ID)

lm_annual = lm(diff_depression_angle~sight_period_days, data=grey_annual_change)
summary(lm_annual)


# Jasmine
lm_angle_vs_day = lm(AoD_J~julian_day, data=angle_data)
summary(lm_angle_vs_day)

lm_angle_vs_day_low_offset = lm(AoD_J~julian_day, data=low_offset_data)
summary(lm_angle_vs_day_low_offset)

lm_angle_vs_day_low_ID = lm(AoD_J~julian_day, data=low_ID_data)
summary(lm_angle_vs_day_low_ID)



### mixed effects
mixed=lmer(depression_angle~julian_day + (1|year), data=angle_data)
summary(mixed)
anova(mixed)

mixed_offset=lmer(depression_angle~julian_day + (1|year), data=low_offset_data)
summary(mixed_offset)
anova(mixed_offset)

mixed_ext=lmer(depression_angle~julian_day  + (1|year) + (1|ID) + (1|offset), data=angle_data)
summary(mixed_ext)
anova(mixed_ext)

mixed_low_offset=lmer(depression_angle~julian_day + (1|year)+ (1|ID) + (1|offset), data=low_offset_data)
summary(mixed_low_offset)
anova(mixed_low_offset)

mixed_low_ID=lmer(depression_angle~julian_day + (1|year)+ (1|ID) + (1|offset), data=low_ID_data)
summary(mixed_low_ID)
anova(mixed_low_ID)

mixed_22=lmer(depression_angle~julian_day + (1|year) + (1|offset), data=data_22)
summary(mixed_22)
anova(mixed_22)

mixed_annual_change=lmer(diff_depression_angle~sight_period_days + (1|year) + (1|ID), data=grey_annual_change)
summary(mixed_annual_change)



# Jasmine measurements 

mixed_ext=lmer(AoD_J~julian_day  + (1|year) + (1|ID) + (1|offset), data=angle_data)
summary(mixed_ext)
anova(mixed_ext)

mixed_low_offset=lmer(AoD_J~julian_day + (1|year)+ (1|ID) + (1|offset), data=low_offset_data)
summary(mixed_low_offset)
anova(mixed_low_offset)

mixed_low_ID=lmer(AoD_J~julian_day + (1|year)+ (1|ID) + (1|offset), data=low_ID_data)
summary(mixed_low_ID)
anova(mixed_low_ID)


###
linear_ID=lm(depression_angle~julian_day:ID, data=angle_data)
summary(linear_ID)

linear_year=lm(depression_angle~julian_day:year, data=angle_data)
summary(linear_year)

ggplot(data = angle_data)+
  geom_point(aes(x=depression_angle, y=bradford))+
  facet_wrap("year")

ggplot(data = angle_data)+
  geom_point(aes(x=julian_day, y=AoD_J))+
  facet_wrap("year")



# MEM plots

all = angle_data%>%
  mutate(fit.m=predict(mixed_ext, re.form=NA),
         fit.c=predict(mixed_ext, re.form=NULL))


