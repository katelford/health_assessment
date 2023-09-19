

library(tidyverse)
library(dplyr)
library(GGally)    


compare_angle_data=read.csv('Data/Updated.csv',
                            col.names=c("AoD_K","julian_day","offset","ID","year", "AoD_J","bradford", "month"))%>%
  filter(!(ID=="NA"))

compare_angle_data$ID=as.factor(compare_angle_data$ID)
compare_angle_data$year=as.factor(compare_angle_data$year)
compare_angle_data$month=as.factor(compare_angle_data$month)
compare_angle_data$bradford=as.factor(compare_angle_data$bradford)

head(compare_angle_data)
class(compare_angle_data$AoD_J)
class(compare_angle_data$bradford)

filter(compare_angle_data, !is.na(AoD_J) | AoD_J!="", !is.na(AoD_K) | AoD_K!="", !is.na(bradford) | bradford!="")

low_offset_data=compare_angle_data%>%
  filter(offset<3)





# paired t-test
t.test(x=compare_angle_data$AoD_J, y=compare_angle_data$AoD_K, paired=T)


cor.test(x=compare_angle_data$AoD_J, y=compare_angle_data$AoD_K, use="complete.obs")



ggplot(data=compare_angle_data, aes(x=AoD_J, y=AoD_K))+
  geom_point()+
  xlab("Witt")+
  ylab("Telford")+
  theme_bw()+
  geom_abline(intercept=0, slope=1, colour="red")
ggsave(filename="Figures/jasmineVkira_all.png", height=5, width=6)


### Bradford
brad=compare_angle_data%>%
  filter(!is.na(bradford))

#brad=low_ID_data%>%
 # filter(!is.na(bradford))


ggplot(data = brad)+
  geom_point(aes(x=AoD_J, y=bradford))+
  xlab("Telford")+
  ylab("Bradford")+
  facet_wrap("ID", nrow = 4)+
  theme_bw()
ggsave(filename="Figures/bradford_all.png", height=7, width=8)


#### low id and offset
t.test(x=low_offset_data$AoD_J, y=low_offset_data$AoD_K, paired=T)

cor(x=low_offset_data$AoD_J, y=low_offset_data$AoD_K, use="complete.obs")


ggplot(data=low_offset_data, aes(x=AoD_J, y=AoD_K))+
  geom_point()+
  xlab("Witt")+
  ylab("Telford")+
  theme_bw()+
  geom_abline(intercept=0, slope=1, colour="red")
ggsave(filename="Figures/jasmineVkira_filtered.png", height=5, width=6)


# Bradford
bradford=low_offset_data%>%
  filter(!is.na(bradford))

summary(bradford)

ggplot(data = bradford)+
  geom_point(aes(x=AoD_J, y=bradford))+
  xlab("Telford")+
  ylab("Bradford")+
  facet_wrap("ID")+
  theme_bw()
ggsave(filename="Figures/bradford_filtered.png", height=5, width=7)
