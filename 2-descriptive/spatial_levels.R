rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)

d <- read_csv("complete-data.csv") 

names(d)

d <- d %>% filter(Day >= "2021-12-15") %>% arrange(ActivityCohortId, Day, ResidentId) %>% group_by(ActivityCohortId, Day)
d <- d %>% replace_na(list(num_pos=0))

summary <- d %>% 
  summarise(residents=list(ResidentId), 
            num_res=n(), 
            BuildingId=first(BuildingId), num_building=length(unique(BuildingId)), 
            Institution=first(Institution), num_inst=length(unique(Institution)),
            prop_inf=mean(num_pos>0)*100, 
            prop_vacc=mean(num_dose_adjusted>0)*100)

summary%>%group_by(Institution) %>% summarise(size=mean(num_res)) %>% ggplot(aes(Institution, size)) + geom_bar(stat="identity") +
  scale_x_continuous(breaks=1:35, limits=c(0,36), expand=c(0,0)) + 
  scale_y_continuous("Activity Cohort Size")
 
summary%>%filter(num_res<=750)%>%ggplot(aes(num_res)) + geom_histogram() + 
  scale_x_continuous("Activity Cohort Size")

summary <- summary %>% group_by(ActivityCohortId) %>% mutate(diff=c(1, diff(Day)))

summary_gap <- summary %>% distinct(residents, diff, .keep_all = T) %>% select(ActivityCohortId, Day) %>%
  group_by(ActivityCohortId) %>% mutate(label=1:n())
summary_gap

summary <- summary %>% left_join(summary_gap) %>% group_by(ActivityCohortId) %>% fill(label, .direction = "down")

summary_overall <- summary %>% group_by(ActivityCohortId, label) %>% 
  summarise(Institution=first(Institution), BuildingId=first(BuildingId), start=first(Day), duration=n(), num_res=first(num_res), prop_inf=mean(prop_inf), prop_vacc=mean(prop_vacc))

summary_overall

summary_overall%>%group_by(Institution) %>% summarise(duration=mean(duration)) %>% ggplot(aes(Institution, duration)) + geom_bar(stat="identity") +
  scale_x_continuous(breaks=1:35, limits=c(0,36), expand=c(0,0)) +
  scale_y_continuous("Duration (days) of the same cohort")


(d %>% group_by(BuildingId,) %>% summarise(num_act = unique(ActivityCohortId) %>% length()))$num_act %>% summary()


summary2 <- d %>% arrange(BuildingId, Day, ResidentId) %>% group_by(BuildingId, Day) %>% 
  summarise(residents=list(ResidentId), 
            num_res=n(), 
            #BuildingId=first(BuildingId), num_building=length(unique(BuildingId)), 
            Institution=first(Institution), num_inst=length(unique(Institution)),
            prop_inf=mean(num_pos>0)*100, 
            prop_vacc=mean(num_dose_adjusted>0)*100)

summary2%>%group_by(Institution) %>% summarise(size=mean(num_res)) %>% ggplot(aes(Institution, size)) + geom_bar(stat="identity") +
  scale_x_continuous(breaks=1:35, limits=c(0,36), expand=c(0,0)) + 
  scale_y_continuous("Building Size")

summary2%>%filter(num_res<=750)%>%ggplot(aes(num_res)) + geom_histogram() + 
  scale_x_continuous("Activity Cohort Size")

summary2 <- summary2 %>% group_by(BuildingId) %>% mutate(diff=c(1, diff(Day)))

summary_gap2 <- summary2 %>% distinct(residents, diff, .keep_all = T) %>% select(BuildingId, Day) %>%
  group_by(BuildingId) %>% mutate(label=1:n())
summary_gap2

summary2 <- summary2 %>% left_join(summary_gap2) %>% group_by(BuildingId) %>% fill(label, .direction = "down")

summary_overall2 <- summary2 %>% group_by(BuildingId, label) %>% 
  summarise(Institution=first(Institution), BuildingId=first(BuildingId), start=first(Day), duration=n(), num_res=first(num_res), prop_inf=mean(prop_inf), prop_vacc=mean(prop_vacc))

summary_overall2

summary_overall2%>%group_by(Institution) %>% summarise(duration=mean(duration)) %>% ggplot(aes(Institution, duration)) + geom_bar(stat="identity") +
  scale_x_continuous(breaks=1:35, limits=c(0,36), expand=c(0,0)) +
  scale_y_continuous("Duration (days) of the same cohort")
