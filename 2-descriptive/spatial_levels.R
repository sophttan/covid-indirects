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


summary2 <- d %>% arrange(BuildingId, Day, ResidentId) %>% group_by(Institution, BuildingId, Day) %>% 
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

pdf("D:/CCHCS_premium/st/indirects/testing/building_vacc_inf_profile.pdf")
for (id in (summary2%>%group_keys())$BuildingId) {
  p <- summary2 %>% ungroup() %>% 
    filter(BuildingId==id) %>% ggplot(aes(Day)) + 
    geom_line(aes(y=prop_vacc, color="Previously vaccinated (any)")) + 
    geom_line(aes(y=prop_inf, color="Previously infected (any)"))
  print(p)
}
dev.off()


spread <- summary2 %>% group_by(Day) %>% summarise(vacc_mean=mean(prop_vacc),
                                         vacc_25=quantile(prop_vacc, .25),
                                         vacc_75=quantile(prop_vacc, .75),
                                         inf_mean=mean(prop_inf),
                                         inf_25=quantile(prop_inf, .25),
                                         inf_75=quantile(prop_inf, .75))
spread %>% ggplot(aes(Day, vacc_mean)) + geom_point() + 
  geom_linerange(aes(ymin=vacc_25, ymax=vacc_75))

spread %>% ggplot(aes(Day, inf_mean)) + geom_point() + 
  geom_linerange(aes(ymin=inf_25, ymax=inf_75))


building_summary <- summary2 %>% 
  group_by(Institution, BuildingId) %>% summarise(Institution=unique(Institution), `Average Number of Residents`=mean(num_res)) 

building_summary %>% write_csv("D:/CCHCS_premium/st/indirects/covid-indirects/data/building_summary.csv")

institution_summary <- building_summary %>% group_by(Institution) %>% summarise_all(mean) %>% select(!BuildingId)
institution_summary

institution_summary %>% write_csv("D:/CCHCS_premium/st/indirects/covid-indirects/data/institution_summary.csv")

institution_summary %>% ggplot(aes(Institution, `Average Number of Residents`)) + geom_bar(stat="identity") +
  scale_x_continuous(breaks=1:35, limits=c(0,36), expand=c(0,0)) + 
  scale_y_continuous("Average Building Size")
