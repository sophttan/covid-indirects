rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)

d <- read_csv("complete-data-vaccperiod.csv") 

names(d)

d <- d %>% filter(Day >= "2020-12-01")
d <- d %>% replace_na(list(num_pos=0))

d <- d %>% mutate(partial = num_dose_adjusted>0,
                  full_vacc_binary = num_dose_adjusted>0 & num_dose_adjusted>=first(full_vacc),
                  booster1 = num_dose_adjusted>0 & num_dose_adjusted-first(full_vacc)>=1,
                  booster2up = num_dose_adjusted>0 & num_dose_adjusted-first(full_vacc)>1)
d %>% select(ResidentId, Day, num_dose, num_dose_adjusted, full_vacc, partial, full_vacc_binary, booster1, booster2up)
d <- d %>% group_by(ResidentId, num_dose) %>% mutate(recent_vacc = Day-first(Day)<=90 & num_dose_adjusted>0) %>%
  mutate(bivalent = (grepl("bivalent", Vaccine)|first(Day)>="2022-09-01") & num_dose_adjusted>0)

d <- d %>% group_by(ResidentId, num_pos) %>% mutate(infection=first(Day)==Day&num_pos>0)

d %>% ungroup() %>% filter(ResidentId==ResidentId[300000]) %>% 
  select(ResidentId, Day, num_pos, num_dose, num_dose_adjusted, 
         full_vacc, partial, full_vacc_binary, booster1, booster2up, bivalent, recent_vacc, infection) %>%
  view()

summary_floor <- d %>% arrange(Institution, BuildingId, FloorNumber, Day, ResidentId) %>% 
  group_by(Institution, BuildingId, FloorNumber, Day) %>% 
  summarise(residents=list(ResidentId), 
            num_res=n(), 
            new_inf=sum(infection),
            inf_floor=sum(num_pos>0), 
            any_vacc_floor=sum(num_dose_adjusted>0),
            partial=sum(partial),
            full_vacc=sum(full_vacc_binary),
            boost1=sum(booster1),
            boost2up=sum(booster2up),
            recent_vacc=sum(recent_vacc),
            bivalent=sum(bivalent),
            inf_any_vacc_floor=sum(num_pos>0|num_dose_adjusted>0),
            inf_full_floor=sum(num_pos>0|full_vacc_binary),
            inf_boost1_floor=sum(num_pos>0|booster1),
            inf_boost2_floor=sum(num_pos>0|booster2up),
            prop_inf_floor=inf_floor/num_res*100,
            prop_any_vacc_floor=any_vacc_floor/num_res*100,
            prop_partial=partial/num_res*100,
            prop_full=full_vacc/num_res*100,
            prop_boost=boost1/num_res*100,
            prop_boost2=boost2up/num_res*100,
            prop_recent_vacc=recent_vacc/num_res*100,
            prop_bivalent=bivalent/num_res*100,
            prop_inf_any_vacc_floor=inf_any_vacc_floor/num_res*100,
            prop_inf_full_floor=inf_full_floor/num_res*100,
            prop_inf_boost1_floor=inf_boost1_floor/num_res*100,
            prop_inf_boost2_floor=inf_boost2_floor/num_res*100)


spread <- summary_floor %>% group_by(Day) %>% summarise(vacc_mean=mean(prop_bivalent),
                                                        vacc_25=quantile(prop_bivalent, .25),
                                                        vacc_75=quantile(prop_bivalent, .75),
                                                        inf_mean=mean(prop_full),
                                                        inf_25=quantile(prop_full, .25),
                                                        inf_75=quantile(prop_full, .75))

summary_floor_week <- summary_floor %>% 
  mutate(week=difftime(Day, "2020-12-01", units="weeks")%>%as.numeric()%>%round()) %>%
  group_by(Institution, BuildingId, FloorNumber, week) %>% 
  mutate(num_unique_res = unique(unlist(residents))%>%length()) %>% 
  select(num_unique_res, new_inf, prop_recent_vacc, prop_bivalent, prop_boost, prop_boost2) %>% 
  summarise_all(c(sum, mean, sd))

summary_floor_week %>% filter(week>=92) %>% 
  mutate(inf_prop=new_inf_fn1/num_unique_res_fn2*100) %>% 
  ggplot(aes(prop_bivalent_fn2, inf_prop)) + 
  geom_point() + scale_x_continuous("Bivalent booster coverage") + 
  scale_y_continuous("Proportion of residents with new infection")

summary_floor_week %>% filter(week>=40) %>% 
  mutate(inf_prop=new_inf_fn1/num_unique_res_fn2*100) %>% 
  ggplot(aes(prop_boost_fn2, inf_prop)) + 
  geom_point(aes(color=week)) + scale_x_continuous("1+ booster coverage") + 
  scale_y_continuous("Proportion of residents with new infection")

spread %>% ggplot(aes(Day, vacc_mean)) + geom_point() + 
  geom_linerange(aes(ymin=vacc_25, ymax=vacc_75)) + 
  scale_y_continuous("Bivalent booster coverage (Mean % (IQR))", limits = c(0,100)) + 
  scale_x_date(limits=as.Date(c("2022-09-01", "2022-12-20"), breaks="month"))

d <- d %>% arrange(ActivityCohortId, Day, ResidentId) %>% group_by(ActivityCohortId, Day)
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


### building and floor
weird_buildings <- read_csv("rooms_mult_buildings.csv")

# how many floors in a building?
d %>% group_by(Institution, BuildingId) %>% 
  summarise(num_activity = length(unique(ActivityCohortId)),
            num_floors= length(unique(FloorNumber))) %>% summary()

summary_floor <- d %>% arrange(Institution, BuildingId, FloorNumber, Day, ResidentId) %>% 
  group_by(Institution, BuildingId, FloorNumber, Day) %>% 
  summarise(residents=list(ResidentId), 
            num_res=n(), 
            #BuildingId=first(BuildingId), num_building=length(unique(BuildingId)), 
            #Institution=first(Institution), num_inst=length(unique(Institution)),
            inf_floor=sum(num_pos>0), 
            vacc_floor=sum(num_dose_adjusted>0),
            inf_vacc_floor=sum(num_pos>0|num_dose_adjusted>0),
            prop_inf_floor=inf_floor/num_res*100,
            prop_vacc_floor=vacc_floor/num_res*100,
            prop_inf_vacc_floor=inf_vacc_floor/num_res*100)

summary_building <- summary_floor %>% 
  group_by(Institution, BuildingId, Day) %>% 
  mutate(num_res_building = sum(num_res),
         prop_inf_building = sum(inf_floor)/num_res_building*100,
         prop_vacc_building = sum(vacc_floor)/num_res_building*100,
         prop_inf_vacc_building = sum(inf_vacc_floor)/num_res_building*100)

summary_building <- summary_building %>% select(!c(inf_floor, vacc_floor, inf_vacc_floor))

summary_floor %>% group_by(Institution) %>% 
  summarise(size=mean(num_res)) %>% 
  ggplot(aes(Institution, size)) + geom_bar(stat="identity") +
  scale_x_continuous(breaks=1:35, limits=c(0,36), expand=c(0,0)) + 
  scale_y_continuous("Floor Size", limits=c(0, 250))

summary_building %>%
  distinct(Institution, BuildingId, Day, .keep_all = T) %>% 
  group_by(Institution) %>% 
  summarise(size=mean(num_res_building)) %>% 
  ggplot(aes(Institution, size)) + geom_bar(stat="identity") +
  scale_x_continuous(breaks=1:35, limits=c(0,36), expand=c(0,0)) + 
  scale_y_continuous("Building Size", limits=c(0, 250))

summary2 %>% filter(num_res<=750)%>%ggplot(aes(num_res)) + geom_histogram() + 
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


spread <- summary_floor %>% group_by(Day) %>% summarise(vacc_mean=mean(prop_vacc_floor),
                                         vacc_25=quantile(prop_vacc_floor, .25),
                                         vacc_75=quantile(prop_vacc_floor, .75),
                                         inf_mean=mean(prop_inf_floor),
                                         inf_25=quantile(prop_inf_floor, .25),
                                         inf_75=quantile(prop_inf_floor, .75))
spread %>% ggplot(aes(Day, vacc_mean)) + geom_point() + 
  geom_linerange(aes(ymin=vacc_25, ymax=vacc_75)) + 
  scale_y_continuous("Floor vaccine coverage (Mean % (IQR))", limits = c(0,100))

spread %>% ggplot(aes(Day, inf_mean)) + geom_point() + 
  geom_linerange(aes(ymin=inf_25, ymax=inf_75))


building_summary <- summary_building %>% 
  distinct(Institution, BuildingId, Day, .keep_all = T) %>%
  mutate(month = format(Day, "%m-%y")) %>% 
  group_by(Institution, BuildingId) %>% 
  summarise(`Average Number of Residents`=mean(num_res_building),
            `Previously infected (%)`=mean(prop_inf_building),
            `Vaccine coverage (%)` = mean(prop_vacc_building),
            `Population immunity (%)` = mean(prop_inf_vacc_building)) #%>%
  # ungroup() %>% as.data.frame() %>% 
  # reshape(idvar = c("Institution", "BuildingId"),
  #         timevar = "month",
  #         v.names = c("Previously infected (%)", "Vaccine coverage (%)", "Population immunity (%)"),
  #         direction = "wide")
building_summary

building_summary %>% round() %>% write_csv("D:/CCHCS_premium/st/indirects/covid-indirects/data/building_summary051523.csv")

floor_summary <- summary_building %>% 
  mutate(month = format(Day, "%m-%y")) %>% 
  group_by(Institution, BuildingId, FloorNumber) %>% 
  summarise(`Average Number of Residents`=mean(num_res),
            `Previously infected (%)`=mean(prop_inf_floor),
            `Vaccine coverage (%)` = mean(prop_vacc_floor),
            `Population immunity (%)` = mean(prop_inf_vacc_floor)) #%>%
# ungroup() %>% as.data.frame() %>% 
# reshape(idvar = c("Institution", "BuildingId"),
#         timevar = "month",
#         v.names = c("Previously infected (%)", "Vaccine coverage (%)", "Population immunity (%)"),
#         direction = "wide")
floor_summary

floor_summary %>% round() %>% write_csv("D:/CCHCS_premium/st/indirects/covid-indirects/data/floor_summary051523.csv")

hist(floor_summary$`Previously infected (%)`, main=NULL, xlab = "Previously infected (%)")
hist(floor_summary$`Vaccine coverage (%)`, main="", xlab = "Vaccine coverage (%)")
hist(floor_summary$`Population immunity (%)`, main="", xlab = "Population immunity (%)")

institution_summary <- building_summary %>% group_by(Institution) %>% summarise_all(mean) %>% select(!BuildingId)
institution_summary

institution_summary %>% round() %>% write_csv("D:/CCHCS_premium/st/indirects/covid-indirects/data/institution_summary.csv")

institution_summary %>% ggplot(aes(Institution, `Average Number of Residents`)) + geom_bar(stat="identity") +
  scale_x_continuous(breaks=1:35, limits=c(0,36), expand=c(0,0)) + 
  scale_y_continuous("Average Building Size")
