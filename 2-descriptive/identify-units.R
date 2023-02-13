# Sophia Tan 1/13/23
# identify units for indirect effects

rm(list=ls())
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)

d <- read_csv("complete-data.csv")
infections <- d %>% group_by(ResidentId, num_pos) %>% filter(!num_pos %>% is.na()) %>% 
  summarise(Day_inf=first(Day))

# keep data only for Omicron period
group_room <- d %>% filter(!is.na(Institution) & !is.na(RoomId)) %>% filter(Day >= "2021-12-15") %>%
  group_by(Institution, RoomId, Day)

# test <- group_room %>% summarise(num_res=n()) %>%
#   mutate(less_than_5 = ifelse(num_res<=5, num_res, 0), less_than_2 = ifelse(num_res<=2, num_res, 0))
# test2 <- test %>%
#   group_by(Day) %>% summarise(prop_people_5 = sum(less_than_5)/sum(num_res), prop_people_2 = sum(less_than_2)/sum(num_res))
# test2 %>% ggplot(aes(Day)) + geom_line(aes(y=prop_people_5, color="Proportion of residents in rooms of 5 or less people")) +
#   geom_line(aes(y=prop_people_2, color="Proportion of residents in rooms of 2 or less people"))

# # plot average room size over time
# # average room rize is 2-3 people
# room_size_over_time <- group_room %>% summarise(n=n()) %>% group_by(Day) %>% summarise(mean=mean(n))
# ggplot(room_size_over_time, aes(Day, mean)) + geom_line() +
#   scale_x_date("Day", breaks="month", expand=c(0.05,0)) +
#   scale_y_continuous("Average room size", breaks=seq(2.1, 2.3, 0.025)) +
#   theme(axis.text.x = element_text(angle=90))

# include only residents that were incarcerated over the entire pandemic (before 4/1/2020)
duration <- read_csv("housing_duration.csv")
duration <- duration %>% filter(first<="2020-03-31")
included <- duration$ResidentId %>% unique()

# include only residents that stayed in rooms of 2 individuals
group_room_summary <- group_room %>% filter(n()==2)

residents <- group_room_summary %>% group_by(ResidentId) %>% group_keys()
residents <- residents %>% filter(ResidentId %in% included)
group_room_summary_entirepandemic <- group_room_summary %>% inner_join(residents)
group_room_summary_entirepandemic <- group_room_summary_entirepandemic %>% filter(n()==2)

# label if residents have had documented infection in the last 90 days
group_room_summary_entirepandemic <- group_room_summary_entirepandemic %>% 
  left_join(infections, by=c("ResidentId", "num_pos")) 
group_room_summary_entirepandemic <- group_room_summary_entirepandemic %>% 
  mutate(inf_90_days = difftime(Day, Day_inf, units="days") + 1 < 90)

group_room_summary_entirepandemic %>% group_by(ResidentId) %>% filter(any(num_pos>=1)) %>% 
  select(ResidentId, Day, num_pos, Day_inf, inf_90_days) %>% head(1000) %>% view()

# label residents in rooms
group_room_2 <- group_room_summary_entirepandemic %>% mutate(num=as.factor(1:n())) %>%
  arrange(Institution, RoomId, Day)
group_room_2 <- group_room_2 %>%
  select(Institution, BuildingId, RoomType, RoomId, Day, ResidentId, num, num_pos, inf_90_days, num_dose_adjusted)
group_room_2 <- group_room_2 %>% mutate(vacc=ifelse(num_dose_adjusted>0,1,0))

# keep residents with clear building reporting
# some rooms have multiple building labels within the same institution
group_room_2 <- group_room_2 %>% group_by(Institution, RoomId) %>%
  filter(length(unique(BuildingId))==1) %>%
  as.data.frame()

# get dataset for BuildingIds based on RoomId and Institution
building_room <- group_room_2 %>% group_by(Institution, RoomId, BuildingId) %>% group_keys()

# reshape dataset to be wide so each row represents a room-day instead of a resident-day over time
group_room_2_wide <- group_room_2 %>% select(!BuildingId) %>%
  reshape(idvar = c("Institution", "RoomId", "Day"),
          timevar = "num",
          v.names = c("ResidentId", "num_pos", "num_dose_adjusted", "vacc", "inf_90_days"),
          direction = "wide")

# add building type
group_room_2_wide <- group_room_2_wide %>% left_join(building_room)
group_room_2_wide <- group_room_2_wide %>% select(Institution, BuildingId, everything())

write_csv(group_room_2_wide, "wide_housing_2room.csv")

# read in dataset
group_room_2_wide <- read_csv("wide_housing_2room.csv") 

group_room_2_wide <- group_room_2_wide %>% arrange(Institution, BuildingId, RoomId, Day)

group_room_2_wide %>% select(Institution, BuildingId, RoomId, Day, ResidentId.1, ResidentId.2) %>% head(20)

# make sure residents co-reside for continuous 30 days (no skipped days)
# make sure both residents have not had prior infection within the last 90 days
g <- group_room_2_wide %>% group_by(Institution, RoomId, ResidentId.1, ResidentId.2) %>% 
  mutate(cont = c(0, diff(Day)), more_1_day = cont==0|cont>1) 

g1 <- g %>% filter(more_1_day) %>% select(Day) %>% mutate(unit_label=1:n())

g <- g %>% left_join(g1) 
g <- g %>% select(!c(cont, more_1_day))
g <- g %>% fill(unit_label, .direction="down")

group_room_2_wide_distinct <- g %>%
  distinct(Institution,RoomId,ResidentId.1,ResidentId.2,unit_label,
           vacc.1,vacc.2,.keep_all=T)

group_room_2_wide_distinct <- group_room_2_wide_distinct %>% 
  replace_na(list(inf_90_days.1=F, inf_90_days.2=F)) %>% 
  mutate(both_unvacc=(vacc.1==0&vacc.2==0),
         one_unvacc=(!both_unvacc&(vacc.1==0|vacc.2==0)),
         num_inf=case_when(
           is.na(num_pos.1)&is.na(num_pos.2)~0,
           is.na(num_pos.1)|is.na(num_pos.2)~1,
           !is.na(num_pos.1)&!is.na(num_pos.2)~2),
         prior_inf_90 = inf_90_days.1==1|inf_90_days.2==1) 

labeled <- group_room_2_wide_distinct %>% group_by(Institution, RoomId) %>% 
  mutate(label=1:n()) %>% ungroup() %>% 
  select(Institution, RoomId, Day, label)

group_room_2_wide <- group_room_2_wide %>% left_join(labeled) 
group_room_2_wide_summary <- group_room_2_wide %>% group_by(Institution, RoomId) %>%
  fill(label, .direction="down") %>% 
  group_by(Institution, RoomId, label) %>%
  summarise(first=first(Day), last=last(Day)) %>%
  mutate(duration=difftime(last, first, units="days")+1)

any_unvacc <- group_room_2_wide_summary %>% select(!label) %>% 
  left_join(group_room_2_wide_distinct, by=c("Institution", "RoomId", "first"="Day")) %>%
  rowwise() %>% filter(any(both_unvacc|one_unvacc))
any_unvacc

any_unvacc %>% ggplot(aes(duration)) + geom_histogram() + 
  scale_x_continuous("Duration of co-residence (days)",limits=c(0,365),
                     expand=c(0,0)) + scale_y_continuous("Number of units",expand=c(0,0))
any_unvacc$duration %>% as.numeric() %>% summary()


p1 <- any_unvacc %>% ggplot(aes(Institution)) + geom_bar() + 
  scale_x_continuous(limits=c(1,35),breaks=1:35,labels=1:35, expand=c(0,0)) + 
  scale_y_continuous("Number of units") 

p2 <- any_unvacc %>% filter(duration >= 30) %>% ggplot(aes(Institution)) + geom_bar() + 
  scale_x_continuous(limits=c(1,35),breaks=1:35,labels=1:35, expand=c(0,0)) + 
  scale_y_continuous("Number of units")

library(patchwork)
p1/p2

any_unvacc_over30 <- any_unvacc %>% filter(duration>=30) 
any_unvacc_over30 %>% ggplot(aes(duration)) + geom_histogram() + 
  scale_x_continuous("Duration of co-residence (days)",limits=c(0,365),
                     expand=c(0,0)) + scale_y_continuous("Number of units",expand=c(0,0))
any_unvacc_over30$duration %>% as.numeric() %>% summary()

any_unvacc_over30_noinf <- any_unvacc_over30 %>% filter(!prior_inf_90)


# check testing data
residents <- c(any_unvacc_over30_noinf$ResidentId.1, any_unvacc_over30_noinf$ResidentId.2) %>% unique()
testing <- read_csv("complete_testing_data.csv")
testing <- testing %>% filter(Day >= "2021-12-15")
testing <- testing %>% filter(ResidentId %in% residents) %>% select(ResidentId, Day) %>% rename("Test" = "Day")

any_unvacc_over30_noinf <- any_unvacc_over30_noinf %>% select(!unit_label) %>% ungroup() %>% mutate(label=1:n())

units_testing1 <- any_unvacc_over30_noinf %>% 
  left_join(testing, by=c("ResidentId.1"="ResidentId")) %>% 
  filter(Test >= first & Test <= last) %>% rename("Test.1" = "Test")
units_testing1 %>% select(Institution, RoomId, first, last, ResidentId.1, Test.1)

# units_testing <- units_testing %>% group_by(label) %>% 
#   mutate(time_between_tests = c(NA, diff(Test.1))) %>%
#   mutate(mean_time.1 = mean(time_between_tests, na.rm=T),
#          time_first.1 = difftime(Test.1[1], first[1]) %>% as.numeric(),
#          time_last.1 = difftime(last[1], Test.1[n()]) %>% as.numeric()) %>%
#   summarise_all(first) %>% 
#   select(!c(Test.1, time_between_tests))

test1 <- units_testing1 %>% mutate(Month=format(as.Date(Test.1), "%Y-%m"))
test1 <- test1 %>% group_by(label, Month) %>% 
  summarise(duration=first(duration), num_tests=n()) %>% 
  group_by(label) %>% 
  mutate(mean_tests=mean(num_tests)) %>% summarise_all(first) %>% 
  select(!num_tests) %>%
  filter(mean_tests>=2|duration <= 35)

units_testing2 <- any_unvacc_over30_noinf %>% left_join(testing, by=c("ResidentId.2"="ResidentId")) %>% 
  filter(Test >= first & Test <= last) %>% rename("Test.2" = "Test")
units_testing2 %>% select(Institution, RoomId, first, last, ResidentId.2, Test.2)

test2 <- units_testing2 %>% mutate(Month=format(as.Date(Test.2), "%Y-%m"))
test2 <- test2 %>% group_by(label, Month) %>% 
  summarise(duration=first(duration), num_tests=n()) %>% 
  group_by(label) %>% 
  mutate(mean_tests=mean(num_tests)) %>% summarise_all(first) %>% 
  select(!num_tests) %>% 
  filter(mean_tests>=2|duration <= 35)

matching <- any_unvacc_over30_noinf %>% filter(label %in% unique(units_testing1$label) & label %in% unique(units_testing2$label))

matching <- matching %>% 
  mutate(treatment = ifelse(both_unvacc, 1, 0),
         adjusted_start = first+7)

matching$treatment %>% table()

write_csv(units, "full_data_prematching.csv")
