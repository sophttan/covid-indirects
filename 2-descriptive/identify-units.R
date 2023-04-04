# Sophia Tan 1/13/23
# identify units for indirect effects

rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)

d <- read_csv("complete-data.csv") 
d <- d %>% select(ResidentId, Day, Institution, BuildingId, RoomType, RoomId, num_pos, num_dose_adjusted)
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

write_csv(group_room_summary_entirepandemic, "group_room_data_2_entireperiod.csv")

# label residents in rooms
group_room_2 <- group_room_summary_entirepandemic %>% mutate(num=as.factor(1:n())) %>%
  arrange(Institution, RoomId, Day)
group_room_2 <- group_room_2 %>%
  select(Institution, BuildingId, RoomType, RoomId, Day, ResidentId, num, num_pos, inf_90_days, num_dose_adjusted)
group_room_2 <- group_room_2 %>% mutate(vacc=ifelse(num_dose_adjusted>0,1,0))
group_room_2

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

group_room_2_wide <- group_room_2_wide %>% replace_na(list(inf_90_days.1=F, inf_90_days.2=F)) %>% 
  mutate(prior_inf_90 = inf_90_days.1==1|inf_90_days.2==1)

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
           vacc.1,vacc.2,prior_inf_90,.keep_all=T) %>% ungroup()

group_room_2_wide_distinct_final <- group_room_2_wide_distinct %>% 
  group_by(Institution, RoomId, ResidentId.1, ResidentId.2, unit_label, vacc.1, vacc.2) %>% 
  mutate(new_unit=n()>1&first(prior_inf_90)) %>% 
  mutate(new_no_inf=ifelse(new_unit, 1:n(), 1)) %>% 
  distinct(Institution,RoomId,ResidentId.1,ResidentId.2,unit_label,
           vacc.1,vacc.2,new_no_inf,.keep_all=T)

group_room_2_wide_distinct_final <- group_room_2_wide_distinct_final %>% 
  mutate(both_unvacc=(vacc.1==0&vacc.2==0),
         one_unvacc=(!both_unvacc&(vacc.1==0|vacc.2==0)),
         num_inf=case_when(
           is.na(num_pos.1)&is.na(num_pos.2)~0,
           is.na(num_pos.1)|is.na(num_pos.2)~1,
           !is.na(num_pos.1)&!is.na(num_pos.2)~2)) 

group_room_2_wide_distinct_final <- group_room_2_wide_distinct_final %>% ungroup() %>% select(!c(unit_label, new_unit, new_no_inf))

labeled <- group_room_2_wide_distinct_final %>% ungroup() %>%
  mutate(label=1:nrow(.)) %>% 
  select(Institution, BuildingId, RoomId, Day, label)

group_room_2_wide <- group_room_2_wide %>% left_join(labeled) 
group_room_2_wide_summary <- group_room_2_wide %>% 
  fill(label, .direction="down") %>% 
  group_by(Institution, BuildingId, RoomId, label) %>%
  summarise(first=first(Day), last=last(Day)) %>%
  mutate(duration=difftime(last, first, units="days")+1)

group_room_2_final <- group_room_2_wide_summary %>% select(!label) %>% 
  left_join(group_room_2_wide_distinct_final, by=c("Institution", "BuildingId", "RoomId", "first"="Day")) %>%
  select(Institution, BuildingId, RoomId, RoomType, everything())

# exclude units if resident's don't co-reside for at least 2 weeks
over_14 <- group_room_2_final %>% filter(duration >= 14)

# keep units where at least 1 resident is unvaccinated
any_unvacc_over14 <- over_14 %>% filter(any(both_unvacc|one_unvacc))

any_unvacc_over14 %>% ggplot(aes(duration)) + geom_histogram() + 
  scale_x_continuous("Duration of co-residence (days)",limits=c(0,365),
                     expand=c(0,0)) + scale_y_continuous("Number of units",expand=c(0,0))
any_unvacc_over14$duration %>% as.numeric() %>% summary()


p1 <- group_room_2_final %>% ggplot(aes(Institution)) + geom_bar() + 
  scale_x_continuous(limits=c(1,35),breaks=1:35,labels=1:35, expand=c(0,0)) + 
  scale_y_continuous("Number of units") 

p2 <- any_unvacc_over14 %>% ggplot(aes(Institution)) + geom_bar() + 
  scale_x_continuous(limits=c(1,35),breaks=1:35,labels=1:35, expand=c(0,0)) + 
  scale_y_continuous("Number of units") 


library(patchwork)
p1/p2

any_unvacc_over14_noinf <- any_unvacc_over14 %>% filter(!prior_inf_90)


# check testing data
residents <- c(any_unvacc_over14_noinf$ResidentId.1, any_unvacc_over14_noinf$ResidentId.2) %>% unique()
testing <- read_csv("complete_testing_data.csv")
testing <- testing %>% filter(ResidentId %in% residents) %>% select(ResidentId, Day) %>% rename("Test" = "Day")

any_unvacc_over14_noinf <- any_unvacc_over14_noinf %>% ungroup() %>% mutate(label=1:n())

units_testing1 <- any_unvacc_over14_noinf %>% 
  left_join(testing, by=c("ResidentId.1"="ResidentId")) %>% 
  filter(Test >= first & Test <= last) %>% rename("Test.1" = "Test")
units_testing1 %>% select(Institution, RoomId, first, last, ResidentId.1, Test.1)

# test breaking apart 
a <- units_testing1 %>% select(label, ResidentId.1, first,last,duration,Test.1) %>%
  group_by(label) %>% 
  mutate(diff_test=diff(c(first(first),Test.1))) 

a <- a %>% group_by(label) %>% mutate(new_chunk=diff_test>14)
chunks <- a %>% filter(Test.1==first(Test.1)|new_chunk) %>% select(label, Test.1) %>% mutate(chunks=1:n())
a <- a %>% left_join(chunks, by=c("label", "Test.1")) 
a <- a %>% arrange(label, Test.1) %>% fill(chunks, .direction="down")
a_final <- a %>% group_by(label, chunks) %>% 
  mutate(first_chunked = case_when(first(diff_test)<=14~first,
                                   T~first(Test.1)),
         adjusted_start=case_when(first==first_chunked~first+5,
                                  first!=first_chunked~first_chunked),
         last_chunked = last(Test.1))%>% 
  summarise_all(first) %>% 
  mutate(duration_testing=last_chunked-first_chunked) %>% 
  group_by(label) %>% 
  filter(duration_testing==max(duration_testing)) %>% 
  filter(duration_testing > 0) %>% filter(chunks==first(chunks)) %>% 
  select(label, ResidentId.1, first_chunked, adjusted_start, last_chunked, duration_testing)


units_testing2 <- any_unvacc_over14_noinf %>% left_join(testing, by=c("ResidentId.2"="ResidentId")) %>% 
  filter(Test >= first & Test <= last) %>% rename("Test.2" = "Test")
units_testing2 %>% select(Institution, RoomId, first, last, ResidentId.2, Test.2)

# test breaking apart 
a2 <- units_testing2 %>% select(label, ResidentId.2, first,last,duration,Test.2) %>%
  group_by(label) %>% 
  mutate(diff_test=diff(c(first(first),Test.2))) 

a2 <- a2 %>% group_by(label) %>% mutate(new_chunk=diff_test>14)
chunks <- a2 %>% filter(Test.2==first(Test.2)|new_chunk) %>% select(label, Test.2) %>% mutate(chunks=1:n())
a2 <- a2 %>% left_join(chunks, by=c("label", "Test.2")) 
a2 <- a2 %>% arrange(label, Test.2) %>% fill(chunks, .direction="down")
a2_final <- a2 %>% group_by(label, chunks) %>% 
  mutate(first_chunked = case_when(first(diff_test)<=14~first,
                                   T~first(Test.2)),
         adjusted_start=case_when(first==first_chunked~first+5,
                                  first!=first_chunked~first_chunked),
         last_chunked = last(Test.2))%>% 
  summarise_all(first) %>% 
  mutate(duration_testing=last_chunked-first_chunked) %>% 
  group_by(label) %>% 
  filter(duration_testing==max(duration_testing)) %>% 
  filter(duration_testing > 0) %>% filter(chunks==first(chunks)) %>% 
  select(label, ResidentId.2, first_chunked, adjusted_start, last_chunked, duration_testing)


treatment <- any_unvacc_over14_noinf %>% filter(one_unvacc) %>% 
  filter((label %in% a_final$label|vacc.1>0) & (label %in% a2_final$label|vacc.2>0)) %>%
  mutate(primary = case_when(vacc.1==0~ResidentId.1, 
                             vacc.2==0~ResidentId.2)) %>% 
  mutate(inf.primary = ifelse(vacc.1==0, num_pos.1>0, num_pos.2>0),
         inf.secondary = ifelse(vacc.1>0, num_pos.1>0, num_pos.2>0)) %>% 
  left_join(a_final, by=c("label", "primary"="ResidentId.1")) %>% 
  left_join(a2_final, by=c("label", "primary"="ResidentId.2")) %>% 
  mutate(adjusted_start.x=as.character(adjusted_start.x), 
         adjusted_start.y=as.character(adjusted_start.y),
         last_chunked.x=as.character(last_chunked.x),
         last_chunked.y=as.character(last_chunked.y)) %>%
  replace_na(list(adjusted_start.x="", adjusted_start.y="",
                  last_chunked.x="", last_chunked.y="")) %>% 
  mutate(adjusted_start=paste0(adjusted_start.x, adjusted_start.y)%>%as.Date(), 
         last_chunked=paste0(last_chunked.x, last_chunked.y)%>%as.Date())
  
  

control <- any_unvacc_over14_noinf %>% filter(both_unvacc) %>% 
  filter(label %in% a_final$label | label %in% a2_final$label) 

controlboth <- control %>% filter(label %in% a_final$label & label %in% a2_final$label) %>%
  left_join(a_final, by=c("label", "ResidentId.1"="ResidentId.1")) %>% 
  left_join(a2_final, by=c("label", "ResidentId.2"="ResidentId.2")) %>% 
  rowwise() %>% 
  mutate(primary=case_when(duration_testing.x>duration_testing.y~ResidentId.1,
                           duration_testing.x<duration_testing.y~ResidentId.2,
                           T~sample(c(ResidentId.1, ResidentId.2), size = 1))) %>%
  mutate(adjusted_start=case_when(primary==ResidentId.1~adjusted_start.x, 
                                  T~adjusted_start.y),
         last_chunked=case_when(primary==ResidentId.1~last_chunked.x, 
                                  T~last_chunked.y)) %>%
  mutate(inf.primary = case_when(primary==ResidentId.1~num_pos.1,
                                 primary==ResidentId.2~num_pos.2), 
         inf.secondary = case_when(primary==ResidentId.1~num_pos.2,
                                   primary==ResidentId.2~num_pos.1)) %>% select(names(treatment))

controlone <- control %>% filter(!label %in% controlboth$label)
  
controlone <- controlone %>%
  rowwise() %>% 
  mutate(both_test=label%in%a_final$label&label%in%a2_final$label) %>%
  mutate(primary = case_when(label %in% a_final$label & !label %in% a2_final$label~ResidentId.1,
                             label %in% a2_final$label & !label %in% a_final$label~ResidentId.2,
                             label %in% a_final$label & label %in% a2_final$label~sample(c(ResidentId.1, ResidentId.2), size = 1))) %>%
  mutate(inf.primary = case_when(primary==ResidentId.1~num_pos.1,
                                 primary==ResidentId.2~num_pos.2), 
         inf.secondary = case_when(primary==ResidentId.1~num_pos.2,
                                   primary==ResidentId.2~num_pos.1)) %>% 
  left_join(a_final, by=c("label", "primary"="ResidentId.1")) %>% 
  left_join(a2_final, by=c("label", "primary"="ResidentId.2")) %>% 
  mutate(adjusted_start.x=as.character(adjusted_start.x), 
         adjusted_start.y=as.character(adjusted_start.y),
         last_chunked.x=as.character(last_chunked.x),
         last_chunked.y=as.character(last_chunked.y)) %>%
  replace_na(list(adjusted_start.x="", adjusted_start.y="",
                    last_chunked.x="", last_chunked.y="")) %>% 
  mutate(adjusted_start=paste0(adjusted_start.x, adjusted_start.y)%>%as.Date(), 
           last_chunked=paste0(last_chunked.x, last_chunked.y)%>%as.Date()) %>% select(names(treatment))

matching <- treatment %>% rbind(controlboth, controlone) %>% 
  mutate(inf.primary=ifelse(inf.primary%>%is.na(), 0, 1), 
         inf.secondary=ifelse(inf.secondary%>%is.na(), 0, 1))
  
matching <- matching %>% 
  mutate(treatment = ifelse(both_unvacc, 1, 0))

matching$treatment %>% table()

write_csv(matching, "full_data_prematching_relaxincarceration_040423.csv")

