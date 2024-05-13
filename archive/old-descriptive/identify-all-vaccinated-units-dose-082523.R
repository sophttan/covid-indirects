rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)

# load in all housing, testing, vaccination data
d <- read_csv("complete-data-081423.csv") 
testing <- d %>% select(ResidentId, Day, Result) %>% filter(!Result%>%is.na())
duration <- read_csv("housing_duration.csv")
infections <- d %>% group_by(ResidentId, num_pos) %>% filter(!num_pos %>% is.na()) %>% 
  summarise(Day_inf=first(Day))
vaccines <- read_csv("cleaned_vaccination_data.csv") %>% select(ResidentId, Date, num_dose) %>% rename("Day_vacc"="Date")

d <- d %>% group_by(ResidentId, num_pos) %>% mutate(num_pos_adjusted=ifelse(Day==first(Day), num_pos-1, num_pos))
d <- d %>% replace_na(list(num_pos_adjusted=0))
d

d <- d %>% 
  mutate(num_dose_grouped = case_when(num_dose_adjusted==0~0,
                                      num_dose_adjusted<=full_vacc~1,
                                      num_dose_adjusted-full_vacc==1~2,
                                      T~3))

d <- d %>% select(ResidentId, Day, Institution, BuildingId, RoomType, RoomId, num_pos_adjusted, num_dose_adjusted, num_dose_grouped)


# keep data only for Omicron period
group_room <- d %>% filter(!is.na(Institution) & !is.na(RoomId)) %>% filter(Day >= "2021-12-15") %>%
  group_by(Institution, RoomId, Day)

# include only residents that stayed in rooms of 2 individuals
group_room_summary <- group_room %>% filter(n()==2)

#### commented if removing requirement for incarceration over the entire pandemic
# include only residents that were incarcerated over the entire pandemic (before 4/1/2020)
duration <- read_csv("housing_duration.csv")
duration <- duration %>% filter(first<="2020-03-31")
duration <- duration %>% mutate(max_duration=last-first+1) %>%
  filter(max_duration-duration<=30)
included <- duration$ResidentId %>% unique()

residents <- group_room_summary %>% group_by(ResidentId) %>% group_keys()
residents <- residents %>% filter(ResidentId %in% included)
group_room_summary_entirepandemic <- group_room_summary %>% inner_join(residents)
group_room_summary_entirepandemic <- group_room_summary_entirepandemic %>% filter(n()==2)

# # label if residents have had documented infection in the last 90 days
# # if not requiring that residents have been incarcerated for the entire pandemic use commented line
group_room_summary_entirepandemic <- group_room_summary_entirepandemic %>%
  left_join(infections, by=c("ResidentId", "num_pos_adjusted"="num_pos"))
group_room_summary_entirepandemic <- group_room_summary_entirepandemic %>% 
  mutate(inf_90_days = difftime(Day, Day_inf, units="days") + 1 < 90)

# check inf_90_days coding
group_room_summary_entirepandemic %>% group_by(ResidentId) %>% filter(any(num_pos_adjusted>=1)) %>% 
  select(ResidentId, Day, num_pos_adjusted, Day_inf, inf_90_days) %>% head(1000) %>% view()

# label residents in rooms
group_room_2 <- group_room_summary_entirepandemic %>% 
  arrange(Institution, RoomId, Day)
group_room_2 <- group_room_2 %>%
  select(Institution, BuildingId, RoomId, Day, ResidentId, 
         num_pos_adjusted, inf_90_days, num_dose_adjusted, num_dose_grouped, Day_inf)

group_room_2 <- group_room_2 %>% left_join(vaccines, by=c("ResidentId", "num_dose_adjusted"="num_dose"))

# label vaccination and prior infection in each resident
group_room_2 <- group_room_2 %>% mutate(vacc=ifelse(num_dose_grouped>0,1,0), 
                                        inf=num_pos_adjusted>0)

# keep residents with clear building reporting
# some rooms have multiple building labels within the same institution (happens for 1% of buildings)
group_room_2 <- group_room_2 %>% group_by(Institution, RoomId) %>%
  filter(length(unique(BuildingId))==1) 

group_room_2

group_room_2_listed <- group_room_2 %>% group_by(Institution, BuildingId, RoomId, Day) %>% summarise_all(list)
group_room_2_listed <- group_room_2_listed %>% rowwise() %>% mutate(prior_inf_90 = any(inf_90_days,na.rm=T))

group_room_2_listed <- group_room_2_listed %>% arrange(Institution, BuildingId, RoomId, ResidentId, Day) %>%
  group_by(Institution, BuildingId, RoomId, ResidentId) %>% mutate(group=cur_group_id())
group_room_2_listed %>% head(20) %>% view()

group_room_2_listed <- group_room_2_listed %>% mutate(time=c(0, diff(Day)))

consecutive_days <- group_room_2_listed %>% filter(time==0|time>1) %>% mutate(label=1:n()) %>% select(Day, label)
group_room_2_listed <- group_room_2_listed %>% left_join(consecutive_days)
group_room_2_listed <- group_room_2_listed %>% fill(label, .direction="down")

units_time <- group_room_2_listed %>%
  group_by(group, label, prior_inf_90, num_dose_adjusted) %>% 
  mutate(id=cur_group_id()) %>% 
  mutate(first=first(Day), last=last(Day)) 

units_time <- units_time %>% group_by(id) %>% summarise_all(first) %>% mutate(duration=as.numeric(difftime(last, first, units="days"))+1)

units_time

# exclude units if resident's don't co-reside for at least 2 weeks
over_14 <- units_time %>% filter(duration >= 14)

# make sure no units have prior infection within the last 90 days
over14_norecentinf <- over_14 %>% filter(!prior_inf_90)

# require prior infection in both residents
# over14_priorinf_norecentinf <- over14_norecentinf %>% filter(all(inf==1))

# check testing data
residents <- over14_norecentinf$ResidentId %>% unique()
testing <- testing %>% ungroup() %>% 
  filter(Day >= "2021-12-15") %>% 
  filter(ResidentId %in% residents) %>% 
  group_by(ResidentId) 
duration <- read_csv("housing_duration.csv")

testing <- testing %>% left_join(duration%>%select(ResidentId, last)%>%mutate(last=last+1))

testing_only <- testing %>% filter(!Result%>%is.na()) %>% 
  select(ResidentId, Result, Day, last) %>% 
  rename("Test" = "Day")
omicron_testing <- testing_only %>% group_by(ResidentId)

omicron_testing_summary <- omicron_testing %>% 
  mutate(time=as.numeric(difftime(last, "2021-12-15"))+1) %>%
  summarise(tests=n(), rate_testing=tests/first(time)*30)

omicron_testing_summary1 <- omicron_testing_summary%>%filter(rate_testing>=1)

avg_1_test <- over14_norecentinf %>% ungroup() %>% 
  mutate(test=ResidentId %in% omicron_testing_summary1$ResidentId) %>%
  group_by(id) %>% 
  filter(any(test))

avg_1_test <- avg_1_test %>% mutate(id=cur_group_id())

demo <- read_csv("demographic_data_clean.csv")
demo <- demo %>% mutate(age=2022-BirthYear)
avg_1_test <- avg_1_test %>% left_join(demo) 

avg_1_test <- avg_1_test %>% select(!c(group, time, label, prior_inf_90, Day))

write_csv(avg_1_test, "allvacc_full_data_prematching_bydose_082523.csv")

