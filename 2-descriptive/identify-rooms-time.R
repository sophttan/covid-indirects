# Sophia Tan 1/13/23, updated 5/16/23
# Identify rooms with 2 residents over time
# Residents can be required to have been incarcerated for the entire pandemic period

rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)

# load in all housing, testing, vaccination data
d <- read_csv("complete-data.csv") 
d <- d %>% select(ResidentId, Day, Institution, BuildingId, RoomType, RoomId, num_pos, num_dose_adjusted)
infections <- d %>% group_by(ResidentId, num_pos) %>% filter(!num_pos %>% is.na()) %>% 
  summarise(Day_inf=first(Day))

d <- d %>% group_by(ResidentId, num_pos) %>% mutate(num_pos_adjusted=ifelse(Day==first(Day), num_pos-1, num_pos))
d <- d %>% replace_na(list(num_pos_adjusted=0))
d

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

# include only residents that stayed in rooms of 2 individuals
group_room_summary <- group_room %>% filter(n()==2)

#### commented only if removing requirement for incarceration over the entire pandemic
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
# group_room_summary_entirepandemic <- group_room_summary %>% 
#   left_join(infections, by=c("ResidentId", "num_pos_adjusted"="num_pos")) 
group_room_summary_entirepandemic <- group_room_summary_entirepandemic %>%
  left_join(infections, by=c("ResidentId", "num_pos_adjusted"="num_pos"))
group_room_summary_entirepandemic <- group_room_summary_entirepandemic %>% 
  mutate(inf_90_days = difftime(Day, Day_inf, units="days") + 1 < 90)

# check inf_90_days coding
group_room_summary_entirepandemic %>% group_by(ResidentId) %>% filter(any(num_pos>=1)) %>% 
  select(ResidentId, Day, num_pos, num_pos_adjusted, Day_inf, inf_90_days) %>% head(1000) %>% view()

# label residents in rooms
group_room_2 <- group_room_summary_entirepandemic %>% mutate(num=as.factor(1:n())) %>%
  arrange(Institution, RoomId, Day)
group_room_2 <- group_room_2 %>%
  select(Institution, BuildingId, RoomType, RoomId, Day, ResidentId, 
         num, num_pos, num_pos_adjusted, inf_90_days, num_dose_adjusted)

# label vaccination and prior infection in each resident
group_room_2 <- group_room_2 %>% mutate(vacc=ifelse(num_dose_adjusted>0,1,0), 
                                        inf=num_pos_adjusted>0)
group_room_2

# keep residents with clear building reporting
# some rooms have multiple building labels within the same institution (happens for 1% of buildings)
group_room_2 <- group_room_2 %>% group_by(Institution, RoomId) %>%
  filter(length(unique(BuildingId))==1) %>%
  as.data.frame()

# get dataset for BuildingIds based on RoomId and Institution
building_room <- group_room_2 %>% group_by(Institution, RoomId, BuildingId) %>% group_keys()

# reshape dataset to be wide so each row represents a room-day instead of a resident-day over time
group_room_2_wide <- group_room_2 %>% select(!c(BuildingId)) %>%
  reshape(idvar = c("Institution", "RoomId", "Day"),
          timevar = "num",
          v.names = c("ResidentId", "num_pos", "num_pos_adjusted", "num_dose_adjusted", "vacc", "inf", "inf_90_days"),
          direction = "wide")

# add building type
group_room_2_wide <- group_room_2_wide %>% left_join(building_room)
group_room_2_wide <- group_room_2_wide %>% select(Institution, BuildingId, everything()) %>% 
  arrange(Institution, BuildingId, RoomId, Day) %>% 
  replace_na(list(inf_90_days.1=F, inf_90_days.2=F)) %>% 
  mutate(prior_inf_90 = inf_90_days.1|inf_90_days.2)

write_csv(group_room_2_wide, "wide_housing_2room051923.csv")
