# Sophia Tan 3/31
# test different order of inclusion/exclusion criteria

rm(list=ls())

setwd("D:/stan5/code_ST")

library(readr)
library(tidyverse)

d <- read_csv("housing_inf_data.csv")
roomtypes <- d %>% group_by(Institution, RoomId, RoomType) %>% summarise(count=n()) %>% group_by(Institution, RoomId) 
roomtypes %>% filter(all(RoomType%>%is.na())) # 13 rooms here have no roomtype
roomtypes %>% filter(any(RoomType%>%is.na()) & n()>1) # 108 rooms have roomtypes that have some missingness in reporting
roomtypes %>% filter(!any(RoomType %>% is.na()) & n()>1) # 17 rooms have two different roomtypes listed 
roomtypes <- roomtypes %>% filter(!RoomType %>% is.na()) %>% arrange(desc(count)) %>% summarise(RoomType=first(RoomType))
d <- d %>% left_join(roomtypes, c("Institution", "RoomId"))
d <- d %>% rename("RoomType" = "RoomType.y") %>% select(!RoomType.x)
d <- d %>% mutate(RoomType = ifelse(Institution ==13, 1, RoomType))
write_csv(d, "housing_inf_data_adjusted_roomtype.csv")

d <- d %>% group_by(ResidentId, num_pos)

# include only if test negative pcr in the 7 days prior to first positive test
infections <- d %>% filter(infectious==1) 
has_infection <- d %>% group_by(ResidentId) %>% filter(any(Result == "Positive"))
first_day <- infections %>% summarise(first_day=first(Day))
has_neg_pcr_prev <- has_infection %>% left_join(first_day, "ResidentId") %>% group_by(ResidentId, num_pos.y) %>% 
  filter(first_day-Day<=8 & Day-first_day<5) %>% mutate(prior_7 = ifelse(first_day-Day<=8 & first_day-Day>0, T, F)) %>%
  filter(any(Result=="Negative"&pcr&prior_7)) 
has_neg_pcr_prev # 24135 total infections with negative pcr in 7 days prior to first positive test

infections_subset <- has_neg_pcr_prev %>% filter(infectious==1)
filter(infections_subset, any(Result=="Negative"&pcr)) 
# 473 have a negative test prior to first positive test 
# 287 have a negative pcr test - exclude

infections_subset <- infections_subset %>% filter(all(!(Result=="Negative"&pcr), na.rm=T))

infections_subset_adjusted <- infections_subset %>% 
  mutate(infectious = ifelse(Result=="Negative", 0, infectious)) %>%
  fill(infectious, .direction="down") %>% filter(infectious==1)

# remove infections that have no housing data over entire infectious period
# 32 infections have no housing data over entire infectious period
infections_subset_adjusted %>% filter(all(is.na(RoomId)))
infections_subset_adjusted <- infections_subset_adjusted %>% filter(any(!is.na(RoomId)))

# how many infections first tested positive on a day without any housing data? (2)
infections_subset_adjusted %>% 
  filter(first(Result)=="Positive" & is.na(first(RoomId))) %>% 
  select(ResidentId, Day, Result, RoomId, RoomCensus, QuarantineIsolation)

# # remove if single resident in room for entire infectious period 
# infections_subset_adjusted %>% filter(all(RoomCensus==1, na.rm=T)) %>% 
#   select(ResidentId, Day, Result, RoomCensus, RoomCapacity, RoomType, Institution)%>%view()
# infections_adjusted_more_than_1_contact <- infections_subset_adjusted %>% filter(any(RoomCensus > 1))

infections_subset_adjusted <- infections_subset_adjusted %>% mutate(days_infectious = 1:n())
infections_subset_adjusted <- infections_subset_adjusted %>% filter(!is.na(RoomId))
infections_subset_adjusted <- infections_subset_adjusted %>% filter(!is.na(RoomType))

# Room types of potential index cases (removed anyone with no prior negative pcr, no housing/no room type data)
infections_subset_adjusted %>% summarise(RoomType=list(unique(RoomType))) %>% group_by(ResidentId, num_pos.y) %>% filter(length(unlist(RoomType))==1)
(infections_subset_adjusted %>% summarise(Roomnum=length(unique(RoomId))) %>% ungroup())$Roomnum %>% summary()
rooms <- infections_subset_adjusted %>% summarise(RoomType=paste(unique(RoomType),collapse =""))
rooms$RoomType %>% as.factor() %>% table() %>% barplot() 
rooms$RoomType %>% as.factor() %>% table() 

infections_subset_adjusted %>% filter(all(RoomCensus==1))
infections_subset_adjusted_has_contacts <- infections_subset_adjusted %>% filter(any(RoomCensus>1))
infections_subset_adjusted_has_contacts_filter_roomtype <- infections_subset_adjusted_has_contacts %>% filter(first(RoomType %in% c(1, 2)))

# assign infections unique labels
labels <- infections_subset_adjusted_has_contacts_filter_roomtype %>% group_keys %>% mutate(no=1:nrow(.))
labels
infections_subset_adjusted_has_contacts_filter_roomtype <- infections_subset_adjusted_has_contacts_filter_roomtype %>% left_join(labels)

write_csv(infections_subset_adjusted_has_contacts_filter_roomtype, "potential-primary-cases/infectious_periods_primary_cases_v2_roomtypes_8days.csv")

