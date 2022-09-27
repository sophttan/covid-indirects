# Sophia Tan 1/28/22
# combine housing and testing data

rm(list=ls())
gc()
#setwd("D:/stan5/code_ST/march-data/")
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")

library(readr)
library(tidyverse)

nh <- read_csv("housing_filtered_omicron.csv")
inf_vacc <- read_csv("has_testing_data_aggregated_infections072122.csv")

# no_housing <- read_csv("has_testing_no_housing.csv")$ResidentId %>% unique()
# inf_vacc <- filter(inf_vacc, !(ResidentId %in% no_housing))
inf_vacc <- inf_vacc %>% mutate(has_test = ifelse(is.na(Result), F, T), 
                                unknown_test_other = has_test & !(antigen|pcr))

inf_vacc_housing <- inf_vacc %>% full_join(nh, c("ResidentId", "Day"="Night"))

rm(nh, inf_vacc) 
gc() # free up memory

inf_vacc_housing <- inf_vacc_housing %>% arrange(ResidentId, Day)# %>% select(!Month.x)
inf_vacc_housing <- inf_vacc_housing %>% group_by(ResidentId) %>% 
  fill(Date_offset, num_dose, max_dose, full_vacc, booster_add_dose, .direction="down")
inf_vacc_housing <- inf_vacc_housing %>% mutate(num_dose_adjusted = ifelse(Day < Date_offset & num_dose > 0, num_dose-1, num_dose)) %>% select(!Date_offset)
# inf_vacc_housing %>% select(ResidentId, RoomId, Day, 
#                             num_pos, num_dose, max_dose, full_vacc,
#                             RoomCapacity) %>%
#   filter(num_pos>=1) %>% arrange(Day) %>% view()


# room_cases <- inf_vacc_housing %>% group_by(RoomId, Day) %>% 
#   summarise(case = sum(!is.na(num_pos), na.rm=T), RoomCapacity=first(RoomCapacity))
# room_had_case <- filter(room_cases, any(case>0)) %>% filter(!RoomId %>% is.na())
# had_case <- room_had_case %>% filter(case>0) %>% mutate(diff=c(0, diff(Day)))
# multiple_Cases_day <- had_case %>% filter(case>1 & (diff<3|diff > 30))# %>% view()
# multiple_Cases_day$case %>% sum()
# 
# inf_vacc_housing %>% filter(RoomId==-2050142432, Day>"2021-10-03" & Day<"2021-10-10") %>% view()
# 
# testing <- filter(inf_vacc_housing, has_test)
# testing %>% nrow()
# sum(testing$has_both_test, na.rm=T)/sum(testing$has_test) # 2.6% of residents with testing data received both PCR and antigen tests on the same day
# sum(testing$antigen, na.rm=T)/sum(testing$has_test) #13.6% residents over time received antigen tests
# sum(testing$pcr, na.rm=T)/sum(testing$has_test) #88.8% residents over time received PCR tests
# sum(testing$unknown_test_other,na.rm=T)/sum(testing$has_test) #0.22% 
# 
# room_size <- inf_vacc_housing %>% ungroup() %>% group_by(RoomId, Day) %>% summarise(num_occ = first(RoomCensus))
# room_size_avg_over_time <- room_size %>% group_by(Day) %>% summarise(avg_occ = mean(num_occ,na.rm=T))
# room_size_avg_over_time %>% ggplot(aes(Day, avg_occ)) + geom_line() + ylab("Average number of room occupants")
# 
# positives <- filter(inf_vacc_housing, !num_pos %>% is.na())
# positives$RoomCensus %>% summary()
# positives$RoomCensus %>% hist()
# 
# filter(positives, QuarantineIsolation==0) %>% nrow()
# filter(positives, RoomCensus <= 8) %>% nrow()
# filter(positives, RoomCensus <=1) %>% nrow()
# filter(positives, RoomCensus <=8 & RoomCensus>1) %>% nrow()
# 
# 
# num_rooms_res <- inf_vacc_housing %>% summarise(num_rooms = unique(RoomId) %>% length())
# num_rooms_res$num_rooms %>% summary()


inf_vacc_housing <- inf_vacc_housing %>% fill(num_pos, .direction="down")
inf_vacc_housing <- inf_vacc_housing %>% group_by(ResidentId, num_pos) %>%
  mutate(infectious = ifelse(!is.na(num_pos) & Day-first(Day)<=4, 1, 0))
inf_vacc_housing <- inf_vacc_housing %>% select(!c(ReceivedDate))
write_csv(inf_vacc_housing, "housing_inf_data072122.csv")

adjust_inf <- inf_vacc_housing %>% group_by(ResidentId, num_pos) %>% 
  mutate(Day_inf = if_else(!is.na(num_pos), first(Day)-2, as.Date(NA))) %>% ungroup()
adjust_inf <- adjust_inf %>% group_by(ResidentId, Day_inf) %>% summarise(num_pos=first(num_pos)) %>% filter(!Day_inf %>% is.na())
inf_vacc_housing_adj_inf <- inf_vacc_housing %>% full_join(adjust_inf, by=c("ResidentId", "Day"="Day_inf")) %>% 
  group_by(ResidentId) %>% arrange(ResidentId, Day) %>% 
  fill(num_pos.y, .direction="down")
inf_vacc_housing_adj_inf <- inf_vacc_housing_adj_inf %>% filter(!num_dose %>% is.na())
inf_vacc_housing_adj_inf <- inf_vacc_housing_adj_inf %>% rename("num_pos"="num_pos.y")
  
  
inf_vacc_housing_adj_inf <- inf_vacc_housing_adj_inf %>% group_by(ResidentId, num_pos) %>% 
  mutate(infectious = ifelse(!is.na(num_pos) & Day-first(Day)<=4, 1, 0))
write_csv(inf_vacc_housing_adj_inf, "housing_inf_data_infperiod2_5.csv")

inf_vacc_housing_adj_inf <- inf_vacc_housing_adj_inf %>% group_by(ResidentId, num_pos) %>% 
  mutate(infectious = ifelse(!is.na(num_pos) & Day-first(Day)<=6, 1, 0))
write_csv(inf_vacc_housing_adj_inf, "housing_inf_data_infperiod2_7.csv")

