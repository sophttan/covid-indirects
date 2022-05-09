# Sophia Tan 1/28/22
# Clean nightly housing data 1/15/22 

rm(list=ls())

setwd("D:/CDCR Data/15 March 25 2022/")
# setwd("D:/CDCR Data/14 January 15 2022")

library(readr)
library(tidyverse)

#nh1 <- read_delim("NightlyHousing1_20220115.csv", delim = ";") # has records from 2016-2018 
nh2 <- read_delim("NightlyHousing2_20220325.csv", delim = ";") # records from 2019 to 1/13/2022
#nh2 <- read_delim("NightlyHousing2_20220115.csv", delim = ";") # records from 2019 to 1/13/2022

min(nh2$Night)
max(nh2$Night)

nh2 <- nh2 %>% mutate(Year = format(Night, "%Y"), Month = format(Night, "%m"))

nh2_after_3_1_2020 <- nh2 %>% filter(Night >= "2020-03-01")

nh2_after_3_1_2020_subset <- nh2_after_3_1_2020 %>% select(ResidentId, Night, #Year, Month, 
                                                           RoomType, RoomCapacity, RoomCensus, RoomId,
                                                           InfectedRoomMates,
                                                           QuarantineIsolation, ActivityCohortId, 
                                                           Institution, LocationStatus)

nh2_after_3_1_2020_subset <- nh2_after_3_1_2020_subset %>% group_by(ResidentId) %>% filter(any(!RoomId %>% is.na()))

unique_res_housing <- nh2_after_3_1_2020_subset$ResidentId %>% unique()

inf_vacc <- read_csv("D:/stan5/code_ST/march-data/has_testing_data_aggregated_infections.csv")


# (inf_vacc %>% filter(num_pos==1))$Day %>% min()
# filter(inf_vacc, num_pos==1 & Day=="2020-03-20")
# # 1634200926 had first confirmed infection on 3/20/2020
# check_one_res <- filter(nh2_after_3_1_2020_subset, ResidentId==1634200926)
# check_one_res <- check_one_res %>% arrange(Night) 
# 
# roomid <- check_one_res$RoomId %>% unique()
# duration_stay_in_rooms <- check_one_res %>% group_by(RoomId) %>% summarise(Night=first(Night), num_days=n()) %>% arrange(Night)
# 
# # was staying in room 626798612 when initially positive
# check_one_res <- check_one_res %>% filter(RoomId == 626798612) 
# 
# all_res_1_period <- filter(nh2_after_3_1_2020_subset, (RoomId==626798612 | ActivityCohortId==-124687074) & Night < "2020-04-15") %>% arrange(Night)
unique_res_with_testing <- inf_vacc$ResidentId %>% unique()

unique_res_with_testing[!(unique_res_with_testing %in% unique_res_housing)] %>% length() # 468 residents with no housing data
unique_res_housing[!(unique_res_housing %in% unique_res_with_testing)] %>% length() # 9259 residents with no housing data
no_housing_data <- filter(inf_vacc, !(ResidentId %in% unique_res_housing))
(!no_housing_data$num_pos%>%is.na()) %>% sum() # losing 61 infections
no_housing_data %>% group_by(ResidentId) %>% filter(any(!num_pos %>% is.na())) # losing 60 individuals that have had positive tests

# save testing but no housing data
write_csv(no_housing_data, "D:/stan5/code_ST/march-data/has_testing_no_housing.csv")

subset_only_res_with_testing_data <- filter(nh2_after_3_1_2020_subset, ResidentId %in% unique_res_with_testing)
write_csv(subset_only_res_with_testing_data, "D:/stan5/code_ST/march-data/housing_filtered.csv")

