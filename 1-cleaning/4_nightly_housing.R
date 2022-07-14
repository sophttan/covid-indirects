# Sophia Tan 5/27/22
# Clean nightly housing data 5/20/22

rm(list=ls())

setwd("/Users/sophiatan/Documents/UCSF")
#setwd("D:/CDCR Data/14 January 15 2022")

library(readr)
library(tidyverse)

nh <- read_delim("ST files/NightlyHousing_20220520.csv", delim = ";")

#min(nh$Night)
#max(nh$Night)

nh_after_3_1_2020 <- nh %>% filter(Night >= "2020-03-01")
nh_after_3_1_2020_subset <- nh_after_3_1_2020 %>% select(!c(BunkId, BedLevel, FloorNumber, HousingProgram, RoomCapacity, BunkCapacity))

summary_housing <- nh_after_3_1_2020_subset %>% group_by(ResidentId) %>% arrange(Night) %>% summarise(first=first(Night), last=last(Night), duration=n())
summary_housing %>% write_csv("cleaned_data/housing_duration.csv")

unique_res_housing <- nh_after_3_1_2020_subset$ResidentId %>% unique()

inf_vacc <- read_csv("cleaned_data/has_testing_data_aggregated_infections.csv")


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

unique_res_with_testing[!(unique_res_with_testing %in% unique_res_housing)] %>% length() # 458 residents with no housing data
unique_res_housing[!(unique_res_housing %in% unique_res_with_testing)] %>% length()

merge(data.frame(ResidentId=unique_res_housing), data.frame(ResidentId=unique_res_with_testing))
no_housing_data <- filter(inf_vacc, !(ResidentId %in% unique_res_housing))
(!no_housing_data$num_pos%>%is.na()) %>% sum() # losing 59 infections
 <- inf_vacc %>% filter(ResidentId %in% unique_res_housing)

# save testing but no housing data
write_csv(no_housing_data, "cleaned_data/has_testing_no_housing.csv")

nh_omicron <- nh_after_3_1_2020_subset %>% filter(Night >= "2021-12-01")
unique_res_housing <- nh_omicron$ResidentId %>% unique()
unique_res_housing[!unique_res_housing %in% unique_res_with_testing]

roomtypes <- nh_omicron %>% group_by(Institution, RoomId, RoomType) %>% summarise(count=n()) %>% group_by(Institution, RoomId)
roomtypes %>% filter(all(RoomType%>%is.na())) # 9 rooms here have no roomtype
roomtypes %>% filter(any(RoomType%>%is.na()) & n()>1) # 13 rooms have roomtypes that have some missingness in reporting
roomtypes %>% filter(!any(RoomType %>% is.na()) & n()>1) # 1 rooms have two different roomtypes listed
roomtypes <- roomtypes %>% filter(!RoomType %>% is.na()) %>% arrange(desc(count)) %>% summarise(RoomType=first(RoomType))
nh_omicron <- nh_omicron %>% left_join(roomtypes, c("Institution", "RoomId"))
nh_omicron <- nh_omicron %>% rename("RoomType" = "RoomType.y") %>% select(!RoomType.x)
nh_omicron <- nh_omicron %>% mutate(RoomType = ifelse(Institution ==13, 1, RoomType))

write_csv(nh_omicron, "cleaned_data/housing_filtered_omicron.csv")

