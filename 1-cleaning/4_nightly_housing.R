# Sophia Tan 1/11/23
# Clean nightly housing data 12/16/22

rm(list=ls())

setwd("D:/CCHCS_premium/CDCR Data/Dec 16 2022 Data")

library(readr)
library(tidyverse)

nh <- read_delim("NightlyHousing_20221216.csv", delim = ";")

min(nh$Night)
max(nh$Night)

nh_after_3_1_2020 <- nh %>% filter(Night >= "2020-03-01")
nh_after_3_1_2020_subset <- nh_after_3_1_2020 %>% 
  select(Night, ResidentId, RoomId, RoomType, Institution, FacilityId, BuildingId, ActivityCohortId)

summary_housing <- nh_after_3_1_2020_subset %>% group_by(ResidentId) %>% arrange(Night) %>% summarise(first=first(Night), last=last(Night), duration=n())
summary_housing %>% write_csv("D:/CCHCS_premium/st/indirects/cleaned-data/housing_duration.csv")

unique_res_housing <- nh_after_3_1_2020_subset %>% group_by(ResidentId) %>% group_keys()

nh_after_3_1_2020_subset %>% filter(is.na(RoomId)&is.na(Institution)&is.na(RoomType)) # no resident-nights where all info is missing

inf_vacc <- read_csv("D:/CCHCS_premium/st/indirects/cleaned-data/testing_vacc_clean.csv")
unique_res_with_testing <- nh_after_3_1_2020_subset %>% group_by(ResidentId) %>% group_keys()

unique_res_with_testing$ResidentId[!(unique_res_with_testing$ResidentId %in% unique_res_housing$ResidentId)] %>% length() # 0 residents with no housing data
unique_res_housing$ResidentId[!(unique_res_housing$ResidentId %in% unique_res_with_testing$ResidentId)] %>% length() # 0 residents no testing data

nh_omicron <- nh_after_3_1_2020_subset %>% filter(Night >= "2021-12-01")

roomtypes <- nh_omicron %>% group_by(Institution, RoomId, RoomType) %>% summarise(count=n()) %>% group_by(Institution, RoomId)
roomtypes %>% filter(all(RoomType%>%is.na())) # 2 rooms here have no roomtype
roomtypes %>% filter(any(RoomType%>%is.na()) & n()>1) # 4 rooms have roomtypes that have some missingness in reporting
roomtypes %>% filter(!any(RoomType %>% is.na()) & n()>1) # 1 rooms has two different roomtypes listed - mostly roomtype 4, some reports of roomtype 2
roomtypes <- roomtypes %>% filter(!RoomType %>% is.na()) %>% arrange(desc(count)) %>% summarise(RoomType=first(RoomType))
nh_omicron <- nh_omicron %>% left_join(roomtypes, c("Institution", "RoomId"))
nh_omicron <- nh_omicron %>% rename("RoomType" = "RoomType.y") %>% select(!RoomType.x)
nh_omicron <- nh_omicron %>% mutate(RoomType = ifelse(Institution ==13, 1, RoomType))

weird_buildings <- nh_omicron %>% 
  group_by(Institution, RoomId, Night, BuildingId) %>% 
  summarise(count=n()) 
w <- weird_buildings %>% group_by(Institution, RoomId) %>% 
  filter(length(unique(BuildingId))>1)

pdf("D:/CCHCS_premium/st/indirects/testing/building.pdf")
for (room in unique(w$RoomId)) {
  print(w %>% filter(RoomId==room) %>% 
    ggplot(aes(Night, group=as.factor(BuildingId), fill=as.factor(BuildingId))) + 
    geom_histogram(position="dodge") + 
    labs(title=room))
}
dev.off()
# some rooms have multiple building ids
nh_omicron %>% 
  group_by(Institution, RoomId) %>% summarise(BuildingId=unique(BuildingId)) %>% filter(n()>1) %>% 
  write_csv("D:/CCHCS_premium/st/indirects/cleaned-data/rooms_mult_buildings.csv")

write_csv(nh_omicron, "D:/CCHCS_premium/st/indirects/cleaned-data/housing_omicron.csv")

