# Sophia Tan 12/15/23
# Clean nightly housing data 5/26/23

source(here::here("config.R"))

setwd("D:/CCHCS_premium/CDCR Data/May 26 2023 Data/")

nh <- read_delim("NightlyHousing_20230526.csv", delim = ";", 
                 col_select = c("Night", "ResidentId", "RoomId", "Institution", "BuildingId", "RoomType"))#, 
                 # n_max = 50000000)

min(nh$Night)
max(nh$Night)

# keep only data during pandemic and study period
nh_after_3_1_2020 <- nh %>% filter(Night >= "2020-03-01" & Night <= "2022-12-15")
rm(nh)
gc()

names(nh_after_3_1_2020)

nh_after_3_1_2020

# remove data if missing housing info
nh_after_3_1_2020 <- nh_after_3_1_2020 %>% filter(!BuildingId %>% is.na())

# update roomtypes (leftover cleaning from a different analysis - not needed for this one)
nh_after_3_1_2020_roomtypes <- nh_after_3_1_2020 %>% group_by(Institution, RoomId, RoomType) %>% 
  summarise(n=n()) %>%
  group_by(Institution, RoomId) %>% filter(!RoomType %>% is.na()) %>% 
  arrange(Institution, RoomId, desc(n)) %>% select(!n) %>%
  distinct(Institution, RoomId, .keep_all = T)

nh_after_3_1_2020 <- nh_after_3_1_2020 %>% left_join(nh_after_3_1_2020_roomtypes, by=c("Institution", "RoomId"))
nh_after_3_1_2020 <- nh_after_3_1_2020 %>% 
  select(!RoomType.x) %>% rename("RoomType"="RoomType.y") 

nh_after_3_1_2020_testroomtypes <- nh_after_3_1_2020 %>% group_by(Institution, RoomId, Night) %>% 
  summarise(RoomType=first(RoomType), n=n())
nh_after_3_1_2020_testroomtypes <- nh_after_3_1_2020_testroomtypes %>% mutate(roomtype_simple=if_else(n<3, "Cell", "Dorm"))
nh_after_3_1_2020_testroomtypes %>% group_by(RoomType) %>% summarise(sum(n>2)/n()) # almost all cells and rooms have <3 people and almost all dorms have >3 people

nh_after_3_1_2020 <- nh_after_3_1_2020 %>% 
  mutate(RoomType.simple=case_when(RoomType%in%c(1,2,4,6)~0,
                                   RoomType%in%c(3,5)~1,
                                   T~NA))

# Residents without RoomType data (n=2 with ResidentIds 1629006254 1630486718)
(nh_after_3_1_2020 %>% filter(Night>="2021-12-15"&RoomType%>%is.na()))$ResidentId %>% unique()

# split raw housing data into four groups for memory
residents <- nh_after_3_1_2020$ResidentId%>%unique()
residents <- data.frame(ResidentId = residents,
                        group = rep(1:4, length.out=length(residents)))

nh_after_3_1_2020 <- nh_after_3_1_2020 %>% left_join(residents)

list_housing_data <- nh_after_3_1_2020 %>% group_by(group) %>% group_split()

for(i in 1:4){
  write_csv(list_housing_data[[i]]%>%select(!group),paste0("D:/CCHCS_premium/st/leaky/raw-data/housing_subset", i,".csv"))
}

