# Sophia Tan 2/26/24
# Identify infections and controls

rm(list=ls())
gc() 

library(tidyverse)
library(readr)
library(lubridate)

inf <- read_csv("D:/CCHCS_premium/st/cleaned-data/infection_data022624.csv")
testing <- read_csv("D:/CCHCS_premium/st/cleaned-data/complete_testing_data022624.csv")

inf <- inf %>% filter(CollectionDate<="2022-12-15" & CollectionDate>="2021-12-15") %>%
  rename("Day"="CollectionDate") %>% 
  select(ResidentId, num_pos, Day)
inf

housing <- NULL
for(i in 1:4){
  housing <- rbind(housing, read_csv(paste0("D:/CCHCS_premium/st/leaky/raw-data/housing_subset", i,".csv")))
}
housing <- housing %>% filter(Night>="2020-03-01"&Night<="2022-12-15")
gc()

residents <- (housing %>% group_by(ResidentId))$ResidentId %>% unique()
included <- (housing %>% group_by(ResidentId) %>% filter(min(Night)<"2020-04-01"))$ResidentId %>% unique()

inf_eligible <- inf %>% filter(ResidentId %in% included)


testing <- testing %>% group_by(ResidentId, num_pos) %>% mutate(last_inf=if_else(num_pos==0, NA, min(Day)))
testing <- testing %>% filter(Day >= "2021-12-15" & Day <= "2022-12-15") 
testing_eligible <- testing %>% group_by(ResidentId) %>% filter(ResidentId %in% included)
testing_eligible <- testing_eligible %>% ungroup() %>%filter(num_pos==0|Day-last_inf>90) %>%
  filter(Result=="Negative")


### add housing data 
housing_relevant <- housing %>% filter(Night >= "2021-12-01")
rm(housing)
gc()

housing_relevant <- housing_relevant %>% group_by(Night, Institution, BuildingId, RoomId) %>% mutate(n=n())
housing_relevant <- housing_relevant %>% mutate(Day=Night+1)
gc()

inf_housing_full <- inf_eligible %>% full_join(housing_relevant, by=c("ResidentId")) %>% filter(Day.x-Day.y<7 & Day.x-Day.y>=3) %>% 
  select(!c(Night)) %>% rename("Day"="Day.y", "inf.Day"="Day.x") %>% group_by(ResidentId, num_pos) %>% filter(n()==4) 
inf_housing_full_withroommate <- inf_housing_full %>% left_join(housing_relevant %>% rename("Roommate"="ResidentId"))
inf_housing_full_withroommate <- inf_housing_full_withroommate %>% filter(n==1 | Roommate != ResidentId)

inf_2 <- inf_housing_full_withroommate %>% group_by(ResidentId, num_pos) %>%
  arrange(ResidentId, num_pos, Day) %>%
  filter(all(n==2))

cases_final <- inf_2 %>%
  filter(all(Roommate==first(Roommate)))

cases_final <- cases_final %>% filter(first(Roommate) %in% included) 

cases_final <- cases_final %>% 
  filter(all(Institution==first(Institution))) %>%
  filter(all(BuildingId==first(BuildingId)))

write_csv(cases_final, "D:/CCHCS_premium/st/indirects/cases3-7daysame-roommate.csv")


testing_eligible <- testing_eligible %>% select(!c(Result, Details, pcr, antigen, unknown, last_inf))
test_final <- NULL
total_excluded_housing <- 0
total_excluded_isolation <- 0
total_excluded_group <- 0
total_excluded_movementroommate <- 0
total_excluded_movementbuild <- 0
total_excluded_roommate <- 0

for(i in seq(1,35)) {
  print(total_excluded_housing)
  print(total_excluded_isolation)
  print(total_excluded_group)
  print(total_excluded_movementroommate)
  print(total_excluded_movementbuild)
  print(total_excluded_roommate)
  
  gc()
  
  testing_sub <- testing_eligible[(i*25000):min(((i+1)*25000-1), nrow(testing_eligible)),]
  n_total <- testing_sub %>% nrow()
  test_housing_full <- testing_sub %>% full_join(housing_relevant, by=c("ResidentId")) 
  
  test_housing_full <- test_housing_full %>% filter(Day.x-Day.y<7 & Day.x-Day.y>=3) %>% 
    group_by(ResidentId, Day.x) %>%
    filter(n()==4) %>%
    select(!c(Night)) %>% 
    rename("Day"="Day.y", "test.Day"="Day.x")
  
  total_excluded_housing <- total_excluded_housing + n_total-(test_housing_full%>%group_keys()%>%nrow())
  test_housing_full_withroommate <- test_housing_full %>% left_join(housing_relevant %>% rename("Roommate"="ResidentId"))
  test_housing_full_withroommate <- test_housing_full_withroommate %>% filter(n==1 | Roommate != ResidentId)
  
  total_excluded_group <- total_excluded_group + (test_housing_full_withroommate %>%filter(any(n>2))%>%group_keys()%>%nrow())
  total_excluded_isolation <- total_excluded_isolation + (test_housing_full_withroommate %>%filter(all(n<=2)&any(n==1))%>%group_keys()%>%nrow())
  
  test_2 <- test_housing_full_withroommate %>% group_by(ResidentId, test.Day) %>% 
    filter(all(n==2)) %>% 
    arrange(ResidentId, test.Day, Day) 
  
  total_excluded_movementroommate <- total_excluded_movement + (test_2 %>%filter(!all(Roommate==first(Roommate)))%>%group_keys()%>%nrow())
  
  tests <- test_2 %>%
    filter(all(Roommate==first(Roommate))) 

  total_excluded_roommate <- total_excluded_roommate + (tests %>%filter(!first(Roommate) %in% included)%>%group_keys()%>%nrow())
  
  tests <- tests %>% filter(first(Roommate) %in% included)
  
  total_excluded_movementbuild <- total_excluded_roommate + (tests %>%filter(any(Institution!=first(Institution))%>%filter(any(Building!=first(BuildingId))))%>%group_keys()%>%nrow())
  
  tests <- tests %>% 
    filter(all(Institution==first(Institution))) %>%
    filter(all(BuildingId==first(BuildingId)))
  
  test_final <- test_final %>% rbind(tests)
}
write_csv(test_final, "D:/CCHCS_premium/st/indirects/control3-7daysame-roommate.csv")

