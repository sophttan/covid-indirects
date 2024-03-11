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
housing <- housing %>% filter(Night<="2022-12-15")

included <- (housing %>% group_by(ResidentId) %>% filter(min(Night)<"2020-04-01"))$ResidentId %>% unique()

inf_eligible <- inf %>% filter(ResidentId %in% included)


testing <- testing %>% group_by(ResidentId, num_pos) %>% mutate(last_inf=if_else(num_pos==0, NA, min(Day)))
testing_eligible <- testing %>% group_by(ResidentId) %>% filter(ResidentId %in% included)
testing_eligible <- testing_eligible %>% ungroup() %>% 
  filter(Result=="Negative") %>%  filter(num_pos==0|Day-last_inf>90) %>% filter(Day >= "2021-12-15" & Day <= "2022-12-15")


### add housing data 
housing_relevant <- housing %>% filter(Night >= "2021-12-01")
rm(housing)
gc()

housing_relevant <- housing_relevant %>% group_by(Night, Institution, BuildingId, RoomId) %>% mutate(n=n())
housing_relevant <- housing_relevant %>% mutate(Day=Night+1)
gc()

inf_housing_full <- inf_eligible %>% full_join(housing_relevant, by=c("ResidentId")) %>% filter(Day.x-Day.y<7 & Day.y<=Day.x) %>% 
  select(!c(Night)) %>% rename("Day"="Day.y", "inf.Day"="Day.x")
inf_housing_full_withroommate <- inf_housing_full %>% left_join(housing_relevant %>% rename("Roommate"="ResidentId"))
inf_housing_full_withroommate <- inf_housing_full_withroommate %>% filter(n==1 | Roommate != ResidentId)

inf_2 <- inf_housing_full_withroommate %>% group_by(ResidentId, num_pos) %>% 
  filter(all(n<=2)&!all(n==1)) %>% 
  filter(n()==7) %>%
  arrange(ResidentId, num_pos, Day) 

cases_final <- inf_2 %>%
  filter(first(n)!=1&all(Roommate[1:4]==first(Roommate)))
write_csv(cases_final, "D:/CCHCS_premium/st/indirects/cases3-7daysame.csv")


library(doParallel)
library(foreach)

# set up parallelization
cl<-makeCluster(detectCores()-1)
registerDoParallel(cl)

testing_eligible <- testing_eligible %>% select(!c(Result, Details, pcr, antigen, unknown, last_inf))
test_final <- NULL
for(i in seq(1,36)) {
  gc()
  testing_sub <- testing_eligible[(i*25000):min(((i+1)*25000-1), nrow(testing_eligible)),]
  test_housing_full <- testing_sub %>% full_join(housing_relevant, by=c("ResidentId")) %>% filter(Day.x-Day.y<7 & Day.y<=Day.x) %>% 
    select(!c(Night)) %>% rename("Day"="Day.y", "test.Day"="Day.x")
  test_housing_full_withroommate <- test_housing_full %>% left_join(housing_relevant %>% rename("Roommate"="ResidentId"))
  test_housing_full_withroommate <- test_housing_full_withroommate %>% filter(n==1 | Roommate != ResidentId)
  
  test_2 <- test_housing_full_withroommate %>% group_by(ResidentId, test.Day) %>% 
    filter(all(n<=2)&!all(n==1)) %>% 
    filter(n()==7) %>%
    arrange(ResidentId, num_pos, Day) 
  tests <- test_2 %>%
    filter(first(n)!=1&all(Roommate[1:4]==first(Roommate)))
  test_final <- test_final %>% rbind(tests)
}
write_csv(test_final, "D:/CCHCS_premium/st/indirects/control3-7daysame.csv")

