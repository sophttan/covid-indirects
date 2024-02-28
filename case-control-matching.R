# Sophia Tan 2/27/24
# Match infections and controls

rm(list=ls())
gc() 

library(tidyverse)
library(readr)
library(lubridate)

cases <- read_csv("D:/CCHCS_premium/st/indirects/cases.csv")%>%mutate(case=1)%>%rename("test.Day"="inf.Day")
controls <- read_csv("D:/CCHCS_premium/st/indirects/control.csv")%>%mutate(case=0)%>%select(names(cases))

cases
controls

total <- rbind(cases, controls)
total
total <- total %>% group_by(ResidentId, test.Day) %>% filter(all(BuildingId==first(BuildingId)))
total <- total %>% distinct(ResidentId, test.Day, .keep_all = T)
total$case %>% table()

library(MatchIt)
total <- total %>% mutate(has.prior.inf=case_when(case==1&num_pos>1~1,
                                                  case==1&num_pos==1~0,
                                                  case==0&num_pos>0~1,
                                                  T~0))
total

vaccine <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/complete_vaccine_data121523.csv") %>% filter(Date <= "2022-12-15")
security <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/cleaned_security_data021324.csv") %>% filter(Starting <= "2022-12-15")

total_vacc <- total %>% 
  left_join(vaccine%>%select(ResidentId, Date_offset, num_dose, full_vacc), by=c("ResidentId")) %>% 
  mutate(Date_offset=if_else(Date_offset>test.Day, NA, Date_offset)) 
total_vacc <- total_vacc %>% 
  filter(num_dose%>%is.na()|all(Date_offset%>%is.na())|Date_offset==max(Date_offset,na.rm=T)) 
total_vacc <- total_vacc %>% distinct(ResidentId, test.Day, .keep_all = T)

total_vacc_security <- total_vacc %>% left_join(security)
total_vacc_security <- total_vacc_security %>% filter(Starting<=test.Day) %>% filter(Ending %>% is.na() | Ending > test.Day)

total_vacc_security <- total_vacc_security %>% mutate(num_dose=if_else(Date_offset%>%is.na(), 0, num_dose))
total_vacc_security <- total_vacc_security %>% select(!c(Starting, Ending))

distance <- function(tbl) {
  test_matrix <- tbl %>% select(test.Day) %>% dist(diag = T, upper = T) %>% as.matrix()
  test_matrix[test_matrix>2] <- Inf
  
  same_room <- tbl %>% select(RoomId) %>% dist(diag = T, upper = T) %>% as.matrix()

  test_matrix[same_room==0] <- Inf
  
  test_matrix
}

match <- NULL
for (i in 1:36) {
  for_matching_inst <- total_vacc_security %>% filter(Institution==i)
  matched_data <- matchit(case ~ Institution + BuildingId + num_dose + has.prior.inf + level,
                          data = for_matching_inst, 
                          distance = distance(for_matching_inst),
                          exact = case ~ Institution + BuildingId + num_dose + has.prior.inf + level)
  print(matched_data %>% summary())
  match <- rbind(match, matched_data %>% get_matches())
}

