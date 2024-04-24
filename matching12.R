# Sophia Tan 2/27/24
# Match infections and controls

rm(list=ls())
gc() 

library(tidyverse)
library(readr)
library(lubridate)
library(MatchIt)

cases <- read_csv("D:/CCHCS_premium/st/indirects/cases3-7daysame-roommate.csv")%>%mutate(case=1)%>%rename("test.Day"="inf.Day")
controls <- read_csv("D:/CCHCS_premium/st/indirects/control3-7daysame-roommate-041624.csv")%>%mutate(case=0)%>%select(names(cases))

cases
controls

total <- rbind(cases, controls)
total
total <- total %>% group_by(ResidentId, test.Day) 
total <- total %>% summarise_all(first)
total$case %>% table()

inf <- read_csv("D:/CCHCS_premium/st/cleaned-data/infection_data022624.csv") %>%
  select(ResidentId, CollectionDate) %>% rename(new.inf=CollectionDate)
total_remove <- total %>% left_join(inf, "ResidentId") %>%
  filter(new.inf>test.Day & new.inf-test.Day<14) %>%
  select(ResidentId, test.Day) %>% mutate(remove=1)
total <- total %>% left_join(total_remove) %>% filter(remove%>%is.na()) %>% select(!remove)

total <- total %>% mutate(has.prior.inf=case_when(case==1&num_pos>1~1,
                                                  case==1&num_pos==1~0,
                                                  case==0&num_pos>0~1,
                                                  T~0))
total

vaccine <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/complete_vaccine_data121523.csv") 
security <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/cleaned_security_data021324.csv") 
demo <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/demographic121523.csv") %>% mutate(age=2022-BirthYear) %>% select(ResidentId, age)
risk <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/covid_risk_score012324.csv")
inf <- read_csv("D:/CCHCS_premium/st/cleaned-data/infection_data022624.csv") %>% filter(CollectionDate <= "2022-12-15") %>%
  select(ResidentId, CollectionDate) %>% rename(last.inf=CollectionDate)

total_inf <- total %>% left_join(inf, "ResidentId") %>% 
  mutate(last.inf=if_else(is.na(last.inf)|last.inf>=test.Day, NA, last.inf)) %>%
  group_by(ResidentId, test.Day) %>%
  filter(all(last.inf%>%is.na())|!last.inf%>%is.na()) %>% 
  filter(all(last.inf%>%is.na())|last.inf==max(last.inf)) %>% distinct(ResidentId, test.Day, last.inf, .keep_all = T)

total_vacc <- total_inf %>% group_by(ResidentId, test.Day) %>%
  left_join(vaccine%>%select(ResidentId, Date_offset, num_dose, full_vacc), by=c("ResidentId")) %>% 
  mutate(Date_offset=if_else(Date_offset>test.Day, NA, Date_offset)) 
total_vacc <- total_vacc %>% 
  filter(all(Date_offset%>%is.na())|!Date_offset%>%is.na()) %>% 
  filter(all(Date_offset%>%is.na())|Date_offset==max(Date_offset)) %>% distinct(ResidentId, test.Day, Date_offset, .keep_all = T)

total_vacc_security <- total_vacc %>% left_join(security)
total_vacc_security <- total_vacc_security %>% filter(Starting<=test.Day) %>% filter(Ending %>% is.na() | Ending > test.Day)

total_vacc_security <- total_vacc_security %>% mutate(num_dose=if_else(Date_offset%>%is.na(), 0, num_dose))
total_vacc_security <- total_vacc_security %>% select(!c(Starting, Ending))

total_vacc_security_demo <- total_vacc_security %>% left_join(demo, by=c("ResidentId")) %>% left_join(demo, by=c("Roommate"="ResidentId"), suffix=c("", ".roommate"))
total_vacc_security_demo_risk <- total_vacc_security_demo %>% 
  left_join(risk, by=c("ResidentId")) %>% 
  filter(risk.start<=test.Day) %>% filter(risk.end > test.Day) %>%
  left_join(risk, by=c("Roommate"="ResidentId"), suffix=c("", ".roommate")) %>% 
  filter(risk.start.roommate<=test.Day) %>% filter(risk.end.roommate > test.Day)


total_vacc_security_demo_risk <- total_vacc_security_demo_risk %>% 
  mutate(time_since_vacc = (test.Day-Date_offset)%>%as.numeric(),
         time_since_inf = (test.Day-last.inf) %>% as.numeric()) 

total_vacc_security_demo_risk <- total_vacc_security_demo_risk %>% ungroup() %>%
  mutate(time_since_vacc.scale = (scale(time_since_vacc, T, T)%>%as.vector())*2,
         time_since_inf.scale = (scale(time_since_inf, T, T)%>%as.vector())*2,
         risk.scale = scale(risk, T, T)%>%as.vector(),
         risk.roommate.scale = scale(risk.roommate, T, T)%>%as.vector(),
         age.scale = scale(age, T, T)%>%as.vector(),
         age.roommate.scale = scale(age.roommate, T, T)%>%as.vector())

total_vacc_security_demo_risk <- total_vacc_security_demo_risk %>% replace_na(list(time_since_vacc.scale=0, time_since_inf.scale=0))

get_distance <- function(tbl) {
  dist_tbl <- dist(tbl %>% select(risk.scale, risk.roommate.scale, age.scale, age.roommate.scale), diag=T, upper=T) %>% as.matrix()
  
  tbl <- tbl %>% mutate(label=1:n()) %>% select(label, ResidentId, Roommate, test.Day) 
  tbl <- cross_join(tbl, tbl) %>% 
    mutate(roommates = ResidentId.x==Roommate.y,
           eligible=abs(test.Day.x-test.Day.y)%>%as.numeric()) 
  
  test_matrix <- tbl%>% 
    select(label.x,label.y,eligible) %>% 
    pivot_wider(id_cols = label.x, names_from = label.y, values_from = eligible) %>% as.data.frame() %>% select(!label.x)
  dist_tbl[test_matrix>2] <- Inf
  
  roommate_matrix <- tbl%>% 
    select(label.x, label.y, roommates) %>% 
    pivot_wider(id_cols = label.x, names_from = label.y, values_from = roommates) %>% as.data.frame() %>% select(!label.x)
  
  dist_tbl[roommate_matrix==T] <- Inf
  
  dist_tbl
}

total_vacc_security_demo_risk <- total_vacc_security_demo_risk %>% 
  mutate(num_dose_adjusted = case_when(num_dose==0~0,
                                       num_dose<full_vacc~1,
                                       num_dose==full_vacc~2,
                                       num_dose-full_vacc==1~3,
                                       num_dose-full_vacc>1~4))

match <- NULL
keys <- total_vacc_security_demo_risk%>%group_by(Institution, BuildingId, num_dose_adjusted, has.prior.inf, level)%>%
  summarise(control=sum(case!=1), case=sum(case==1)) %>% ungroup() %>% mutate(key=1:n())
keep <- keys %>% filter(control>0&case>0)
total_vacc_security_demo_risk <- total_vacc_security_demo_risk %>% left_join(keep %>% select(!c(control, case))) 

for (i in keep$key) {
  gc()
  for_matching_inst <- total_vacc_security_demo_risk %>% filter(key==i) %>% ungroup()
  for_matching_inst <- for_matching_inst %>% arrange(desc(case))
  
  distance_matrix <- get_distance(for_matching_inst)
  
  num_cases <- sum(for_matching_inst$case==1)
  num_controls <- sum(for_matching_inst$case==0)
  
  valid_matches <- all(distance_matrix[(num_cases+1):nrow(for_matching_inst),1:num_cases]%>%is.infinite())
  if(valid_matches) {next}
  
  if(num_cases==1) {
    for_matching_inst$distance <- distance_matrix[,1]
    
    num_match <- min(nrow(for_matching_inst%>%filter(!distance%>%is.infinite())), 3)
    for_matching_inst <- (for_matching_inst %>% arrange(distance))[1:num_match,] %>% select(!distance)
    
    match <- rbind(match, cbind(id=1:num_match, subclass=rep(1, num_match), for_matching_inst))
    next
  }
  
  if(num_controls==1) {
    for_matching_inst$distance <- distance_matrix[,1]
    
    for_matching_inst <- (for_matching_inst %>% arrange(case, distance))[1:2,] %>% select(!distance)
    
    match <- rbind(match, cbind(id=1:2, subclass=c(1,1), for_matching_inst))
    next
  }
  
  matched_data <- matchit(case ~ Institution + BuildingId + num_dose_adjusted + has.prior.inf + level,
                          data = for_matching_inst, 
                          distance = distance_matrix,
                          exact = case ~ Institution + BuildingId + num_dose_adjusted + has.prior.inf + level, 
                          ratio=2)
  
  print(matched_data %>% summary())
  match <- rbind(match, matched_data %>% get_matches() %>% select(!weights))
}

match

match %>% group_by(key, subclass) %>% arrange(key, subclass, desc(case)) %>% filter(abs(test.Day-first(test.Day))<=2 & first(ResidentId)!=Roommate)
match %>% select(!c(n, Day, Night)) %>% write_csv("D:/CCHCS_premium/st/indirects/matched_building_3_7days-12matching-notimematch-042324.csv")

