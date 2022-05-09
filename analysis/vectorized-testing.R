# Sophia Tan 3/8/22
# Test for loop code

rm(list=ls())
setwd("D:/stan5/code_ST/march-data/")

library(tidyverse)
library(readr)

d <- read_csv("housing_inf_data_adjusted_roomtype1.csv")
d2 <- read_csv("housing_inf_data_adjusted_roomtype2.csv")
d <- bind_rows(d, d2)
rm(d2)
gc()

infections <- read_csv("D:/stan5/code_ST/potential-primary-cases/march_infectious_periods_primary_cases_v2_roomtypes_8days_somecells.csv")

group_room <- d %>% group_by(Institution, RoomId, Day)
group_room <- group_room %>% summarise(residents=list(unique(ResidentId)))
#group_room %>% unnest("residents") %>% write_csv("residents_by_room.csv")
gc()

sum_vacc <- infections%>%group_by(ResidentId, num_pos.x) %>% summarise_all(first) %>% select(ResidentId, num_pos.x, no, Day, num_dose, max_dose, full_vacc)
sum_vacc <- sum_vacc %>% arrange(Day)
gc()

get_contacts <- function(inf, resident) {
  inf <- inf %>% filter(RoomType %in% c(1,2)|(RoomType==4&!(Institution %in% c(4,5,9,10,11,12,14,15,17,18,30,32))))
  contacts <- unique(unlist(inf$residents))
  contacts <- contacts[contacts!=resident]
  contacts
}

valid_contact <- function(inf, contact) {
  inf1 <- inf %>% mutate(is_in = contact %in% unlist(residents))
  same_room <- (inf1 %>% filter(is_in))
  first_share <- same_room$Day[1]
  last_share <- (same_room %>% arrange(desc(Day)))$Day[1]
  
  contact_housing <- d %>% filter(ResidentId==contact)
  previous_inf <- contact_housing %>% filter(Day < first_share)
  sum_prev_inf <- previous_inf %>% group_by(num_pos) %>% summarise_all(first) %>% arrange(desc(Day))
  
  if (first_share-sum_prev_inf$Day[1]<90 & !(sum_prev_inf$num_pos[1] %>% is.na())) {
    prev_inf_sub <- previous_inf %>% filter(num_pos==sum_prev_inf$num_pos[1])
    if(!any(prev_inf_sub$Result=="Negative"&prev_inf_sub$pcr, na.rm=T)){
      return(F)
    }
  }
  
  all_related <- contact_housing %>% filter(first_share - Day <= 8 & Day <= last_share + 14 & ResidentId==contact)
  
  include <- all_related %>% filter(first_share-Day<=8 & Day-first_share<=2)
  if(all(include$Result %>% is.na())){
    return(F)
  } else if (any(include$Result=="Positive",na.rm=T)) {
    return(NA)
  } else if(any(include$Result=="Negative",na.rm=T)) {
    c <- all_related %>% filter(Day >= first_share + 3 & Day <= last_share + 14 & ResidentId==contact)
    
    if(all(is.na(c$Result))) {
      return(F)
    }
    return(T)
  }
}

vectorize_valid_contact <- Vectorize(valid_contact, c("contact"))

valid_contacts <- function(inf, contacts) {
  valid <- contacts
  results <- vectorize_valid_contact(inf, contacts)
  if(any(results %>% is.na())|all(results==F)){return(NULL)}
  return(valid[results])
}

infections <- infections %>% select(ResidentId, no, RoomId,RoomType, Institution, Day) %>% left_join(group_room, by=c("Institution", "RoomId", "Day"))

test_primary_infection <- function(inf_no) {
  infectious_p <- infections %>% filter(no==inf_no)
  resident <- infectious_p$ResidentId[1]
  contacts <- get_contacts(infectious_p, resident)
  if(contacts %>% length() == 0) {return(NULL)}
  list(valid_contacts(infectious_p %>% group_by(Day), contacts))
}

vectorize_test_primary_infection <- Vectorize(test_primary_infection)

sum_vacc_subset <- filter(sum_vacc, Day >= "2021-01-01") %>% group_by(no) %>% mutate(contacts = as.vector(vectorize_test_primary_infection(no)))
has_contacts <- sum_vacc_subset %>% group_by(no) %>% filter(!unlist(contacts) %>% is.null())
has_contacts <- has_contacts %>% mutate(contacts = ifelse(contacts[[1]] %>% is.list(), contacts[[1]], contacts)) 
has_contacts <- has_contacts %>% unnest(contacts) %>% left_join(infections %>% select(!c(residents, RoomId)) %>% group_by(no) %>% summarise_all(first))
has_contacts %>% write_csv("D:/stan5/code_ST/final samples/march_final_sample_8day_somecells3_8daypcrcontact.csv")

gc()

only_valid_contacts <- prim %>% group_by(no) %>% filter(!unlist(contacts)%>%is.null())
only_valid_contacts %>% filter(num_dose>0)

no_valid_contacts <- prim %>% group_by(no) %>% filter(unlist(contacts)%>%is.null())

test_res <- function(inf_no, contact=NULL) {
  infectious_p <- infections %>% filter(no == inf_no) %>% group_by(RoomId, Day)
  resident <- infectious_p$ResidentId[1]
  inf <- infectious_p %>% select(RoomId, Day, residents)
  contacts <- unique(unlist(inf$residents))
  contacts <- contacts[contacts!=resident]
  print(contacts) 
  print(inf)
  
  test_c <- function(c){
    inf <- inf %>% group_by(Day) %>% mutate(is_in = c %in% unlist(residents))
    same_room <- (inf %>% filter(is_in))
    first_share <- same_room$Day[1]
    last_share <- (same_room %>% arrange(desc(Day)))$Day[1]
    
    previous_inf <- d %>% filter(ResidentId==c & Day < first_share)
    sum_prev_inf <- previous_inf %>% group_by(num_pos) %>% summarise_all(first) %>% arrange(desc(Day))
    
    if (first_share-sum_prev_inf$Day[1]<90 & !(sum_prev_inf$num_pos[1] %>% is.na())) {
      prev_inf_sub <- previous_inf %>% filter(num_pos==sum_prev_inf$num_pos[1])
      if(!any(prev_inf_sub$Result=="Negative"&prev_inf_sub$pcr, na.rm=T)){
        print(c(first_share, last_share))
        print(sum_prev_inf)
        print("prior infection")
        return()
      }
    }
    
    all_related <- d %>% filter(first_share - Day <= 2 & Day <= last_share + 14 & ResidentId==c)
    print(all_related %>% select(ResidentId, Day, RoomId, Result, Details, num_dose, max_dose, full_vacc))
    
    include <- all_related %>% filter(abs(Day-first_share)<=2)
    if(all(include$Result %>% is.na())){
      print("no prior neg result")
      return()
    } else if(any(include$Result=="Positive",na.rm=T)) {
      print("concurrent inf")
      return()
    } else if(any(include$Result=="Negative",na.rm=T)) {
      contact <- d %>% filter(Day >= first_share + 3 & Day <= last_share + 14 & ResidentId==c)
      if(all(is.na(contact$Result))) {
        print("contact with no test result")
        return()
      }
      
      print("valid contact")
    } 
  }
  
  if(!contact %>% is.null()) {
    test_c(contact)
  } else {
    for (c in contacts) {
      test_c(c)
    }
  }
}

final <- read_csv("D:/stan5/code_ST/final samples/march_final_sample_8day_somecells3.csv")
