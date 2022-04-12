# Sophia Tan 3/8/22
# Test for loop code

rm(list=ls())
setwd("D:/stan5/code_ST")

library(tidyverse)
library(readr)

d <- read_csv("housing_inf_data_adjusted_roomtype.csv")
d <- d %>% filter(Day >= "2020-03-01")

infections <- read_csv("infectious_periods_primary_cases_v2_roomtypes.csv")

group_room <- d %>% group_by(Institution, RoomId, Day)
group_room <- group_room %>% summarise(residents=list(unique(ResidentId)))

sum_vacc <- infections%>%group_by(ResidentId, num_pos) %>% summarise_all(first) %>% select(ResidentId, num_pos, Day, num_dose, max_dose, full_vacc)
sum_vacc <- sum_vacc %>% arrange(Day)

gc()

get_contacts <- function(inf, resident) {
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
  
  all_related <- contact_housing %>% filter(first_share - Day <= 2 & Day <= last_share + 14 & ResidentId==contact)
  
  include <- all_related %>% filter(abs(Day-first_share)<=2)
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

infections <- infections %>% select(ResidentId, no, RoomId, Day) %>% left_join(group_room, by=c("RoomId", "Day"))

test_primary_infection <- function(inf_no) {
  infectious_p <- infections %>% filter(no==inf_no) %>% group_by(RoomId, Day)
  resident <- infectious_p$ResidentId[1]
  contacts <- get_contacts(infectious_p, resident)
  valid_contacts(infectious_p %>% group_by(Day), contacts)
}

vectorize_test_primary_infection <- Vectorize(test_primary_infection)

prim <- prim %>% mutate(contacts = vectorize_test_primary_infection(no))
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
