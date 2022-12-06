# Sophia Tan 4/1/22
# Test for loop code

rm(list=ls())
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")

library(tidyverse)
library(readr)

d <- read_csv("housing_inf_data072122.csv")

infections <- read_csv("infectious_periods_primary_cases100722_nopcr.csv")

group_room <- d %>% filter(!is.na(Institution) & !is.na(RoomId)) %>% group_by(Institution, RoomId, Day)
group_room <- group_room %>% summarise(residents=list(unique(ResidentId)))
gc()

prim <- infections%>%group_by(ResidentId, num_pos) %>% summarise_all(first) %>% select(ResidentId, num_pos, no, Day, Institution, num_dose, max_dose, full_vacc)
prim <- prim %>% mutate(contacts=list(NULL), has_inf = list(NULL), multiple_inf = list(NULL), 
                        has_neg_test=list(NULL), has_followup=list(NULL),
                        first_contact = list(NULL),
                        has_prior_inf=list(NULL), num_vacc_doses=list(NULL), 
                        num_days_in_contact=list(NULL),
                        num_tests_followup=list(NULL), 
                        first_followup=list(NULL),last_followup=list(NULL), 
                        day_secondary_case=list(NULL),
                        neg_pos_contact=list(NULL))

infections <- infections %>% select(ResidentId, no, num_pos, Institution, RoomId, RoomType, Day) %>% left_join(group_room, by=c("Institution", "RoomId", "Day"))

prim <- prim %>% arrange(Day)
gc()

find_infectious_period <- function(prim_no) {
  inf <- infections %>% filter(no==prim_no) %>% group_by(RoomId, Day)
  # only include rooms where residents is in closed door cell)
  inf <- inf %>% filter(RoomType %in% c(1,2)|(RoomType==4&!(Institution %in% c(9,11,12,14,18,32))))
  # only include rooms where total of 10 or fewer residents
  inf <- inf %>% group_by(Day) %>% filter(length(unlist(residents))<=10)
  inf
}

find_contacts <- function(inf, resident) {
  contacts <- unique(unlist(inf$residents))
  contacts <- contacts[contacts!=resident]
  contacts
}

check_prior_inf <- function(contact_data) {
  # want to check if contact is susceptible to infection
  # no infection in the last 90 days unless they have a negative pcr test in the interim
  # returns 1 if prior unresolved infection, 0 if susceptible
  
  previous_inf <- contact_data %>% filter(Day < first_share)
  sum_prev_inf <- previous_inf %>% group_by(num_pos) %>% summarise_all(first) %>% arrange(desc(Day))
  
  if (first_share-sum_prev_inf$Day[1]<90 & !(sum_prev_inf$num_pos[1] %>% is.na())) {
    prev_inf_sub <- previous_inf %>% filter(num_pos==sum_prev_inf$num_pos[1])
    if(!any(prev_inf_sub$Result=="Negative"&prev_inf_sub$pcr, na.rm=T)){
      return(1)
    }
  }
  return(0)
}

check_concurrent_inf <- function(initial) {
  if (any(initial$Result=="Positive",na.rm=T)) {
    return(1)
  }
  return(0)
}

check_intial_testing <- function(initial) {
  if(all(initial$Result %>% is.na())) {
    return(0)
  }
  return(1)
}

check_followup_testing <- function(followup) {
  if(all(followup$Result %>% is.na())) {
    return(0)
  }
  return(1)
}

check_secondary_case <- function(followup) {
  if(any(followup$Result=="Positive")) {
    return(1)
  }
  return(0)
}

number_test <- function(followup) {
  tests <- followup %>% filter(!Result %>% is.na())
  tests %>% nrow()
}

first_test <- function(followup) {
  tests <- followup %>% filter(!Result %>% is.na())
  tests$Day[1]  
}

latest_test <- function(followup) {
  tests <- followup %>% arrange(desc(Day)) %>% filter(!Result %>% is.na())
  tests$Day[1]  
}

first_positive_test <- function(followup) {
  tests <- followup %>% filter(Result=="Positive")
  tests$Day[1]
}

for (p in 1:nrow(prim)) {
  
  print_res <- F
  if (p%%50==0){
    print(p)
    print_res <- T
    gc()
  }
  
  # find ResidentId and infectious period of the primary infection
  primary_case <- prim[p,]
  resident <- primary_case$ResidentId
  no <- primary_case$no
  inf <- find_infectious_period(no)
  contacts <- find_contacts(inf, resident)
  
  if(print_res) {print(inf)}
  
  if (length(contacts)==0) {next}

  prim$contacts[[p]] <- contacts
  
  if(print_res) {print(contacts)}
  
  for (c in contacts) {
    inf <- inf %>% group_by(Day) %>% mutate(is_in = c %in% unlist(residents))
    same_room <- (inf %>% filter(is_in))
    first_share <- same_room$Day[1]
    last_share <- (same_room %>% arrange(desc(Day)))$Day[1]
    
    prim$first_contact[[p]] <- c(prim$first_contact[[p]], as.character(first_share))
    prim$num_days_in_contact[[p]] <- c(prim$num_days_in_contact[[p]], nrow(same_room))
    
    contact_data <- d %>% filter(ResidentId==c)
    
    prior_inf <- check_prior_inf(contact_data)
    
    if (prior_inf==1) {
      prim$has_inf[[p]] <- c(prim$has_inf[[p]], 1)
      prim$multiple_inf[[p]] <- c(prim$multiple_inf[[p]], NA)
      prim$has_neg_test[[p]] <- c(prim$has_neg_test[[p]], NA)
      prim$has_followup[[p]] <- c(prim$has_followup[[p]], NA)
      prim$neg_pos_contact[[p]] <- c(prim$neg_pos_contact[[p]], NA)
      prim$num_tests_followup[[p]] <- c(prim$num_tests_followup[[p]], NA)
      prim$day_secondary_case[[p]] <- c(prim$day_secondary_case[[p]], NA)
      prim$last_followup[[p]] <- c(prim$last_followup[[p]], NA)
      prim$first_followup[[p]] <- c(prim$first_followup[[p]], NA)
      prim$has_prior_inf[[p]] <- c(prim$has_prior_inf[[p]], NA)
      prim$num_vacc_doses[[p]] <- c(prim$num_vacc_doses[[p]], NA)
      if (print_res) {print("has prior infection")}
      next
    } else {
      prim$has_inf[[p]] <- c(prim$has_inf[[p]], 0)
    }
    
    all_related <- contact_data %>% filter(first_share - Day <= 2 & Day <= last_share + 14)
    initial <- all_related %>% filter(abs(Day-first_share)<=2)
    
    if(print_res){
      print(all_related)
    }
    
    concurrent_inf <- check_concurrent_inf(initial)
    
    if (concurrent_inf==1) {
      prim$multiple_inf[[p]] <- c(prim$multiple_inf[[p]], 1)
      if (print_res) {print("has concurrent infection")}
    } else {      
      prim$multiple_inf[[p]] <- c(prim$multiple_inf[[p]], 0)
    }

    status_first_contact <- filter(all_related, Day==first_share)
    prim$has_prior_inf[[p]] <- c(prim$has_prior_inf[[p]], ifelse(status_first_contact$num_pos %>% is.na(), 0, 1))
    prim$num_vacc_doses[[p]] <- c(prim$num_vacc_doses[[p]], status_first_contact$num_dose_adjusted)
   
    followup <- d %>% filter(Day >= first_share + 3 & Day <= last_share + 14 & ResidentId==c)

    initial_testing <- check_intial_testing(initial)
    if (initial_testing==0) {
      prim$has_neg_test[[p]] <- c(prim$has_neg_test[[p]], 0)
      if(print_res) {print("contact with no prior negative test")}
    } else {
      prim$has_neg_test[[p]] <- c(prim$has_neg_test[[p]], 1)
    }
    
    followup_testing <- check_followup_testing(followup)
    if (followup_testing==0) {
      prim$has_followup[[p]] <- c(prim$has_followup[[p]], 0)
      prim$num_tests_followup[[p]] <- c(prim$num_tests_followup[[p]], NA)
      prim$last_followup[[p]] <- c(prim$last_followup[[p]], NA)
      prim$first_followup[[p]] <- c(prim$first_followup[[p]], NA)
      prim$day_secondary_case[[p]] <- c(prim$day_secondary_case[[p]], NA)
      prim$neg_pos_contact[[p]] <- c(prim$neg_pos_contact[[p]], NA)
      if(print_res) {print("contact with no followup test")}
    } else {
      prim$has_followup[[p]] <- c(prim$has_followup[[p]], 1)
      prim$num_tests_followup[[p]] <- c(prim$num_tests_followup[[p]], number_test(followup))
      prim$last_followup[[p]] <- c(prim$last_followup[[p]], as.character(latest_test(followup)))
      prim$first_followup[[p]] <- c(prim$first_followup[[p]], as.character(first_test(followup)))
      
      if(any(followup$Result=="Positive",na.rm=T)) {
        first_pos_test <- first_positive_test(followup)
        prim$day_secondary_case[[p]] <- c(prim$day_secondary_case[[p]], as.character(first_pos_test))
        prim$neg_pos_contact[[p]] <- c(prim$neg_pos_contact[[p]], 1)
      } else{
        prim$neg_pos_contact[[p]] <- c(prim$neg_pos_contact[[p]], 0)
        prim$day_secondary_case[[p]] <- c(prim$day_secondary_case[[p]], NA)
      }  
    }  
    
    if (initial_testing==1&followup_testing==1) {
      if(print_res) {print("valid contact")}
    }
  }
}

prim <- prim %>% mutate(week = round(as.numeric(difftime(Day,as.Date("2020-03-01"), units="weeks"))))
# only 58% have any contacts
# 42% in isolation or living alone beginning on the first day of infectious period
all_contacts <- prim %>% unnest(c(contacts, has_inf, multiple_inf, has_neg_test, has_followup, 
                  first_contact, has_prior_inf, num_vacc_doses, num_days_in_contact, 
                  num_tests_followup, first_followup, last_followup, day_secondary_case, neg_pos_contact)) %>% group_by(no)

write.csv(all_contacts, "all_contacts2_7day_100722.csv")

#check multiple close contacts
multiple_index <- all_contacts %>% group_by(contacts) %>% filter(n()>1 & !any(has_inf==1))
multiple_index %>% arrange(contacts) %>% select(contacts, Day, has_inf, multiple_inf, has_neg_test, has_followup, num_tests_followup, first_contact, first_followup, last_followup, day_secondary_case)

all_contacts %>% filter(any(multiple_inf==1))

remove_prior_inf <- all_contacts %>% filter(all(multiple_inf!=1,na.rm=T)) %>% filter((has_inf)==0)
remove_prior_inf %>% filter(has_followup==0&has_neg_test==1)

final <- all_contacts %>% group_by(no) %>% filter(all(multiple_inf==0, na.rm=T)) %>% ungroup() %>%
  filter(has_inf==0)

has_followup <- final %>% filter(has_followup==1)%>%group_by(contacts) %>% filter(n()==1|!all(neg_pos_contact==1, na.rm=T))
has_followup %>% write_csv("final_sample100722_nopcr_nonegtest.csv")

all_pos2days <- all_contacts %>% filter(has_inf==0) %>% filter(has_neg_test==1&has_followup==1)
all_pos2days %>% filter(any(multiple_inf==1)) %>% filter(n()>1) %>% view()
all_pos2days <- all_pos2days %>% filter(!(no==2727&contacts==1619247598))
all_pos2days <- all_pos2days %>% group_by(contacts) %>% filter(n()==1|!all(neg_pos_contact==1, na.rm=T))
all_pos2days %>% write_csv("final_sample100722_nopcr_pos2days.csv")

final <- final %>% filter(has_neg_test==1&has_followup==1)
# check if any contacts matched with mutiple index cases
final %>% group_by(contacts) %>% filter(n()>1) 

final <- final %>% group_by(contacts) %>% filter(n()==1|any(neg_pos_contact==0)) 

write_csv(final, "final_sample100722_nopcr_2_7days.csv")
