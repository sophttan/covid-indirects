# Sophia Tan 3/8/22
# Test for loop code

rm(list=ls())
setwd("D:/stan5/code_ST")

library(tidyverse)
library(readr)
# if (!require("devtools")) install.packages("devtools")
# devtools::install_github("mkuhn/dict")

d <- read_csv("housing_inf_data.csv")
d <- d %>% filter(Day >= "2020-03-01")
#d <- d %>% select(ResidentId, Day, Result, num_dose, max_dose, full_vacc, booster_add_dose, QuarantineIsolation, RoomCensus, RoomId)

prim <- read_csv("potential-primary-cases/all_infections_less_than_8res.csv") #24/100
#prim <- read_csv("potential-primary-cases/all_infections.csv")
infections <- read_csv("infectious_periods_primary_cases.csv")

group_room <- d %>% group_by(RoomId, Day)
group_room <- group_room %>% summarise(residents=list(unique(ResidentId)))

sum_vacc <- infections%>%group_by(ResidentId, num_pos) %>% summarise_all(first) %>% select(ResidentId, num_pos, Day, num_dose, max_dose, full_vacc)
prim <- left_join(prim, sum_vacc, by=c("ResidentId", "num_pos")) 
prim <- prim %>% mutate(quarantine = F, contacts=list(NULL), pos_contacts=list(NULL), neg_contacts=list(NULL), 
                        multiple_inf = list(NULL), num_possible_contacts = 0, num_no_prior_neg_test = 0, num_no_testing = 0, num_prior_inf = 0)
prim <- prim %>% arrange(Day)
set.seed(42)
prim_sub <- sample(1:nrow(prim), 100)

j <- 1
for (p in prim_sub) {
  print_res <- F
  if (j%%20==0){
    print_res <- T
    gc()
  }
  # find ResidentId and infectious period of the primary infection
  primary_case <- prim[p,]
  resident <- primary_case$ResidentId
  infectious_p <- infections %>% filter(no==primary_case$no) %>% group_by(RoomId, Day)
  
  # mark if resident was in quarantine/isolation the entire infectious period
  if(all(infectious_p$QuarantineIsolation==2)) {prim$quarantine[p] <- T}
  
  inf <- infectious_p %>% select(RoomId, Day) %>% left_join(group_room, by=c("RoomId", "Day"))
  contacts <- unique(unlist(inf$residents))
  contacts <- contacts[contacts!=resident]
  prim$num_possible_contacts[p]<-length(contacts)
  
  if(print_res) {
    print(inf)
    print(contacts)
  }
  
  if (length(contacts)==0) {next}
  
  for (c in contacts) {
    inf <- inf %>% group_by(Day) %>% mutate(is_in = c %in% unlist(residents))
    same_room <- (inf %>% filter(is_in))
    first_share <- same_room$Day[1]
    last_share <- (same_room %>% arrange(desc(Day)))$Day[1]
    
    previous_inf <- d %>% filter(ResidentId==c & Day < first_share)
    sum_prev_inf <- previous_inf %>% group_by(num_pos) %>% summarise_all(first) %>% arrange(desc(Day))
    
    if (first_share-sum_prev_inf$Day[1]<90 & !(sum_prev_inf$num_pos[1] %>% is.na())) {
      prev_inf_sub <- previous_inf %>% filter(num_pos==sum_prev_inf$num_pos[1])
      if(!any(prev_inf_sub$Result=="Negative"&prev_inf_sub$pcr, na.rm=T)){
        if(print_res) {
          print(c(first_share, last_share))
          print(sum_prev_inf)
          print("prior infection")
        }
        prim$num_prior_inf[p] <- prim$num_prior_inf[p] + 1
        next
      }
    }
    
    all_related <- d %>% filter(first_share - Day <= 2 & Day <= last_share + 14 & ResidentId==c)
    if(print_res){
      print(all_related)
    }
    
    include <- all_related %>% filter(abs(Day-first_share)<=2)
    if(all(include$Result %>% is.na())){
      if(print_res) {
        print("no prior neg result")
      }
      prim$num_no_prior_neg_test[p]<-prim$num_no_prior_neg_test[p]+1
      next
    } else if (any(include$Result=="Positive",na.rm=T)) {
      if (print_res) {
        print("concurrent inf")
      }
      prim$multiple_inf[[p]] <- c(prim$multiple_inf[[p]], c)
      next
    } else if(any(include$Result=="Negative",na.rm=T)) {
      contact <- d %>% filter(Day >= first_share + 3 & Day <= last_share + 14 & ResidentId==c)
      
      if(all(is.na(contact$Result))) {
        if(print_res) {
          print("contact with no test result")
        }
        prim$num_no_testing[p]<-prim$num_no_testing[p]+1
        next
      }
      
      prim$contacts[[p]] <- c(prim$contacts[[p]], c)
      if(print_res) {
        print("valid contact")
      }
      
      if(any(contact$Result=="Positive",na.rm=T)) {
        prim$pos_contacts[[p]] <- c(prim$pos_contacts[[p]], c)
      } else{
        prim$neg_contacts[[p]] <- c(prim$neg_contacts[[p]], c)
      }  
    }
  }
  new_primary_case <- prim[p,]
  print(paste0("Total of ", new_primary_case$num_possible_contacts, " possible contacts"))
  print(paste0("There were a total of ", new_primary_case$num_prior_inf, " contacts excluded because they had prior (unresolved) infection within 90 days of first exposure"))
  print(paste0("There were a total of ", new_primary_case$multiple_inf %>% unlist() %>% length(), " other concurrent infections"))
  print(paste0("There were a total of ", new_primary_case$num_no_prior_neg_test, " contacts excluded because they had no testing data within +/- 2 days of primary infection"))
  print(paste0("There were a total of ", new_primary_case$num_no_testing, " contacts excluded because they had no testing data between 3-14 days after first and last exposure"))
  j <- j+1
} 

prim_subset <- prim[prim_sub,]
prim_final <- prim_subset %>% group_by(no) %>% filter(is.null(unlist(multiple_inf)) & !is.null(unlist(contacts))) %>% ungroup()

test <- prim_subset %>% group_by(no) %>% filter(is.null(unlist(multiple_inf)) & !is.null(unlist(contacts))) 
test$num_possible_contacts %>% summary()
(test %>% mutate(contacts = unlist(contacts) %>% length()))$contacts %>% summary()

test_res <- function(resident, contact=NULL) {
  infectious_p <- infections %>% filter(ResidentId == resident) %>% group_by(RoomId, Day)
  inf <- infectious_p %>% select(RoomId, Day) %>% left_join(group_room, by=c("RoomId", "Day"))
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
    print(all_related)
    
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
