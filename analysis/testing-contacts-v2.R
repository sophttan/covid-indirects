# Sophia Tan 4/1/22
# Test for loop code

rm(list=ls())
setwd("D:/stan5/code_ST")

library(tidyverse)
library(readr)

d <- read_csv("housing_inf_data_adjusted_roomtype.csv")
d <- d %>% filter(Day >= "2020-03-01")
gc()

infections <- read_csv("potential-primary-cases/infectious_periods_primary_cases_v2_roomtypes_9days.csv")

group_room <- d %>% group_by(Institution, RoomId, Day)
group_room <- group_room %>% summarise(residents=list(unique(ResidentId)))
gc()

prim <- infections%>%group_by(ResidentId, num_pos.y) %>% summarise_all(first) %>% select(ResidentId, num_pos.y, no, Day, num_dose, max_dose, full_vacc)
prim <- prim %>% mutate(contacts=list(NULL), pos_contacts=list(NULL), neg_contacts=list(NULL), 
                        multiple_inf = list(NULL), num_possible_contacts = 0, num_no_prior_neg_test = 0, num_no_testing = 0, num_prior_inf = 0)

infections <- infections %>% select(ResidentId, no, num_pos.x, Institution, RoomId, RoomType, Day) %>% left_join(group_room, by=c("Institution", "RoomId", "Day"))
#contact_details <- matrix(ncol=6, nrow=10000)

prim <- prim %>% arrange(Day)
# set.seed(42)
# prim_sub <- sample(1:nrow(prim), 1000)

#num_contact <- 1
for (p in 1:nrow(prim)) {
  print_res <- F
  if (p%%20==0){
    print(p)
    print_res <- T
    gc()
  }
  # find ResidentId and infectious period of the primary infection
  primary_case <- prim[p,]
  resident <- primary_case$ResidentId
  inf <- infections %>% filter(no==primary_case$no) %>% group_by(RoomId, Day)
  inf <- inf %>% filter(RoomType %in% c(1,2))
  
  # mark if resident was in quarantine/isolation the entire infectious period
  #if(all(inf$QuarantineIsolation==2)) {prim$quarantine[p] <- T}
  
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
    
    contact_data <- d %>% filter(ResidentId==c)
    
    previous_inf <- contact_data %>% filter(Day < first_share)
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
    
    all_related <- contact_data %>% filter(first_share - Day <= 2 & Day <= last_share + 14)
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
      
      #contact_details[num_contact,] <- c(c, resident, first_share, contact$Day[nrow(contact)], nrow(same_room), round(mean(contact$num_dose)))
      #num_contact <- num_contact+1
      
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
} 

prim_subset <- prim[prim_sub,]
prim <- prim %>% mutate(week = round(as.numeric(difftime(Day,as.Date("2020-03-01"), units="weeks"))))
prim_final <- prim %>% group_by(no) %>% filter(is.null(unlist(multiple_inf)) & !is.null(unlist(contacts))) %>% ungroup()

# includes rooms with multiple infections if they have additional contacts
prim %>% group_by(no) %>% filter(!is.null(unlist(contacts))) %>% ungroup()

# choices - exclude or attribute to a single infectious person
has_multiple_inf_and_contact <- prim %>% group_by(no) %>% filter(!is.null(unlist(contacts))&!is.null(unlist(multiple_inf))) %>% ungroup()


prim_final %>% group_by(num_pos.y, num_dose, full_vacc) %>% summarise(count=n())

(prim_final %>% group_by(no) %>% mutate(contacts = unlist(contacts) %>% length()))$contacts %>% summary()

# explore selection 
grid <- expand.grid(week = 0:98)
prim %>% group_by(week) %>% summarise(count=n()) %>% full_join(grid) %>% replace_na(list(count=0)) %>% ggplot(aes(x=week,y=count)) + geom_line()
prim_final %>% group_by(week) %>% summarise(count=n()) %>% full_join(grid) %>% replace_na(list(count=0)) %>% ggplot(aes(x=week,y=count)) + geom_line()

total_prim <- prim %>% group_by(week) %>% summarise(count=n()) 
total_prim <- total_prim %>% left_join(prim_final %>% group_by(week) %>% summarise(included=n()), "week") 
total_prim <- total_prim %>% replace_na(list(included=0))
total_prim %>% mutate(excluded=count-included, excluded_prop=excluded/count*100)

prim_final$contacts %>% unlist() %>% length()
prim_final$contacts %>% unlist() %>% unique() %>% length()

prim_final %>% select(!multiple_inf) %>% unnest(cols = c("contacts", "pos_contacts", "neg_contacts"))
write_csv(prim_final %>% select(!c(pos_contacts, neg_contacts, multiple_inf)) %>% unnest(contacts), "final samples/final_sample041122_9day.csv")
write_csv(has_multiple_inf_and_contact %>% unnest(c(contacts, pos_contacts, neg_contacts, multiple_inf)), "final samples/final_sample041122_multinf_9day.csv")

test_res <- function(resident, contact=NULL) {
  inf <- infections %>% filter(ResidentId == resident)
  contacts <- unique(unlist(inf$residents))
  contacts <- contacts[contacts!=resident]
  print(contacts) 
  print(inf)
  
  inf <- inf %>% filter(RoomType %in% c(1,2))
  
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
