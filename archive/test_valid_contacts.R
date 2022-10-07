# Sophia Tan 4/1/22
# Test for loop code

rm(list=ls())
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")

library(tidyverse)
library(readr)

d <- read_csv("housing_inf_data072122.csv")

infections <- read_csv("infectious_periods_primary_cases092722_nopcr.csv")

group_room <- d %>% filter(!is.na(Institution) & !is.na(RoomId)) %>% group_by(Institution, RoomId, Day)
group_room <- group_room %>% summarise(residents=list(unique(ResidentId)))
gc()

prim <- infections%>%group_by(ResidentId, num_pos) %>% summarise_all(first) %>% select(ResidentId, num_pos, no, Day, Institution, num_dose, max_dose, full_vacc)
prim <- prim %>% mutate(contacts=list(NULL), neg_pos_contact=list(NULL), multiple_inf = list(NULL), 
                        has_prior_inf=list(NULL), num_vacc_doses=list(NULL), num_days_in_contact=list(NULL))

infections <- infections %>% select(ResidentId, no, num_pos, Institution, RoomId, RoomType, Day) %>% left_join(group_room, by=c("Institution", "RoomId", "Day"))
#contact_details <- matrix(ncol=6, nrow=10000)

prim <- prim %>% arrange(Day)
# set.seed(42)
# prim_sub <- sample(1:nrow(prim), 1000)

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
  inf <- inf %>% filter(RoomType %in% c(1,2)|(RoomType==4&!(Institution %in% c(9,11,12,14,18,32))))
  inf <- inf %>% group_by(Day) %>% filter(length(unlist(residents))<=10)
  
  contacts <- unique(unlist(inf$residents))
  contacts <- contacts[contacts!=resident]

  if (length(contacts)==0) {next}
  
  if(print_res) {
    print(inf)
    print(contacts)
  }
  
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
        next
      }
      
      prim$contacts[[p]] <- c(prim$contacts[[p]], c)

      status_first_contact <- filter(all_related, Day==first_share)
      prim$has_prior_inf[[p]] <- c(prim$has_prior_inf[[p]], ifelse(status_first_contact$num_pos %>% is.na(), 0, 1))
      prim$num_vacc_doses[[p]] <- c(prim$num_vacc_doses[[p]], status_first_contact$num_dose_adjusted)
      
      prim$num_days_in_contact[[p]] <- c(prim$num_days_in_contact[[p]], nrow(same_room))
      
      if(print_res) {
        print("valid contact")
      }
      
      #contact_details[num_contact,] <- c(c, resident, first_share, contact$Day[nrow(contact)], nrow(same_room), round(mean(contact$num_dose)))
      #num_contact <- num_contact+1
      
      if(any(contact$Result=="Positive",na.rm=T)) {
        prim$neg_pos_contact[[p]] <- c(prim$neg_pos_contact[[p]], 1)
      } else{
        prim$neg_pos_contact[[p]] <- c(prim$neg_pos_contact[[p]], 0)
      }  
    }
  }
} 

prim_subset <- prim[prim_sub,]
prim <- prim %>% mutate(week = round(as.numeric(difftime(Day,as.Date("2020-03-01"), units="weeks"))))
prim_final <- prim %>% group_by(no) %>% filter(is.null(unlist(multiple_inf)) & !is.null(unlist(contacts))) %>% ungroup()

# includes rooms with multiple infections if they have additional contacts
prim %>% group_by(no) %>% filter(!is.null(unlist(contacts))) %>% ungroup()

# choices - exclude or attribute to a single infectious person
has_multiple_inf_and_contact <- prim %>% group_by(no) %>% filter(!is.null(unlist(contacts))&!is.null(unlist(multiple_inf))) %>% ungroup()

prim_final_select_col <- prim_final %>% select(!c(multiple_inf))
final <- prim_final_select_col %>% unnest(c("contacts", "neg_pos_contact", "has_prior_inf", "num_vacc_doses", "num_days_in_contact"))

write_csv(final, "final_sample062822_2_5infper.csv")

# test_res <- function(resident, contact=NULL) {
#   inf <- infections %>% filter(ResidentId == resident)
#   contacts <- unique(unlist(inf$residents))
#   contacts <- contacts[contacts!=resident]
#   print(contacts) 
#   print(inf)
#   
#   inf <- inf %>% filter(RoomType %in% c(1,2))
#   
#   test_c <- function(c){
#     inf <- inf %>% group_by(Day) %>% mutate(is_in = c %in% unlist(residents))
#     same_room <- (inf %>% filter(is_in))
#     first_share <- same_room$Day[1]
#     last_share <- (same_room %>% arrange(desc(Day)))$Day[1]
#     
#     previous_inf <- d %>% filter(ResidentId==c & Day < first_share)
#     sum_prev_inf <- previous_inf %>% group_by(num_pos) %>% summarise_all(first) %>% arrange(desc(Day))
#     
#     if (first_share-sum_prev_inf$Day[1]<90 & !(sum_prev_inf$num_pos[1] %>% is.na())) {
#       prev_inf_sub <- previous_inf %>% filter(num_pos==sum_prev_inf$num_pos[1])
#       if(!any(prev_inf_sub$Result=="Negative"&prev_inf_sub$pcr, na.rm=T)){
#         print(c(first_share, last_share))
#         print(sum_prev_inf)
#         print("prior infection")
#         return()
#       }
#     }
#     
#     all_related <- d %>% filter(first_share - Day <= 2 & Day <= last_share + 14 & ResidentId==c)
#     print(all_related)
#     
#     include <- all_related %>% filter(abs(Day-first_share)<=2)
#     if(all(include$Result %>% is.na())){
#       print("no prior neg result")
#       return()
#     } else if(any(include$Result=="Positive",na.rm=T)) {
#       print("concurrent inf")
#       return()
#     } else if(any(include$Result=="Negative",na.rm=T)) {
#       contact <- d %>% filter(Day >= first_share + 3 & Day <= last_share + 14 & ResidentId==c)
#       if(all(is.na(contact$Result))) {
#         print("contact with no test result")
#         return()
#       }
#       
#       print("valid contact")
#     } 
#   }
#   
#   if(!contact %>% is.null()) {
#     test_c(contact)
#   } else {
#     for (c in contacts) {
#       test_c(c)
#     }
#   }
# }
