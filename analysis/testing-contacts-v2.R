# Sophia Tan 4/1/22
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
infections <- infections %>% group_by(no) %>% filter(first(Day) >= "2021-12-15")

group_room <- d %>% group_by(Institution, RoomId, Day)
group_room <- group_room %>% summarise(residents=list(unique(ResidentId)))
#group_room %>% unnest("residents") %>% write_csv("residents_by_room.csv")
gc()

prim <- infections%>%group_by(ResidentId, num_pos.y) %>% summarise_all(first) %>% select(ResidentId, num_pos.y, no, Day, num_dose, max_dose, full_vacc)
prim <- prim %>% mutate(contacts=list(NULL), neg_pos_contact=list(NULL), multiple_inf = list(NULL), 
                        num_possible_contacts = 0, num_no_prior_neg_test = 0, num_no_testing = 0, num_prior_inf = 0,
                        has_prior_inf=list(NULL), num_vacc_doses=list(NULL))

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
  inf <- inf %>% filter(RoomType %in% c(1,2)|(RoomType==4&!(Institution %in% c(4,5,9,10,11,12,14,15,17,18,30,32))))
  
  # mark if resident was in quarantine/isolation the entire infectious period
  #if(all(inf$QuarantineIsolation==2)) {prim$quarantine[p] <- T}
  
  contacts <- unique(unlist(inf$residents))
  contacts <- contacts[contacts!=resident]
  prim$num_possible_contacts[p]<-length(contacts)
  
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
      status_first_contact <- filter(all_related, Day==first_share)
      prim$has_prior_inf[[p]] <- c(prim$has_prior_inf[[p]], ifelse(status_first_contact$num_pos %>% is.na(), 0, 1))
      prim$num_vacc_doses[[p]] <- c(prim$num_vacc_doses[[p]], status_first_contact$num_dose_adjusted)
      if(print_res) {
        print("valid contact")
      }
      
      #contact_details[num_contact,] <- c(c, resident, first_share, contact$Day[nrow(contact)], nrow(same_room), round(mean(contact$num_dose)))
      #num_contact <- num_contact+1
      
      if(any(contact$Result=="Positive",na.rm=T)) {
        prim$neg_pos_contact[[p]] <- c(prim$neg_pos_contact[[p]], 0)
      } else{
        prim$neg_pos_contact[[p]] <- c(prim$neg_pos_contact[[p]], 1)
      }  
    }
  }
  # new_primary_case <- prim[p,]
  # print(paste0("Total of ", new_primary_case$num_possible_contacts, " possible contacts"))
  # print(paste0("There were a total of ", new_primary_case$num_prior_inf, " contacts excluded because they had prior (unresolved) infection within 90 days of first exposure"))
  # print(paste0("There were a total of ", new_primary_case$multiple_inf %>% unlist() %>% length(), " other concurrent infections"))
  # print(paste0("There were a total of ", new_primary_case$num_no_prior_neg_test, " contacts excluded because they had no testing data within +/- 2 days of primary infection"))
  # print(paste0("There were a total of ", new_primary_case$num_no_testing, " contacts excluded because they had no testing data between 3-14 days after first and last exposure"))
} 

prim_subset <- prim[prim_sub,]
prim <- prim %>% mutate(week = round(as.numeric(difftime(Day,as.Date("2020-03-01"), units="weeks"))))
prim_final <- prim %>% group_by(no) %>% filter(is.null(unlist(multiple_inf)) & !is.null(unlist(contacts))) %>% ungroup()

# includes rooms with multiple infections if they have additional contacts
prim %>% group_by(no) %>% filter(!is.null(unlist(contacts))) %>% ungroup()

# choices - exclude or attribute to a single infectious person
has_multiple_inf_and_contact <- prim %>% group_by(no) %>% filter(!is.null(unlist(contacts))&!is.null(unlist(multiple_inf))) %>% ungroup()


prim_final_select_col <- prim_final %>% select(!c(multiple_inf, num_possible_contacts, num_no_prior_neg_test, num_no_testing, num_prior_inf))
final <- prim_final_select_col %>% unnest(c("contacts", "neg_pos_contact", "has_prior_inf", "num_vacc_doses"))

write_csv(final, "D:/stan5/code_ST/final samples/march_final_sample_8day_somecells050622.csv")
summary(lm(contacts ~ Institution, data=total%>%mutate(Institution = as.factor(Institution))))

prim_final %>% group_by(num_pos.y, num_dose, full_vacc) %>% summarise(count=n())

(prim_final %>% group_by(no) %>% mutate(contacts = unlist(contacts) %>% length()))$contacts %>% summary()

# explore selection 
grid <- expand.grid(week = 0:max(prim$week), vacc=as.factor(c("unvacc", "any vacc")))
#prim %>% group_by(week) %>% summarise(count=n()) %>% full_join(grid) %>% replace_na(list(count=0)) %>% ggplot(aes(x=week,y=count)) + geom_line()
prim_final %>% mutate(vacc=factor(ifelse(num_dose>=1, "any vacc", "unvacc"))) %>% group_by(week, vacc) %>% 
  summarise(count=n()) %>% right_join(grid) %>% replace_na(list(count=0)) %>% 
  ggplot(aes(x=week,y=count,color=vacc)) + geom_line() + scale_y_continuous(name="number of included index cases") + 
  scale_x_continuous(name="weeks since 3/1/2020")


total_prim <- prim %>% group_by(week) %>% summarise(count=n()) 
total_prim <- total_prim %>% left_join(prim_final %>% group_by(week) %>% summarise(included=n()), "week") 
total_prim <- total_prim %>% replace_na(list(included=0))
total_prim %>% mutate(excluded=count-included, excluded_prop=excluded/count*100)

prim_final$contacts %>% unlist() %>% length()
prim_final$contacts %>% unlist() %>% unique() %>% length()

prim_final %>% select(!multiple_inf) %>% unnest(cols = c("contacts", "pos_contacts", "neg_contacts"))
write_csv(prim_final %>% select(!c(pos_contacts, neg_contacts, multiple_inf)) %>% unnest(contacts), "D:/stan5/code_ST/final samples/march_final_sample_8day_somecells.csv")
write_csv(has_multiple_inf_and_contact %>% unnest(c(contacts, pos_contacts, neg_contacts, multiple_inf)), "D:/stan5/code_ST/final samples/march_final_sample_multinf_8day_somecells.csv")

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
