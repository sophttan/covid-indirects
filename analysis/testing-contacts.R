# Sophia Tan 3/8/22
# Test for loop code

setwd("D:/stan5/code_ST")

library(tidyverse)
library(readr)
# if (!require("devtools")) install.packages("devtools")
# devtools::install_github("mkuhn/dict")

d <- read_csv("housing_inf_data.csv")
d <- d %>% select(ResidentId, Day, Result, num_dose, max_dose, full_vacc, booster_add_dose, QuarantineIsolation, RoomCensus, RoomId)

prim <- read_csv("potential-primary-cases/all_infections_less_than_8res.csv") #24/100
prim <- read_csv("potential-primary-cases/all_infections.csv")
infections <- read_csv("infectious_periods_primary_cases.csv")

group_room <- d %>% group_by(RoomId, Day)
group_room <- group_room %>% summarise(residents=list(unique(ResidentId)))

prim <- prim %>% mutate(quarantine = F, contacts=list(NULL), pos_contacts=list(NULL), neg_contacts=list(NULL), 
                        multiple_inf = list(NULL), num_possible_contacts = 0, num_no_prior_neg_test = 0, num_no_testing = 0)
set.seed(42)
prim_sub <- sample(1:nrow(prim), 100)

for (p in prim_sub) {
  
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
  
  if (length(contacts)==0) {next}
  
  for (c in contacts) {
    inf <- inf %>% group_by(Day) %>% mutate(is_in = c %in% unlist(residents))
    same_room <- (inf %>% filter(is_in))
    first <- same_room$Day[1]
    last <- (same_room %>% arrange(desc(Day)))$Day[1]
    
    all_related <- d %>% filter(first - Day <= 2 & Day <= last + 14 & ResidentId==c)
    
    include <- all_related %>% filter(abs(Day-first)<=2)
    if(all(include$Result %>% is.na())){
      prim$num_no_prior_neg_test[p]<-prim$num_no_prior_neg_test[p]+1
      next
    } else if(any(include$Result=="Positive",na.rm=T)) {
      prim$multiple_inf[[p]] <- c(prim$multiple_inf[[p]], c)
      next
    } else if(any(include$Result=="Negative",na.rm=T)) {
      contact <- d %>% filter(Day >= first + 3 & Day <= last + 14 & ResidentId==c)
      
      if(all(is.na(contact$Result))) {
        prim$num_no_testing[p]<-prim$num_no_testing[p]+1
        next
      }
      
      prim$contacts[[p]] <- c(prim$contacts[[p]], c)
      
      if(any(contact$Result=="Positive",na.rm=T)) {
        prim$pos_contacts[[p]] <- c(prim$pos_contacts[[p]], c)
      } else{
        prim$neg_contacts[[p]] <- c(prim$neg_contacts[[p]], c)
      }  
    }
  }
  
  new_primary_case <- prim[p,]
  print(paste0("Total of ", prim[p,]$num_possible_contacts, " possible contacts"))
  print(paste0("There were a total of ", prim[p,]$multiple_inf %>% unlist() %>% length(), " other concurrent infections"))
  print(paste0("There were a total of ", prim[p,]$num_no_prior_neg_test, " contacts excluded because they had no testing data within +/- 2 days of primary infection"))
  print(paste0("There were a total of ", prim[p,]$num_no_testing, " contacts excluded because they had no testing data between 3-14 days after first and last exposure"))
  gc()
} 

prim_subset <- prim[prim_sub,]
prim_final <- prim_subset %>% group_by(no) %>% filter(is.null(unlist(multiple_inf)) & !is.null(unlist(contacts))) %>% ungroup()
sum_vacc <- infections%>%group_by(ResidentId, num_pos) %>% summarise_all(first) %>% select(ResidentId, num_pos, Day, num_dose, max_dose, full_vacc)
prim_final <- left_join(prim, sum_vacc, by=c("ResidentId", "num_pos"))

