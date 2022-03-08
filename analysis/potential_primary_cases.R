# Sophia Tan 2/10/22
# Test sample sizes

rm(list=ls())

setwd("D:/code_ST")

library(readr)
library(tidyverse)

d <- read_csv("D:/code_ST/housing_inf_data.csv")

d <- d %>% group_by(ResidentId, num_pos)

# some people test negative within infectious period (within first 5 days of infection)
# exclude primary cases that have negative pcr test within 5 days of first positive test
# modify infectious period of primary cases that have negative antigen test
d %>% filter(infectious==1) %>% filter(any(Result=="Negative"))%>%view()
d %>% filter(infectious==1) %>% filter(all(Result!="Negative", na.rm=T))%>%view()

infections <- d %>% filter(infectious==1) 
neg_pcr <- infections %>% filter(any(Result=="Negative"&pcr)) %>% group_keys()
infections_remove_neg_pcr <- infections %>% filter(all(!(Result=="Negative"&pcr), na.rm=T))

infections_remove_neg_pcr_adjusted <- infections_remove_neg_pcr %>% 
  mutate(infectious = ifelse(Result=="Negative", 0, infectious)) %>%
  fill(infectious, .direction="down") %>% filter(infectious==1)

# remove infections that have no housing data over entire infectious period
infections_remove_neg_pcr_adjusted %>% filter(all(is.na(RoomId)))
infections_remove_neg_pcr_adjusted <- infections_remove_neg_pcr_adjusted %>% filter(any(!is.na(RoomId)))
infections_remove_neg_pcr_adjusted %>% group_keys()

# how many infections first tested positive on a day without any housing data? (15)
infections_remove_neg_pcr_adjusted %>% 
  filter(first(Result)=="Positive" & is.na(first(RoomId))) %>% 
  select(ResidentId, Day, num_pos, Result, RoomId, RoomCensus, QuarantineIsolation)

# remove if single resident in room for entire infectious period 
infections_remove_neg_pcr_adjusted %>% filter(all(RoomCensus==1, na.rm=T)) %>% select(ResidentId, Day, num_pos, Result, RoomCensus) 
infections_adjusted_more_than_1_contact <- infections_remove_neg_pcr_adjusted %>% filter(any(RoomCensus > 1))

# could also exclude if resident is alweys in quarantine/isolation for entire infectious period
infections_adjusted_more_than_1_contact %>% filter(any(QuarantineIsolation==0))

infections_adjusted_more_than_1_contact <- infections_adjusted_more_than_1_contact %>% mutate(days_infectious = 1:n())
infections_adjusted_more_than_1_contact <- infections_adjusted_more_than_1_contact %>% filter(!is.na(RoomId))

# assign infections unique labels
labels <- infections_adjusted_more_than_1_contact %>% group_keys %>% mutate(no=1:nrow(.))
labels
infections_adjusted_more_than_1_contact <- infections_adjusted_more_than_1_contact %>% left_join(labels)

multiple_infections <- infections_adjusted_more_than_1_contact %>% filter(days_infectious %in% 1:3) %>% 
  group_by(RoomId, Day) %>% filter(n()>1 & any(days_infectious==1))
residents_multiple <- multiple_infections$no %>% unique()
included <- filter(infections_adjusted_more_than_1_contact, !(no %in% residents_multiple))

days_quarantine_isolation <- infections_adjusted_more_than_1_contact %>% summarise(inf = max(days_infectious), q_i = sum(QuarantineIsolation!=0), no=first(no))
all_quarantine_no <- (days_quarantine_isolation %>% filter(q_i==inf))$no
all_quarantine <- infections_adjusted_more_than_1_contact %>% filter(no %in% all_quarantine_no) %>% select(ResidentId, Day, no, RoomCensus, RoomId, QuarantineIsolation)
filter(included, sum(QuarantineIsolation > 0) < max(days_quarantine_isolation))

unique <- unique <- infections_adjusted_more_than_1_contact %>% 
  summarise(Day=first(Day), RoomId=first(RoomId), no=first(no)) %>% ungroup() %>%
  mutate(no=1:nrow(.)) %>% arrange(Day)

infections_remove_neg_pcr_adjusted %>% filter(any(QuarantineIsolation > 0)) %>% select(ResidentId, Day, Result, QuarantineIsolation, RoomCensus) 

included_primary <- included %>% filter(sum(QuarantineIsolation > 0) < max(days_infectious) & all(RoomCensus <= 8)) %>%
  summarise(Day=first(Day), num_inf = max(days_infectious)) 

num_included <- 0
res <- included_primary$ResidentId %>% unique()
d_primary <- filter(d, ResidentId %in% res)
for (i in 1:nrow(included_primary)) {
  print(i)
  test <- filter(d_primary, ResidentId==included_primary$ResidentId[i] & 
                   Day >= included_primary$Day[i]-7 & Day <= included_primary$Day[i])
  if(any(test$Result=="Negative", na.rm=T)) {num_included <- num_included + 1}
}
num_included

infections_adjusted_more_than_1_contact %>% filter(all(RoomCensus<=8)) %>% select(ResidentId, Day, Result, QuarantineIsolation, RoomCensus)
infections_adjusted_more_than_1_contact %>% filter(any(RoomCensus>8)) %>% select(ResidentId, Day, Result, QuarantineIsolation, RoomCensus)
 

unique <- infections_adjusted_more_than_1_contact %>% 
  summarise(Day=first(Day), RoomId=first(RoomId), potential_first=Day-2, potential_last=Day+2) %>% ungroup() %>%
  mutate(no=1:nrow(.)) %>% arrange(Day)
unique$counted <- 0

mult_inf_room_day <- infections_adjusted_more_than_1_contact %>% group_by(RoomId, Day) %>% filter(n() > 1)
sing_inf_room_day <- unique %>% group_by(RoomId, Day) %>% filter(n() == 1)

# infections_adjusted_more_than_1_contact <- infections_adjusted_more_than_1_contact %>% 
#   left_join(unique %>% select(ResidentId, num_pos, no), c("ResidentId", "num_pos"))
# infections_unique <- infections_adjusted_more_than_1_contact %>% summarise_all(first)
# 
# for (i in 1:100){#nrow(unique)) {
#   print(i)
#   if(unique$counted[i]==1) {next}
#   
#   case_no <- unique$no[i]
#   
#   find_rooms <- filter(infections_adjusted_more_than_1_contact, no==16286)
#   
#   for (day in seq(unique$Day[i],unique$potential_last[i],1)) {
#     room <- filter(find_rooms, Day==day)$RoomId[1]
#     contacts <- filter(infections_unique, no!=16286 & Day==day & RoomId==room) 
#     
#     if(nrow(contacts)==0) {next}
#     
#     unique$counted[unique$no %in% contacts$no] <- 1
#     unique$counted[i] <- 1
#   }
# }
