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

write_csv(infections_adjusted_more_than_1_contact, "infectious_periods_primary_cases.csv")

infections_adjusted_more_than_1_contact %>% filter(days_infectious==1) %>% 
  group_by(RoomId, Day) %>% filter(n()>1) %>% group_by(ResidentId, num_pos)
infections_adjusted_more_than_1_contact %>% filter(days_infectious==1) %>% 
  group_by(RoomId, Day) %>% filter(n()==1) %>% group_by(ResidentId, num_pos)

multiple_infections <- infections_adjusted_more_than_1_contact %>% filter(days_infectious %in% 1:3) %>% 
  group_by(RoomId, Day) %>% filter(n()>1 & any(days_infectious==1))
residents_multiple <- multiple_infections$no %>% unique()
included <- filter(infections_adjusted_more_than_1_contact, !(no %in% residents_multiple))

filter(included, any(QuarantineIsolation==0,na.rm=T))
filter(included, all(RoomCensus <= 8, na.rm=T)& any(QuarantineIsolation==0,na.rm=T))




prep_inf_spread_data <- function(d_test) {
  d_test <- d_test %>% group_by(ResidentId, num_pos) %>% summarise(Day=first(Day), days_infectious=first(days_infectious),no=first(no))
  d_test <- d_test %>% mutate(Day=dplyr::if_else(days_infectious>1, Day-days_infectious, Day)) %>% ungroup ()
  d_test %>% mutate(num_pos=factor(num_pos, levels=1:4, labels=c("inf1", "inf2", "inf3", "inf4"))) %>% 
    select(!c(no,days_infectious)) %>% 
    spread(num_pos, Day, fill=NA, drop = F) 
}

filter_has_negative_test_within_week <- function(res, spread) {
  d_test <- filter(d, ResidentId %in% res)

  d_test <- d_test %>% left_join(spread, by="ResidentId")
  d_test <- d_test %>% mutate(within_week = ifelse((inf1-Day<=7&inf1-Day>0)|
                                                     (inf2-Day<=7&inf2-Day>0)|
                                                     (inf3-Day<=7&inf3-Day>0)|
                                                     (inf4-Day<=7&inf4-Day>0),T,NA),
                              num_pos_adj = ifelse(within_week, num_pos+1, NA),
                              num_pos_adj = ifelse(within_week&is.na(num_pos_adj), 1, num_pos_adj))
  
  d_test %>% group_by(ResidentId, num_pos_adj) %>% filter(within_week) %>% filter(any(Result=="Negative"&pcr))
}

prep_data_to_save <- function(included) {
  included %>% group_keys() %>% rename("num_pos"="num_pos_adj") %>% left_join(labels, by=c("ResidentId", "num_pos"))
}

# potential primary cases if including multiple infections
test1 <- prep_inf_spread_data(infections_adjusted_more_than_1_contact)
res <- test1$ResidentId %>% unique()
clean_test1 <- filter_has_negative_test_within_week(res, test1)
prep_data_to_save(clean_test1) %>% write_csv("potential-primary-cases/all_infections.csv")

infections_adjusted_more_than_1_contact %>% filter(all(RoomCensus<=8, na.rm=T))
test1 <- prep_inf_spread_data(infections_adjusted_more_than_1_contact %>% filter(all(RoomCensus<=8, na.rm=T)))
res <- test1$ResidentId %>% unique()
clean_test1 <- filter_has_negative_test_within_week(res, test1)
prep_data_to_save(clean_test1) %>% write_csv("potential-primary-cases/all_infections_less_than_8res.csv")


# # potential primary cases if not including multiple infections
# test2 <- prep_inf_spread_data(included)
# res <- test2$ResidentId %>% unique()
# clean_test2 <- filter_has_negative_test_within_week(res, test2)
# prep_data_to_save(clean_test2) %>% write_csv("potential-primary-cases/all_infections.csv")
# 
# infections_adjusted_more_than_1_contact %>% filter(all(RoomCensus<=8, na.rm=T))
# test2 <- prep_inf_spread_data(infections_adjusted_more_than_1_contact %>% filter(all(RoomCensus<=8, na.rm=T)))
# res <- test2$ResidentId %>% unique()
# clean_test2 <- filter_has_negative_test_within_week(res, test2)
# prep_data_to_save(clean_test2) %>% write_csv("potential-primary-cases/all_infections_less_than_8res.csv")


# 
# infections_adjusted_more_than_1_contact %>% filter(all(RoomCensus<=8)) %>% select(ResidentId, Day, Result, QuarantineIsolation, RoomCensus)
# infections_adjusted_more_than_1_contact %>% filter(any(RoomCensus>8)) %>% select(ResidentId, Day, Result, QuarantineIsolation, RoomCensus)
#  
# 
# unique <- infections_adjusted_more_than_1_contact %>% 
#   summarise(Day=first(Day), RoomId=first(RoomId), potential_first=Day-2, potential_last=Day+2) %>% ungroup() %>%
#   mutate(no=1:nrow(.)) %>% arrange(Day)
# unique$counted <- 0
# 
# mult_inf_room_day <- infections_adjusted_more_than_1_contact %>% group_by(RoomId, Day) %>% filter(n() > 1)
# sing_inf_room_day <- unique %>% group_by(RoomId, Day) %>% filter(n() == 1)
# 
