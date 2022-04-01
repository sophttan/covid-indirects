# Sophia Tan 2/10/22
# Test inclusion/exclusion criteria

rm(list=ls())

setwd("D:/stan5/code_ST")

library(readr)
library(tidyverse)

d <- read_csv("housing_inf_data.csv")

d <- d %>% group_by(ResidentId, num_pos)

# some people test negative within infectious period (within first 5 days of infection)
# exclude primary cases that have negative pcr test within 5 days of first positive test
# modify infectious period of primary cases that have negative antigen test
d %>% filter(infectious==1) %>% filter(any(Result=="Negative"))%>%view()
d %>% filter(infectious==1) %>% filter(all(Result!="Negative", na.rm=T))%>%view()

infections <- d %>% filter(infectious==1) 
neg_pcr <- infections %>% filter(any(Result=="Negative"&pcr)) %>% summarise(first_pos_test = first(Day))# %>% group_keys()

# are there residents who test negative with pcr within 7 days prior to positive test and then test negative? 
# 265-275 residents 
has_infection <- d %>% group_by(ResidentId) %>% filter(any(Result == "Positive"))
has_infection <- has_infection %>% left_join(neg_pcr, "ResidentId")
has_neg_pcr_prev <- has_infection %>% group_by(ResidentId, num_pos.y) %>% mutate(prior_7 = ifelse(first_pos_test-Day<=7 & first_pos_test>=Day, 1, 0)) %>% 
  filter(first_pos_test-Day<=7 & Day-first_pos_test<5) %>% filter(any(Result=="Negative"&prior_7&pcr)) # test with and without pcr test requirement
has_neg_pcr_prev_filtered <-has_neg_pcr_prev %>% filter((has_test&pcr)|Result=="Positive") %>%  mutate(diff=Day-first_pos_test)
has_neg_pcr_prev_filtered_pos <- has_neg_pcr_prev_filtered %>% filter(diff>0) %>% filter(diff==min(diff))
has_neg_pcr_prev_filtered_neg <- has_neg_pcr_prev_filtered %>% filter(diff<0) %>% filter(diff==max(diff))
has_neg_pcr_prev_filtered <- rbind(has_neg_pcr_prev_filtered_neg, has_neg_pcr_prev_filtered %>% filter(diff==0), has_neg_pcr_prev_filtered_pos) %>% group_by(ResidentId, num_pos.y)
has_neg_pcr_prev_filtered <- has_neg_pcr_prev_filtered %>% arrange(ResidentId, Day)

rm(has_neg_pcr_prev_filtered_neg, has_neg_pcr_prev_filtered_pos) 
gc()

has_neg_pcr_prev_filtered_summary <- has_neg_pcr_prev_filtered %>% summarise(days_before_pos = abs(min(diff)), days_after_pos = abs(max(diff)), time_between_neg = abs(min(diff))+max(diff))
has_neg_pcr_prev_filtered_summary$time_between_neg %>% as.numeric() %>% hist()#%>% summary()
(has_neg_pcr_prev_filtered %>% filter(diff==0)) %>% ggplot(aes(Day)) + geom_histogram(bins = 20)
infections %>% filter(Day==first(Day)) %>% ggplot(aes(Day)) + geom_histogram(bins=20)

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


# Room types of 46000 index cases
infections_adjusted_more_than_1_contact %>% summarise(RoomType=list(unique(RoomType))) %>% group_by(ResidentId, num_pos) %>% filter(length(unlist(RoomType))==1)
(infections_adjusted_more_than_1_contact %>% summarise(Roomnum=length(unique(RoomId))) %>% ungroup())$Roomnum %>% summary()
rooms <- infections_adjusted_more_than_1_contact %>% summarise(RoomType=paste(unique(RoomType),collapse =""))
rooms$RoomType %>% as.factor() %>% table() %>% barplot()
rooms$RoomType %>% as.factor() %>% table() 

approximate_first_day <- infections_adjusted_more_than_1_contact %>% summarise(first_day=first(Day))
three_week_window <-  d %>% left_join(approximate_first_day, "ResidentId") %>% filter(first_day-Day <= 14 & Day-first_day <=7)
(three_week_window %>% group_by(ResidentId, num_pos.y) %>% summarise(Roomnum=length(unique(RoomId))) %>% ungroup())$Roomnum %>% summary()

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
