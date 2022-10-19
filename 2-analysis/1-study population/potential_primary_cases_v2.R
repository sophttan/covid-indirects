# Sophia Tan 3/31
# test inclusion/exclusion criteria

rm(list=ls())
gc()

setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")

library(readr)
library(tidyverse)

# replace with different datasets with different infectious period definitions
d <- read_csv("housing_inf_data_infperiod2_7.csv")
d <- d %>% group_by(ResidentId, num_pos)

# include only if test negative pcr in the 7 days prior to first positive test
# removed this criteria for sample size reasons
num_days <- 8
infections <- d %>% filter(infectious==1) 
has_infection <- d %>% group_by(ResidentId) %>% filter(any(Result == "Positive"))
first_day <- infections %>% summarise(first_day=first(Day))
# has_neg_pcr_prev <- has_infection %>% left_join(first_day, "ResidentId") %>% group_by(ResidentId, num_pos.y) %>% 
#   filter(first_day-Day<=num_days & Day-first_day<5) %>% mutate(prior_7 = ifelse(first_day-Day<=num_days & first_day-Day>0, T, F)) %>%
#   filter(any(Result=="Negative"&pcr&prior_7)) 
# has_neg_pcr_prev # 24135 total infections with negative pcr in 7 days prior to first positive test
# 
# infections_subset <- has_neg_pcr_prev %>% filter(infectious==1 & !prior_7) %>% ungroup() %>% select(!c(num_pos.y, first_day))
# infections_subset <- infections_subset %>% distinct() %>% rename("num_pos"="num_pos.x")
# infections_subset %>% group_by(ResidentId, num_pos) %>% filter(!num_pos %>% is.na() & first(Day) >= "2021-12-15")
# filter(infections_subset %>% group_by(ResidentId, num_pos), any(Result=="Negative"&pcr)) 

infections <- infections %>% filter(first(Day) >= "2021-12-15")
infections

# infections <- infections %>% filter(first(Day)+2 >= "2021-12-15")
summary_data <- read_csv("housing_duration.csv")
infections <- infections %>% left_join(summary_data) 
infections <- infections %>% filter(first < "2020-04-01")
infections

#infections_subset <- infections %>% group_by(ResidentId, num_pos) %>% filter(all(!(Result=="Negative"&pcr&Day<(first(Day)+5)), na.rm=T))

# when infectious period starts 2 days prior to first positive test
# exclude cases with negative test in 2 days prior to first positive test
# exclude cases with negative pcr test during infectious period after first positive test
infections <- infections %>% group_by(ResidentId, num_pos) %>% filter(all(!(Day<(first(Day)+2)&Result=="Negative"),na.rm=T))
infections_subset <- infections %>% group_by(ResidentId, num_pos) %>% filter(all(!(Result=="Negative"&pcr), na.rm=T))

infections_subset_adjusted <- infections_subset %>%
  mutate(infectious = ifelse(Result=="Negative", 0, NA)) %>%
  fill(infectious, .direction="down") %>% replace_na(list(infectious=1)) %>%
  filter(infectious==1)

# how many infections first tested positive on a day without any housing data? (2)
infections_subset_adjusted %>% 
  filter(first(Result)=="Positive" & is.na(first(RoomId))) %>% 
  select(ResidentId, Day, Result, RoomId)

infections_subset_adjusted <- infections_subset_adjusted %>% mutate(days_infectious = 1:n())
infections_subset_adjusted <- infections_subset_adjusted %>% filter(!is.na(RoomId))
infections_subset_adjusted <- infections_subset_adjusted %>% filter(!is.na(RoomType))

# Room types of potential index cases (removed anyone with no prior negative pcr, no housing/no room type data)
infections_subset_adjusted %>% summarise(RoomType=list(unique(RoomType))) %>% group_by(ResidentId, num_pos) %>% filter(length(unlist(RoomType))==1)
(infections_subset_adjusted %>% summarise(Roomnum=length(unique(RoomId))) %>% ungroup())$Roomnum %>% summary()
rooms <- infections_subset_adjusted %>% summarise(RoomType=paste(unique(RoomType),collapse =""))
rooms$RoomType %>% as.factor() %>% table() %>% barplot() 

# keep infections if they stayed in any solid door cell
infections_subset_adjusted_has_contacts_filter_roomtype <- infections_subset_adjusted %>% 
  filter(any(RoomType %in% c(1, 2)) | (any(RoomType==4 & !(Institution %in% c(9,11,12,14,18,32)))))

# assign infections unique labels
labels <- infections_subset_adjusted_has_contacts_filter_roomtype %>% group_keys %>% mutate(no=1:nrow(.))
labels
infections_subset_adjusted_has_contacts_filter_roomtype <- infections_subset_adjusted_has_contacts_filter_roomtype %>% left_join(labels)

write_csv(infections_subset_adjusted_has_contacts_filter_roomtype, "infectious_periods_primary_cases100722_nopcr2_7days.csv")

