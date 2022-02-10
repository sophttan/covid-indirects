# Sophia Tan 2/10/22
# Test sample sizes

rm(list=ls())

setwd("D:/code_ST")

library(readr)
library(tidyverse)

d <- read_csv("D:/code_ST/housing_inf_data.csv")

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

infections_remove_neg_pcr_adjusted %>% filter(all(is.na(RoomId)))
infections_remove_neg_pcr_adjusted <- infections_remove_neg_pcr_adjusted %>% filter(any(!is.na(RoomId)))
infections_remove_neg_pcr_adjusted %>% group_keys()

# how many infections begin in quarantine/isolation
exclude_as_primary <- infections_remove_neg_pcr_adjusted %>% filter(all(QuarantineIsolation>0)|all(RoomCensus==1))
exclude_as_primary <- exclude_as_primary %>% group_by(ResidentId, num_pos) %>% summarise()

unique_inf <- d %>% filter(!is.na(num_pos)) %>% summarise(Day=first(Day)) %>% group_by(ResidentId, num_pos)

unique_inf

test_inf <- filter(d, ResidentId==1611661374 & infectious==1)
test_inf_not_isolated <- test_inf %>% filter(QuarantineIsolation==0)
test_inf_not_isolated$RoomId
filter(d, Day %in% test_inf_not_isolated$Day & RoomId==997491705)
contact <- filter(d, ResidentId==1612985646 & Day >= "2021-01-8" & Day <= "2021-01-19")
all(contact$Result=="Negative", na.rm=T)
