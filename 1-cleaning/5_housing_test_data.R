# Sophia Tan 1/11/23
# combine housing and testing data from 12/16/22 data pull

rm(list=ls())
gc()

setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(readr)
library(tidyverse)

nh <- read_csv("housing_omicron.csv")
inf_vacc <- read_csv("testing_vacc_clean.csv")

inf_vacc <- inf_vacc %>% mutate(has_test = ifelse(is.na(Result), F, T), 
                                unknown_test_other = has_test & !(antigen|pcr))

inf_vacc_housing <- inf_vacc %>% full_join(nh, c("ResidentId", "Day"="Night"))

rm(nh, inf_vacc) 
gc() # free up memory

inf_vacc_housing <- inf_vacc_housing %>% arrange(ResidentId, Day)
inf_vacc_housing <- inf_vacc_housing %>% group_by(ResidentId) %>% 
  fill(Date_offset, num_dose, .direction="down") %>% 
  fill(max_dose, full_vacc, booster_add_dose, .direction="downup")
inf_vacc_housing <- inf_vacc_housing %>%
  mutate(num_dose_adjusted = ifelse(Day < Date_offset & num_dose > 0, num_dose-1, num_dose))# %>% select(!Date_offset)
inf_vacc_housing <- inf_vacc_housing %>% replace_na(list(num_dose=0, num_dose_adjusted=0, max_dose=0, full_vacc=0, booster_add_dose=0))

# check <- (inf_vacc_housing%>%group_keys())$ResidentId[130000]
# inf_vacc_housing %>% filter(ResidentId==check) %>% view()

inf_vacc_housing <- inf_vacc_housing %>% fill(num_pos, .direction="down")

# inf_vacc_housing1 <- inf_vacc_housing %>% filter(!(Result %>% is.na() & RoomId %>% is.na() & Institution %>% is.na()))

inf_vacc_housing <- inf_vacc_housing %>% group_by(ResidentId, num_pos) %>%
  mutate(infectious = ifelse(!is.na(num_pos) & Day-first(Day)<=4, 1, 0))
inf_vacc_housing <- inf_vacc_housing %>% select(!c(ReceivedDate))
write_csv(inf_vacc_housing, "complete-data-081423.csv")

