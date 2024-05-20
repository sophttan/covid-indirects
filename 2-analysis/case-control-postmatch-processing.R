# Sophia Tan 3/4/24
# Post match processing

rm(list=ls())
gc() 

library(tidyverse)
library(readr)
library(lubridate)

# change file path for different study criteria/matching specifications
matched <- read_csv("D:/CCHCS_premium/st/indirects/matched_building_3_7days-12matching-051724.csv") 

inf <- read_csv("D:/CCHCS_premium/st/cleaned-data/infection_data051324.csv") %>% filter(Day <= "2022-12-15") %>%
  select(ResidentId, Day) %>% rename(last.inf.roommate=Day)
vaccine <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/complete_vaccine_data121523.csv") %>% 
  filter(Date <= "2022-12-15") %>% select(ResidentId, Date_offset, num_dose, full_vacc) %>%
  rename(last.vacc.roommate=Date_offset,
         dose.roommate=num_dose,
         full_vacc.roommate=full_vacc)


# add roommate's most recent infection to determine prior infection status
# prior infection must be more than 14 days prior to test collection
matched_inf_roommate <- matched %>% left_join(inf, by=c("Roommate"="ResidentId")) %>%
  group_by(id) %>% 
  filter(last.inf.roommate%>%is.na()|all(test.Day-last.inf.roommate<14)|test.Day-last.inf.roommate>=14) %>%
  mutate(last.inf.roommate=if_else(is.na(last.inf.roommate)|test.Day-last.inf.roommate<14, NA, last.inf.roommate)) %>%
  summarise_all(last)

matched_inf_roommate <- matched_inf_roommate %>% mutate(has.prior.inf.roommate=if_else(last.inf.roommate%>%is.na(), 0, 1))


# add roommate's most recent vaccine to determine vaccine status
matched_infvacc_roommate <- matched_inf_roommate %>% left_join(vaccine, by=c("Roommate"="ResidentId")) %>%
  group_by(id) %>% 
  filter(last.vacc.roommate%>%is.na()|all(last.vacc.roommate>=test.Day)|last.vacc.roommate<test.Day) %>%
  mutate(last.vacc.roommate=if_else(last.vacc.roommate>=test.Day, NA, last.vacc.roommate)) %>% 
  summarise_all(last) %>%
  mutate(has.vacc.roommate.binary=if_else(last.vacc.roommate%>%is.na(), 0, 1)) %>%
  mutate(dose.roommate.adjusted = case_when(last.vacc.roommate%>%is.na()~0,
                                            dose.roommate<full_vacc.roommate~1,
                                            dose.roommate==full_vacc.roommate~2,
                                            dose.roommate-full_vacc.roommate==1~3,
                                            dose.roommate-full_vacc.roommate>1~4))


# find and categorize time since last vaccine, time since last infection and, time since last vaccine or infection
# time since last vaccine and or infection reflects time since onset of protection (14 days after vaccine or infection)
matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(time_since_vacc.roommate = (test.Day-last.vacc.roommate) %>% as.numeric()) %>%
  mutate(time_since_vacc_cut=cut(time_since_vacc, breaks=c(0, 90, 182, 365, Inf), right = F),
         time_since_vacc_cut.roommate=cut(time_since_vacc.roommate, breaks=c(0, 90, 182, 365, Inf), right = F)) 

levels(matched_infvacc_roommate$time_since_vacc_cut)<-c(levels(matched_infvacc_roommate$time_since_vacc_cut), "None") 
matched_infvacc_roommate$time_since_vacc_cut[is.na(matched_infvacc_roommate$time_since_vacc_cut)] <- "None"

levels(matched_infvacc_roommate$time_since_vacc_cut.roommate)<-c(levels(matched_infvacc_roommate$time_since_vacc_cut.roommate), "None") 
matched_infvacc_roommate$time_since_vacc_cut.roommate[is.na(matched_infvacc_roommate$time_since_vacc_cut.roommate)] <- "None"

matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(time_since_inf.roommate = (test.Day-(last.inf.roommate-14)) %>% as.numeric()) %>%
  mutate(time_since_inf_cut.roommate=cut(time_since_inf.roommate, breaks=c(0, 90, 182, 365, Inf), right = F)) 

levels(matched_infvacc_roommate$time_since_inf_cut.roommate)<-c(levels(matched_infvacc_roommate$time_since_inf_cut.roommate), "None") 
matched_infvacc_roommate$time_since_inf_cut.roommate[is.na(matched_infvacc_roommate$time_since_inf_cut.roommate)] <- "None"

matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(latest=pmax(last.inf.roommate-14, last.vacc.roommate, na.rm=T)) %>%
  mutate(time_since_infvacc.roommate = (test.Day-latest)%>%as.numeric()) %>%
  mutate(time_since_infvacc_cut.roommate=cut(time_since_infvacc.roommate, breaks=c(0, 90, 182, 365, Inf), right = F)) 
levels(matched_infvacc_roommate$time_since_infvacc_cut.roommate)<-c(levels(matched_infvacc_roommate$time_since_infvacc_cut.roommate), "None") 
matched_infvacc_roommate$time_since_infvacc_cut.roommate[is.na(matched_infvacc_roommate$time_since_infvacc_cut.roommate)] <- "None"


write_csv(matched_infvacc_roommate, "D:/CCHCS_premium/st/indirects/case_control_postmatchprocessing.csv")
