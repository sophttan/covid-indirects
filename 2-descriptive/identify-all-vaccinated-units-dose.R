# Sophia Tan 1/13/23, updated 5/16/23
# Identify units based on vaccination over time

rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)

# read in dataset
group_room_2_wide <- read_csv("wide_housing_2room_noincarcreq081423.csv") 

group_room_2_wide %>% head(20)

# make sure residents co-reside for continuous 30 days (no skipped days)
# make sure both residents have not had prior infection within the last 90 days
g <- group_room_2_wide %>% group_by(Institution, RoomId, ResidentId.1, ResidentId.2) %>% 
  mutate(cont = c(0, diff(Day)), more_1_day = cont==0|cont>1) 

g1 <- g %>% filter(more_1_day) %>% select(Day) %>% mutate(unit_label=1:n())

g <- g %>% left_join(g1) 
g <- g %>% select(!c(cont, more_1_day))
g <- g %>% fill(unit_label, .direction="down")

# distinct rooms if any resident receives first vaccination or there is a new infection
group_room_2_wide_distinct <- g %>%
  distinct(Institution,RoomId,ResidentId.1,ResidentId.2,unit_label,
           num_dose_adjusted.1,num_dose_adjusted.2,prior_inf_90,.keep_all=T) %>% ungroup() %>% 
  mutate(label=1:n())

group_room_2_wide_distinct_final <- group_room_2_wide_distinct %>% 
  rowwise() %>% 
  mutate(both_unvacc=(vacc.1==0&vacc.2==0),
         one_unvacc=(!both_unvacc&(vacc.1==0|vacc.2==0)),
         inf.1=as.numeric(inf.1),
         inf.2=as.numeric(inf.2),
         num_inf=inf.1+inf.2) 

labeled <- group_room_2_wide_distinct_final %>% ungroup() %>%
  mutate(label=1:nrow(.)) %>% 
  select(Institution, BuildingId, RoomId, Day, label)

group_room_2_wide <- group_room_2_wide %>% left_join(labeled) 
group_room_2_wide_summary <- group_room_2_wide %>% 
  fill(label, .direction="down") %>% 
  group_by(Institution, BuildingId, RoomId, label) %>%
  summarise(first=first(Day), last=last(Day)) %>%
  mutate(duration=difftime(last, first, units="days")+1)

group_room_2_final <- group_room_2_wide_summary %>% select(!label) %>% 
  left_join(group_room_2_wide_distinct_final, by=c("Institution", "BuildingId", "RoomId", "first"="Day")) %>%
  select(Institution, BuildingId, RoomId, RoomType, everything())


write_csv(group_room_2_final, "allunits_noincarcreq_vaccinationdose_analysis081423.csv")

