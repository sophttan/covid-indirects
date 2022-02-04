# Sophia Tan 1/28/22
# Clean nightly housing data 1/15/22 

rm(list=ls())

setwd("D:/code_ST")

library(readr)
library(tidyverse)

nh <- read_csv("D:/code_ST/housing_filtered.csv")
inf_vacc <- read_csv("D:/code_ST/has_testing_data_aggregated_infections.csv")

no_housing <- read_csv("D:/code_ST/has_testing_no_housing.csv")$ResidentId %>% unique()
inf_vacc <- filter(inf_vacc, !(ResidentId %in% no_housing))

inf_vacc_housing <- inf_vacc %>% full_join(nh, c("ResidentId", "Day"="Night"))


room_cases <- inf_vacc_housing %>% group_by(RoomId, Day) %>% 
  summarise(case = sum(num_pos>1, na.rm=T), RoomCapacity=first(RoomCapacity))
room_had_case <- filter(room_cases, any(case>0))
room_had_case %>% filter(RoomId==-2050142432, case>0)

inf_vacc_housing %>% filter(RoomId==-2050142432, Day>"2021-10-03" & Day<"2021-10-10") %>% view()

# how may have symptom data?
inf_vacc_housing %>% group_by(ResidentId) %>% filter(any(!SxOnset%>%is.na())) 

view(room_had_case %>% filter(case>0)) 
