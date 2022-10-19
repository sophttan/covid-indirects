# Sophia Tan 6/28/22
# Create incidence dataset

rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")
#setwd("D:/stan5/code_ST")

library(tidyverse)

calculate_inc <- function(institution, index_first_pos_test_date) {
  d_inst <- filter(d, Institution==institution & Day <= index_first_pos_test_date & index_first_pos_test_date - Day < 7)
  total_pop <- d_inst$ResidentId %>% unique() %>% length()
  infections_inst <- filter(infections_total, Institution==institution & Day <= index_first_pos_test_date & index_first_pos_test_date - Day < 7)
  total_inf <- infections_inst %>% nrow()
  total_inf/total_pop * 100000
}
vectorized_calculate_inc <- Vectorize(calculate_inc)

d <- read_csv("housing_inf_data072122.csv")
#d <- read_csv("housing_inf_data_infperiod7.csv")
#d <- read_csv("housing_inf_data_infperiod2_5.csv")
#d <- read_csv("housing_inf_data_infperiod2_7.csv")

infectious_period <- d %>% filter(infectious==1) %>% group_by(ResidentId, num_pos)
infections_total <- infectious_period %>% 
  select(ResidentId, num_pos, Day, Institution, Result) %>% filter(num_pos>=1) %>%
  summarise_all(first) %>%
  group_by(ResidentId, num_pos)

#infections <- read_csv("final_sample100722_nopcr.csv")
#infections <- read_csv("final_sample100722_nopcr_7days.csv")
#infections <- read_csv("final_sample100722_nopcr_2_5days.csv")
#infections <- read_csv("final_sample100722_nopcr_2_7days.csv")
#infections <- read_csv("final_sample100722_nopcr_pos2days.csv")
infections <- read_csv("final_sample100722_nopcr_nonegtest.csv")

inst_day <- infections %>% group_by(Institution, Day) %>% group_keys()
inst_day <- inst_day %>% mutate(incidence=vectorized_calculate_inc(Institution, Day))

#inst_day %>% write_csv("incidence_final.csv")
#inst_day %>% write_csv("incidence_final_7.csv")
#inst_day %>% write_csv("incidence_final_2_5infper.csv")
#inst_day %>% write_csv("incidence_final_2_7infper.csv")
#inst_day %>% write_csv("incidence_final_pos2days.csv")
inst_day %>% write_csv("incidence_final_nonegtest.csv")
