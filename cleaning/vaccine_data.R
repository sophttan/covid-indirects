# Sophia Tan 1/28/22
# Cleaning CDCR COVID-19 vaccination data through 1/15/22 

rm(list=ls())

setwd("D:/CDCR Data/14 January 15 2022")

library(tidyverse)


###### COVID Infection Data ######
vacc <- read.csv("Immunization_20220115.csv", sep=";")
head(vacc)

covid_vacc <- vacc %>% filter(grepl("SARS", Vaccine))
unique(covid_vacc$Vaccine)

# rename so all JJ vaccines named the same
covid_vacc <- covid_vacc %>% mutate(Date = as.Date(Date), 
                                    Vaccine = ifelse(grepl("Ad26", Vaccine), "SARS-CoV-2 (COVID-19) Ad26 , recomb", Vaccine))
# resident mixed and matched vaccine types (largely for boosters)
summary_resident_data <- covid_vacc %>% filter(Result=="Received") %>% group_by(ResidentId) %>% summarise(num_doses=n(), num_types=length(unique(Vaccine))) %>%
  filter(num_types != 1)

covid_vacc_cleaned <- covid_vacc %>% filter(Result == "Received") %>% arrange(Date) %>% 
  group_by(ResidentId) %>% mutate(num_dose = 1:n(), max_dose = n()) %>% arrange(ResidentId)

covid_vacc_cleaned <- covid_vacc_cleaned %>% 
  mutate(full_vacc = ifelse(grepl("mRNA", Vaccine[1]), 2, 1),
         booster_add_dose = ifelse(max(num_dose) > 2 | (grepl("Ad26", Vaccine[1]) & max(num_dose > 1)), 1, 0),
         booster_add_dose_mixed = ifelse(booster_add_dose==1 & sum(Vaccine[1]==Vaccine) != n(), 1, 0),
         incomplete = ifelse(max(num_dose)==1 & grepl("mRNA", Vaccine[1]), 1, 0))

# 99128 residents received at least 1 dose of a vaccine
num_res_vacc <- covid_vacc_cleaned$ResidentId %>% unique() %>% length()

# 3332 residents (3% of vaccinated residents) received only 1 dose of an mRNA vaccine
covid_vacc_cleaned %>% filter(incomplete==1)

# 38516 residents (38% of vaccinated residents) have completed series but no booster (not accounting for time eligibility)
covid_vacc_cleaned %>% filter(booster_add_dose == 0 & incomplete ==0) 

##### Examining booster doses #####
# 57280 residents have received a booster
boosted <- covid_vacc_cleaned %>% filter(booster_add_dose==1)

(boosted %>% filter(num_dose==1))$Vaccine %>% table()
# most residents that got a different booster than primary series received J&J as their primary series
(boosted %>% filter(booster_add_dose_mixed==1) %>% filter(num_dose==1))$Vaccine %>% table()


write_csv(covid_vacc_cleaned, "D:/code_ST/cleaned_vaccination_data.csv")
