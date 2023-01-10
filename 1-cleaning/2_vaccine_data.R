# Sophia Tan 1/9/23
# Cleaning CDCR COVID-19 vaccination data through 12/16/22 

rm(list=ls())
gc()

setwd("D:/CCHCS_premium/CDCR Data/Dec 16 2022 Data")

library(tidyverse)
library(lubridate)

###### COVID Infection Data ######
vacc <-  read_delim("Immunization_20221216.csv", delim=";")
head()

unique(vacc$Vaccine)
covid_vacc <- vacc %>% filter(grepl("SARS", Vaccine))
unique(covid_vacc$Vaccine)

# rename so all JJ vaccines named the same
covid_vacc <- covid_vacc %>% mutate(Date = as.Date(Date), 
                                    Vaccine = ifelse(grepl("Ad26", Vaccine), "SARS-CoV-2 (COVID-19) Ad26 , recomb", Vaccine),
                                    Vaccine = ifelse(grepl("tozinameran", Vaccine), "SARS-CoV-2 (COVID-19) mRNA BNT-162b2 vax", Vaccine))

# there are 31 residents with multiple records of vaccination on a single day (i.e. 1 dose of J&J and 1 dose of Moderna vaccine on 6/5/21)
# collapse records but unsure which vaccine type is correct
covid_vacc_cleaned <- covid_vacc %>% filter(Result=="Received") 

# also some who have incorrect years in the reporting
early <- covid_vacc_cleaned %>% group_by(ResidentId) %>% arrange(ResidentId, Date) %>% filter(any(Date<"2020-12-01")) # all likely 2021 vaccinations - question mark is 1611265470

covid_vacc_cleaned <- covid_vacc_cleaned %>% mutate(Date=if_else(Date<"2020-12-01", ymd(format(Date, "2021-%m-%d")), Date)) %>% select(!Month)
covid_vacc_cleaned <- covid_vacc_cleaned %>% distinct()
covid_vacc_cleaned <- covid_vacc_cleaned %>% group_by(ResidentId, Date) %>% arrange(Vaccine) %>% 
  summarise(Vaccine=ifelse(all(Vaccine==first(Vaccine)), first(Vaccine), paste(Vaccine, collapse = "|")))
covid_vacc_cleaned <- covid_vacc_cleaned %>% arrange(Date) %>% 
  group_by(ResidentId) %>% mutate(num_dose = 1:n(), max_dose = n()) %>% arrange(ResidentId)

# if someone was marked to have received any mrna vaccine as their primary series (even if both j&j and mrna reported), mark as mrna vacc
covid_vacc_cleaned <- covid_vacc_cleaned %>% 
  mutate(full_vacc = ifelse(grepl("mRNA", Vaccine[1]), 2, 1),
         booster_add_dose = ifelse(max(num_dose) > 2 | (grepl("Ad26", Vaccine[1]) & max(num_dose > 1)), 1, 0),
         incomplete = ifelse(max(num_dose)==1 & grepl("mRNA", Vaccine[1]), 1, 0))

# 116532 residents received at least 1 dose of a vaccine
num_res_vacc <- covid_vacc_cleaned$ResidentId %>% unique() %>% length()

# 2947 residents (3% of vaccinated residents) received only 1 dose of an mRNA vaccine 
covid_vacc_cleaned %>% filter(incomplete==1)

# 37404 residents (38% of vaccinated residents) have completed series but no booster (not accounting for time eligibility)
covid_vacc_cleaned %>% filter(booster_add_dose == 0 & incomplete ==0) 

covid_vacc_cleaned %>% summarise(type=first(Vaccine)) %>% filter(!grepl("\\|", type)) %>%
  group_by(type) %>% summarise(count=n(), prop=count/nrow(.))

# offset doses of covid vaccines to account for delay in protection 
covid_vacc_cleaned <- covid_vacc_cleaned %>% mutate(Date_offset = Date + 14)

write_csv(covid_vacc_cleaned, "D:/CCHCS_premium/st/indirects/cleaned-data/cleaned_vaccination_data.csv")


