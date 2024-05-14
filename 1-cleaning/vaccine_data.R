# Sophia Tan 12/13/23
# Cleaning CDCR COVID-19 vaccination data through 5/26/23

rm(list=ls())
gc()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/CDCR Data/May 26 2023 Data/")

###### COVID Infection Data ######
vacc <-  read_delim("Immunization_20230526.csv", delim=";")
head(vacc)

unique(vacc$Vaccine)

# keep only COVID-19 vaccine records
covid_vacc <- vacc %>% filter(grepl("SARS", Vaccine))
unique(covid_vacc$Vaccine)

# rename so vaccines have consistent names
covid_vacc <- covid_vacc %>% mutate(Date = as.Date(Date), 
                                    Vaccine = ifelse(grepl("Ad26", Vaccine), "SARS-CoV-2 (COVID-19) Ad26 , recomb", Vaccine),
                                    Vaccine = ifelse(grepl("tozinameran", Vaccine), "SARS-CoV-2 (COVID-19) mRNA BNT-162b2 vax", Vaccine))

# keep only received records (remove vaccine refusal)
covid_vacc_cleaned <- covid_vacc %>% filter(Result=="Received") 

# 16 residents have incorrect years in the reporting (vaccine received before vaccines became available in 12/2020)
# all likely 2021 vaccinations - vaccines for 1611265470 could also be 2022 but will assume 2021
# some vaccine records are duplicated (have one wrong year and one correct year) 
early <- covid_vacc_cleaned %>% group_by(ResidentId) %>% arrange(ResidentId, Date) %>% filter(any(Date<"2020-12-01"))

covid_vacc_cleaned <- covid_vacc_cleaned %>% mutate(Date=if_else(Date<"2020-12-01", ymd(format(Date, "2021-%m-%d")), Date)) %>% select(!Month)
covid_vacc_cleaned <- covid_vacc_cleaned %>% distinct() 


# there are 62 residents with multiple records of different vaccines on a single day (i.e. 1 dose of J&J and 1 dose of Moderna vaccine on 6/5/21)
# collapse records so rows represent unique resident-doses 
# keep all vaccine type info
covid_vacc_cleaned <- covid_vacc_cleaned %>% group_by(ResidentId, Date) %>% arrange(Vaccine) %>% 
  summarise(Vaccine=ifelse(all(Vaccine==first(Vaccine)), first(Vaccine), paste(Vaccine, collapse = "|")))
covid_vacc_cleaned <- covid_vacc_cleaned %>% arrange(Date) %>% 
  group_by(ResidentId) %>% mutate(num_dose = 1:n(), max_dose = n()) %>% arrange(ResidentId, Date)


# descriptive labeling only affects vaccine status if first vaccine reported as j&j and mrna vaccine (# doses for primary series is different)
# if someone was marked to have received any j&j as first dose and second dose is >60 days, mark as receiving j&j, even if
# mrna also reported as first dose
# 13 have both j&j and mrna vacc reported on the same day for their first vaccine, 7 marked as mrna primary series 
covid_vacc_cleaned <- covid_vacc_cleaned %>% 
  mutate(time_since=c(diff(Date)%>%as.numeric(),NA)) %>%
  mutate(full_vacc = ifelse(grepl("Ad26", first(Vaccine)), 1, 2),
         full_vacc = ifelse(full_vacc==1&grepl("mRNA", first(Vaccine))&!is.na(first(time_since))&first(time_since)<60, 2, full_vacc),
         booster_add_dose = ifelse(num_dose<=full_vacc, 0, num_dose - full_vacc),
         incomplete = ifelse(max(num_dose) < full_vacc, 1, 0))


# offset doses of covid vaccines to account for delay in protection 
covid_vacc_cleaned <- covid_vacc_cleaned %>% mutate(Date_offset = Date + 14)

write_csv(covid_vacc_cleaned, "D:/CCHCS_premium/st/leaky/cleaned-data/complete_vaccine_data121523.csv")


  
