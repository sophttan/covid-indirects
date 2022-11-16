# demographics of index cases by number of vaccine doses

rm(list=ls())
gc()

setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")

library(tidyverse)
vacc <- read_csv("cleaned_vaccination_data.csv")
vacc_primary <- vacc %>% group_by(ResidentId) %>% filter(num_dose <= full_vacc)
vacc_primary <- vacc_primary %>% summarise(primary_series=first(Vaccine), all_same=all(Vaccine==first(Vaccine)))

d <- read_csv("matched_data_ps100722.csv")
demo <- read_csv("demographic_data_clean.csv")

cases <- d %>% select(index_id, index_prior_inf, covid_risk, index_prior_vacc, index_prior_vacc_doses, num_days_in_contact) %>%
  rename("ResidentId"="index_id", "prior_vacc"="index_prior_vacc", "prior_inf"="index_prior_inf", "num_doses"="index_prior_vacc_doses") %>%
  mutate(group="Index cases")

library(lubridate)
covid_risk <- read_csv("covid_risk_score.csv")
covid_risk_subset <- covid_risk %>% filter(ResidentId %in% contacts$ResidentId) 
covid_risk_subset <- covid_risk_subset %>% rowwise() %>% mutate(first=interval %>% str_extract_all("[0-9]+-[0-9]+-[0-9]+"), 
                                                                interval=interval(first[1], first[2])) %>% select(!first)

cases <- cases %>% left_join(vacc, by=c("ResidentId", "num_doses"="num_dose"))
cases <- cases %>% left_join(demo, by="ResidentId")

cases <- cases %>% mutate(Age=2022-BirthYear)
cases <- cases %>% left_join(vacc_primary)

library(tableone)
cases <- cases %>% mutate(primary_series=ifelse(num_doses==0, "Unvaccinated", primary_series),
                          boosted=ifelse(num_doses>full_vacc & full_vacc>0, 1, 0), 
                          Race = case_when(Race=="A"~"Asian or Pacific Islander",
                                           Race=="B"~"Black",
                                           Race=="H"|Race=="M"~"Hispanic",
                                           Race=="I"~"American Indian/Alaskan Native",
                                           Race=="O"~"Other",
                                           Race=="W"~"White")) 

cases <- cases %>% mutate(status=case_when(num_doses>0&num_doses==full_vacc~"Fully vaccinated", 
                                           num_doses>0&num_doses<full_vacc~"Partially vaccinated",
                                           num_doses>0&num_doses>full_vacc~"Boosted",
                                           num_doses==0~""), 
                          group=group %>% factor(levels=c("Index cases")))

prim_series <- cases %>% group_by(num_doses, primary_series) %>% summarise(n=n())
prim_series <- prim_series %>% group_by(num_doses) %>% summarise(primary_series=primary_series, n=n, perc=signif(n/sum(n)*100,2))

prim_series <- prim_series %>% mutate(primary_series = case_when(grepl("Ad26",primary_series)~"Ad26.COV2",
                                                                 grepl("BNT",primary_series)~"BNT162b2",
                                                                 grepl("1273",primary_series)~"mRNA-1273",
                                                                 primary_series=="Unvaccinated"~"Unvaccinated")) %>% 
  mutate(`N (%)` = paste0(n, " (", perc, "%)"))

prim_series <- prim_series %>% select(!c(n, perc)) %>% 
  pivot_wider(names_from=c(num_doses), values_from=`N (%)`, values_fill = "-") 

vars <- c("Sex", "Age", "Race", "covid_risk", "prior_inf", "num_days_in_contact")
catVars <- c("Sex", "Race", "prior_inf")
tbl1 <- CreateTableOne(vars = vars, strata = c("num_doses"), data = cases, factorVars = catVars)
tbl1_matrix <- print(tbl1)
tbl1_matrix <- tbl1_matrix[,1:4]


prim_series <- prim_series %>% data.frame(row.names = as.character(prim_series$primary_series), check.names = F) %>% select(!primary_series)
names(prim_series) <- NULL
colnames(prim_series) <- colnames(tbl1_matrix)


tbl1_matrix %>% rbind(prim_series) %>% write.csv("/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/tables/demographics_table_by_doses.csv")
