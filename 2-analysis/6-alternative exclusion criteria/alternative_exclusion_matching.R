# Sophia Tan 6/24/22
# Matching under alternative matching specifications

rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")
#setwd("D:/stan5/code_ST")

library(tidyverse)
library(broom)
library(readr)
library(MatchIt)

vacc <- read_csv("cleaned_vaccination_data.csv")
vacc_primary <- vacc %>% group_by(ResidentId) %>% filter(num_dose <= full_vacc)
vacc_primary <- vacc_primary %>% summarise(primary_series=first(Vaccine), all_same=all(Vaccine==first(Vaccine)))

# prep data for matching
summary_housing <- read_csv("housing_duration.csv")
inst_day <- read_csv("incidence_final.csv")
covid_risk <- read_csv("covid_risk_score.csv")
demographic <- read_csv("demographic_data_clean.csv")

infections <- read_csv("final_sample092922_nopcr.csv") %>% 
  left_join(vacc %>% select(ResidentId, num_dose, Date_offset), by=c("ResidentId", "num_dose"))
infections <- infections %>% mutate(num_dose_adjusted = ifelse(num_dose>0 & Day<Date_offset, num_dose-1, num_dose))
infections <- infections %>% left_join(summary_housing) 

# covariates 
# add incidence
infections <- infections %>% left_join(inst_day)
infections <- infections %>% mutate(incidence_log = log(incidence))

# rename columns and aggregate covariates
infections <- infections %>% rename("index_id"="ResidentId", 
                                    "index_prior_vacc_doses"="num_dose_adjusted",
                                    "contact_id"="contacts",
                                    "contact_status"="neg_pos_contact")

infections <- infections %>% mutate(
  index_prior_inf = ifelse(num_pos==1, 0, 1),
  index_prior_vacc = ifelse(index_prior_vacc_doses==0, 0, 1))

infections <- infections %>% mutate(index_prior_vacc_doses=ifelse(index_prior_vacc_doses>3, 3, index_prior_vacc_doses),
                                    num_vacc_doses=ifelse(num_vacc_doses>3, 3, num_vacc_doses))
infections <- infections %>% mutate(index_has_vacc_or_inf=ifelse(index_prior_inf==1|index_prior_vacc==1, 1, 0))

infections %>% group_by(index_prior_vacc, index_prior_inf) %>% summarise(case_contact_pairs=n(), inf_risk=mean(contact_status))

# keep only 1 contact for each index case
mult_contacts <- infections %>% group_by(no) %>% filter(n()>1) 
mult_contacts %>% summarise(count=n())
set.seed(42)
mult_contacts <- mult_contacts[sample(1:nrow(mult_contacts)),]
mult_contacts_single <- mult_contacts %>% group_by(no) %>% summarise_all(first)

# subset data for matching 
infections_unique <- infections %>% group_by(no) %>% filter(n()==1) %>% summarise_all(first)
infections_unique <- infections_unique %>% rbind(mult_contacts_single)
infections_unique <- infections_unique %>% mutate(treatment = as.factor(ifelse(index_prior_vacc_doses==0, 1, 0)))
infections_unique %>% group_by(index_prior_vacc, index_prior_inf) %>% summarise(case_contact_pairs=n(), inf_risk=mean(contact_status))

inf_omicron_subset <- infections_unique %>% filter(first < "2020-04-01") %>% arrange(Day)

# add COVID-19 risk score
library(lubridate)
covid_risk <- read_csv("covid_risk_score.csv")
covid_risk_subset <- covid_risk %>% filter(ResidentId %in% inf_omicron_subset$index_id) 
covid_risk_subset <- covid_risk_subset %>% rowwise() %>% mutate(first=interval %>% str_extract_all("[0-9]+-[0-9]+-[0-9]+"), 
                                                                interval=interval(first[1], first[2])) %>% select(!first)

inf_omicron_subset <- inf_omicron_subset %>% full_join(covid_risk_subset, by=c("index_id"="ResidentId")) %>% 
  filter(Day %>% lubridate::`%within%`(interval)) %>%
  rename("covid_risk"="Value")
inf_omicron_subset

# add age and other demographic data
inf_omicron_subset <- inf_omicron_subset %>% select(!interval) %>% mutate(Year=format(Day, format='%Y'))
inf_omicron_subset <- inf_omicron_subset %>% left_join(demographic, by=c("index_id"="ResidentId"))
inf_omicron_subset <- inf_omicron_subset %>% mutate(age=as.numeric(Year)-as.numeric(BirthYear))
inf_omicron_subset

inf_omicron_subset <- inf_omicron_subset %>% left_join(vacc_primary, by=c("index_id"="ResidentId")) 
inf_omicron_subset <- inf_omicron_subset %>% filter(treatment==1|primary_series != "SARS-CoV-2 (COVID-19) Ad26 , recomb")

# estimate propensity scores (age, COVID-19 risk, and prior infection history)
ps <- glm(treatment ~ age + covid_risk + index_prior_inf, data = inf_omicron_subset, family = binomial(link='logit'))
summary(ps)
inf_omicron_subset <- inf_omicron_subset %>% mutate(logodds = predict.glm(ps),
                                                    ps=exp(logodds)/(1+exp(logodds)))
inf_omicron_subset

# distance matrices for Day and propensity score
distance_propensity <- dist(as.matrix(inf_omicron_subset%>%select(ps)), diag = T, upper = T) %>% as.matrix()
distance_matrix <- dist(as.matrix(inf_omicron_subset%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()


match <- function(propensity, day, caliper_propensity, caliper_day_dist,
                  weight_p_d, k, include_propensity=T) {
  propensity[propensity > caliper_propensity] <- Inf
  day[day > caliper_day_dist] <- Inf
  
  day <- day/caliper_day_dist*caliper_propensity
  distance <- propensity*weight_p_d[1] + day*weight_p_d[2]
  print(distance[1:10,1:10])
  m <- matchit(treatment ~ age + covid_risk + index_prior_inf + Day, data = inf_omicron_subset,
               method = "nearest", 
               exact = ~Institution,
               distance = distance, ratio=k, replace = F)
  
  print(summary(m))
  match.data(m)
}

reweigh_matches <- function(data, caliper_propensity, caliper_day_dist){
  data <- data %>% group_by(subclass) %>% arrange(desc(treatment)) %>%
    filter(abs(first(Day)-Day)<=caliper_day_dist&abs(first(ps)-ps)<=caliper_propensity)
  # reweigh matches
  data <- data %>% mutate(weights=ifelse(treatment==1,1,1/sum(treatment==0)))
  mean_weights <- mean((data %>% filter(treatment==0))$weights)
  data <- data %>% mutate(weights=ifelse(treatment==1,1,weights/mean_weights))
}

ps_dist <- 0.1
day_dist <- 30

matched <- match(distance_propensity, distance_matrix, ps_dist, day_dist, c(0.5, 0.5), 10, T)
matched %>% group_by(subclass) %>% arrange(subclass, desc(treatment)) %>% 
  select(subclass, treatment, ps, Day) %>% 
  filter(treatment==1|(abs(first(Day)-Day)>day_dist|abs(first(ps)-ps)>ps_dist)) %>% filter(n()>1)
matched <- reweigh_matches(matched, ps_dist, day_dist)
matched %>% write_csv("matched_data_nojcovden.csv")

