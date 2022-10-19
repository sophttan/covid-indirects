# Sophia Tan 6/28/22
# Matching for different definitions of the infectious period

rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")

library(tidyverse)
library(broom)
library(readr)
library(MatchIt)
library(lubridate)

vacc <- read_csv("cleaned_vaccination_data.csv")
summary_housing <- read_csv("housing_duration.csv")
covid_risk <- read_csv("covid_risk_score.csv")
demographic <- read_csv("demographic_data_clean.csv")

prep_data <- function(inst_day, infections) {
  infections <- infections %>% left_join(vacc %>% select(ResidentId, num_dose, Date_offset), by=c("ResidentId", "num_dose"))
  infections <- infections %>% mutate(num_dose_adjusted = ifelse(num_dose>0 & Day<Date_offset, num_dose-1, num_dose))
  infections <- infections %>% left_join(summary_housing) 
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
  
  mult_contacts <- infections %>% group_by(no) %>% filter(n()>1) 
  set.seed(42)
  mult_contacts <- mult_contacts[sample(1:nrow(mult_contacts)),]
  mult_contacts_single <- mult_contacts %>% group_by(no) %>% summarise_all(first)
  
  # subset data for matching 
  infections_unique <- infections %>% group_by(no) %>% filter(n()==1) %>% summarise_all(first)
  infections_unique <- infections_unique %>% rbind(mult_contacts_single)
  infections_unique <- infections_unique %>% mutate(treatment = as.factor(ifelse(index_prior_vacc_doses==0, 1, 0)))

  # add COVID-19 risk score
  covid_risk_subset <- covid_risk %>% filter(ResidentId %in% infections_unique$index_id) 
  covid_risk_subset <- covid_risk_subset %>% rowwise() %>% mutate(first=interval %>% str_extract_all("[0-9]+-[0-9]+-[0-9]+"), 
                                                                  interval=interval(first[1], first[2])) %>% select(!first)
  
  infections_unique <- infections_unique %>% full_join(covid_risk_subset, by=c("index_id"="ResidentId")) %>% 
    filter(Day %>% lubridate::`%within%`(interval)) %>%
    rename("covid_risk"="Value")

  # add age and other demographic data
  infections_unique <- infections_unique %>% #select(!interval) %>% 
    mutate(Year=format(Day, format='%Y'))
  infections_unique <- infections_unique %>% left_join(demographic, by=c("index_id"="ResidentId"))
  infections_unique <- infections_unique %>% mutate(age=as.numeric(Year)-as.numeric(BirthYear))

  ps <- glm(treatment ~ age + covid_risk + index_prior_inf, data = infections_unique, family = binomial(link='logit'))
  summary(ps)
  infections_unique <- infections_unique %>% mutate(logodds = predict.glm(ps),
                                                      ps=exp(logodds)/(1+exp(logodds)))
  infections_unique
}

match <- function(propensity, day, caliper_propensity, caliper_day_dist,
                  weight_p_d, k, include_propensity=T) {
  propensity[propensity > caliper_propensity] <- Inf
  day[day > caliper_day_dist] <- Inf
  
  if(include_propensity==F) {
    distance <- day
    print(distance[1:10,1:10])
    m <- matchit(treatment ~ Day, data = d,
                 method = "nearest", 
                 exact = ~Institution,
                 distance = distance, ratio=k, replace = F)
  }else{
    day <- day/caliper_day_dist*caliper_propensity
    distance <- propensity*weight_p_d[1] + day*weight_p_d[2]
    print(distance[1:10,1:10])
    m <- matchit(treatment ~ age + covid_risk + index_prior_inf + Day, data = d,
                 method = "nearest", 
                 exact = ~Institution,
                 distance = distance, ratio=k, replace = F)
    # print(plot(m, type = "qq", interactive = FALSE, which.xs = c("age", "covid_risk", "index_prior_inf", "Day")))
  }
  
  print(summary(m))
  match.data(m)
}

reweigh_matches <- function(data, caliper_propensity, caliper_day_dist){
  data <- data %>% group_by(subclass) %>% arrange(desc(treatment)) %>%
    filter(abs(first(Day)-Day)<=caliper_day_dist&abs(first(ps)-ps)<=caliper_propensity)
  data <- data %>% filter(n()>1)
  # reweigh matches
  data <- data %>% mutate(weights=ifelse(treatment==1,1,1/sum(treatment==0)))
  mean_weights <- mean((data %>% filter(treatment==0))$weights)
  data <- data %>% mutate(weights=ifelse(treatment==1,1,weights/mean_weights))
  data
}

# infectious period starts day of first positive test, lasts 7 days
inc <- read_csv("incidence_final_7.csv")
inf <- read_csv("final_sample100722_nopcr_7days.csv") 
d <- prep_data(inc, inf)

distance_propensity <- dist(as.matrix(d%>%select(ps)), diag = T, upper = T) %>% as.matrix()
distance_matrix <- dist(as.matrix(d%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()
ps_dist <- 0.1
day_dist <- 30

matched <- match(distance_propensity, distance_matrix, ps_dist, day_dist, c(0.5, 0.5), 10, T)
matched %>% group_by(subclass) %>% arrange(subclass, desc(treatment)) %>% 
  select(subclass, treatment, ps, Day) %>% 
  filter(treatment==1|(abs(first(Day)-Day)>day_dist|abs(first(ps)-ps)>ps_dist)) %>% filter(n()>1)
matched <- reweigh_matches(matched, ps_dist, day_dist)
matched %>% write_csv("matched_data_alternative_inf_per7.csv")


# infectious period starts 2 days before first postive test, lasts 5 days
inc <- read_csv("incidence_final_2_5infper.csv")
inf <- read_csv("final_sample100722_nopcr_2_5days.csv") 
d <- prep_data(inc, inf)
gc()

distance_propensity <- dist(as.matrix(d%>%select(ps)), diag = T, upper = T) %>% as.matrix()
distance_matrix <- dist(as.matrix(d%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()
ps_dist <- 0.1
day_dist <- 30

matched <- match(distance_propensity, distance_matrix, ps_dist, day_dist, c(0.5, 0.5), 1, T)
matched %>% group_by(subclass) %>% arrange(subclass, desc(treatment)) %>% 
  select(subclass, treatment, ps, Day) %>% 
  filter(treatment==1|(abs(first(Day)-Day)>day_dist|abs(first(ps)-ps)>ps_dist)) %>% filter(n()>1)
matched <- reweigh_matches(matched, ps_dist, day_dist)
matched %>% write_csv("matched_data_alternative_inf_per2_5.csv")


# infectious period starts 2 days before first positive test, lasts 7 days
inc <- read_csv("incidence_final_2_7infper.csv")
inf <- read_csv("final_sample100722_nopcr_2_7days.csv") 
d <- prep_data(inc, inf)

distance_propensity <- dist(as.matrix(d%>%select(ps)), diag = T, upper = T) %>% as.matrix()
distance_matrix <- dist(as.matrix(d%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()
ps_dist <- 0.1
day_dist <- 30

matched <- match(distance_propensity, distance_matrix, ps_dist, day_dist, c(0.5, 0.5), 7, T)
matched %>% group_by(subclass) %>% arrange(subclass, desc(treatment)) %>% 
  select(subclass, treatment, ps, Day) %>% 
  filter(treatment==1|(abs(first(Day)-Day)>day_dist|abs(first(ps)-ps)>ps_dist)) %>% filter(n()>1)
matched <- reweigh_matches(matched, ps_dist, day_dist)
matched %>% write_csv("matched_data_alternative_inf_per2_7.csv")


