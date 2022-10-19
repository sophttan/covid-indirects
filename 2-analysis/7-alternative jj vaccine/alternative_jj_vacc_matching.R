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

inf_omicron_subset <- read_csv("unmatched_all_covariates100722.csv")

inf_omicron_subset <- inf_omicron_subset %>% left_join(vacc_primary, by=c("index_id"="ResidentId")) 
inf_omicron_subset <- inf_omicron_subset %>% filter(treatment==1|primary_series != "SARS-CoV-2 (COVID-19) Ad26 , recomb")

# estimate propensity scores (age, COVID-19 risk, and prior infection history)
ps <- glm(treatment ~ age + covid_risk + index_prior_inf, data = inf_omicron_subset, family = binomial(link='logit'))
summary(ps)
inf_omicron_subset <- inf_omicron_subset %>% select(!c(ps, logodds)) %>% 
  mutate(logodds = predict.glm(ps),ps=exp(logodds)/(1+exp(logodds)))
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
