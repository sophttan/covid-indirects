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

inf_omicron_subset <- read_csv("unmatched_all_covariates092922.csv")

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
  
  if(include_propensity==F) {
    distance <- day
    print(distance[1:10,1:10])
    m <- matchit(treatment ~ Day, data = inf_omicron_subset,
                 method = "nearest", 
                 exact = ~Institution,
                 distance = distance, ratio=k, replace = F)
  }else{
    day <- day/caliper_day_dist*caliper_propensity
    distance <- propensity*weight_p_d[1] + day*weight_p_d[2]
    print(distance[1:10,1:10])
    m <- matchit(treatment ~ age + covid_risk + index_prior_inf + Day, data = inf_omicron_subset,
                 method = "nearest", 
                 exact = ~Institution,
                 distance = distance, ratio=k, replace = F)
    print(plot(m, type = "qq", interactive = FALSE, which.xs = c("age", "covid_risk", "index_prior_inf", "Day")))
  }
  
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

# caliper testing
# manually set calipers for Day and propensity score
# propensity score caliper = 0.2
ps_dist <- 0.2
day_dist <- 30

matched <- match(distance_propensity, distance_matrix, ps_dist, day_dist, c(0.5, 0.5), 10, T)
matched %>% group_by(subclass) %>% arrange(subclass, desc(treatment)) %>% 
  select(subclass, treatment, ps, Day) %>% 
  filter(treatment==1|(abs(first(Day)-Day)>day_dist|abs(first(ps)-ps)>ps_dist)) %>% filter(n()>1)
matched <- reweigh_matches(matched, ps_dist, day_dist)
matched %>% write_csv("matched_data_alternative_cal_propensity.csv")

# day caliper = 15
ps_dist <- 0.1
day_dist <- 15
matched <- match(distance_propensity, distance_matrix, ps_dist, day_dist, c(0.5, 0.5), 10, T)
matched %>% group_by(subclass) %>% arrange(subclass, desc(treatment)) %>% 
  select(subclass, treatment, ps, Day) %>% 
  filter(treatment==1|(abs(first(Day)-Day)>day_dist|abs(first(ps)-ps)>ps_dist)) %>% filter(n()>1)
matched <- reweigh_matches(matched, ps_dist, day_dist)
matched %>% group_by(subclass) %>% filter(n()==1)
matched %>% write_csv("matched_data_alternative_cal_days.csv")

# 1:4 matching 
ps_dist <- 0.1
day_dist <- 30
matched <- match(distance_propensity, distance_matrix, ps_dist, day_dist, c(0.5, 0.5), 4, T)
matched %>% group_by(subclass) %>% arrange(subclass, desc(treatment)) %>% 
  select(subclass, treatment, ps, Day) %>% 
  filter(treatment==1|(abs(first(Day)-Day)>day_dist|abs(first(ps)-ps)>ps_dist)) %>% filter(n()>1)
matched <- reweigh_matches(matched, ps_dist, day_dist)
matched %>% group_by(subclass) %>% filter(n()==1)
matched %>% write_csv("matched_data_alternative1_4ratio.csv")

# changing weights
ps_dist <- 0.1
day_dist <- 30
weight <- c(0.25, 0.75)
matched <- match(distance_propensity, distance_matrix, ps_dist, day_dist, weight, 10, T)
matched %>% group_by(subclass) %>% arrange(subclass, desc(treatment)) %>% 
  select(subclass, treatment, ps, Day) %>% 
  filter(treatment==1|(abs(first(Day)-Day)>day_dist|abs(first(ps)-ps)>ps_dist)) %>% filter(n()>1)
matched <- reweigh_matches(matched, ps_dist, day_dist)
matched %>% group_by(subclass) %>% filter(n()==1)
matched %>% write_csv("matched_data_alternative_weights_1p_3d.csv")

weight <- c(0.75, 0.25)
matched <- match(distance_propensity, distance_matrix, ps_dist, day_dist, weight, 10, T)
matched %>% group_by(subclass) %>% arrange(subclass, desc(treatment)) %>% 
  select(subclass, treatment, ps, Day) %>% 
  filter(treatment==1|(abs(first(Day)-Day)>day_dist|abs(first(ps)-ps)>ps_dist)) %>% filter(n()>1)
matched <- reweigh_matches(matched, ps_dist, day_dist)
matched %>% group_by(subclass) %>% filter(n()==1)
matched %>% write_csv("matched_data_alternative_weights_3p_1d.csv")


# no propensity score
matched <- match(distance_propensity, distance_matrix, NULL, day_dist, NULL, 10, F)
matched %>% group_by(subclass) %>% arrange(subclass, desc(treatment)) %>% 
  select(subclass, treatment, ps, Day) %>% 
  filter(treatment==1|(abs(first(Day)-Day)>day_dist)) %>% filter(n()>1)
matched %>% write_csv("matched_data_alternative_nopropensity.csv")


# additional factors in propensity score
# estimate propensity scores (age, COVID-19 risk, and prior infection history)
ps_model <- glm(treatment ~ age + covid_risk + index_prior_inf + Race + Sex, data = inf_omicron_subset, family = binomial(link='logit'))
summary(ps_model)
inf_omicron_subset <- inf_omicron_subset %>% mutate(logodds = predict.glm(ps_model),ps=exp(logodds)/(1+exp(logodds)))

# distance matrices for Day and propensity score
distance_propensity <- dist(as.matrix(inf_omicron_subset%>%select(ps)), diag = T, upper = T) %>% as.matrix()
distance_matrix <- dist(as.matrix(inf_omicron_subset%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()

ps_dist <- 0.1
day_dist <- 30
distance_propensity[distance_propensity > ps_dist] <- Inf
distance_matrix[distance_matrix > day_dist] <- Inf
distance_matrix <- distance_matrix/day_dist*ps_dist
weights <- c(0.5, 0.5)
totaldistance <- distance_matrix*weights[1] + distance_propensity*weights[2]

m <- matchit(treatment ~ age + covid_risk + index_prior_inf + Day + Sex + Race, data = inf_omicron_subset,
             method = "nearest", 
             exact = ~Institution,
             distance = totaldistance, ratio=10, replace = F)
mout <- match.data(m)
mout %>% group_by(subclass) %>% arrange(subclass, desc(treatment)) %>% 
  select(subclass, treatment, ps, Day) %>% 
  filter(treatment==1|(abs(first(Day)-Day)>day_dist)) %>% filter(n()>1)
mout %>% write_csv("matched_data_alternative_propensity_sex_race.csv")
