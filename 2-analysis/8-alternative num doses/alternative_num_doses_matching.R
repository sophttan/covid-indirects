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

test <- read_csv("unmatched_all_covariates100722.csv")
test$new_weights <- NA
test <- test %>% group_by(treatment) %>% mutate(new_subclass = ifelse(treatment==1, 1:n(), NA)) %>% ungroup()

# matching multiple control groups
# distance matrices for Day and propensity score
for (vacc in 1:3) {
  subset <- test %>% filter(treatment==1|index_prior_vacc_doses==vacc)
  
  distance_propensity <- dist(as.matrix(subset%>%select(ps)), diag = T, upper = T) %>% as.matrix()
  distance_matrix <- dist(as.matrix(subset%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()
  
  # manually set calipers for Day and propensity score
  # main analysis, propensity score caliper = 0.1 (varied in sensitivity analysis)
  ps_dist <- 0.1
  day_dist <- 30
  distance_propensity[distance_propensity > ps_dist] <- Inf
  distance_matrix[distance_matrix > day_dist] <- Inf
  
  # scale day distance matrix to match scale of difference in propensity scores
  distance_matrix <- distance_matrix/day_dist*ps_dist
  
  # sum together distance matrices for total distance
  # weights between day distance and propensity score distance varied in sensitivity analyses
  weights <- c(0.5, 0.5)
  totaldistance <- distance_matrix*weights[1] + distance_propensity*weights[2]
  
  # match using combined distance with exact matches by Institution
  # 10:1 ratio of vaccinated to unvaccinated cases without replacement
  m <- matchit(treatment ~ age + covid_risk + index_prior_inf + Day, data = subset,
               method = "nearest", 
               exact = ~Institution,
               distance = totaldistance, ratio=10, replace = F)
  print(summary(m))
  
  mout <- match.data(m) 
  
  check <- mout %>% group_by(subclass) %>% arrange(desc(treatment)) %>% 
    filter(any(abs(first(Day)-Day)>day_dist|abs(first(ps)-ps)>ps_dist)) %>% nrow()
  print(check)
  
  if(check > 0) {
    mout <- mout %>% group_by(subclass) %>% arrange(desc(treatment)) %>%
      filter(abs(first(Day)-Day)<=day_dist&abs(first(ps)-ps)<=ps_dist)
    mean_weights <- mean((mout %>% filter(treatment==0))$weights)
    mout <- mout %>% mutate(weights=ifelse(treatment==1,1,weights/mean_weights))
  }
  
  test <- test %>% left_join(mout %>% select(no, weights, subclass) %>% 
                               rename("w"="weights", 
                                      "s"="subclass"), 
                             by="no") #Assign matching weights
  gc()
}


test %>% names()
test %>% select(no, treatment, new_subclass, w.x, s.x, w.y, s.y, w, s) %>% arrange(s.x, desc(treatment))
test <- test %>% group_by(s.x) %>% arrange(s.x, desc(treatment)) %>% mutate(new_subclass=ifelse(s.x %>% is.na(), new_subclass, first(new_subclass)),
                                                                            new_weights=ifelse(new_weights %>% is.na(), w.x, new_weights)) %>%
  ungroup()
test %>% select(no, index_prior_vacc_doses, new_subclass, w.x, s.x, w.y, s.y, w, s)
test <- test %>% group_by(s.y) %>% arrange(s.y, desc(treatment)) %>% mutate(new_subclass=ifelse(s.y %>% is.na(), new_subclass, first(new_subclass)),
                                                                            new_weights=ifelse(new_weights %>% is.na(), w.y, new_weights)) %>% 
  ungroup()
test %>% group_by(new_subclass) %>% arrange(new_subclass) %>% 
  select(no, index_prior_vacc_doses, new_subclass, w.x, s.x, w.y, s.y, w, s) %>% filter(max(index_prior_vacc_doses)>1)
test <- test %>% group_by(s) %>% arrange(s, desc(treatment)) %>% mutate(new_subclass=ifelse(s %>% is.na(), new_subclass, first(new_subclass)),
                                                                        new_weights=ifelse(new_weights %>% is.na(), w, new_weights))
test %>% group_by(new_subclass) %>% arrange(new_subclass) %>%  select(no, index_prior_vacc_doses, treatment, new_weights, s.x, w.x, s.y, w.y, s, w)

test %>% group_by(new_subclass) %>% filter(n()>1) %>% filter(!new_subclass %>% is.na()) %>% group_by(index_prior_vacc_doses) %>% summarise(n=n())
test %>% group_by(new_subclass) %>% filter(n()>1) %>% filter(!new_subclass %>% is.na()) %>% nrow()

test <- test %>% group_by(new_subclass) %>% filter(n()>1) %>% filter(!new_subclass %>% is.na())
test %>% write_csv("matched_data_doses.csv")
