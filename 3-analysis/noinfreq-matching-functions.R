# Sophia Tan 9/6/23
# functions for main indirects analysis

library(tidyverse)
library(readr)
library(lubridate)
library(foreach)
library(doParallel)
library(survival)
library(ggfortify)
library(gtsummary)
library(rms)
library(MatchIt)
library(gtools)

generate_distance_matrix <- function(tbl) {
  inf_primary <- tbl %>% select(inf.primary) %>% dist(diag = T, upper = T) %>% as.matrix()
  inf_secondary <- tbl %>% select(inf.secondary) %>% dist(diag = T, upper = T) %>% as.matrix()
  inf <- inf_primary+inf_secondary
  # create weighted distance matrix (age, COVID-19 risk, and prior infection history)
  # 50% weight from primary resident (time since infection)
  # 50% weight from secondary resident (time since infection + propensity based on age + risk)
  # distance is Inf if units do not start within 2 weeks of each other
  distance_primary <- tbl %>% select(time_since_inf_scaled.primary) %>% dist(diag = T, upper = T) %>% as.matrix()
  distance_propensity <- dist(as.matrix(tbl%>%select(ps_scaled.secondary)), diag = T, upper = T) %>% as.matrix()
  distance_secondary <- dist(as.matrix(tbl%>%select(time_since_inf_scaled.secondary)), diag = T, upper = T) %>% as.matrix()
  
  weight_primary <- matrix(data = 0.5, nrow=nrow(tbl), ncol=nrow(tbl))
  weight_propensity <- matrix(data = 0.25, nrow=nrow(tbl), ncol=nrow(tbl))
  weight_secondary <- matrix(data = 0.25, nrow=nrow(tbl), ncol=nrow(tbl))
  
  # secondary resident no infection
  weight_propensity[!distance_primary%>%is.na()&distance_secondary%>%is.na()] <- 0.5
  weight_secondary[!distance_primary%>%is.na()&distance_secondary%>%is.na()] <- 0
  
  # primary resident no infection
  weight_primary[distance_primary%>%is.na()&!distance_secondary%>%is.na()] <- 0
  weight_propensity[distance_primary%>%is.na()&!distance_secondary%>%is.na()] <- 0.5
  weight_secondary[distance_primary%>%is.na()&!distance_secondary%>%is.na()] <- 0.5
  
  # neither resident has infection
  weight_primary[distance_primary%>%is.na()&distance_secondary%>%is.na()] <- 0
  weight_propensity[distance_primary%>%is.na()&distance_secondary%>%is.na()] <- 1
  weight_secondary[distance_primary%>%is.na()&distance_secondary%>%is.na()] <- 0
  
  distance_primary[distance_primary%>%is.na()] <- 0
  distance_secondary[distance_secondary%>%is.na()] <- 0
  
  total_distance <- weight_primary*distance_primary + weight_propensity*distance_propensity + weight_secondary*distance_secondary
  
  overlap <- cross_join(tbl %>% select(label, first, duration_interval),
                        tbl %>% select(label, first, duration_interval)) %>% 
    mutate(eligible=abs(first.x-first.y)<=13) %>%
    mutate(overlap=!(intersect(duration_interval.x, duration_interval.y)%>%time_length())%>%is.na())
  
  eligible_wide <- overlap%>% 
    select(label.x,label.y,eligible) %>% 
    pivot_wider(id_cols = label.x, names_from = label.y, values_from = eligible) %>% as.data.frame() %>% select(!label.x)
  
  overlap_wide <- overlap%>% 
    select(label.x,label.y,overlap) %>% 
    pivot_wider(id_cols = label.x, names_from = label.y, values_from = overlap) %>% as.data.frame() %>% select(!label.x)
  
  total_distance[!overlap_wide|!eligible_wide] <- Inf
  total_distance[inf>0] <- Inf
  rownames(total_distance) <- 1:nrow(tbl)
  colnames(total_distance) <- 1:nrow(tbl)
  total_distance
}


filter_data <- function(tbl) {
  tbl <- tbl %>% mutate(label=1:n())
  tblselect <- tbl %>% select(label, treatment, first, duration_interval)
  control <- tblselect %>% filter(treatment==1) 
  treatment <- tblselect %>% filter(treatment==0) 
  
  cross_joined <- control %>% cross_join(treatment) %>% 
    mutate(eligible=abs(first.x-first.y)<=13&!intersect(duration_interval.x, duration_interval.y)%>%is.na())
  
  remove_controls <- cross_joined %>% 
    group_by(label.x) %>% 
    filter(all(!eligible)) %>% group_keys()
  
  remove_treatments <- cross_joined %>% 
    group_by(label.y) %>% 
    filter(all(!eligible)) %>% group_keys()
  
  tbl %>% filter(!label %in% remove_controls$label.x & !label %in% remove_treatments$label.y)
}

matching_specifications <- function(tbl) {
  test <- tbl %>% group_by(inf.primary, inf.secondary, num_dose_grouped.primary) %>% summarise(has_matches=any(treatment==0)&any(treatment==1))
  if(!any(test$has_matches)) {
    print("no matches")
    print(tbl%>%select(id_stable, first, inf.primary, inf.secondary, num_dose_grouped.primary, num_dose_grouped.secondary))
    return()}
  
  tbl <- tbl %>% left_join(test) %>% 
    filter(has_matches) 
  tbl <- tbl %>% mutate(label=1:n())
  
  if(sum(tbl$treatment==0)==1 | sum(tbl$treatment==1)==1){
    if(sum(tbl$treatment==0)==1) {
      tbl <- tbl %>% arrange(treatment)
    } else {
      tbl <- tbl %>% arrange(desc(treatment))
    }
    
    tbl <- tbl %>% filter(abs(first-first(first))<=13)
    if(nrow(tbl)==1){return()}
    
    print("single control or treatment group")
    print(tbl%>%select(id_stable, first, inf.primary, inf.secondary, num_dose_grouped.primary, num_dose_grouped.secondary))
    
    distance <- tbl %>% 
      generate_distance_matrix()
    tbl$distance <- distance[,1]
    tbl <- (tbl %>% arrange(distance))[1:2,] %>% select(!distance)
    return(cbind(id=1:2, subclass=c(1,1), tbl))
  }
  
  matchit(treatment ~ Institution + BuildingId + num_dose_grouped.primary + 
            age.primary + risk.primary + inf.primary +
            age.secondary + risk.secondary + inf.secondary,
          data = tbl,
          distance = generate_distance_matrix(tbl), 
          exact = treatment ~ Institution + BuildingId + num_dose_grouped.primary + inf.primary + inf.secondary,
          ratio = 1, method="nearest")  %>%
    get_matches() %>% select(!c(weights))
}

matching <- function(tbl) {
  m <- tbl %>% 
    matching_specifications()
  
  if(is.null(m)) {return()}
  
  m <- m %>% 
    group_by(subclass) %>% 
    mutate(subclass=cur_group_id())
  
  m <- m %>% 
    group_by(subclass) %>% 
    arrange(subclass, desc(treatment)) %>% 
    filter(!intersect(first(duration_interval), duration_interval)%>%time_length()%>%is.na())%>%
    filter(abs(first(first)-first)<=13) %>% filter(n()>1) %>% 
    ungroup()
  
  if(nrow(m)==0){return()}
  
  m %>% mutate(id=1:n())
}

post_match_processing <- function(tbl) {
  tbl %>% ungroup() %>% 
    mutate(adjusted_start = first+5, 
           adjusted_end = last+5, 
           duration_interval = interval(adjusted_start, adjusted_end)) %>%
    group_by(subclass) %>%
    mutate(overlap=intersect(duration_interval[1], duration_interval[2])) %>% 
    mutate(final_start = int_start(first(overlap)), 
           final_end = int_end(first(overlap)))
}

generate_assignment <- function(i, assignments, data_mixed, data_other) {
  assignment_full <- c(rbind(abs(assignments[i,]-1), assignments[i,]))
  a <- data_mixed %>% ungroup() %>% mutate(type=factor(assignment_full, labels =c("primary","secondary")))
  full <- a %>% rbind(data_other) 
  full
}

data_reshape <- function(d) {
  data <- d %>% as.data.frame() %>% 
    reshape(idvar = "id",
            timevar = "type",
            direction = "wide") %>% 
    mutate(both_has_test = test.primary & test.secondary) 
  
  data <- data %>% left_join(unit_info) %>% rename("id_stable"="id") 
  
  data %>% ungroup() %>%
    mutate(treatment = ifelse(vacc.secondary==0, 0, 1)) %>% 
    mutate(duration_interval = interval(first, last)) 
}

test_assignment <- function(data) {
  if(nrow(data)==0){return()}
  
  matched <- matching(data%>% mutate(label=1:n())) 
  
  if(matched%>%is.null()){return()}
  
  matched
}
