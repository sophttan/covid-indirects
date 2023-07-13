rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(lubridate)
library(tidyverse)

d <- read_csv("matching_data_071223/matching_data_allvaccdoses_priorinf_infvacc071223.csv")

fix_intersection <- function(v) {
  v%>%str_extract_all("[0-9]{4}-[0-9]{2}-[0-9]{2}", simplify = T)
}

intersection <- fix_intersection(d$intersection)
d$start <- intersection[,1]%>%as.vector()%>%as.Date()
d$end <- intersection[,2]%>%as.vector()%>%as.Date()
d <- d %>% mutate(intersection=interval(start=start, end=end))

d <- d %>% group_by(subclass)

# use matched time where we use maximum overlapped time
treatment <- d %>% filter(treatment==0)
control <- d %>% filter(treatment==1)

maximize_obs_time_start <- function(start, end) {
  best <- as.Date(start[1])
  time <- 0
  for (s in start) {
    end_filtered <- end[start <= s]
    total_obs <- sum((end_filtered-s)%>%as.numeric())
    if(total_obs>time) {
      best <- as.Date(s)
      time <- total_obs
    }
  }
  as.Date(best)
}

maximize_cal_time_start <- function(start, end) {
  best <- as.Date(start[1])
  time <- 0
  for (s in start) {
    end_filtered <- end[start <= s]
    total_obs <- max((end_filtered-s)%>%as.numeric())
    if(total_obs>time) {
      best <- as.Date(s)
      time <- total_obs
    }
  }
  as.Date(best)
}

treatment <- treatment %>% 
  mutate(best_start=maximize_cal_time_start(start, end))

treatment_filter <- treatment %>% filter(start <= best_start)

treatment_filter <- treatment_filter %>% mutate(max_end = max(end))

treatment_summary <- treatment_filter %>% summarise(best_start=first(best_start), max_end=first(max_end))

control <- left_join(control, treatment_summary) %>% mutate(end=max_end)

control

all <- control %>% rbind(treatment_filter) %>% arrange(subclass, id)

all <- all %>% rename("group_start"="best_start")

all %>% group_by(treatment) %>% summarise(n=n(), observation=sum((end-group_start)%>%as.numeric()))
 