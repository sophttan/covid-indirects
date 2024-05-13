# Sophia Tan 8/16/23
# Identify units that meet different vaccine requirements by dose

rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

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

registerDoParallel(4)

d <- read_csv("allvacc_full_data_prematching_relaxincarceration_priorinf_bydose_082523.csv") %>% group_by(id)
testing <- read_csv("complete_testing_data.csv") %>% select(ResidentId, Day, Result)

# add time-varying month
# create time dataset
dates <- data.frame(date=seq(as.Date("2021-12-15"), as.Date("2022-12-15"), by=1))
# dates <- dates %>% mutate(day=0:(n()-1), month=floor(day/30.5)) %>% rowwise() %>% mutate(month=min(month, 11))
dates <- dates %>% mutate(month=floor((0:(n()-1))/91.5)) %>% rowwise() %>% mutate(month=min(month, 11)) %>%
  group_by(month) %>% filter(date==first(date)|date==last(date)) %>% mutate(group=c("first", "last")) %>%
  pivot_wider(id_cols=month, names_from=group, names_prefix = "month.", values_from=date)

risk <- read_csv("covid_risk_score.csv")
fix_intersection <- function(v) {
  v%>%str_extract_all("[0-9]{4}-[0-9]{2}-[0-9]{2}", simplify = T)
}
intersection <- fix_intersection(risk$interval)
risk$start <- intersection[,1]%>%as.vector()%>%as.Date()
risk$end <- intersection[,2]%>%as.vector()%>%as.Date()
risk <- risk %>% mutate(risk_interval=interval(start=start, end=end)) %>% select(!c(start, end, interval))

unit_info <- d %>% select(id, Institution, BuildingId, RoomId, Day, first, last, duration) %>% summarise_all(first) %>% select(!Day)
resident_info <- d %>% select(!names(unit_info)) %>% arrange(id, desc(test)) 

generate_distance_matrix <- function(tbl) {
  overlap <- expand.grid(x=tbl$label,y=tbl$label) %>% 
    left_join(tbl %>% dplyr::select(label, duration_interval), by=c("x"="label")) %>% 
    left_join(tbl %>% dplyr::select(label, duration_interval), by=c("y"="label")) %>% 
    mutate(overlap=intersect(duration_interval.x,duration_interval.y), 
           duration_overlap=366-(time_length(overlap, unit = "day")+1))
  
  duration_overlap_wide <- overlap%>% 
    dplyr::select(x,y,duration_overlap) %>% replace_na(list(duration_overlap=1000)) %>% 
    pivot_wider(id_cols = x, names_from = y, values_from = duration_overlap)
  duration_overlap_wide <- duration_overlap_wide %>% as.data.frame(row.names = .$x) %>% dplyr::select(!x)
  
  duration_overlap_wide %>% as.matrix()
}

matching_specifications <- function(tbl) {
  # Iterate using the for loop from 1 to 5
  # And print the square of each number
  # Using parallelism
  sets <- list(1:10, 11:20, 21:27, 28:35)
  foreach(set=1:4, .packages=c("dplyr","lubridate","MatchIt"), .combine=rbind) %dopar%  {
    a <- tbl %>% filter(Institution %in% sets[[set]]) %>% mutate(label=1:n())
    matchit(treatment ~ Institution + BuildingId + duration_interval + vacc.primary,
            data = a,
            distance = generate_distance_matrix(a), 
            exact = treatment ~ Institution + BuildingId + vacc.primary, 
            ratio = 7, min.controls = 1, max.controls = 8, method="optimal") %>%
      get_matches()
  }
}

matching <- function(tbl) {
  m <- matching_specifications(tbl) %>% group_by(Institution, subclass) %>% 
    mutate(subclass=cur_group_id())
  
  m <- m %>% group_by(subclass) %>% arrange(subclass, desc(treatment)) %>%
    mutate(intersect=time_length(intersect(first(duration_interval),duration_interval),unit="day")+1) %>% 
    replace_na(list(intersect=0)) %>% 
    mutate(include=any(intersect[2:n()]>0)) %>% 
    filter(include|treatment==0) %>%
    ungroup() %>% select(!c(id, subclass, weights)) %>% mutate(label=1:nrow(.))
  
  m <- matching_specifications(m) %>% group_by(Institution, subclass) %>% 
    mutate(subclass=cur_group_id())
  
  m %>% 
    group_by(subclass) %>% 
    arrange(subclass, desc(treatment)) %>% 
    mutate(intersection=intersect(first(duration_interval),duration_interval)) %>% 
    mutate(start=as.Date(int_start(intersection)), end=as.Date(int_end(intersection)),
           intersect=time_length(intersection,unit="day")+1) %>% 
    replace_na(list(intersect=0)) %>% 
    filter(treatment==1|intersect>=7) %>% filter(n()>1) %>% 
    ungroup() %>% 
    mutate(id=1:n())
}

post_match_processing <- function(tbl) {
  tbl <- tbl %>% group_by(subclass)
  treatment <- tbl %>% filter(treatment==0)
  control <- tbl %>% filter(treatment==1)
  
  maximize_cal_time_start <- function(start, end) {
    best <- as.Date(start[1])
    time <- 0
    for (i in 1:length(start)) {
      end_filtered <- end[start <= start[i] & end>start[i]]
      total_obs <- max((end_filtered-start[i])%>%as.numeric())
      if(total_obs>time) {
        best <- as.Date(start[i])
        time <- total_obs
      }
    }
    as.Date(best)
  }
  
  treatment <- treatment %>% 
    mutate(best_start=maximize_cal_time_start(start, end))
  
  treatment_filter <- treatment %>% filter(start <= best_start & end > best_start)
  
  treatment_filter <- treatment_filter %>% mutate(max_end = max(end))
  
  treatment_summary <- treatment_filter %>% summarise(best_start=first(best_start), max_end=first(max_end))
  
  control <- left_join(control, treatment_summary) %>% mutate(end=max_end)
  
  control %>% rbind(treatment_filter) %>% arrange(subclass, id) %>% 
    rowwise() %>% 
    mutate(final_start = best_start+5, final_end = min(as.Date("2022-12-15"), end + 5))
}

save_sample_size <- function(i, tbl) {
  tbl%>%
    group_by(treatment)%>%
    summarise(units=n(), cases=sum(status), obs_time=sum(survival_time))%>%
    mutate(inc_rate=cases/obs_time*100000, i=i) %>% 
    pivot_wider(id_cols=i, names_from = treatment, names_sep = ".", values_from =c(units, cases, obs_time, inc_rate))
  }

find_survival <- function(tbl) {
  add_testing <- function(tbl) {
    tbl %>% left_join(testing, by=c("ResidentId.primary"="ResidentId"))
  }
  
  filter_testing <- function(tbl) {
    tbl <- tbl %>% mutate(has_test=any(Day>=final_start&Day<=final_end))
    tbl %>% filter((Day >= final_start & Day <= final_end)|(!has_test&Day==first(Day))) %>% 
      mutate(Day=if_else(!has_test, as.Date(NA), Day), 
             Result=if_else(!has_test, as.character(NA), Result))
  }
  
  tbl %>% add_testing() %>% 
    group_by(id) %>% 
    filter_testing() %>%
    mutate(last = ifelse(any(Result=="Positive",na.rm=T)&Result=="Positive", Result=="Positive", Day==last(Day))) %>% 
    filter(last|!has_test) %>% summarise_all(first) %>% select(!c(last)) %>% 
    mutate(final_end = if_else(!has_test|Result!="Positive", final_end, Day)) %>%
    mutate(survival_time=as.numeric(final_end-final_start)) 
}

survival_data <- function(tbl) {
  tbl <- tbl %>% find_survival()
  
  tbl <- tbl %>% mutate(treatment=1-treatment)
  
  tbl <- tbl %>%
    mutate(status=ifelse(!Result%>%is.na()&Result=="Positive", 1, 0), 
           Institution=as.factor(Institution),
           BuildingId=as.factor(BuildingId),
           subclass=as.factor(subclass)) 
  
  tbl <- tbl %>% 
    mutate(intersection=interval(final_start, final_end),
           time_since_inf.primary=(difftime(final_start, Day_inf.primary, units="days")%>%as.numeric())/30.417,
           time_since_inf.secondary=(difftime(final_start, Day_inf.secondary, units="days")%>%as.numeric())/30.417,
           time_since_vacc.primary = ((final_start-Day_vacc.primary)%>%as.numeric())/30.417,
           time_since_vacc.secondary = ((final_start-Day_vacc.secondary)%>%as.numeric())/30.417)
  
  tbl <- tbl %>% left_join(risk, by=c("ResidentId.primary"="ResidentId")) %>% 
    mutate(overlap_risk = intersect(intersection, risk_interval)) %>%
    filter(!is.na(overlap_risk)) %>% 
    mutate(days_risk=time_length(overlap_risk, unit = "day")+1) %>% 
    group_by(id) %>% 
    mutate(risk.primary=sum(days_risk*Value)/sum(days_risk)) %>% 
    summarise_all(first) %>%
    select(!c(risk_interval, overlap_risk, days_risk, Value))
  
  tbl <- tbl %>% left_join(risk, by=c("ResidentId.secondary"="ResidentId")) %>% 
    mutate(overlap_risk = intersect(intersection, risk_interval)) %>%
    filter(!is.na(overlap_risk)) %>% 
    mutate(days_risk=time_length(overlap_risk, unit = "day")+1) %>% 
    group_by(id) %>% 
    mutate(risk.secondary=sum(days_risk*Value)/sum(days_risk)) %>% 
    summarise_all(first) %>%
    select(!c(risk_interval, overlap_risk, days_risk, Value))
}

prepare_month_survival_data <- function(tbl) {
  # join time dataset with data
  dtime <- tbl %>% cross_join(dates) %>% 
    filter(final_start <= month.first | (final_start >= month.first & final_start <= month.last)) %>%
    filter(final_end >= month.last | (final_end <= month.last & final_end >= month.first)) %>% 
    rowwise() %>%
    mutate(time1=(max(final_start, month.first)-final_start)%>%as.numeric(),
           time2=(min(final_end, month.last)-final_start)%>%as.numeric()) %>%
    mutate(time2=min(time2+1, first(survival_time))) %>%
    mutate(status=if_else(time2!=first(survival_time), 0, status)) %>% 
    filter(time1<time2)
  
  dtime %>% mutate(month=factor(month, levels=0:3))
}

test_ph_assumptions <- function(d) {
  results <- coxph(Surv(time1, time2, status) ~ 
                     treatment + vacc.primary + 
                     time_since_inf.primary + time_since_inf.secondary + 
                     age.primary + age.secondary + risk.primary + risk.secondary + 
                     month + Institution +
                     frailty(subclass), 
                   data=d)
  res <- (cox.zph(results))$table %>% as.data.frame() %>% mutate(var=row.names(.))
  row.names(res) <- NULL
  res
}

regression <- function(d) {
  clean_results <- function(results) {
    summary(results)%>%coef()%>%as.data.frame()%>%
      mutate(lb=(coef-2*`se(coef)`)%>%exp(), ub=(coef+2*`se(coef)`)%>%exp(), coef=coef%>%exp()) %>%
      select(coef, lb, ub, p) %>% round(4)
  }
  
  results <- coxph(Surv(time1, time2, status) ~ 
                     treatment + vacc.primary + 
                     tt(time_since_inf.primary) + tt(time_since_inf.secondary) + 
                     age.primary + age.secondary + risk.primary + risk.secondary + 
                     month + Institution +
                     frailty(subclass), 
                   data=d, 
                   tt=list(function(x,t,...){time_since_inf.primary<-x+t/30.417}, 
                           function(x,t,...){time_since_inf.secondary<-x+t/30.417}))
  res <- clean_results(results) %>% mutate(var=row.names(.))
  row.names(res) <- NULL
  res
}

sample_sizes <- NULL
regression_res <- NULL
ph_test <- NULL
for (i in 51:100) {
  print(i)
  start <- Sys.time()
  resident_info_primary <- resident_info %>%  
    mutate(primary=if_else(!all(test), "primary", sample(c("primary", "secondary"), 1))) %>% 
    mutate(type=case_when(ResidentId==first(ResidentId)~primary,
                          first(primary)=="primary"~"secondary",
                          T~"primary")) %>%
    select(!primary)
  
  data <- resident_info_primary %>% as.data.frame() %>% 
    reshape(idvar = "id",
            timevar = "type",
            direction = "wide")
  
  data <- unit_info %>% left_join(data) %>% select(!id) 
  
  data <- data %>% ungroup() %>%
    mutate(treatment = ifelse(vacc.secondary==0, 1, 0)) %>% 
    mutate(duration_interval = interval(first, last)) 

  matched <- matching(data) 
  
  processed <- post_match_processing(matched)
  
  final_data <- survival_data(processed)

  final_data_month <- prepare_month_survival_data(final_data)
  
  sample_sizes <- rbind(sample_sizes, save_sample_size(i, final_data))
  regression_res <- rbind(regression_res, regression(final_data_month) %>% mutate(i=i))
  ph_test <- rbind(ph_test, test_ph_assumptions(final_data_month) %>% mutate(i=i))
  end <- Sys.time()
}

sample_sizes %>% write_csv("results/sample_sizes_100.csv")
regression_res %>% write_csv("results/regression_100.csv")
ph_test %>% write_csv("results/ph_test_100.csv")

