rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)
library(survival)
library(ggfortify)
library(gtsummary)

m_adjusted <- read_csv("matching_data_051923/matching_data_allvacc_noincarcreq_priorinf_bydose_infvacc052423.csv") 
testing <- read_csv("testing_vacc_clean.csv") %>% 
  select(ResidentId, Day, Result, num_pos) %>% filter(!Result%>%is.na())

fix_intersection <- function(v) {
  v%>%str_extract_all("[0-9]{4}-[0-9]{2}-[0-9]{2}", simplify = T)
}

intersection <- fix_intersection(m_adjusted$intersection)
m_adjusted$start <- intersection[,1]%>%as.vector()%>%as.Date()
m_adjusted$end <- intersection[,2]%>%as.vector()%>%as.Date()
m_adjusted <- m_adjusted %>% mutate(intersection=interval(start=start, end=end))

add_testing <- function(d) {
  d %>% left_join(testing, by=c("primary"="ResidentId"))
}

filter_testing <- function(d) {
  d <- d %>% mutate(has_test=any(Day>=start&Day<=end))
  d %>% filter((Day >= start & Day <= end)|(!has_test&Day==first(Day))) %>% 
    mutate(Day=if_else(!has_test, as.Date(NA), Day), 
           Result=if_else(!has_test, as.character(NA), Result))
}

# use matched time where we use maximum overlapped time
treatment <- m_adjusted %>% filter(treatment==0)
control <- m_adjusted %>% filter(treatment==1)

treatment_testing <- treatment %>% add_testing()
treatment_time_test <- treatment_testing %>% 
  group_by(id) %>%
  filter_testing() %>% 
  mutate(survival_time=as.numeric(Day-start)+1) %>%
  mutate(last = ifelse(any(Result=="Positive",na.rm=T)&Result=="Positive", Result=="Positive", Day==last(Day))) %>% 
  mutate(pos_first=first(Result)=="Positive") %>%
  filter(last|!has_test) %>% summarise_all(first) %>% select(!last) %>% 
  mutate(survival_time=ifelse(!has_test|Result!="Positive", as.numeric(end-start)+1, survival_time))

control_testing <- treatment %>% select(subclass, id, intersection, intersect) %>% rename("id_treat"="id") %>% 
  left_join(control %>% select(!c(intersection, intersect)), by="subclass") %>%
  mutate(first_intersection=int_start(intersection)%>%as.Date(), last_intersection=int_end(intersection)%>%as.Date()) %>%
  arrange(id, first_intersection, desc(intersect))
control_testing %>%  select(id, subclass, first_intersection, intersection, intersect) %>% arrange(id)

control_testing_unique <- control_testing %>% group_by(id) %>% distinct(id, first_intersection, .keep_all = T)

control_test_overlap <- control_testing_unique %>% group_by(id) %>% 
  mutate(label=1:n()) %>% 
  mutate(next_start=lag(first_intersection, 1),
         next_end=lag(last_intersection, 1)) %>% 
  mutate(overlap_first=first_intersection < first(last_intersection),
         overlap_between= first_intersection < next_end) %>%
  mutate(distinct = !overlap_first & !overlap_between)

distinct_overlap <- control_test_overlap %>% 
  filter(label==1|distinct) %>% ungroup() %>%
  mutate(label=1:n()) %>%
  select(id, first_intersection, label)
control_test_overlap <- control_test_overlap %>% select(!label) %>% left_join(distinct_overlap)
control_test_overlap <- control_test_overlap %>% fill(label, .direction="down") %>% group_by(id, label) %>% 
  mutate(start=min(first_intersection)%>%as.Date(), end=max(last_intersection)%>%as.Date())

control_test_overlap %>% 
  select(id, id_treat, subclass, intersection, start, end, label) %>% head(20)

control_testing_unique <- control_test_overlap %>% group_by(id, label) %>% summarise_all(first)
control_time_test <- control_testing_unique %>% group_by(id, label) %>% 
  add_testing() %>% 
  filter_testing() %>% 
  mutate(survival_time=as.numeric(Day-start)+1) %>% 
  mutate(last = ifelse(any(Result=="Positive",na.rm=T)&Result=="Positive", Result=="Positive", Day==last(Day))) %>%
  mutate(pos_first=first(Result)=="Positive") %>%
  filter(!has_test|last) %>% summarise_all(first) %>% select(!last) %>% 
  mutate(survival_time=ifelse(!has_test|Result!="Positive", as.numeric(end-start)+1, survival_time))

control_time_test <- control_time_test %>% select(!c(id_treat, first_intersection, last_intersection, next_end, next_start, overlap_first, overlap_between, distinct))


full <- treatment_time_test %>% 
  select(id, treatment, subclass, weights, BuildingId, primary, secondary, vacc.primary, vacc.secondary, inf.primary, inf.secondary, start, end, survival_time, Result) %>% 
  rbind(control_time_test %>% select(id, treatment, subclass, weights, BuildingId, primary, secondary,  vacc.primary, vacc.secondary, inf.primary, inf.secondary, start, end, survival_time, Result))

full <- full %>% group_by(subclass) %>% filter(any(treatment==1)&any(treatment==0))
full <- full %>% mutate(treatment=1-treatment)
full$treatment%>%table()

full <- full %>% 
  mutate(status=ifelse(!Result%>%is.na()&Result=="Positive", 1, 0), 
         BuildingId=as.factor(BuildingId),
         subclass=as.factor(subclass)) 

full%>%
  group_by(treatment)%>%
  summarise(units=length(unique(id)), cases=sum(status), person_time=n()*mean(survival_time))%>%
  mutate(inc_rate=cases/person_time)

# full <- full %>% mutate(time_since_start=as.numeric(difftime(start, "2021-12-15", units="days")))
full <- full %>% group_by(treatment) %>% mutate(n=1:n()) %>% mutate(n=ifelse(treatment==0, n, NA))
full <- full %>% arrange(subclass, start) %>% ungroup() %>% 
  fill(n, .direction="down") %>%
  group_by(n) %>% 
  mutate(time_since_start_rel=start-first(start))

infections <- testing %>% group_by(ResidentId, num_pos) %>% 
  summarise_all(first) %>% filter(!is.na(num_pos)) %>% select(!Result)
full_inf <- full %>% 
  left_join(infections, by=c("primary"="ResidentId")) %>% 
  rename("infDay.primary"="Day") %>%
  group_by(id) 

full_inf_clean <- full_inf %>%
  filter(infDay.primary <= start) %>% 
  arrange(id, desc(infDay.primary)) %>% 
  summarise_all(first) %>% 
  mutate(time_since_inf.primary=difftime(start, infDay.primary, units="days")%>%as.numeric())
full_inf_clean     

full_inf_clean <- full_inf_clean %>%
  left_join(infections, by=c("secondary"="ResidentId")) %>% 
  rename("infDay.secondary"="Day") %>%
  group_by(id) %>%
  filter(infDay.secondary <= start) %>% 
  arrange(id, desc(infDay.secondary)) %>% 
  summarise_all(first) %>% 
  mutate(time_since_inf.secondary=(difftime(start, infDay.secondary, units="days")%>%as.numeric())/30)
full_inf_clean   

vacc <- read_csv("cleaned_vaccination_data.csv") %>% select(ResidentId, num_dose, Date)
full_inf_clean_vacc <- full_inf_clean %>% 
  left_join(vacc, by=c("primary"="ResidentId", "vacc.primary"="num_dose")) %>% 
  rename("vaccday.primary"="Date")
full_inf_clean_vacc <- full_inf_clean_vacc %>% 
  left_join(vacc, by=c("secondary"="ResidentId", "vacc.secondary"="num_dose")) %>% 
  rename("vaccday.secondary"="Date")
full_inf_clean_vacc <- full_inf_clean_vacc %>% 
  mutate(vaccday.primary=if_else(vacc.primary==0, as.Date("2020-03-01"), vaccday.primary),
         vaccday.secondary=if_else(vaccday.secondary==0, as.Date("2020-03-01"), vaccday.secondary)) %>% 
  mutate(time_since_vacc.primary = ((start-vaccday.primary)%>%as.numeric())/30,
         time_since_vacc.secondary = ((start-vaccday.secondary)%>%as.numeric())/30)


fit <- survfit(Surv(survival_time, status, type="right")~treatment, data = full)
autoplot(fit) + 
  xlab("Time") + ylab("Survival") + 
  scale_fill_discrete(name=element_blank(), label=c("Control", "Treatment")) + 
  guides(color=F)

results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary + time_since_vacc.primary + time_since_inf.primary + time_since_inf.secondary + inf.primary + inf.secondary + BuildingId + frailty(subclass), 
                 data=full_inf_clean_vacc)

tbl_regression(results, exp = TRUE) 

