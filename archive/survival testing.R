rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)
library(survival)
library(ggfortify)
library(gtsummary)

m_adjusted <- read_csv("matching_data_051923/matching_data_allvacc_noincarcreq_infvacc070723.csv") %>%
  rbind(read_csv("matching_data_051923/matching_data_allvacc_noincarcreq_infvacc_set2070723.csv")) %>% 
  rbind(read_csv("matching_data_051923/matching_data_allvacc_noincarcreq_infvacc_set3070723.csv"))
m_adjusted <- m_adjusted %>% mutate(id=1:nrow(.)) %>%
  group_by(Institution, subclass) %>% 
  
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

#vacc.primary, vacc.secondary,
full <- treatment_time_test %>% 
  select(id, treatment, subclass, weights, BuildingId, primary, secondary, vacc.primary, vacc.secondary, inf.primary, inf.secondary, start, end, survival_time, Result) %>% 
  rbind(control_time_test %>% select(id, treatment, subclass, weights, BuildingId, primary, secondary, vacc.primary, vacc.secondary, inf.primary, inf.secondary, start, end, survival_time, Result)) %>%
  mutate(intersection=interval(start, end))

full <- full %>% group_by(subclass) %>% filter(any(treatment==1)&any(treatment==0))
full <- full %>% mutate(treatment=1-treatment)
full$treatment%>%table()

full <- full %>% 
  mutate(status=ifelse(!Result%>%is.na()&Result=="Positive", 1, 0), 
         BuildingId=as.factor(BuildingId),
         subclass=as.factor(subclass)) 

full%>%
  group_by(treatment)%>%
  summarise(units=n(), cases=sum(status), person_time=n()*mean(survival_time))%>%
  mutate(inc_rate=cases/person_time*100000)

# full <- full %>% mutate(time_since_start=as.numeric(difftime(start, "2021-12-15", units="days")))
full <- full %>% group_by(treatment) %>% mutate(n=1:n()) %>% mutate(n=ifelse(treatment==0, n, NA))
full <- full %>% arrange(subclass, start) %>% ungroup() %>% 
  fill(n, .direction="down") %>%
  group_by(n) %>% 
  mutate(time_since_start_rel=start-first(start))

results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary + inf.primary + inf.secondary + BuildingId + frailty(subclass), 
                 data=full)
tbl_regression(results, exponentiate = T) 

full <- full %>% ungroup() %>% mutate(id2=1:n())

infections <- testing %>% group_by(ResidentId, num_pos) %>% 
  summarise_all(first) %>% filter(!is.na(num_pos)) %>% select(!Result)
full_inf <- full %>% 
  left_join(infections, by=c("primary"="ResidentId")) %>% 
  rename("infDay.primary"="Day") %>%
  group_by(id2) 

full_inf_clean <- full_inf %>%
  filter(infDay.primary <= start|inf.primary==0) %>% 
  arrange(id2, desc(infDay.primary)) %>% 
  summarise_all(first) %>% 
  mutate(time_since_inf.primary=(difftime(start, infDay.primary, units="days")%>%as.numeric())/30.417)
full_inf_clean     

full_inf_clean <- full_inf_clean %>%
  left_join(infections, by=c("secondary"="ResidentId")) %>% 
  rename("infDay.secondary"="Day") %>%
  group_by(id2) %>%
  filter(infDay.secondary <= start|inf.secondary==0) %>% 
  arrange(id2, desc(infDay.secondary)) %>% 
  summarise_all(first) %>% 
  mutate(time_since_inf.secondary=(difftime(start, infDay.secondary, units="days")%>%as.numeric())/30.417)
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
  mutate(time_since_vacc.primary = ((start-vaccday.primary)%>%as.numeric())/30.417,
         time_since_vacc.secondary = ((start-vaccday.secondary)%>%as.numeric())/30.417)

demo <- read_csv("demographic_data_clean.csv")
demo <- demo %>% mutate(age=2022-BirthYear)
demo

full_inf_clean_vacc_demo <- full_inf_clean_vacc %>% left_join(demo, by=c("primary"="ResidentId"))
full_inf_clean_vacc_demo <- full_inf_clean_vacc_demo %>% left_join(demo, by=c("secondary"="ResidentId"), suffix=c(".primary", ".secondary"))
full_inf_clean_vacc_demo

risk <- read_csv("covid_risk_score.csv") %>% 
  filter(ResidentId %in% full_inf_clean_vacc_demo$primary | ResidentId %in% full_inf_clean_vacc_demo$secondary)
intersection <- fix_intersection(risk$interval)
risk$start <- intersection[,1]%>%as.vector()%>%as.Date()
risk$end <- intersection[,2]%>%as.vector()%>%as.Date()
risk <- risk %>% mutate(risk_interval=interval(start=start, end=end)) %>% select(!c(start, end, interval))

full_inf_clean_vacc_demo_risk <- full_inf_clean_vacc_demo %>% left_join(risk, by=c("primary"="ResidentId")) %>% 
  mutate(overlap_risk = intersect(intersection, risk_interval)) %>%
  filter(!is.na(overlap_risk)) %>% 
  mutate(days_risk=time_length(overlap_risk, unit = "day")+1) %>% 
  group_by(id2) %>% 
  mutate(risk.primary=sum(days_risk*Value)/sum(days_risk)) %>% 
  summarise_all(first) %>%
  select(!c(risk_interval, overlap_risk, days_risk, Value))

full_inf_clean_vacc_demo_risk <- full_inf_clean_vacc_demo_risk %>% left_join(risk, by=c("secondary"="ResidentId")) %>% 
  mutate(overlap_risk = intersect(intersection, risk_interval)) %>%
  filter(!is.na(overlap_risk)) %>% 
  mutate(days_risk=time_length(overlap_risk, unit = "day")+1) %>% 
  group_by(id2) %>% 
  mutate(risk.secondary=sum(days_risk*Value)/sum(days_risk)) %>% 
  summarise_all(first) %>%
  select(!c(risk_interval, overlap_risk, days_risk, Value))

fit <- survfit(Surv(survival_time, status, type="right")~treatment, data = full_inf_clean_vacc_demo_risk)
autoplot(fit) + 
  xlab("Time (days)") + ylab("Survival (no SARS-CoV-2 infection)") + 
  scale_fill_discrete(name=element_blank(), label=c("Control", "Treatment")) + 
  guides(color=F)

results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary + 
                   inf.primary + inf.secondary +
                   age.primary + age.secondary + 
                   risk.primary + risk.secondary + BuildingId + frailty(subclass), 
                 data=full_inf_clean_vacc_demo_risk)

test_hazard <- cox.zph(results)
ggcoxzph(test_hazard)
test_hazard

tbl_regression(results, exponentiate = T, 
               include = c("treatment", "vacc.primary", 
                           "inf.primary", "inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"),
               label = list("treatment"="Vaccination in secondary resident",
                            "vacc.primary"="Number of vaccine doses in primary resident",
                            "age.primary"="Age (years) of primary resident",
                            "age.secondary"="Age (years) of secondary resident",
                            "risk.primary"="Severe COVID-19 risk score of primary resident",
                            "risk.secondary"="Severe COVID-19 risk score of secondary resident",
                            "frailty(subclass)"="Frailty")) 

results_summary <- summary(results)%>%coef()%>%as.data.frame()%>%
  filter(!grepl("Building", row.names(.))) %>% 
  mutate(lb=coef-2*`se(coef)`, ub=coef+2*`se(coef)`) %>% exp()

full_inf_clean_vacc_demo_risk%>%
  group_by(inf.primary) %>% 
  summarise(risk.primary=mean(risk.primary), age.primary=mean(age.primary),
            time_since_inf.primary=mean(time_since_inf.primary), time_since_vacc.primary=mean(time_since_vacc.primary))

full_inf_clean_vacc_demo_risk%>%
  group_by(inf.secondary) %>% 
  summarise(risk.secondary=mean(risk.secondary), age.secondary=mean(age.secondary),
            time_since_inf.secondary=mean(time_since_inf.secondary), time_since_vacc.secondary=mean(time_since_vacc.secondary,na.rm=T))

results2 <- coxph(Surv(survival_time, status) ~ 
                    treatment + vacc.primary + age.primary + age.secondary + 
                    risk.primary + risk.secondary + time_since_vacc.primary + time_since_inf.secondary + BuildingId + frailty(subclass), 
                  data=full_inf_clean_vacc_demo_risk)

tbl_regression(results2, exponentiate = T, 
               include = c("treatment","age.secondary","risk.secondary","time_since_inf.secondary", 
                           "vacc.primary", "age.primary", "risk.primary", "time_since_inf.primary", "frailty(subclass)"),
               label = list("treatment"="Vaccination in secondary resident",
                            "vacc.primary"="Number of vaccine doses in primary resident",
                            "age.primary"="Age (years) of primary resident",
                            "age.secondary"="Age (years) of secondary resident",
                            "risk.primary"="Severe COVID-19 risk score of primary resident",
                            "risk.secondary"="Severe COVID-19 risk score of secondary resident",
                            "time_since_inf.primary"="Time (months) since last SARS-CoV-2 infection of primary resident",
                            "time_since_inf.secondary"="Time (months) since last SARS-CoV-2 infection of secondary resident",
                            "frailty(subclass)"="Frailty")) 





full_cases <- full_inf_clean_vacc_demo_risk%>%filter(status==1)
full_cases$survival_time
full_cases <- full_cases %>% left_join(infections, by=c("secondary"="ResidentId")) 
full_cases <- full_cases %>% group_by(id2) %>% mutate(has_inf=any(Day>=start&Day<=end)) %>%
  filter((!has_inf&Day==first(Day))|(Day>=start&Day<=end))
full_cases <- full_cases %>% mutate(Day=if_else(!has_inf, as.Date(NA), Day)) %>% 
  mutate(survival_time.secondary=(Day-start+1)%>%as.numeric()) %>% 
  select(treatment, id2, id, primary, secondary, start, end, survival_time, survival_time.secondary, Day)
full_cases %>% filter(survival_time==survival_time.secondary) # 17 cases same day or after secondary resident
full_cases %>% filter(survival_time>survival_time.secondary) # 12 cases same day or after secondary resident
full_cases %>% filter(survival_time<survival_time.secondary) # 13 cases before secondary resident
full_cases %>% filter(survival_time.secondary %>%is.na()) # 119 cases where no infection in secondary resident was detected 

full_cases_test <- full_cases %>% rename("Infection"="Day") %>%
  left_join(testing %>% select(ResidentId, Day, Result), by=c("secondary"="ResidentId")) %>%
  filter((start+survival_time-1-Day)<=5&start+survival_time-1-Day>0)

full_cases %>% group_by(treatment) %>% summarise(diff_days_primary_secondary=mean(survival_time-survival_time.secondary,na.rm=T), prop_na=mean(survival_time.secondary%>%is.na()))
