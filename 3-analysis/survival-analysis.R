rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)
library(survival)
library(ggfortify)
library(gtsummary)

d <- read_csv("survival_data/allvacc_priorinf_infvacc071023.csv") %>% 
  mutate(time_since_vacc.primary=ifelse(vacc.primary==0, NA, time_since_vacc.primary)) %>%
  filter(vacc.primary==1)

d <- d %>% mutate(time_since_vacc.primary.cat=cut(time_since_vacc.primary, right=F, seq(0,30,3)))
d%>%
  group_by(time_since_vacc.primary.cat)%>%
  summarise(units=n(), cases=sum(status), person_time=n()*mean(survival_time))%>%
  mutate(inc_rate=cases/person_time*100000)

fit <- survfit(Surv(survival_time, status, type="right")~time_since_vacc.primary.cat, data = d)
autoplot(fit) + 
  xlab("Time (days)") + ylab("Survival (no SARS-CoV-2 infection)") + 
  scale_fill_discrete(name=element_blank()) + 
  guides(color=F)

results <- coxph(Surv(survival_time, status) ~ 
                   time_since_vacc.primary + 
                   frailty(subclass), 
                 data=d)

results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary + time_since_vacc.primary + 
                   inf.primary + inf.secondary + BuildingId + frailty(subclass), 
                 data=d)
tbl_regression(results, exponentiate = T, 
               include = c("treatment", "vacc.primary", 
                           "time_since_vacc.primary",
                           "inf.primary", "inf.secondary",
                           "frailty(subclass)"),
               label = list("treatment"="Vaccination in secondary resident",
                            "vacc.primary"="Vaccination in primary resident",
                            "time_since_vacc.primary"="Months since most recent vaccination in primary resident",
                            "inf.primary"="Prior infection in primary resident",
                            "inf.secondary"="Prior infection in secondary resident",
                            "frailty(subclass)"="Frailty")) 

results <- coxph(Surv(survival_time, status) ~ 
                   treatment + time_since_vacc.primary + 
                   #inf.primary + inf.secondary +
                   age.primary + age.secondary + 
                   risk.primary + risk.secondary + BuildingId + frailty(subclass), 
                 data=d)

test_hazard <- cox.zph(results)
ggcoxzph(test_hazard)
test_hazard

results_summary <- summary(results)%>%coef()%>%as.data.frame()%>%
  filter(!grepl("Building", row.names(.))) %>% 
  mutate(lb=(coef-2*`se(coef)`)%>%exp(), ub=(coef+2*`se(coef)`)%>%exp(), coef=coef%>%exp()) %>%
  select(coef, lb, ub, p)
results_summary

tbl_regression(results, exponentiate = T, 
               include = c("treatment", "vacc.primary", 
                           "inf.primary", "inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"),
               label = list("treatment"="Vaccination in secondary resident",
                            "vacc.primary"="Vaccination in primary resident",
                            "age.primary"="Age (years) of primary resident",
                            "age.secondary"="Age (years) of secondary resident",
                            "risk.primary"="Severe COVID-19 risk score of primary resident",
                            "risk.secondary"="Severe COVID-19 risk score of secondary resident",
                            "frailty(subclass)"="Frailty")) 

