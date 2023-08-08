rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)
library(survival)
library(ggfortify)
library(gtsummary)
library(survminer)
library(autoReg)


d <- read_csv("survival_data/allvacc_dose_primaryres_infvacc080323.csv")
d <- d %>% mutate(Institution=as.factor(Institution),
                  InstBuild=as.factor(InstBuild), subclass=as.factor(subclass))
d <- d %>% mutate(vacc.primary.binary=(vacc.primary>0)%>%as.numeric())
d <- d %>% rowwise() %>% 
  mutate(time_since_inf_vacc.primary = min(time_since_inf.primary, time_since_vacc.primary, na.rm=T),
         time_since_inf_vacc.secondary = min(time_since_inf.secondary, time_since_vacc.secondary, na.rm=T))


results <- coxph(Surv(survival_time, status) ~ 
                   vacc.primary + vacc.secondary + 
                   age.primary + risk.primary +
                   age.secondary + risk.secondary + 
                   frailty(subclass), 
                 data=d)

tbl_regression(results, exponentiate = T, 
               include = c("vacc.primary", "vacc.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"),
               label = list("vacc.primary"="*Vaccination in primary resident",
                            "vacc.secondary"="Vaccination in secondary resident",
                            "age.primary"="Age (years) of primary resident",
                            "age.secondary"="Age (years) of secondary resident",
                            "risk.primary"="Severe COVID-19 risk score of primary resident",
                            "risk.secondary"="Severe COVID-19 risk score of secondary resident",
                            "frailty(subclass)"="Frailty")) 


results <- coxph(Surv(survival_time, status) ~ 
                   vacc.primary + 
                   age.primary + risk.primary + 
                   frailty(subclass), 
                 data=d)

tbl_regression(results, exponentiate = T, 
               include = c("vacc.primary",
                           "age.primary", "risk.primary", "frailty(subclass)"),
               label = list("vacc.primary"="*Vaccination in primary resident",
                            "age.primary"="Age (years) of primary resident",
                            "risk.primary"="Severe COVID-19 risk score of primary resident",
                            "frailty(subclass)"="Frailty")) 


vacc <- read_csv("cleaned_vaccination_data.csv") %>% select(ResidentId, num_dose, full_vacc)
d <- d %>% left_join(vacc, by=c("primary"="ResidentId", "vacc.primary"="num_dose")) %>%
  mutate(vacc.primary.cat = case_when(vacc.primary==0~"unvacc",
                                      vacc.primary<=full_vacc~"primary",
                                      vacc.primary>full_vacc~"boosted") %>% 
           factor(levels=c("primary", "unvacc", "boosted")))

results <- coxph(Surv(survival_time, status) ~ 
                   vacc.primary.cat + 
                   age.primary + risk.primary + 
                   frailty(subclass), 
                 data=d)

tbl_regression(results, exponentiate = T, 
               include = c("vacc.primary.cat",
                           "age.primary", "risk.primary", "frailty(subclass)"),
               label = list("vacc.primary.cat"="*Vaccination in primary resident",
                            "age.primary"="Age (years) of primary resident",
                            "risk.primary"="Severe COVID-19 risk score of primary resident",
                            "frailty(subclass)"="Frailty")) 

d <- d %>% mutate(recent_vacc = (vacc.primary>0&time_since_vacc.primary<=6)%>%as.numeric())
results <- coxph(Surv(survival_time, status) ~ 
                   recent_vacc + 
                   age.primary + risk.primary + 
                   frailty(subclass), 
                 data=d)

tbl_regression(results, exponentiate = T, 
               include = c("recent_vacc",
                           "age.primary", "risk.primary", "frailty(subclass)"),
               label = list("recent_vacc"="*Vaccination in primary resident",
                            "age.primary"="Age (years) of primary resident",
                            "risk.primary"="Severe COVID-19 risk score of primary resident",
                            "frailty(subclass)"="Frailty")) 

