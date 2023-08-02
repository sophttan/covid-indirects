rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)
library(survival)
library(ggfortify)
library(gtsummary)

d <- read_csv("survival_data/allvacc_dose_infvacc072023.csv")
d <- d %>% mutate(InstBuild=as.factor(InstBuild), subclass=as.factor(subclass))
d <- d %>% mutate(vacc.primary.binary=(vacc.primary>0)%>%as.numeric())
d <- d %>% rowwise() %>% 
  mutate(time_since_inf_vacc.primary = min(time_since_inf.primary, time_since_vacc.primary, na.rm=T),
         time_since_inf_vacc.secondary = min(time_since_inf.secondary, time_since_vacc.secondary, na.rm=T))

results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary + 
                   inf.primary + inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   frailty(subclass), 
                 data=d)

tbl_regression(results, exponentiate = T, 
               include = c("treatment", "vacc.primary", 
                           "inf.primary", "inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"),
               label = list("treatment"="*Vaccination in secondary resident",
                            "vacc.primary"="Vaccination in primary resident",
                            "inf.primary"="Prior infection in primary resident",
                            "inf.secondary"="Prior infection in secondary resident",
                            "age.primary"="Age (years) of primary resident",
                            "age.secondary"="Age (years) of secondary resident",
                            "risk.primary"="Severe COVID-19 risk score of primary resident",
                            "risk.secondary"="Severe COVID-19 risk score of secondary resident",
                            "frailty(subclass)"="Frailty")) 


results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary + 
                   inf.primary + inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   InstBuild + frailty(subclass), 
                 data=d %>% group_by(InstBuild) %>% filter(!all(status==0)))

tbl_regression(results, exponentiate = T, 
               include = c("treatment", "vacc.primary", 
                           "inf.primary", "inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"),
               label = list("treatment"="*Vaccination in secondary resident",
                            "vacc.primary"="Vaccination in primary resident",
                            "inf.primary"="Prior infection in primary resident",
                            "inf.secondary"="Prior infection in secondary resident",
                            "age.primary"="Age (years) of primary resident",
                            "age.secondary"="Age (years) of secondary resident",
                            "risk.primary"="Severe COVID-19 risk score of primary resident",
                            "risk.secondary"="Severe COVID-19 risk score of secondary resident",
                            "frailty(subclass)"="Frailty")) 

test_hazard <- cox.zph(results)
test_hazard
ggcoxzph(test_hazard)
