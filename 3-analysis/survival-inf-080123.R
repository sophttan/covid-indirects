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


d <- read_csv("survival_data/allvacc_dose_infvacc08023.csv")

# add time-varying month
# create time dataset
dates <- data.frame(date=seq(as.Date("2021-12-15"), as.Date("2022-12-15"), by=1))
# dates <- dates %>% mutate(day=0:(n()-1), month=floor(day/30.5)) %>% rowwise() %>% mutate(month=min(month, 11))
dates <- dates %>% mutate(day=0:(n()-1), month=floor(day/91.5)) %>% rowwise() %>% mutate(month=min(month, 11))

full <- expand.grid(id=d$id, date=dates$date) %>% left_join(dates, by="date") 

# join time dataset with data
dtime <- d %>% left_join(full, by="id") %>% 
  filter(date>=final_start&date<=final_end) # keep only days included in eligible observation period

dtime_filtered <- dtime %>% select(!c(date, day)) %>% 
  mutate(survival_time=survival_time-1) %>% # fix survival time estimate (1 day too long)
  group_by(id) %>% mutate(survival_time1=0:(n()-1)) %>% filter(survival_time1<=survival_time) %>% # remove days after censoring
  group_by(id, month) %>% mutate(time1=first(survival_time1), time2=min(last(survival_time1)+1, first(survival_time))) %>% 
  distinct(id, month, .keep_all = T)

dtime_filtered <- dtime_filtered %>%
  group_by(id) %>%
  mutate(status=if_else(time2!=first(survival_time), 0, status)) %>% 
  filter(time1<time2)

dtime_filtered <- dtime_filtered %>% mutate(month=factor(month, levels=0:3),
                                            Institution=as.factor(Institution),
                                            InstBuild=as.factor(InstBuild), 
                                            subclass=as.factor(subclass))

dtime_filtered <- dtime_filtered %>% mutate(vacc.primary.binary=(vacc.primary>0)%>%as.numeric())
# d <- d %>% rowwise() %>% 
#   mutate(time_since_inf_vacc.primary = min(time_since_inf.primary, time_since_vacc.primary, na.rm=T),
#          time_since_inf_vacc.secondary = min(time_since_inf.secondary, time_since_vacc.secondary, na.rm=T))
# 

vacc <- read_csv("cleaned_vaccination_data.csv") %>% select(ResidentId, num_dose, full_vacc)
dtime_filtered <- dtime_filtered %>% left_join(vacc, by=c("primary"="ResidentId", "vacc.primary"="num_dose")) %>%
  mutate(vacc.primary.cat = case_when(vacc.primary==0~"unvacc",
                                      vacc.primary<=full_vacc~"primary",
                                      vacc.primary>full_vacc~"boosted") %>% factor(levels=c("unvacc", "primary", "boosted")))

clean_results <- function(results) {
  summary(results)%>%coef()%>%as.data.frame()%>%
    mutate(lb=(coef-2*`se(coef)`)%>%exp(), ub=(coef+2*`se(coef)`)%>%exp(), coef=coef%>%exp()) %>%
    select(coef, lb, ub, p) %>% round(4)
}

ggsurvplot(fit  = survfit(Surv(time1, time2, status) ~ vacc.primary.binary, dtime_filtered), 
           fun = "cloglog")
ggsurvplot(fit  = survfit(Surv(time1, time2, status) ~ vacc.primary.cat, dtime_filtered), 
           fun = "cloglog")

results <- coxph(Surv(time1, time2, status) ~ 
                   treatment + vacc.primary + tt(vacc.primary) + 
                   inf.primary + inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary +
                   month + Institution + frailty(subclass), 
                 data=dtime_filtered, tt=function(x,t,...){x*t/30.417})
clean_results(results)
tbl_regression(results, exponentiate = T, 
               include = c("treatment", "vacc.primary", 
                           "inf.primary", "inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"))
               
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
                   frailty(subclass), 
                 data=d)
tbl_regression(results, exponentiate = T, 
               include = c("treatment", "vacc.primary", 
                           "inf.primary", "inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"))


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
loglogplot(results)

results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary + 
                   inf.primary + inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   InstBuild + frailty(subclass), 
                 data=d %>% group_by(InstBuild) %>% filter(!all(status==0)))
tbl_regression(results, exponentiate = T, 
               include = c("treatment", "vacc.primary", 
                           "inf.primary", "inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"))
               
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
                   frailty(subclass), 
                 data=d %>% group_by(InstBuild) %>% filter(!all(status==0)))
tbl_regression(results, exponentiate = T, 
               include = c("treatment", "vacc.primary", 
                           "inf.primary", "inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"))

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

d <- d %>% mutate(time_since_start=difftime(final_start, as.Date("2022-12-15"), units = "weeks")%>%as.numeric())

results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary + 
                   inf.primary + inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   time_since_start + frailty(subclass), 
                 data=d %>% group_by(InstBuild) %>% filter(!all(status==0)))

tbl_regression(results, exponentiate = T, 
               include = c("treatment", "vacc.primary", 
                           "inf.primary", "inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary",
                           "time_since_start",
                           "frailty(subclass)"),
               label = list("treatment"="*Vaccination in secondary resident",
                            "vacc.primary"="Vaccination in primary resident",
                            "inf.primary"="Prior infection in primary resident",
                            "inf.secondary"="Prior infection in secondary resident",
                            "age.primary"="Age (years) of primary resident",
                            "age.secondary"="Age (years) of secondary resident",
                            "risk.primary"="Severe COVID-19 risk score of primary resident",
                            "risk.secondary"="Severe COVID-19 risk score of secondary resident",
                            "time_since_start"="Days since start of study period",
                            "frailty(subclass)"="Frailty")) 

test_hazard <- cox.zph(results)
test_hazard
ggcoxzph(test_hazard)


d <- d %>% mutate(susceptible = case_when(inf.primary==0&vacc.primary==0~"Most susceptible",
                                          inf.primary==1&time_since_inf.primary<6~"Not susceptible",
                                          T~"Somewhat susceptible")%>%
                    factor(levels=c("Most susceptible", "Somewhat susceptible", "Not susceptible")))
d%>%group_by(treatment, susceptible) %>% summarise(inc_rate=sum(status)/sum(survival_time)*100000)

fit <- survfit(Surv(survival_time, status, type="right")~treatment + susceptible, data = d)
autoplot(fit) + 
  xlab("Time (days)") + ylab("Survival (no SARS-CoV-2 infection)") + 
  scale_fill_discrete(name=element_blank()) + 
  guides(color=F)

results <- coxph(Surv(survival_time, status) ~ 
                   treatment + susceptible + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   frailty(subclass), 
                 data=d)

tbl_regression(results, exp=T)

results <- coxph(Surv(survival_time, status) ~ 
                   treatment*susceptible + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   frailty(subclass), 
                 data=d)
results


results_summary <- summary(results)%>%coef()%>%as.data.frame()%>%
  filter(!grepl("Build", row.names(.))) %>% 
  mutate(lb=(coef-2*`se(coef)`)%>%exp(), ub=(coef+2*`se(coef)`)%>%exp(), coef=coef%>%exp()) %>%
  select(coef, lb, ub, p)
results_summary$var <- row.names(results_summary) %>% factor(levels=row.names(results_summary))
results_summary$var <- results_summary$var %>% 
  factor(levels=rev(levels(results_summary$var)))
results_summary <- results_summary %>% filter(var!="frailty(subclass)")

results_summary %>% ggplot(aes(coef, var)) + geom_point() + 
  geom_errorbarh(aes(xmin=lb, xmax=ub, height=0.5)) + 
  geom_vline(aes(xintercept=1)) + 
  scale_x_continuous("Hazard Ratio", limits=c(0.1, 1.5), breaks=seq(0.2, 1.4, 0.2)) + 
  scale_y_discrete("Variable")
