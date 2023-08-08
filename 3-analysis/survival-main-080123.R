rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)
library(survival)
library(ggfortify)
library(gtsummary)
library(rms)

d <- read_csv("survival_data/allvacc_dose_noincarcreq_priorinf_infvacc080823.csv")
d <- d %>% mutate(Institution=as.factor(Institution),
                  InstBuild=as.factor(InstBuild), subclass=as.factor(subclass))

d <- d %>% mutate(vacc.primary.binary=(vacc.primary>0)%>%as.numeric())
d <- d %>% mutate(time_since_start=(difftime(final_start, as.Date("2022-12-15"))%>%as.numeric()))
d <- d %>% rowwise() %>% 
  mutate(time_since_inf_vacc.primary = min(time_since_inf.primary, time_since_vacc.primary, na.rm=T),
         time_since_inf_vacc.secondary = min(time_since_inf.secondary, time_since_vacc.secondary, na.rm=T))

vacc <- read_csv("cleaned_vaccination_data.csv") %>% select(ResidentId, num_dose, full_vacc)
d <- d %>% left_join(vacc, by=c("primary"="ResidentId", "vacc.primary"="num_dose")) %>%
  mutate(vacc.primary.cat = case_when(vacc.primary==0~"unvacc",
                                      vacc.primary<=full_vacc~"primary",
                                      vacc.primary>full_vacc~"boosted") %>% factor(levels=c("unvacc", "primary", "boosted")))
d %>% select(primary, vacc.primary, vacc.primary.cat, full_vacc)

dates <- data.frame(date=seq(as.Date("2021-12-15"), as.Date("2022-12-15"), by=1))
dates <- dates %>% mutate(month=format(date, '%Y-%m')) %>% group_by(month) %>% mutate(month_num=cur_group_id()) %>% ungroup() %>% select(!month)
d <- d %>% left_join(dates, by=c("final_start"="date")) %>% rename("start_month"="month_num") %>% 
  left_join(dates, by=c("final_end"="date")) %>% rename("end_month"="month_num")
full <- expand.grid(id=d$id, month_num=dates$month_num%>%unique())
d2 <- d %>% left_join(full, by="id") %>% filter(month_num>=start_month&month_num<=end_month)


d %>% ggplot(aes(vacc.primary.cat)) + geom_bar() + 
  scale_x_discrete("Vaccination in primary resident", labels=c("Unvaccinated", "Primary series only", "Boosted"))

d3 <- d %>% group_by(Institution) %>% filter(!all(status==0))


# for plotting log-log plots
ggsurvplot(fit  = survfit(Surv(survival_time, status) ~ treatment, d), 
           fun = "cloglog")
ggsurvplot(fit  = survfit(Surv(survival_time, status) ~ vacc.primary, d), 
           fun = "cloglog")
ggsurvplot(fit  = survfit(Surv(survival_time, status) ~ vacc.primary.binary, d), 
           fun = "cloglog")
ggsurvplot(fit  = survfit(Surv(survival_time, status) ~ vacc.primary.cat, d), 
           fun = "cloglog")

# original analysis
results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary +
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   InstBuild + frailty(subclass), 
                 data=d)

tbl_regression(results,exp=T, 
               include = c("treatment", "vacc.primary", 
                           "time_since_inf.primary", "time_since_inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"))


### compare InstBuild with little variability
results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary +
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   InstBuild + frailty(subclass), 
                 data=d %>% group_by(InstBuild) %>% filter(length(unique(subclass))==1))
tbl_regression(results,exp=T, 
               include = c("treatment", "vacc.primary", 
                           "time_since_inf.primary", "time_since_inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"))


# convergence issues are with buildings where there are no events (no variability in survival status)
# when i remove these buildings and adjust for building, results make sense again
results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary +
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   frailty(subclass), 
                 data=d %>% group_by(InstBuild) %>% filter(all(status==0)))
tbl_regression(results,exp=T, 
               include = c("treatment", "vacc.primary", 
                           "time_since_inf.primary", "time_since_inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"))

results <- coxph(Surv(survival_time, status) ~ 
                    treatment + vacc.primary +
                    time_since_inf.primary + time_since_inf.secondary + 
                    age.primary + age.secondary + risk.primary + risk.secondary + 
                    InstBuild + frailty(subclass), 
                  data=d %>% group_by(InstBuild) %>% filter(!all(status==0)))
tbl_regression(results,exp=T, 
               include = c("treatment", "vacc.primary", 
                           "time_since_inf.primary", "time_since_inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"))


# remove building adjustment
# simple anova test shows more complex model with building adjustment is not a significatnly better predictor (null)
results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary +
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   frailty(subclass), 
                 data=d)
tbl_regression(results, exponentiate = T, 
               include = c("treatment", "vacc.primary", 
                           "time_since_inf.primary", "time_since_inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"),
               label = list("treatment"="*Vaccination in secondary resident",
                            "vacc.primary"="Vaccination in primary resident",
                            "time_since_inf.primary"="Months since most recent infection in primary resident",
                            "time_since_inf.secondary"="Months since most recent infection in secondary resident",
                            "age.primary"="Age (years) of primary resident",
                            "age.secondary"="Age (years) of secondary resident",
                            "risk.primary"="Severe COVID-19 risk score of primary resident",
                            "risk.secondary"="Severe COVID-19 risk score of secondary resident",
                            "frailty(subclass)"="Frailty")) 

results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary +
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + Institution + 
                   frailty(subclass), 
                 data=d)
results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary +
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + Institution + 
                   frailty(subclass), 
                 data=d %>% group_by(InstBuild) %>% filter(any(status==1)&any(status==0)))

results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary +
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + Institution + 
                   frailty(subclass), 
                 data=d %>% group_by(InstBuild) %>% filter(any(status==1)&any(status==0)))
tbl_regression(results,exp=T, 
               include = c("treatment", "vacc.primary", 
                           "time_since_inf.primary", "time_since_inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"))

test_hazard <- cox.zph(results)
test_hazard
ggcoxzph(test_hazard)

loglogplot(results)

# adjust for time since start
results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary +
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   Institution + time_since_start + frailty(subclass), 
                 data=d)
tbl_regression(results, exponentiate = T, 
               include = c("treatment", "vacc.primary", 
                           "time_since_inf.primary", "time_since_inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", 
                           "time_since_start",
                           "frailty(subclass)"),
               label = list("treatment"="*Vaccination in secondary resident",
                            "vacc.primary"="Vaccination in primary resident",
                            "time_since_inf.primary"="Months since most recent infection in primary resident",
                            "time_since_inf.secondary"="Months since most recent infection in secondary resident",
                            "age.primary"="Age (years) of primary resident",
                            "age.secondary"="Age (years) of secondary resident",
                            "risk.primary"="Severe COVID-19 risk score of primary resident",
                            "risk.secondary"="Severe COVID-19 risk score of secondary resident",
                            "time_since_start"="Days since start of study period",
                            "frailty(subclass)"="Frailty")) 


# adjust for institution instead for better sample size
results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary +
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   Institution + frailty(subclass), 
                 data=d)
tbl_regression(results,exp=T, 
               include = c("treatment", "vacc.primary", 
                           "time_since_inf.primary", "time_since_inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"))

# adjust for institution and add time-dependent covariate of time since omicron start
results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary +
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + tt(time_since_start) + 
                   Institution + frailty(subclass), 
                 data=d, tt=function(x,t,...){time_since_start<-x+t})
tbl_regression(results,exp=T, 
               include = c("treatment", "vacc.primary", 
                           "time_since_inf.primary", "time_since_inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"))




# what about recent vaccination
recent_vacc <- d %>% filter(time_since_vacc.primary<6|vacc.primary==0) #%>% group_by(subclass) %>% filter(n()>1) # %>% filter(vacc.primary.cat!="primary") %>%
  # mutate(vacc.primary.cat=factor(vacc.primary.cat, levels=c("unvacc", "boosted")))
recent_vacc %>% select(primary, vacc.primary, vacc.primary.cat, time_since_vacc.primary)

recent_vacc %>% ggplot(aes(vacc.primary.cat)) + geom_bar() + 
  scale_x_discrete("Vaccination in primary resident", labels=c("Unvaccinated", "Boosted"))
recent_vacc %>% ggplot(aes(vacc.primary)) + geom_bar() + 
  scale_x_continuous("Vaccination in primary resident", breaks=0:6, labels=0:6)

recent_vacc %>% filter(vacc.primary>0) %>% ggplot(aes(time_since_vacc.primary)) + geom_histogram() + 
  scale_x_continuous("Time since most recent vaccine in primary resident")

recent_vacc <- recent_vacc %>% mutate(InstBuild=as.factor(InstBuild), subclass=as.factor(subclass))
results <- coxph(Surv(survival_time, status) ~ 
                   vacc.primary.binary,
                   #treatment + 
                   #time_since_inf.primary + time_since_inf.secondary + 
                   #age.primary + age.secondary + risk.primary + risk.secondary + 
                   #InstBuild + frailty(subclass), 
                 data=recent_vacc)

tbl_regression(results, exp=T)

fit <- survfit(Surv(survival_time, status, type="right")~vacc.primary.cat, data = recent_vacc)
autoplot(fit) + 
  xlab("Time (days)") + ylab("Survival (no SARS-CoV-2 infection)") + 
  scale_fill_discrete(name=element_blank()) + 
  guides(color=F)

fit <- survfit(Surv(survival_time, status, type="right")~treatment, data = recent_vacc)
autoplot(fit) + 
  xlab("Time (days)") + ylab("Survival (no SARS-CoV-2 infection)") + 
  scale_fill_discrete(name=element_blank()) + 
  guides(color=F)

recent_vacc %>% group_by(vacc.primary.binary) %>% summarise(inc_rate=sum(status)/sum(survival_time)*100000)


results <- coxph(Surv(survival_time, status) ~ 
                   vacc.primary.binary + frailty(subclass), 
                 data=recent_vacc)

tbl_regression(results, exponentiate = T, 
               include = c("vacc.primary.binary"),
               label = list("vacc.primary.binary"="Vaccination in primary resident",
                            # "time_since_inf.primary"="Months since most recent infection in primary resident",
                            # "time_since_inf.secondary"="Months since most recent infection in secondary resident",
                            # "age.primary"="Age (years) of primary resident",
                            # "age.secondary"="Age (years) of secondary resident",
                            # "risk.primary"="Severe COVID-19 risk score of primary resident",
                            # "risk.secondary"="Severe COVID-19 risk score of secondary resident",
                            "frailty(subclass)"="Frailty")) 

results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary.cat +
                   time_since_inf.primary + time_since_inf.secondary +
                   age.primary + age.secondary + risk.primary + risk.secondary +
                   frailty(subclass),
                 data=recent_vacc)

tbl_regression(results, exponentiate = T, 
               include = c("treatment", "vacc.primary.cat", "frailty(subclass)",
                           "time_since_inf.primary", "time_since_inf.secondary",
                           "age.primary", "age.secondary", "risk.primary", "risk.secondary", "frailty(subclass)"),
               label = list("treatment"="*Vaccination in secondary resident",
                            "vacc.primary.cat"="Vaccination in primary resident",
                            "time_since_inf.primary"="Months since most recent infection in primary resident",
                            "time_since_inf.secondary"="Months since most recent infection in secondary resident",
                            "age.primary"="Age (years) of primary resident",
                            "age.secondary"="Age (years) of secondary resident",
                            "risk.primary"="Severe COVID-19 risk score of primary resident",
                            "risk.secondary"="Severe COVID-19 risk score of secondary resident",
                            "frailty(subclass)"="Frailty")) 

recent_vacc %>% group_by(treatment, vacc.primary.cat) %>% summarise(inc_rate=sum(status)/sum(survival_time)*100000)
