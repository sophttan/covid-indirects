rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)
library(survival)
library(ggfortify)
library(gtsummary)

d <- read_csv("survival_data/allvacc_dose_noincarcreq_priorinf_infvacc071223.csv")
d <- d %>% mutate(InstBuild=as.factor(InstBuild), subclass=as.factor(subclass))
d <- d %>% mutate(vacc.primary.binary=(vacc.primary>0)%>%as.numeric())
d <- d %>% rowwise() %>% 
  mutate(time_since_inf_vacc.primary = min(time_since_inf.primary, time_since_vacc.primary, na.rm=T),
         time_since_inf_vacc.secondary = min(time_since_inf.secondary, time_since_vacc.secondary, na.rm=T))

vacc <- read_csv("cleaned_vaccination_data.csv") %>% select(ResidentId, num_dose, full_vacc)
d <- d %>% left_join(vacc, by=c("primary"="ResidentId", "vacc.primary"="num_dose")) %>%
  mutate(vacc.primary.cat = case_when(vacc.primary==0~"unvacc",
                                      vacc.primary<=full_vacc~"primary",
                                      vacc.primary>full_vacc~"boosted") %>% factor(levels=c("unvacc", "primary", "boosted")))
d %>% select(primary, vacc.primary, vacc.primary.cat, full_vacc)

d %>% ggplot(aes(vacc.primary.cat)) + geom_bar() + 
  scale_x_discrete("Vaccination in primary resident", labels=c("Unvaccinated", "Primary series only", "Boosted"))


# original analysis
results2 <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary +
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   InstBuild + frailty(subclass), 
                 data=d)


### compare InstBuild with little variability
results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary +
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   frailty(subclass), 
                 data=d %>% group_by(InstBuild) %>% filter(length(unique(subclass))==1))
tbl_regression(results,exp=T)

# convergence issues are with buildings where there are no events (no variability in survival status)
# when i remove these buildings and adjust for building, results make sense again
results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary +
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   InstBuild + frailty(subclass), 
                 data=d %>% group_by(InstBuild) %>% filter(all(status==0)))

results <- coxph(Surv(survival_time, status) ~ 
                    treatment + vacc.primary +
                    time_since_inf.primary + time_since_inf.secondary + 
                    age.primary + age.secondary + risk.primary + risk.secondary + 
                    InstBuild + frailty(subclass), 
                  data=d %>% group_by(InstBuild) %>% filter(!all(status==0)))
tbl_regression(results,exp=T)


# remove building adjustment
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

test_hazard <- cox.zph(results)
test_hazard
ggcoxzph(test_hazard)


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
