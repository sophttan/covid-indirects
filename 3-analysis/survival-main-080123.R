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
ggcoxfunctional(Surv(time1, time2, status) ~ time_since_inf.primary + time_since_inf.secondary + 
                  +                   age.primary + age.secondary + 
                  +                   risk.primary + risk.secondary, data=dtime_filtered, ylim=c(0.5, 1))
d <- read_csv("survival_data/allvacc_dose_noincarcreq_priorinf_infvacc081423.csv")

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
dtime_filtered <- dtime_filtered %>% left_join(vacc, by=c("primary"="ResidentId", "vacc.primary.doses"="num_dose")) %>%
  mutate(vacc.primary.cat = case_when(vacc.primary.doses==0~"unvacc",
                                      vacc.primary.doses<=full_vacc~"primary",
                                      vacc.primary.doses-full_vacc==1~"boosted1",
                                      T~"boosted2+") %>% factor(levels=c("unvacc", "primary", "boosted1", "boosted2+")))

clean_results <- function(results) {
  summary(results)%>%coef()%>%as.data.frame()%>%
    mutate(lb=(coef-2*`se(coef)`)%>%exp(), ub=(coef+2*`se(coef)`)%>%exp(), coef=coef%>%exp()) %>%
    select(coef, lb, ub, p) %>% round(4)
}


dtime_filtered <- dtime_filtered %>% mutate(obs_time=time2-time1)

# exploring ph violation
dtime_filtered %>% distinct(id, .keep_all = T) %>% ggplot(aes(vacc.primary.cat)) + geom_bar() + 
  scale_x_discrete("Vaccination in primary resident", labels=c("Unvaccinated", "Primary series only", "1 Booster", "2+ Boosters"))

# for plotting log-log plots
ggsurvplot(fit  = survfit(Surv(time1, time2, status) ~ treatment, dtime_filtered), 
           fun = "cloglog")
ggsurvplot(fit  = survfit(Surv(time1, time2, status) ~ vacc.primary, dtime_filtered), 
           fun = "cloglog")
ggsurvplot(fit  = survfit(Surv(time1, time2, status) ~ vacc.primary.binary, dtime_filtered), 
           fun = "cloglog")

dtime_filtered <- dtime_filtered%>%cbind(model.matrix(~-1+vacc.primary.cat, data=dtime_filtered)[,2:3])
results <- coxph(Surv(time1, time2, status) ~ 
                   treatment + vacc.primary.catprimary + vacc.primary.catboosted +
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   month + Institution +
                   frailty(subclass), 
                 data=dtime_filtered, 
                 tt=list(function(x,t,...){time_since_inf.primary<-x+t/30.417}, 
                         function(x,t,...){time_since_inf.secondary<-x+t/30.417}))
clean_results(results)

results <- coxph(Surv(time1, time2, status) ~ 
                   treatment + vacc.primary + 
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   month + Institution +
                   frailty(subclass), 
                 data=dtime_filtered, 
                 tt=list(function(x,t,...){time_since_inf.primary<-x+t/30.417}, 
                         function(x,t,...){time_since_inf.secondary<-x+t/30.417}))
clean_results(results)
cox.zph(results)

results <- coxph(Surv(time1, time2, status) ~ 
                   treatment + vacc.primary + tt(vacc.primary) + 
                   tt(time_since_inf.primary) + tt(time_since_inf.secondary) + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   month + Institution +
                   frailty(subclass), 
                 data=dtime_filtered, 
                 tt=list(function(x,t,...) x*t/30.417,
                         function(x,t,...){time_since_inf.primary<-x+t/30.417}, 
                         function(x,t,...){time_since_inf.secondary<-x+t/30.417}))
clean_results(results)

# adjust for month only
results <- coxph(Surv(time1, time2, status) ~ 
                   treatment + vacc.primary + 
                   tt(time_since_inf.primary) + tt(time_since_inf.secondary) + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   month + 
                   frailty(subclass), 
                 data=dtime_filtered %>% group_by(month) %>% filter(any(status==1)), 
                 tt=list(function(x,t,...){time_since_inf.primary<-x+t/30.417}, 
                         function(x,t,...){time_since_inf.secondary<-x+t/30.417}))
clean_results(results)
              
# adjust for month and institution 
results <- coxph(Surv(time1, time2, status) ~ 
                   treatment + vacc.primary +
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   month + Institution +
                   frailty(subclass), 
                 data=dtime_filtered %>% group_by(Institution) %>% filter(any(status==1)) %>% 
                   group_by(month) %>% filter(any(status==1)), 
                 tt=list(function(x,t,...){time_since_inf.primary<-x+t/30.417}, 
                         function(x,t,...){time_since_inf.secondary<-x+t/30.417}))
clean <- clean_results(results)
clean <- clean %>% 
  mutate(var=row.names(.)) %>% filter(!grepl("Institution|month|frailty", var)) %>%
  mutate(group=c("secondary", "primary", "primary", "secondary", "primary", "secondary", "primary", "secondary"))
clean$var <- factor(clean$var, 
                    levels=c("treatment", "vacc.primary", 
                             "tt(time_since_inf.seconda", "age.secondary", "risk.secondary",
                             "tt(time_since_inf.primary", "age.primary", "risk.primary"), 
                    labels=c("Vaccination", 
                             "Vaccination",
                             "Time since last infection (months)",
                             "Age (years)",
                             "Severe COVID-19 risk",
                             "Time since last infection (months)",
                             "Age (years)",
                             "Severe COVID-19 risk"))
clean$var <- clean$var %>% 
  factor(levels=rev(levels(clean$var)))

clean %>% ggplot(aes(coef, var)) + geom_point() + 
  geom_errorbarh(aes(xmin=lb, xmax=ub, height=0.5)) + 
  geom_vline(aes(xintercept=1)) + 
  scale_x_continuous("Hazard Ratio", expand = c(0.05, 0)) + 
  scale_y_discrete(element_blank()) + 
  facet_wrap(~group) # ADDED

# primary and secondary vacc as binary
results <- coxph(Surv(time1, time2, status) ~ 
                   treatment + vacc.primary.binary +
                   tt(time_since_inf.primary) + tt(time_since_inf.secondary) + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   month + Institution +
                   frailty(subclass), 
                 data=dtime_filtered %>% group_by(Institution) %>% filter(any(status==1)) %>% 
                   group_by(month) %>% filter(any(status==1)), 
                 tt=list(function(x,t,...){time_since_inf.primary<-x+t/30.417}, 
                         function(x,t,...){time_since_inf.secondary<-x+t/30.417}))
clean_results(results)

# primary vacc categorical
results <- coxph(Surv(time1, time2, status) ~ 
                   treatment + vacc.primary.cat +
                   tt(time_since_inf.primary) + tt(time_since_inf.secondary) + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   month + Institution +
                   frailty(subclass), 
                 data=dtime_filtered %>% group_by(Institution) %>% filter(any(status==1)) %>% 
                   group_by(month) %>% filter(any(status==1)), 
                 tt=list(function(x,t,...){time_since_inf.primary<-x+t/30.417}, 
                         function(x,t,...){time_since_inf.secondary<-x+t/30.417}))
clean_results(results)

# treatment as number of doses
results <- coxph(Surv(time1, time2, status) ~ 
                   vacc.secondary + vacc.primary +
                   tt(time_since_inf.primary) + tt(time_since_inf.secondary) + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   month + Institution +
                   frailty(subclass), 
                 data=dtime_filtered %>% group_by(Institution) %>% filter(any(status==1)) %>% group_by(month) %>% filter(any(status==1)), 
                 tt=list(function(x,t,...){time_since_inf.primary<-x+t/30.417}, 
                         function(x,t,...){time_since_inf.secondary<-x+t/30.417}))
clean_results(results)



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
                   tt(time_since_inf.primary) + tt(time_since_inf.secondary) + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   Institution + frailty(subclass), 
                 data=d, tt=list(function(x,t,...){time_since_inf.primary<-x+t/30.417}, function(x,t,...){time_since_inf.secondary<-x+t/30.417}))
tbl_regression(results,exp=T, 
               include = c("treatment", "vacc.primary", 
                           "tt(time_since_inf.primary", "tt(time_since_inf.secondary",
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
