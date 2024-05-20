# use logistic regression for main model results instead of conditional logistic regression
rm(list=ls())
gc() 

library(tidyverse)
library(readr)
library(survival)

data <- read_csv("D:/CCHCS_premium/st/indirects/case_control_postmatchprocessing.csv")
data <- data %>% mutate(time_since_inf_cut.roommate = factor(time_since_inf_cut.roommate, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))
data <- data %>% mutate(time_since_vacc_cut.roommate = factor(time_since_vacc_cut.roommate, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))
data <- data %>% mutate(time_since_infvacc_cut.roommate = factor(time_since_infvacc_cut.roommate, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))


binary_model <- glm(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + 
                      age + age.roommate + risk + risk.roommate, 
                    data=data,  family = "binomial")


dose_model <- glm(case ~ dose.roommate.adjusted + has.prior.inf.roommate + 
                    age + age.roommate + risk + risk.roommate, 
                  data=data, family="binomial")

dose_model <- (summary(dose_model)$coefficients)[2,]
or <- exp(dose_model[1]*1:4)
low <- exp(dose_model[1]*1:4-1.96*dose_model[2])
high <- exp(dose_model[1]*1:4+1.96*dose_model[2])
dose_results <- cbind(or, low, high)
rownames(dose_results) <- c("dose.1", "dose.2", "dose.3", "dose.4")

dose_results <- dose_results %>% as.data.frame()
dose_results <- dose_results %>% mutate(x=rownames(dose_results))
names(dose_results) <- c("point", "lb", "ub", "x")


inf_model <- glm(case ~ time_since_inf_cut.roommate + has.vacc.roommate.binary + 
                   age + age.roommate + risk + risk.roommate, 
                 data=data, family="binomial")

vacc_model <- glm(case ~ time_since_vacc_cut.roommate + has.prior.inf.roommate + 
                    age + age.roommate + risk + risk.roommate, 
                  data=data, family="binomial")

infvacc_model <- glm(case ~ time_since_infvacc_cut.roommate + 
                       age + age.roommate + risk + risk.roommate, data=data, family="binomial")

rbind(format_results(model)[2:3,], dose_results, format_results(inf_model)[2:5,], format_results(vacc_model)[2:5,], format_results(infvacc_model)[2:5,]) %>%
  write_csv(here::here("results/logistic/full_results.csv"))
