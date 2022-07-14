# Sophia Tan 
# Unadjusted results of infectiousness under different infectious period definitions

rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")
#setwd("D:/stan5/code_ST")

library(tidyverse)
library(broom)
library(readr)
library(clubSandwich)

# function to estimate relative risks from coefficients
est_relative_risk <- function(data) {
  data %>% mutate(risk_red = round(100*(exp(beta*x)-1), 1),
                  lb = round(100*(exp(beta*x-2*SE*x)-1), 1),
                  ub = round(100*(exp(beta*x+2*SE*x)-1), 1))
}

format_table <- function(data) {
  data %>% 
    mutate(risk_red = paste0(risk_red, " (", lb, ", ", ub, ")")) %>%
    select(Coef, risk_red)
}


# 5 day infectious period beginning 2 days prior to first positive test
matched_2_5 <- read_csv("matched_data_alternative_inf_per2_5.csv")
matched_2_5 <- matched_2_5 %>% mutate(Institution=as.factor(Institution),
                                      index_id=as.factor(index_id))

model <- glm(contact_status ~ index_prior_vacc + index_prior_inf +
               num_vacc_doses+has_prior_inf+incidence_log+Institution, 
             data=matched_2_5, weights=weights, family="poisson")
summary(model)
model <- coef_test(model, vcov = "CR2", cluster = matched_2_5$subclass) %>% data.frame(row.names=NULL)

model <- model[2:3,] %>% mutate(x=1)
res_2_5 <- model %>% est_relative_risk() %>% format_table()


# 7 day infectious period beginning 2 days prior to first positive test
matched_2_7 <- read_csv("matched_data_alternative_inf_per2_7.csv")
matched_2_7 <- matched_2_7 %>% mutate(Institution=as.factor(Institution),
                                      index_id=as.factor(index_id))

model <- glm(contact_status ~ index_prior_vacc + index_prior_inf +
               num_vacc_doses+has_prior_inf+incidence_log+Institution, 
             data=matched_2_7, weights=weights, family="poisson")
summary(model)
model <- coef_test(model, vcov = "CR2", cluster = matched_2_7$subclass) %>% data.frame(row.names=NULL)

model <- model[2:3,] %>% mutate(x=1)
res_2_7 <- model %>% est_relative_risk() %>% format_table()


res <- rbind(res_2_5, res_2_7)
res$Coef <- rep(c("Prior vaccination", "Prior infection"),2)
res$group <- c("Begins 2 days prior to first positive test, duration of 5 days", "",
               "Begins 2 days prior to first positive test, duration of 7 days", "")
res <- res %>% select(group, Coef, risk_red)
names(res) <- c("Infectious period", "Index case", "Relative % change in attack rate of infection in close contact (95% CI)")
res %>% write_csv("/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/tables/alternative_infperiod_s7.csv")
