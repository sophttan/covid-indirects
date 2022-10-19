# Sophia Tan 6/24/22
# Regression under alternative matching specifications

rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")
#setwd("D:/stan5/code_ST")

library(tidyverse)
library(broom)
library(readr)
library(clubSandwich)
library(RColorBrewer)


make_res <- function(model, group, names=NULL) {
  if(!names %>% is.null()){
    model <- model %>% filter(Coef %in% names)
  }
  data.frame(group=group, 
             relrisk=100*(exp(model$beta)-1), 
             lb=100*(exp(model$beta-2*model$SE)-1),
             ub=100*(exp(model$beta+2*model$SE)-1))
}

run_regression <- function(data) {
  data <- data %>% mutate(Institution=as.factor(Institution),
                          index_id=as.factor(index_id))
  model <- glm(contact_status ~ index_prior_vacc + index_prior_inf + num_days_in_contact + 
                 num_vacc_doses+has_prior_inf+incidence_log+Institution, 
               data=data, weights=weights, family="poisson")
  print(summary(model))
  
  model <- coef_test(model, vcov = "CR2", cluster = data$subclass) %>% as.data.frame()
  
  model[2:3,]
}

res <- make_res(run_regression(read_csv("matched_data_nojcovden.csv")), "No J&J") 

res <- res %>% 
  mutate(covariate=rep(c("Prior vaccination", "Prior infection"), 1)) %>% 
  select(1,5,2,3,4)

res_tbl <- res %>% mutate(res = paste0(round(relrisk, 1), " (", round(lb, 1), ", ", round(ub, 1), ")"))
res_tbl <- res_tbl %>% select(!c(3,4,5)) 
names(res_tbl) <- c("", "Index case", "Relative % change in attack rate of infection in close contact (95% CI)")


write_csv(res_tbl, "/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/tables/alternative_nojj.csv")
