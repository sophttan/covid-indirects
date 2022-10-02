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

res <- make_res(run_regression(read_csv("matched_data_alternative_cal_propensity.csv")), "Caliper of propensity score = 0.1") 
res <- rbind(res, 
             make_res(run_regression(read_csv("matched_data_alternative_cal_days.csv")), "Caliper of days between index cases = 15") )
res <- rbind(res, 
             make_res(run_regression(read_csv("matched_data_alternative1_4ratio.csv")), "1:4 matching") )
res <- rbind(res, 
             make_res(run_regression(read_csv("matched_data_alternative_weights_1p_3d.csv")), "1:3 ratio") )
res <- rbind(res, 
             make_res(run_regression(read_csv("matched_data_alternative_weights_3p_1d.csv")), "3:1 ratio") )
res <- rbind(res, 
             make_res(run_regression(read_csv("matched_data_alternative_nopropensity.csv")), "Matching without propensity score") )
res <- rbind(res, 
             make_res(run_regression(read_csv("matched_data_alternative_propensity_sex_race.csv")), "Matching with propensity score with sex and race") )


# no matching
nomatch <- read_csv("unmatched_all_covariates092922.csv")
nomatch <- nomatch %>% mutate(Institution=as.factor(Institution),
                                       index_id=as.factor(index_id))
model <- glm(contact_status ~ index_prior_vacc + index_prior_inf + num_days_in_contact+
               num_vacc_doses+has_prior_inf+incidence_log+Institution, data=nomatch, family="poisson")
summary(model)

library(sandwich)
library(lmtest)
model <- coeftest(model, vcov = sandwich)[2:3,]
res_nomatch <- data.frame(group="No matching", 
                          relrisk=100*(exp(model[,1])-1), 
                          lb=100*(exp(model[,1]-2*model[,2])-1),
                          ub=100*(exp(model[,1]+2*model[,2])-1), row.names=NULL)

res <- rbind(res,res_nomatch) %>% 
  mutate(covariate=rep(c("Prior vaccination", "Prior infection"), 8)) %>% 
  select(1,5,2,3,4)

res_tbl <- res %>% mutate(res = paste0(round(relrisk, 1), " (", round(lb, 1), ", ", round(ub, 1), ")"))
res_tbl <- res_tbl %>% mutate(header=c("Varying choice of caliper","","","",
                                      "Varying k in 1:k matching","",
                                      "Varying weights between propensity score and time","","","",
                                      "Overall changes to propensity score","","","",
                                      "No matching",""))
res_tbl <- res_tbl %>% select(!c(3,4,5)) %>% select(4,1:3)
names(res_tbl) <- c("","Matching specification", "Index case", "Relative % change in attack rate of infection in close contact (95% CI)")


write_csv(res_tbl, "/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/tables/alternative_matching_specs_s6.csv")
