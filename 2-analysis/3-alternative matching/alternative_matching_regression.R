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


# 1:4 matched results
matched_1_4 <- read_csv("matched_data_alternative1_4_30.csv")

matched_1_4 <- matched_1_4 %>% mutate(Institution=as.factor(Institution),
                          index_id=as.factor(index_id))
model <- glm(contact_status ~ index_prior_vacc + index_prior_inf +
               num_vacc_doses+has_prior_inf+incidence_log+Institution, 
             data=matched_1_4, weights=weights, family="poisson")
summary(model)

model <- coef_test(model, vcov = "CR2", cluster = matched_1_4$subclass) %>% as.data.frame()

model <- model[2:3,]
res_1_4_30 <- make_res(model, "1:4 matching of index cases by institution and within 30 days") 



# 1:10 matched results
matched_1_10 <- read_csv("matched_data_alternative1_10_15.csv")

matched_1_10 <- matched_1_10 %>% mutate(Institution=as.factor(Institution),
                                      index_id=as.factor(index_id))
model <- glm(contact_status ~ index_prior_vacc + index_prior_inf +
               num_vacc_doses+has_prior_inf+incidence_log+Institution, 
             data=matched_1_10, weights=weights, family="poisson")
summary(model)

model <- coef_test(model, vcov = "CR2", cluster = matched_1_10$subclass) %>% as.data.frame()

model <- model[2:3,]
res_1_10_15 <- make_res(model, "1:10 matching of index cases by institution and within 15 days") 



# no matching
nomatch <- read_csv("unmatched_all_covariates.csv")
nomatch <- nomatch %>% mutate(Institution=as.factor(Institution),
                                       index_id=as.factor(index_id))
model <- glm(contact_status ~ index_prior_vacc + index_prior_inf +
               num_vacc_doses+has_prior_inf+incidence_log+Institution, data=nomatch, family="poisson")
summary(model)

library(sandwich)
library(lmtest)
model <- coeftest(model, vcov = sandwich)[2:3,]
res_nomatch <- data.frame(group="No matching", 
                          relrisk=100*(exp(model[,1])-1), 
                          lb=100*(exp(model[,1]-2*model[,2])-1),
                          ub=100*(exp(model[,1]+2*model[,2])-1), row.names=NULL)

res <- rbind(res_1_10_15, res_1_4_30, res_nomatch) %>% 
  mutate(covariate=rep(c("Prior vaccination", "Prior infection"), 3)) %>% 
  select(1,5,2,3,4)

res_tbl <- res %>% mutate(res = paste0(round(relrisk, 1), " (", round(lb, 1), ", ", round(ub, 1), ")"))
res_tbl <- res_tbl %>% select(!c(3,4,5))
names(res_tbl) <- c("Matching specification", "Index case", "Relative % change in attack rate of infection in close contact (95% CI)")

write_csv(res_tbl, "/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/tables/alternative_matching_specs_s6.csv")
