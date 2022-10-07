# Estimate adjusted relative reduction in infectiousness associated with vaccination and/or prior infection
# using alternative regression model (logistic model)
# Sophia Tan
rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")

library(clubSandwich)
library(tidyverse)

d <- read_csv("matched_data_ps100722.csv")

d <- d %>% mutate(Institution=as.factor(Institution),
                  index_id=as.factor(index_id))

model <- glm(contact_status ~ index_prior_vacc + index_prior_inf +
               num_vacc_doses+has_prior_inf+incidence_log+Institution, data=d, weights=weights, 
             family=binomial(link = "logit"))
summary(model)

model <- coef_test(model, vcov = "CR2", cluster = d$subclass) %>% data.frame(row.names=NULL)
model

# function to estimate relative reduction in odds ratio from coefficients
est_odds <- function(data) {
  data %>% mutate(risk_red = round(100*(exp(beta*x)-1), 1),
                  lb = round(100*(exp(beta*x-2*SE*x)-1), 1),
                  ub = round(100*(exp(beta*x+2*SE*x)-1), 1))
}

format_table <- function(data) {
  data %>% 
    mutate(risk_red = paste0(risk_red, " (", lb, ", ", ub, ")")) %>%
    select(Coef, risk_red)
}

results <- model[2:3,] %>% mutate(x=1)
results_tbl <- results %>% est_odds()
results_tbl <- results_tbl %>% format_table()
results_tbl

results_tbl$Coef <- c("Prior vaccination", "Prior infection")
names(results_tbl) <- c("","Relative reduction in odds ratio of infection in close contact (95% CI)")
results_tbl %>% write_csv("/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/tables/logistic_regression_s8.csv")
