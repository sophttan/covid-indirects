rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")
#setwd("D:/stan5/code_ST")

library(tidyverse)
library(broom)
library(readr)
library(MatchIt)
library(RColorBrewer)


d <- read_csv("matched_data_ps100722.csv") 

labels <- expand.grid(index_prior_vacc=0:1, index_prior_inf=0:1) %>% 
  mutate(label=c("No prior vaccination or infection", "Prior vaccination", "Prior infection", 
                 "Both prior vaccination and infection")%>%factor(levels=c("No prior vaccination or infection", "Prior vaccination", "Prior infection", 
                                                                           "Both prior vaccination and infection")))


binomial <- function(data) {
  data %>%
    rowwise %>%
    mutate(out = list(prop.test(secondary_cases, secondary_total, conf.level=.95) %>%
                        tidy)) %>%
    ungroup %>%
    unnest(out) %>% select(!c(statistic, p.value, parameter, method, alternative))
}

transmission_frac <- d %>% group_by(index_prior_inf,index_prior_vacc) %>% 
  summarise(prim_cases=n(), total_prim_cases=1261, secondary_cases=sum(contact_status), secondary_total=d$contact_status %>% sum()) %>% 
  ungroup() %>% 
  mutate(prop_primary = prim_cases/total_prim_cases*100, 
         prop_secondary = secondary_cases/secondary_total*100) %>% binomial() %>% select(!c(3:6))



d <- d %>% mutate(Institution=as.factor(Institution),
                  index_id=as.factor(index_id))

# pre-specified model
model <- glm(contact_status ~ index_prior_vacc + index_prior_inf +num_days_in_contact+
               num_vacc_doses+has_prior_inf+incidence_log+Institution, data=d, weights=weights, family="poisson")
# find SE of linear combination of covariates (prior vaccination and prior infection)
error <- c(sqrt(t(c(0,1,1,rep(0,29))) %*% vcovCR(model, cluster = d$subclass, type="CR2") %*% c(0,1,1,rep(0,29))))
model <- coef_test(model, vcov = "CR2", cluster = d$subclass) %>% data.frame(row.names = NULL)
main_results <- model[2:3,] %>% select(Coef, beta, SE) %>% 
  rbind(list("both_vacc_inf", sum(.$beta), error))

est_relative_risk <- function(data) {
  data %>% mutate(risk_red = exp(beta),
                  lb = exp(beta-2*SE),
                  ub = exp(beta+2*SE))
}

main_results <- main_results %>% est_relative_risk()
main_results

# attributable fraction among all omicron infections
full <- read_csv("housing_inf_data072122.csv")
full %>% group_by(ResidentId) %>% filter(any(!Institution %>% is.na())|any(!RoomId %>% is.na()))
omicron <- full %>% group_by(ResidentId, num_pos) %>% filter(num_pos>0) %>% filter(first(Day)>="2021-12-15")
omicron <- omicron %>% summarise_all(first)

summary_omicron <- omicron %>% mutate(prior_inf = ifelse(num_pos>1, 1, 0), 
                   prior_vacc = ifelse(num_dose_adjusted>0, 1, 0)) %>%
  group_by(prior_inf,prior_vacc) %>% summarise(count=n())
est_transmission <- summary_omicron$count*(exp(model[1,]$beta)*c(1, main_results$risk_red))
est <- est_transmission/sum(est_transmission)*100

est_transmission <- summary_omicron$count*(exp(model[1,]$beta)*c(1, main_results$lb))
lb <- est_transmission/sum(est_transmission)*100

est_transmission <- summary_omicron$count*(exp(model[1,]$beta)*c(1, main_results$ub))
ub <- est_transmission/sum(est_transmission)*100


format_res <- function(x1, x2, x3) {
  paste0(x1, " (", x2, ", ", x3, ")")
}
secondary_results <- round(100*(transmission_frac %>% select(estimate, conf.low, conf.high)),1) 
tbl <- cbind(labels$label%>%as.character, format_res(secondary_results[,1], secondary_results[,2], secondary_results[,3]),
             format_res(round(est,1), round(lb,1), round(ub,1)))


# keeping only those with complete infection record
summary_housing <- read_csv("housing_duration.csv")
omicron_full_data <- omicron %>% left_join(summary_housing)
summary_omicron <- omicron_full_data %>% 
  filter(first <= "2020-03-31") %>% 
  mutate(prior_inf = ifelse(num_pos>1, 1, 0), 
         prior_vacc = ifelse(num_dose_adjusted>0, 1, 0)) %>% 
  group_by(prior_inf, prior_vacc) %>% summarise(count=n())

est_transmission <- summary_omicron$count*(exp(model[1,]$beta)*c(1, main_results$risk_red))
est <- est_transmission/sum(est_transmission)*100

est_transmission <- summary_omicron$count*(exp(model[1,]$beta)*c(1, main_results$lb))
lb <- est_transmission/sum(est_transmission)*100

est_transmission <- summary_omicron$count*(exp(model[1,]$beta)*c(1, main_results$ub))
ub <- est_transmission/sum(est_transmission)*100

tbl <- cbind(tbl, format_res(round(est,1), round(lb,1), round(ub,1)))
tbl

tbl <- tbl %>% as.data.frame()
names(tbl) <- c("", "Fraction of transmission from SARS-CoV-2 infections (%) (95% CI)\nWithin secondary cases in study population",
                "Among all Omicron SARS-CoV-2 infections",
                "Among Omicron SARS-CoV-2 infections with complete prior infection history")
tbl

write_csv(tbl,"/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/tables/attributable_fraction_s9.csv")
