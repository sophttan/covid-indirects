rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")
#setwd("D:/stan5/code_ST")

library(tidyverse)
library(broom)
library(readr)
library(MatchIt)
library(RColorBrewer)


d <- read_csv("matched_data.csv") 

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

transmission_frac <- d %>% group_by(index_prior_vacc, index_prior_inf) %>% 
  summarise(prim_cases=n(), total_prim_cases=1261, secondary_cases=sum(contact_status), secondary_total=d$contact_status %>% sum()) %>% 
  arrange(index_prior_vacc, index_prior_inf) %>% ungroup() %>% 
  mutate(prop_primary = prim_cases/total_prim_cases*100, 
         prop_secondary = secondary_cases/secondary_total*100) %>% binomial() %>% select(!c(3:6))


# attributable fraction among 
d <- read_csv("housing_inf_data.csv")
d %>% group_by(ResidentId) %>% filter(any(!Institution %>% is.na())|any(!RoomId %>% is.na()))
omicron <- d %>% group_by(ResidentId, num_pos) %>% filter(num_pos>0) %>% filter(first(Day)>="2021-12-15")
omicron <- omicron %>% summarise_all(first)
summary_omicron <- omicron %>% mutate(prior_inf = ifelse(num_pos>1, 1, 0), 
                   prior_vacc = ifelse(num_dose_adjusted>0, 1, 0)) %>% group_by(prior_vacc, prior_inf) %>% summarise(count=n())
est_transmission <- summary_omicron$count * (exp(-1.34337)*c(1, 1+(main_results$est[c(2,1,6)])/100))
est_transmission/sum(est_transmission)*100

est_transmission <- summary_omicron$count * (exp(-1.34337)*c(1, 1+(main_results$lb[c(2,1,6)])/100))
est_transmission/sum(est_transmission)*100

est_transmission <- summary_omicron$count * (exp(-1.34337)*c(1, 1+(main_results$ub[c(2,1,6)])/100))
est_transmission/sum(est_transmission)*100

summary_housing <- read_csv("housing_duration.csv")
omicron_full_data <- omicron %>% left_join(summary_housing)
summary_omicron <- omicron_full_data %>% 
  filter(first <= "2020-03-31") %>% 
  mutate(prior_inf = ifelse(num_pos>1, 1, 0), 
         prior_vacc = ifelse(num_dose_adjusted>0, 1, 0)) %>% 
  group_by(prior_vacc, prior_inf) %>% summarise(count=n())
est_transmission <- summary_omicron$count * (exp(-1.34337)*c(1, 1+(main_results$est[c(2,1,6)])/100))
est_transmission/sum(est_transmission)*100


est_transmission <- summary_omicron$count * (exp(-1.34337)*c(1, 1+(main_results$lb[c(2,1,6)])/100))
est_transmission/sum(est_transmission)*100

est_transmission <- summary_omicron$count * (exp(-1.34337)*c(1, 1+(main_results$ub[c(2,1,6)])/100))
est_transmission/sum(est_transmission)*100
