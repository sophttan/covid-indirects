# Sophia Tan 4/7/22
# Temporal matching of index cases

rm(list=ls())
gc()
setwd("D:/stan5/code_ST")

library(tidyverse)
library(readr)
library(MatchIt)

infections_data <- read_csv("potential-primary-cases/march_infectious_periods_primary_cases_v2_roomtypes_8days_somecells052322.csv") %>% 
  select(no, ResidentId, Institution, RoomType)
vacc <- read_csv("march-data/cleaned_vaccination_data.csv")

infections <- read_csv("final samples/march_final_sample_8day_somecells052422.csv")
infections_unique <- infections %>% group_by(no) %>% filter(n()<=8) %>% summarise_all(first)
infections_unique <- infections_unique %>% left_join(vacc %>% select(ResidentId, num_dose, Date_offset), by=c("ResidentId", "num_dose"))
infections_unique <- infections_unique %>% mutate(num_dose_adjusted = ifelse(num_dose>0 & Day<Date_offset, num_dose-1, num_dose))

infections_unique <- infections_unique %>% left_join(infections_data %>% group_by(no) %>% summarise_all(first), c("ResidentId", "no"))
infections_unique <- infections_unique %>% mutate(treatment = as.factor(ifelse(num_dose_adjusted==0, 1, 0)))
# infections_unique %>% filter(week >= 50) %>% mutate(treatment = as.factor(treatment)) %>% #group_by(week, treatment) %>% summarise(count=n()) %>% 
#   ggplot(aes(week, group=treatment, fill=treatment)) + geom_histogram(aes(group=treatment))
# infections_unique %>% filter(week >= 50) %>% mutate(treatment = as.factor(treatment)) %>% group_by(week, treatment) %>% summarise(count=n()) %>% 
#   ggplot(aes(week, count, group=treatment, color=treatment)) + geom_line()

infections_unique_omicron <- infections_unique %>% filter(Day>="2021-12-15")
infections_unique_omicron <- infections_unique_omicron %>% arrange(Day)
inf_omicron_subset <- infections_unique_omicron %>% select(no, ResidentId, num_dose_adjusted, treatment, num_pos.y, Day, Institution)

day_dist <- 30
distance_matrix <- dist(as.matrix(infections_unique_omicron%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()
#distance_matrix[distance_matrix>day_dist]<-Inf
m <- matchit(treatment ~ Day + Institution, data = inf_omicron_subset,
              method = "nearest", exact = "Institution", caliper = c(30),std.caliper = F,
              distance = distance_matrix, ratio=3, replace = F)
summary(m)
m$match.matrix
mout <- match.data(m)
final <- mout %>% group_by(subclass) %>% arrange(desc(treatment)) %>% 
  filter(abs(Day-first(Day))<=day_dist & Institution==first(Institution)) %>% group_by(subclass) %>% filter(n()>1)

final %>% group_by(treatment) %>% summarise(count=n())
final %>% arrange(subclass) %>% select(!c(ResidentId, num_pos.y)) %>% rename("index_case_no"="no", "index_num_doses"="num_dose_adjusted") %>% head(20)


final <- final %>% 
  left_join(infections %>% select(!c(full_vacc, max_dose, num_dose)))

final <- final %>% rename("index_id"="ResidentId", 
                 "index_prior_vacc_doses"="num_dose_adjusted",
                 "contact_id"="contacts",
                 "contact_status"="neg_pos_contact")

final <- final %>% mutate(#index_id = as.factor(index_id),
                          index_prior_inf = ifelse(num_pos.y==1, 0, 1),
                          index_prior_vacc = ifelse(index_prior_vacc_doses==0, 0, 1), 
                          Institution = as.factor(Institution))
final <- final %>% mutate(index_prior_vacc_doses=ifelse(index_prior_vacc_doses>3, 3, index_prior_vacc_doses),
                          num_vacc_doses=ifelse(num_vacc_doses>3, 3, num_vacc_doses))

final <- final %>% mutate(index_has_vacc_or_inf=ifelse(index_prior_inf==1|index_prior_vacc==1, 1, 0))

final %>% group_by(index_prior_vacc) %>% summarise(inf_risk=mean(contact_status))

# visualizations

final %>% mutate(index_prior_vacc = as.factor(index_prior_vacc)) %>% group_by(index_prior_vacc, num_vacc_doses) %>% summarise(count=n()) %>% 
  group_by(index_prior_vacc) %>% summarise(num_vacc_doses=num_vacc_doses,prop=count/sum(count)) %>% 
  ggplot(aes(x=num_vacc_doses, y=prop, fill=index_prior_vacc)) + stat_summary(geom="bar", position=position_dodge(.9)) +
  scale_y_continuous(name="proportion of contacts") + 
  xlab("close contact - number of vaccine doses") +
  scale_fill_discrete(name = "index case - prior vaccination") + 
  theme(legend.position = "bottom")

final %>% mutate(index_prior_vacc = as.factor(index_prior_vacc)) %>% group_by(index_prior_vacc) %>% 
  summarise(inf_risk=mean(contact_status)) %>% 
  ggplot(aes(index_prior_vacc, inf_risk)) + stat_summary(geom="bar") +
  scale_y_continuous(name="risk of infection in close contact") + 
  xlab("index case - prior vaccination")

final %>% mutate(index_prior_vacc_doses = as.factor(index_prior_vacc_doses)) %>% group_by(index_prior_vacc_doses) %>% 
  summarise(inf_risk=mean(contact_status)) %>% 
  ggplot(aes(index_prior_vacc_doses, inf_risk)) + stat_summary(geom="bar") +
  scale_y_continuous(name="risk of infection in close contact") + 
  xlab("index case - number of vaccine doses")

final %>% mutate(num_vacc_doses=ifelse(num_vacc_doses==4, 3, num_vacc_doses)) %>% group_by(index_prior_vacc, num_vacc_doses) %>% 
  summarise(inf_risk=mean(contact_status)) %>% ungroup() %>% mutate(index_prior_vacc=as.factor(index_prior_vacc), 
                                                                    num_vacc_doses=as.factor(num_vacc_doses)) %>%
  ggplot(aes(x=num_vacc_doses, y=inf_risk, fill=index_prior_vacc)) + stat_summary(geom="bar", position=position_dodge(.9)) +
  scale_y_continuous(name="risk of infection in close contact") + 
  xlab("close contact - number of vaccine doses") +
  scale_fill_discrete(name = "index case - prior vaccination") + 
  theme(legend.position = "bottom")


final %>% mutate(index_has_vacc_or_inf=as.factor(index_has_vacc_or_inf)) %>% 
  group_by(index_has_vacc_or_inf) %>% summarise(inf_risk = mean(contact_status)) %>%
  ggplot(aes(index_has_vacc_or_inf, inf_risk)) + stat_summary(geom="bar") +
  scale_y_continuous(name="risk of infection in close contact") + 
  xlab("index case - prior vaccination or infection")

final %>% group_by(index_prior_vacc, num_vacc_doses) %>% 
  summarise(inf_risk=mean(contact_status)) %>% ungroup() %>% mutate(index_prior_vacc=as.factor(index_prior_vacc), 
                                                                    num_vacc_doses=as.factor(num_vacc_doses)) %>%
  ggplot(aes(x=num_vacc_doses, y=inf_risk, fill=index_prior_vacc)) + stat_summary(geom="bar", position=position_dodge(.9)) +
  scale_y_continuous(name="risk of infection in close contact") + 
  xlab("close contact - number of vaccine doses") +
  scale_fill_discrete(name = "index case - prior vaccination or infection") + 
  theme(legend.position = "bottom")

final %>% group_by(index_has_vacc_or_inf, num_vacc_doses) %>% 
  summarise(inf_risk=mean(contact_status)) %>% ungroup() %>% mutate(index_has_vacc_or_inf=as.factor(index_has_vacc_or_inf), 
                                                                    num_vacc_doses=as.factor(num_vacc_doses)) %>%
  ggplot(aes(x=num_vacc_doses, y=inf_risk, fill=index_has_vacc_or_inf)) + stat_summary(geom="bar", position=position_dodge(.9)) +
  scale_y_continuous(name="risk of infection in close contact") + 
  xlab("close contact - number of vaccine doses") +
  scale_fill_discrete(name = "index case - prior vaccination or infection") + 
  theme(legend.position = "bottom")
