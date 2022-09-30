# Sophia Tan 4/7/22
# Primary analysis - matching index cases

rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")
#setwd("D:/stan5/code_ST")

library(tidyverse)
library(broom)
library(readr)
library(MatchIt)

# prep data for matching
vacc <- read_csv("cleaned_vaccination_data.csv")
summary_housing <- read_csv("housing_duration.csv")
inst_day <- read_csv("incidence_final.csv")
covid_risk <- read_csv("covid_risk_score.csv")
demographic <- read_csv("demographic_data_clean.csv")

infections <- read_csv("final_sample092922_nopcr.csv") %>% 
  left_join(vacc %>% select(ResidentId, num_dose, Date_offset), by=c("ResidentId", "num_dose"))
infections <- infections %>% mutate(num_dose_adjusted = ifelse(num_dose>0 & Day<Date_offset, num_dose-1, num_dose))
infections <- infections %>% left_join(summary_housing) 

# covariates 
# add incidence
infections <- infections %>% left_join(inst_day)
infections <- infections %>% mutate(incidence_log = log(incidence))

# rename columns and aggregate covariates
infections <- infections %>% rename("index_id"="ResidentId", 
                                    "index_prior_vacc_doses"="num_dose_adjusted",
                                    "contact_id"="contacts",
                                    "contact_status"="neg_pos_contact")

infections <- infections %>% mutate(
  index_prior_inf = ifelse(num_pos==1, 0, 1),
  index_prior_vacc = ifelse(index_prior_vacc_doses==0, 0, 1))

infections <- infections %>% mutate(index_prior_vacc_doses=ifelse(index_prior_vacc_doses>3, 3, index_prior_vacc_doses),
                                    num_vacc_doses=ifelse(num_vacc_doses>3, 3, num_vacc_doses))
infections <- infections %>% mutate(index_has_vacc_or_inf=ifelse(index_prior_inf==1|index_prior_vacc==1, 1, 0))

infections %>% group_by(index_prior_vacc, index_prior_inf) %>% summarise(case_contact_pairs=n(), inf_risk=mean(contact_status))

# keep only 1 contact for each index case
mult_contacts <- infections %>% group_by(no) %>% filter(n()>1) 
mult_contacts %>% summarise(count=n())
set.seed(42)
mult_contacts <- mult_contacts[sample(1:nrow(mult_contacts)),]
mult_contacts_single <- mult_contacts %>% group_by(no) %>% summarise_all(first)

# subset data for matching 
infections_unique <- infections %>% group_by(no) %>% filter(n()==1) %>% summarise_all(first)
infections_unique <- infections_unique %>% rbind(mult_contacts_single)
infections_unique <- infections_unique %>% mutate(treatment = as.factor(ifelse(index_prior_vacc_doses==0, 1, 0)))
infections_unique %>% group_by(index_prior_vacc, index_prior_inf) %>% summarise(case_contact_pairs=n(), inf_risk=mean(contact_status))

inf_omicron_subset <- infections_unique %>% filter(first < "2020-04-01") %>% arrange(Day)

# add COVID-19 risk score
library(lubridate)
covid_risk <- read_csv("covid_risk_score.csv")
covid_risk_subset <- covid_risk %>% filter(ResidentId %in% inf_omicron_subset$index_id) 
covid_risk_subset <- covid_risk_subset %>% rowwise() %>% mutate(first=interval %>% str_extract_all("[0-9]+-[0-9]+-[0-9]+"), 
                                                  interval=interval(first[1], first[2])) %>% select(!first)

inf_omicron_subset <- inf_omicron_subset %>% full_join(covid_risk_subset, by=c("index_id"="ResidentId")) %>% 
  filter(Day %>% lubridate::`%within%`(interval)) %>%
  rename("covid_risk"="Value")
inf_omicron_subset

# add age and other demographic data
inf_omicron_subset <- inf_omicron_subset %>% select(!interval) %>% mutate(Year=format(Day, format='%Y'))
inf_omicron_subset <- inf_omicron_subset %>% left_join(demographic, by=c("index_id"="ResidentId"))
inf_omicron_subset <- inf_omicron_subset %>% mutate(age=as.numeric(Year)-as.numeric(BirthYear))
inf_omicron_subset


write_csv(inf_omicron_subset, "unmatched_all_covariates092922.csv")


# Matching
# estimate propensity scores (age, COVID-19 risk, and prior infection history)
ps <- glm(treatment ~ age + covid_risk + index_prior_inf, data = inf_omicron_subset, family = binomial(link='logit'))
summary(ps)
inf_omicron_subset <- inf_omicron_subset %>% mutate(logodds = predict.glm(ps),
                                                    ps=exp(logodds)/(1+exp(logodds)))
inf_omicron_subset

# distance matrices for Day and propensity score
distance_propensity <- dist(as.matrix(inf_omicron_subset%>%select(ps)), diag = T, upper = T) %>% as.matrix()
distance_matrix <- dist(as.matrix(inf_omicron_subset%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()

# manually set calipers for Day and propensity score
# main analysis, propensity score caliper = 0.1 (varied in sensitivity analysis)
ps_dist <- 0.1
day_dist <- 30
distance_propensity[distance_propensity > ps_dist] <- Inf
distance_matrix[distance_matrix > day_dist] <- Inf

# scale day distance matrix to match scale of difference in propensity scores
distance_matrix <- distance_matrix/day_dist*ps_dist

# sum together distance matrices for total distance
# weights between day distance and propensity score distance varied in sensitivity analyses
weights <- c(0.5, 0.5)
totaldistance <- distance_matrix*weights[1] + distance_propensity*weights[2]

# match using combined distance with exact matches by Institution
# 10:1 ratio of vaccinated to unvaccinated cases without replacement
m <- matchit(treatment ~ age + covid_risk + index_prior_inf + Day, data = inf_omicron_subset,
             method = "nearest", 
             exact = ~Institution,
             distance = totaldistance, ratio=10, replace = F)
mout <- match.data(m)
plot(m, type = "qq", interactive = FALSE, which.xs = c("age", "covid_risk", "index_prior_inf", "Day"))

# check quality of matches
mout %>% group_by(subclass) %>% arrange(desc(treatment)) %>% 
  filter(abs(first(Day)-Day)>day_dist|abs(first(ps)-ps)>ps_dist) %>% select(no, index_id, subclass)
mout %>% filter(subclass==83) %>% select(no, index_id, Day, age, covid_risk, index_prior_inf, ps, weights, subclass)

final <- mout %>% group_by(subclass) %>% arrange(desc(treatment)) %>%
  filter(abs(first(Day)-Day)<=day_dist&abs(first(ps)-ps)<=ps_dist)
# reweigh matches
final <- final %>% mutate(weights=ifelse(treatment==1,1,1/sum(treatment==0)))
mean_weights <- mean((final %>% filter(treatment==0))$weights)
final <- final %>% mutate(weights=ifelse(treatment==1,1,weights/mean_weights))

final %>% write_csv("matched_data_ps092922.csv")

# check final sample sizes
final %>% group_by(treatment) %>% summarise(count=n())

setwd("/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/figures/appendix")
# diagnostic matching plots
# Figure A2
# distribution of days of contact between index cases and close contacts
colors<-brewer.pal(n=8, "Accent")[c(7,5)]
p <- final %>% mutate(treatment=factor(treatment, levels=c(1,0), labels=c("No prior vaccination", "Prior vaccination"))) %>%
  group_by(treatment) %>% summarise(total=n(), num_days_in_contact=num_days_in_contact) %>%
  group_by(treatment, num_days_in_contact) %>% summarise(prop=n()/total) %>%
  ggplot(aes(x=num_days_in_contact, y=prop, group=treatment, fill=treatment)) +
  geom_bar(stat="identity",position ="dodge") + 
  scale_x_continuous(name="Number of days of exposure between\nindex case and close contact") + 
  scale_y_continuous("Proportion of index cases", expand=c(0,0)) + 
  scale_fill_manual(values=colors, name="Prior vaccination in index case")+
  theme(legend.title = element_text(),
        legend.position = "bottom",
        panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())
p
unvacc_exposure <- (final %>% filter(treatment==1))$num_days_in_contact
vacc_exposure <- (final %>% filter(treatment==0))$num_days_in_contact
t.test(unvacc_exposure, vacc_exposure, var.equal = F)
ggsave(p, filename="days_contact_a2.jpg", width=5, height=4)

# Figure A3
# distribution of the number of matches per unvaccinated index case
p <- final %>% filter(treatment==0) %>% group_by(subclass) %>% summarise(count=n()) %>% 
  ggplot(aes(as.factor(count))) + geom_bar() + 
  scale_x_discrete(name="Number of matches per unvaccinated index case") + 
  scale_y_continuous("Number of unvaccinated index cases", expand=c(0,0), limits=c(0,80)) + 
  theme(panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())
ggsave(p, filename="num_matches_dist_a3.jpg", width=5, height=4)

# Figure A4
# distribution of the distance between matched cases
p2 <- final %>% group_by(subclass) %>% arrange(desc(treatment)) %>% 
  mutate(distance=as.numeric(abs(first(Day)-Day))) %>% filter(treatment==0) %>% 
  ggplot(aes(distance)) + geom_histogram(color="white", bins=31) + 
  scale_y_continuous(expand=c(0,0), limits=c(0, 250)) + 
  scale_x_continuous("Absolute number of days\nbetween matched index cases", expand=c(0,0)) + 
  labs(subtitle="B")+
  theme(panel.background = element_blank(), 
        legend.title=element_blank(),
        legend.key=element_blank(),
        axis.title.y = element_blank(),
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line()) 

p1 <- final %>% group_by(subclass) %>% arrange(desc(treatment)) %>% 
  mutate(distance=as.numeric(abs(first(ps)-ps))) %>% filter(treatment==0) %>% 
  ggplot(aes(distance)) + geom_histogram(color="white", bins=20) + 
  scale_y_continuous("Number of matches", expand=c(0,0), limits=c(0, 250)) + 
  scale_x_continuous("Absolute difference in propensity score\nbetween matched index cases", expand=c(0,0),
                     breaks=seq(0,0.1,0.02)) + 
  labs(subtitle="A")+
  theme(panel.background = element_blank(), 
        legend.title=element_blank(),
        legend.key=element_blank(),
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line()) 

library(patchwork)
p1|p2
ggsave(p1|p2, filename="distance_between_matches_a4.jpg", width=6.5, height=3.5)

