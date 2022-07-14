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

infections <- read_csv("final_sample060222_nopcr.csv") %>% 
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
set.seed(42)
mult_contacts <- mult_contacts[sample(1:nrow(mult_contacts)),]
mult_contacts_single <- mult_contacts %>% group_by(no) %>% summarise_all(first)

# subset data for matching 
infections_unique <- infections %>% group_by(no) %>% filter(n()==1) %>% summarise_all(first)
infections_unique <- infections_unique %>% rbind(mult_contacts_single)
infections_unique <- infections_unique %>% mutate(treatment = as.factor(ifelse(index_prior_vacc_doses==0, 1, 0)))
infections_unique %>% group_by(index_prior_vacc, index_prior_inf) %>% summarise(case_contact_pairs=n(), inf_risk=mean(contact_status))

inf_omicron_subset <- infections_unique %>% filter(first < "2020-04-01") %>% arrange(Day)
write_csv(inf_omicron_subset, "unmatched_all_covariates.csv")

# matching
# 1:10 matching of unvaccinated to vaccinated index cases
# match by institution exactly and by time - vaccinated index cases must be within 30 days of 
# unvaccinated case match
day_dist <- 30
distance_matrix <- dist(as.matrix(inf_omicron_subset%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()
m <- matchit(treatment ~ Day + Institution, data = inf_omicron_subset,
             method = "nearest", exact = "Institution", caliper = c(30),std.caliper = F,
             distance = distance_matrix, ratio=10, replace = F)
summary(m)
m$match.matrix
mout <- match.data(m)

#final matched dataset
final <- mout %>% group_by(subclass) %>% arrange(desc(treatment)) %>% 
  filter(abs(Day-first(Day))<=day_dist & Institution==first(Institution)) %>% group_by(subclass) %>% filter(n()>1)

# check final sample sizes
final %>% group_by(treatment) %>% summarise(count=n())


# save final matched dataset
write_csv(final, "matched_data.csv")

setwd("/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/figures/appendix")
# diagnostic matching plots
# Figure A2
# distribution of days of contact between index cases and close contacts
p <- final %>% ggplot(aes(num_days_in_contact))+geom_histogram(bins = 5, color="white") + 
  scale_y_continuous("Number of close contacts", expand=c(0,0)) + 
  scale_x_continuous("Days of contact between index cases and close contacts") + 
  theme(panel.background = element_blank(), 
        legend.title=element_blank(),
        legend.key=element_blank(),
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())
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
p <- final %>% group_by(subclass) %>% arrange(desc(treatment)) %>% 
  mutate(distance=as.numeric(abs(first(Day)-Day))) %>% filter(treatment==0) %>% 
  ggplot(aes(distance)) + geom_histogram(color="white") + 
  scale_y_continuous("Number of matches", expand=c(0,0)) + 
  scale_x_continuous("Absolute number of days between matched index cases", expand=c(0,0)) + 
  theme(panel.background = element_blank(), 
        legend.title=element_blank(),
        legend.key=element_blank(),
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line()) 
ggsave(p, filename="distance_between_matches_a4.jpg", width=5, height=4)

