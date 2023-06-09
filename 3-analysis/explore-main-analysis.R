# Sophia Tan 6/1/23
# Main analysis descriptive statistics

rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)

# read in dataset
d <- read_csv("matching_data_051923/matching_data_allvacc_noincarcreq_priorinf_bydose_infvacc052423.csv") 
testing <- read_csv("testing_vacc_clean.csv") %>% 
  select(ResidentId, Day, Result, num_pos) %>% filter(!Result%>%is.na())

fix_intersection <- function(v) {
  v%>%str_extract_all("[0-9]{4}-[0-9]{2}-[0-9]{2}", simplify = T)
}

intersection <- fix_intersection(d$intersection)
d$start <- intersection[,1]%>%as.vector()%>%as.Date()
d$end <- intersection[,2]%>%as.vector()%>%as.Date()
d <- d %>% mutate(intersection=interval(start=start, end=end))

add_testing <- function(d) {
  d %>% left_join(testing, by=c("primary"="ResidentId"))
}

filter_testing <- function(d) {
  d <- d %>% mutate(has_test=any(Day>=start&Day<=end))
  d %>% filter((Day >= start & Day <= end)|(!has_test&Day==first(Day))) %>% 
    mutate(Day=if_else(!has_test, as.Date(NA), Day), 
           Result=if_else(!has_test, as.character(NA), Result))
}

# use matched time where we use maximum overlapped time
treatment <- d %>% filter(treatment==0)
control <- d %>% filter(treatment==1)

treatment_testing <- treatment %>% add_testing()
treatment_time_test <- treatment_testing %>% 
  group_by(id) %>%
  filter_testing() %>% 
  mutate(survival_time=as.numeric(Day-start)+1) %>%
  mutate(last = ifelse(any(Result=="Positive",na.rm=T)&Result=="Positive", Result=="Positive", Day==last(Day))) %>% 
  mutate(pos_first=first(Result)=="Positive") %>%
  filter(last|!has_test) %>% summarise_all(first) %>% select(!last) %>% 
  mutate(survival_time=ifelse(!has_test|Result!="Positive", as.numeric(end-start)+1, survival_time))

control_testing <- treatment %>% select(subclass, id, intersection) %>% rename("id_treat"="id") %>% 
  left_join(control %>% select(!intersection), by="subclass") %>%
  mutate(first_intersection=start, last_intersection=end) %>%
  arrange(id, first_intersection, desc(intersect))
control_testing %>%  select(id, subclass, intersection, intersect) %>% arrange(id)

control_testing_unique <- control_testing %>% group_by(id) %>% distinct(id, first_intersection, .keep_all = T)

control_test_overlap <- control_testing %>% group_by(id) %>% 
  mutate(next_intersection=lag(intersection, 1)) %>% 
  mutate(overlap_next=!intersect(intersection, next_intersection) %>% is.na()) 

distinct_overlap <- control_test_overlap %>% filter(!overlap_next) %>% mutate(label=1:n())
control_test_overlap <- control_test_overlap %>% left_join(distinct_overlap)
control_test_overlap <- control_test_overlap %>% fill(label, .direction="down") %>% group_by(id, label) %>% 
  mutate(start=min(first_intersection)%>%as.Date(), end=max(last_intersection)%>%as.Date())

control_test_overlap %>% 
  select(id, id_treat, subclass, intersection, start, end, label) %>% head(20)

control_testing_unique <- control_test_overlap %>% group_by(id, label) %>% summarise_all(first)
control_time_test <- control_testing_unique %>% group_by(id, label) %>% 
  add_testing() %>% 
  filter_testing() %>% 
  mutate(survival_time=as.numeric(Day-start)+1) %>% 
  mutate(last = ifelse(any(Result=="Positive",na.rm=T)&Result=="Positive", Result=="Positive", Day==last(Day))) %>%
  mutate(pos_first=first(Result)=="Positive") %>%
  filter(!has_test|last) %>% summarise_all(first) %>% select(!last) %>% 
  mutate(survival_time=ifelse(!has_test|Result!="Positive", as.numeric(end-start)+1, survival_time))

control_time_test <- control_time_test %>% select(!c(id_treat, first_intersection, last_intersection, next_intersection, overlap_next))


full <- treatment_time_test %>% 
  select(id, treatment, subclass, weights, BuildingId, primary, secondary, vacc.primary, vacc.secondary, inf.primary, inf.secondary, start, end, survival_time, Result) %>% 
  rbind(control_time_test %>% select(id, treatment, subclass, weights, BuildingId, primary, secondary, vacc.primary, vacc.secondary, inf.primary, inf.secondary, start, end, survival_time, Result))

full <- full %>% group_by(subclass) %>% filter(any(treatment==1)&any(treatment==0))
full <- full %>% mutate(treatment=1-treatment)
full$treatment%>%table()

full <- full %>% 
  mutate(status=ifelse(!Result%>%is.na()&Result=="Positive", 1, 0), 
         BuildingId=as.factor(BuildingId),
         subclass=as.factor(subclass)) 

full%>%
  group_by(treatment)%>%
  summarise(units=length(unique(id)), cases=sum(status), person_time=n()*mean(survival_time))%>%
  mutate(inc_rate=cases/person_time)

# full <- full %>% mutate(time_since_start=as.numeric(difftime(start, "2021-12-15", units="days")))
full <- full %>% group_by(treatment) %>% mutate(n=1:n()) %>% mutate(n=ifelse(treatment==0, n, NA))
full <- full %>% arrange(subclass, start) %>% ungroup() %>% 
  fill(n, .direction="down") %>%
  group_by(n) %>% 
  mutate(time_since_start_rel=start-first(start))

demo <- read_csv("demographic_data_clean.csv")
demo <- demo %>% mutate(age=2022-BirthYear)
demo

d <- d %>% left_join(demo, by=c("primary"="ResidentId"))
d <- d %>% left_join(demo, by=c("secondary"="ResidentId"), suffix=c(".primary", ".secondary"))
d

(d %>% group_by(subclass) %>% summarise(count=n())) %>% ggplot(aes(count)) + geom_bar() + 
  scale_x_continuous("Match group size", breaks=2:7,labels=2:7) + 
  ylab("Count")

secondary_age <- d %>% arrange(subclass, desc(treatment)) %>% group_by(subclass) %>% 
  summarise(age.mean=mean(age.secondary),
            age.25=quantile(age.secondary, .25),
            age.75=quantile(age.secondary, .75), 
            range=max(age.secondary)-min(age.secondary))

d %>% arrange(subclass, desc(treatment)) %>% group_by(subclass) %>% 
  mutate(diffage=abs(age.secondary-first(age.secondary))) %>% 
  filter(treatment==0) %>% 
  ggplot(aes(diffage)) + geom_histogram() + 
  xlab("Difference in age (years) between secondary residents in control and treatment units") + 
  ylab("Count")

secondary_age %>% ggplot(aes(range)) + geom_histogram() + 
  xlab("Range of age (years) within matched groups") + ylab("Count")

secondary_age %>% ggplot(aes(subclass, age.mean)) + geom_point() +
  geom_errorbar(aes(ymin=age.25, ymax=age.75)) + coord_flip()

d %>% ggplot() + 
  geom_histogram(aes(age.primary, fill="Primary resident"), alpha=0.5) +
  geom_histogram(aes(age.secondary, fill="Secondary resident"), alpha=0.5) + 
  scale_fill_discrete("") + 
  xlab("Age (years)") + ylab("Count")

library(tableone)
d <- d %>% mutate(Race.primary=ifelse(Race.primary=="C"|Race.primary=="M", "H", Race.primary),
                  Race.secondary=ifelse(Race.secondary=="C"|Race.secondary=="M", "H", Race.secondary), 
                  Race.primary=case_when(Race.primary=="A"~"Asian or Pacific Islander",
                                         Race.primary=="B"~"Black",
                                         Race.primary=="H"~"Hispanic",
                                         Race.primary=="I"~"American Indian/Alaskan Native",
                                         Race.primary=="O"~"Other",
                                         Race.primary=="W"~"White"),
                  Race.secondary=case_when(Race.secondary=="A"~"Asian or Pacific Islander",
                                           Race.secondary=="B"~"Black",
                                           Race.secondary=="H"~"Hispanic",
                                           Race.secondary=="I"~"American Indian/Alaskan Native",
                                           Race.secondary=="O"~"Other",
                                           Race.secondary=="W"~"White"))
tableone::CreateTableOne(data = d, vars = c("Race.primary", "Race.secondary"))

infections <- testing %>% group_by(ResidentId, num_pos) %>% summarise_all(first) %>% filter(!is.na(num_pos)) %>% select(!Result)
full_withtest <- full %>% select(!Result) %>% left_join(testing%>%select(!num_pos), c("primary"="ResidentId"))
  
testing_avg <- full_withtest %>% group_by(id) %>% 
  mutate(has_test = any(Day >= start & Day <= end)) %>%
  filter(all(Day < start|Day > end)|(Day >= start & Day <= end)) %>% 
  summarise(treatment=first(treatment), has_test=first(has_test), days_between_tests=first(end-start+1)/n()) %>%
  mutate(days_between_tests = ifelse(!has_test, Inf, days_between_tests))

testing_avg %>% filter(days_between_tests<200) %>% 
  ggplot(aes(days_between_tests, y=..density.., group=as.factor(treatment), fill=as.factor(treatment))) + 
  geom_histogram(alpha=0.5, position="identity") +
  xlab("Average days between tests during observation time") + 
  ylab("Density") + 
  scale_fill_discrete(element_blank(), labels=c("Control", "Treatment"))
  
testing_avg %>% filter(days_between_tests < Inf) %>% group_by(treatment) %>% 
  summarise(mean=mean(days_between_tests))
testing_avg %>% filter(days_between_tests %>% is.infinite()) %>% group_by(treatment) %>% 
  summarise(n=n()) %>% mutate(n=n/c(1124, 3620))

full_inf <- full %>% 
  left_join(infections, by=c("primary"="ResidentId")) %>% 
  rename("infDay.primary"="Day") %>%
  group_by(id) 

full_inf %>% 
  filter(all(infDay.primary>start)) %>% 
  arrange(id, desc(infDay.primary)) %>% 
  summarise_all(first) %>% select(primary)

full_inf_clean <- full_inf %>%
  filter(infDay.primary <= start) %>% 
  arrange(id, desc(infDay.primary)) %>% 
  summarise_all(first) %>% 
  mutate(time_since_inf.primary=difftime(start, infDay.primary, units="days")%>%as.numeric())
full_inf_clean     

full_inf_clean <- full_inf_clean %>%
  left_join(infections, by=c("secondary"="ResidentId")) %>% 
  rename("infDay.secondary"="Day") %>%
  group_by(id) %>%
  filter(infDay.secondary <= start) %>% 
  arrange(id, desc(infDay.secondary)) %>% 
  summarise_all(first) %>% 
  mutate(time_since_inf.secondary=difftime(start, infDay.secondary, units="days")%>%as.numeric())
full_inf_clean    
     
full_inf_clean %>% 
  ggplot(aes(time_since_inf.primary, y=..density.., group=as.factor(treatment), fill=as.factor(treatment))) + 
  geom_histogram(alpha=0.5, position="identity") +
  scale_x_continuous("Days since most recent SARS-CoV-2 infection", limits=c(0,1000)) + 
  ylab("Density") + 
  scale_fill_discrete(element_blank(), labels=c("Control", "Treatment"))

full_inf_clean%>%group_by(treatment) %>% summarise(mean=mean(time_since_inf.primary))

full_inf_clean %>% group_by(subclass) %>% arrange(subclass, treatment) %>%
  mutate(time_since_inf.primary=abs(first(time_since_inf.primary)-time_since_inf.primary)) %>%
  filter(treatment==1) %>% 
  ggplot(aes(time_since_inf.primary)) + geom_histogram() + 
  xlab("Absolute difference in days since last infection within matched groups") + 
  ylab("Count")

full_inf_clean %>% group_by(subclass) %>% arrange(subclass, treatment) %>%
  mutate(time_since_inf.secondary=abs(first(time_since_inf.secondary)-time_since_inf.secondary)) %>%
  filter(treatment==1) %>% 
  ggplot(aes(time_since_inf.secondary)) + geom_histogram() + 
  xlab("Absolute difference in days since last infection within matched groups") + 
  ylab("Count")

full_cases <- full%>%filter(status==1)
full_cases$survival_time
full_cases <- full_cases %>% left_join(infections, by=c("secondary"="ResidentId")) 
full_cases <- full_cases %>% group_by(id) %>% mutate(has_inf=any(Day>=start&Day<=end)) %>%
  filter((!has_inf&Day==first(Day))|(Day>=start&Day<=end))
full_cases %>% mutate(Day=if_else(!has_inf, as.Date(NA), Day)) %>% 
  mutate(survival_time.secondary=(Day-start+1)%>%as.numeric()) %>% 
  select(id, primary, secondary, start, end, survival_time, survival_time.secondary, Day) %>% 
  filter(survival_time.secondary%>%is.na())

vacc <- read_csv("cleaned_vaccination_data.csv") %>% select(ResidentId, num_dose, Date)
full_vacc <- full %>% 
  left_join(vacc, by=c("primary"="ResidentId", "vacc.primary"="num_dose")) %>% 
  rename("vaccday.primary"="Date")
full_vacc <- full_vacc %>% 
  left_join(vacc, by=c("secondary"="ResidentId", "vacc.secondary"="num_dose")) %>% 
  rename("vaccday.secondary"="Date")

full_vacc <- full_vacc %>% mutate(time_since_vacc.primary = (start-vaccday.primary)%>%as.numeric(),
                                  time_since_vacc.secondary = (start-vaccday.secondary)%>%as.numeric())

full_vacc %>% ggplot(aes(time_since_vacc.primary, group=as.factor(treatment), 
                         fill=as.factor(treatment))) + 
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5) + 
  scale_x_continuous("Days since most recent COVID-19 vaccination") + 
  ylab("Density") + 
  scale_fill_discrete(element_blank(), labels=c("Control", "Treatment"))

full_vacc %>% group_by(subclass) %>% arrange(subclass, treatment) %>%
  mutate(time_since_vacc.primary=abs(first(time_since_vacc.primary)-time_since_vacc.primary)) %>%
  filter(treatment==1) %>% 
  ggplot(aes(time_since_vacc.primary)) + geom_histogram() + 
  xlab("Absolute difference in days since last vaccination within matched groups") + 
  ylab("Count")

full_vacc %>% ggplot(aes(time_since_vacc.secondary, group=as.factor(treatment), 
                         fill=as.factor(treatment))) + geom_histogram(aes(y=..density..), position="identity") + 
  scale_x_continuous("Days since most recent COVID-19 vaccination") + 
  ylab("Density") + 
  scale_fill_discrete(element_blank(), labels=c("Control", "Treatment"))
