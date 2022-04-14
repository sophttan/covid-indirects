# Sophia Tan 4/7/22
# Temporal matching of index cases

rm(list=ls())
setwd("D:/stan5/code_ST")

library(tidyverse)
library(readr)
library(MatchIt)

infections_data <- read_csv("potential-primary-cases/infectious_periods_primary_cases_v2_roomtypes_8days-somecells.csv") %>% 
  select(no, ResidentId, Institution, RoomType)

infections <- read_csv("final samples/final_sample041322_8day_somecell.csv")
infections_unique <- infections %>% group_by(no) %>% summarise_all(first)

infections_unique <- infections_unique %>% left_join(infections_data %>% group_by(no) %>% summarise_all(first), c("ResidentId", "no"))
infections_unique <- infections_unique %>% mutate(treatment = ifelse(num_dose==0, 0, 1))
infections_unique %>% filter(week >= 50) %>% mutate(treatment = as.factor(treatment)) %>% #group_by(week, treatment) %>% summarise(count=n()) %>% 
  ggplot(aes(week, group=treatment, fill=treatment)) + geom_histogram(aes(group=treatment))
infections_unique %>% filter(week >= 50) %>% mutate(treatment = as.factor(treatment)) %>% group_by(week, treatment) %>% summarise(count=n()) %>% 
  ggplot(aes(week, count, group=treatment, color=treatment)) + geom_line()

total_matches <- NULL
for (i in infections_unique$Institution %>% unique()) {
  inst_subset <- infections_unique %>% filter(Institution==i)
  if(length(inst_subset$treatment%>%unique())==1) {next}
  distance_matrix <- dist(as.matrix(inst_subset%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()
  distance_matrix[distance_matrix<=60]<-0 # any match within 1 week is acceptable
  m <- matchit(treatment ~ Day, data = inst_subset,
               method = "nearest", distance = distance_matrix)
  m <- match.data(m)
  total_matches <- rbind(total_matches, m %>% group_by(subclass) %>% filter(abs(diff(Day))<=60)) # keeps only 43 matches, 86 total infections
  
}

day_dist <- 90
infections_unique <- infections_unique %>% arrange(Day)
distance_matrix <- dist(as.matrix(infections_unique%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()
distance_matrix[distance_matrix<=day_dist]<-0 # any match within 1 week is acceptable
m <- matchit(treatment ~ Day, data = infections_unique,
             method = "nearest", distance = distance_matrix)
m <- match.data(m)
m %>% group_by(subclass) %>% filter(abs(diff(Day))<=day_dist)
