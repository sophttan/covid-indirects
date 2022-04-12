# Sophia Tan 4/7/22
# Temporal matching of index cases

rm(list=ls())
setwd("D:/stan5/code_ST")

library(tidyverse)
library(readr)
library(MatchIt)

infections <- read_csv("final samples/final_sample041122_9day.csv")
infections_unique <- infections %>% group_by(no) %>% summarise_all(first)
infections_unique
infections_unique <- infections_unique %>% mutate(treatment = ifelse(num_dose==0, 0, 1))
infections_unique %>% filter(week >= 50) %>% mutate(treatment = as.factor(treatment)) %>% #group_by(week, treatment) %>% summarise(count=n()) %>% 
  ggplot(aes(week, group=treatment, fill=treatment)) + geom_histogram(aes(group=treatment))
infections_unique %>% filter(week >= 50) %>% mutate(treatment = as.factor(treatment)) %>% group_by(week, treatment) %>% summarise(count=n()) %>% 
  ggplot(aes(week, count, group=treatment, color=treatment)) + geom_line()


m.out1 <- matchit(treatment ~ Day, data = infections_unique,
                  method = "nearest", distance = "glm")
summary(m.out1, un = FALSE)
m.data1 <- match.data(m.out1)
m.data1 %>% group_by(subclass) %>% filter(abs(diff(Day))<=7) # keeps only 43 matches, 86 total infections

infections_unique <- infections_unique %>% arrange(Day)
distance_matrix <- dist(as.matrix(infections_unique%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()
distance_matrix[distance_matrix<=7]<-0 # any match within 1 week is acceptable
m.out2 <- matchit(treatment ~ Day, data = infections_unique,
                  method = "nearest", distance = distance_matrix)
summary(m.out2, un = FALSE)
m.data2 <- match.data(m.out2)
m.data2 %>% group_by(subclass) %>% filter(abs(diff(Day))<=7) # keeps only 43 matches, 86 total infections
(m.data2 %>% group_by(subclass) %>% summarise(diff=abs(diff(Day))))$diff %>% as.numeric() %>% summary()
