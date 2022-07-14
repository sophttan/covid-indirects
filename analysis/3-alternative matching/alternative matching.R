# Sophia Tan 6/24/22
# Matching under alternative matching specifications

rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")
#setwd("D:/stan5/code_ST")

library(tidyverse)
library(broom)
library(readr)
library(MatchIt)

inf_omicron_subset <- read_csv("unmatched_all_covariates.csv")

# 1:4 matching with 30 day caliper
day_dist <- 30
distance_matrix <- dist(as.matrix(inf_omicron_subset%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()
m <- matchit(treatment ~ Day + Institution, data = inf_omicron_subset,
             method = "nearest", exact = "Institution", caliper = c(30),std.caliper = F,
             distance = distance_matrix, ratio=4, replace = F)
summary(m)
m$match.matrix
mout <- match.data(m)
final <- mout %>% group_by(subclass) %>% arrange(desc(treatment)) %>% 
  filter(abs(Day-first(Day))<=day_dist & Institution==first(Institution)) %>% 
  group_by(subclass) %>% filter(n()>1)

write_csv(final, "matched_data_alternative1_4_30.csv")



# 1:10 matching with 15 caliper
day_dist <- 15
distance_matrix <- dist(as.matrix(inf_omicron_subset%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()
m <- matchit(treatment ~ Day + Institution, data = inf_omicron_subset,
             method = "nearest", exact = "Institution", caliper = c(15),std.caliper = F,
             distance = distance_matrix, ratio=10, replace = F)
summary(m)
m$match.matrix
mout <- match.data(m)
final <- mout %>% group_by(subclass) %>% arrange(desc(treatment)) %>% 
  filter(abs(Day-first(Day))<=day_dist & Institution==first(Institution)) %>% group_by(subclass) %>% filter(n()>1)

write_csv(final, "matched_data_alternative1_10_15.csv")


  