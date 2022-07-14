# summary statistics about testing frequency among study population (including both index cases and close contacts)

rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")

library(tidyverse)
library(readr)
library(scales)
library(patchwork)
library(reshape2)

d <- read_csv("housing_inf_data.csv")
matched <- read_csv("matched_data.csv")

omicron_data <- d %>% filter(Day >= "2021-12-15") %>% filter(ResidentId %in% matched$index_id | ResidentId %in% matched$contact_id) %>% 
  filter(!Result %>% is.na()) 
omicron_all_data_summary <- omicron_data %>% group_by(ResidentId) %>% summarise(count=n())
# how many days were residents tested?
omicron_all_data_summary$count %>% summary()

# time between tests
omicron_data <- omicron_data %>% group_by(ResidentId) %>% arrange(Day) %>% summarise(betweentests = c(0,diff(Day)))
(omicron_data %>% filter(betweentests > 0))$betweentests %>% summary() 

p1 <- omicron_all_data_summary %>% ggplot(aes(count))+geom_histogram(color="white", bins=20) + 
  scale_y_continuous("Number of residents", expand=c(0,0)) + 
  scale_x_continuous("Number of days received tests", expand=c(0,0)) + 
  labs(subtitle="A") + 
  theme(panel.background = element_blank(), 
        legend.title=element_blank(),
        legend.key=element_blank(),
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())

p2 <- omicron_data %>% filter(betweentests != 0 & betweentests <= 100) %>% 
  ggplot(aes(betweentests))+geom_histogram(color="white", bins=20) + 
  scale_y_continuous("Number of tests", expand=c(0,0)) + 
  scale_x_continuous("Days between tests", expand=c(0,0)) + 
  labs(subtitle="B") + 
  theme(panel.background = element_blank(), 
        legend.title=element_blank(),
        legend.key=element_blank(),
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())

(p1+p2) %>% ggsave(filename="/Users/sophiatan/Documents/UCSF/CDCR-Calprotect/figures/appendix/testing_diagnostics_a1.jpg", width=6, height=4)





