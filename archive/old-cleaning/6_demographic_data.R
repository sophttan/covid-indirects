# clean demographic data to approximate age and obtain demographic statistics about the study population
rm(list=ls())

library(tidyverse)
library(readr)

setwd("D:/CCHCS_premium/CDCR Data/Dec 16 2022 Data")

demographic_data <- read.csv("Demographics_20221216.csv", sep = ";")
demographic_data
demographic_data$Demographic %>% unique()

demo_wide <- demographic_data %>% filter(Demographic %in% c("BirthYear", "Sex", "Ethnicity", "Race")) %>% 
  pivot_wider(id_cols = c("ResidentId"),
              names_from = "Demographic", 
              values_from = "Value",
              values_fill = NA) %>% arrange(ResidentId)

demo_wide <- demo_wide %>% mutate(BirthYear=as.numeric(BirthYear))

demo_wide

write_csv(demo_wide, "D:/CCHCS_premium/st/indirects/cleaned-data/demographic_data_clean.csv")
