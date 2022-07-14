# clean demographic data to approximate age and obtain demographic statistics about the study population
rm(list=ls())

library(tidyverse)
library(readr)

demographic_data <- read.csv("/Users/sophiatan/Documents/UCSF/ST files/Demographics_20220520.csv", sep = ";")
demographic_data
demographic_data$Demographic %>% unique()

demo_wide <- demographic_data %>% filter(Demographic %in% c("BirthYear", "Sex", "Ethnicity", "Race")) %>% 
  pivot_wider(id_cols = c("ResidentId"),
              names_from = "Demographic", 
              values_from = "Value",
              values_fill = NA) %>% arrange(ResidentId)

demo_wide

write_csv(demo_wide, "/Users/sophiatan/Documents/UCSF/cleaned_data/demographic_data_clean.csv")
