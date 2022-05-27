
library(tidyverse)

demographic_data <- read.csv("D:/CDCR Data/15 March 25 2022/Demographics_20220325.csv", sep = ";")
demographic_data

demo_wide <- demographic_data %>% filter(Demographic %in% c("BirthYear", "Sex")) %>% 
  pivot_wider(id_cols = c("ResidentId"),
              names_from = "Demographic", 
              values_from = "Value",
              values_fill = NA) %>% arrange(ResidentId)

demo_wide

write_csv(demo_wide, "D:/stan5/code_ST/demographic_data_clean.csv")
