# Sophia Tan 12/15/23
# Clean demographic data 5/26/23

rm(list=ls())
gc()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/CDCR Data/May 26 2023 Data/")

demographic_data <- read.csv("Demographics_20230526.csv", sep = ";")
demographic_data
demographic_data$Demographic %>% unique()

# keep only demographic variables of interest
demo_wide <- demographic_data %>% filter(Demographic %in% c("BirthYear", "Sex", "Ethnicity", "Race")) %>% 
  pivot_wider(id_cols = c("ResidentId"),
              names_from = "Demographic", 
              values_from = "Value",
              values_fill = NA) %>% arrange(ResidentId)

demo_wide <- demo_wide %>% mutate(BirthYear=as.numeric(BirthYear))

demo_wide

write_csv(demo_wide, "D:/CCHCS_premium/st/leaky/cleaned-data/demographic121523.csv")


# clean security level data separately since residents change security level over time
security <- demographic_data %>% filter(Demographic=="SecurityLevel")
security
security <- security %>% filter((Ending==""|Ending>"2021-12-15")&Starting<"2023-03-15") %>% 
  mutate(level = case_when(Value=="I"|Value=="II"~1, # group levels 1 and 2 as low security level
                           Value=="III"~3,
                           Value=="IV"~4)) %>% 
  group_by(ResidentId) %>% arrange(ResidentId, Starting) %>% 
  mutate(same=c(NA, diff(level)))

security_summary <- security %>% filter(same%>%is.na()|same!=0) %>% 
  mutate(group=1:n()) %>% select(ResidentId, Starting, Value, level, group)
security <- security %>% left_join(security_summary)
security <- security %>% fill(group, .direction="down")
security <- security %>% 
  group_by(ResidentId, group) %>% summarise(Starting=first(Starting), Ending=last(Ending), level=first(level)) %>%
  ungroup() %>% select(!group)

security %>% write_csv("D:/CCHCS_premium/st/leaky/cleaned-data/cleaned_security_data021324.csv")
