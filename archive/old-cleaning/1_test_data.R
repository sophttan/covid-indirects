# Sophia Tan 1/3/23
# Combining CDCR COVID Infection and Testing Data through 12/16/22 

setwd("D:/CCHCS_premium/CDCR Data/Dec 16 2022 Data")

library(tidyverse)


###### COVID Testing Data ######
tests <- read_delim("CovidTests_20221216.csv", delim=";")
head(tests)
total_tests<-tests %>% group_by(ResidentId) %>% summarise(tests=n())
total_tests$tests %>% summary() # average resident took about 22 tests (not including possibility of multiple tests with same result)

tests <- tests %>% mutate(CollectionDate = as.Date(CollectionDate), ReceivedDate = as.Date(ReceivedDate))
# number of tests
nrow(tests) # 3.75 million resident-day tests (possibility of multiple tests)
tests$Result %>% unique()
tests %>% filter(Result %in% c("FalsePositive","Inconclusive")) # 17k tests FP or inconclusive
# Keep only clear results - no FP or Inconclusive results
tests_clear <- tests %>% filter(Result %in% c("Positive", "Negative")) %>% 
  mutate(pcr = ifelse(grepl("RNA|NA|PCR", Details), T, F), 
         antigen = ifelse(grepl("POC|Antigen", Details), T, F),
         unknown = !(pcr|antigen)) %>% 
  group_by(ResidentId, CollectionDate)


# Some residents have multiple tests on a given day with clear results 
multiple_tests <- tests_clear %>% filter(n()>1) # only 3386 resident-days
one_result_day <- tests_clear %>% filter(n()==1)


# if multiple tests on same day (antigen and pcr), keep pcr result -
## if any missing test type and conflicting results, exclude both/all tests collected on that day
keep_pcr_if_multiple <- multiple_tests %>% filter(all(!unknown)) %>%  # 394 resident-days have missing test type
  filter(!all(pcr)) %>% filter(pcr) # 173 resident-days have pcr tests with conflicting results


final_tests <- rbind(one_result_day, keep_pcr_if_multiple) %>% arrange(ResidentId, CollectionDate) #3,731,885 tests per resident/day included
final_tests %>% select(!c(Institution)) %>% rename("Day"="CollectionDate") %>% 
  write_csv("D:/CCHCS_premium/st/indirects/cleaned-data/complete_testing_data.csv")

# setwd("D:/stan5/code_ST")
# write_csv(all_tests_Cleaned %>% rename("Day"="CollectionDate") %>% select(!c(Institution)), "march-data/complete_testing_data.csv", append=F)
