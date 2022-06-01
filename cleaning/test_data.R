# Sophia Tan 1/27/22
# Combining CDCR COVID Infection and Testing Data through 1/15/22 

setwd("/Users/sophiatan/Documents/UCSF")
#setwd("D:/CDCR Data/15 March 25 2022/")
#setwd("D:/CDCR Data/14 January 15 2022")

library(tidyverse)

# # symptom data not complete - no need for infection data
# ###### COVID Infection Data ######
# infection <- read_delim("ST files/CovidInfection_20220520.csv", delim=";")
# #infection <- read.csv("CovidInfection_20220325.csv", sep=";")
# #infection <- read.csv("CovidInfection_20220115.csv", sep=";")
# head(infection)
# 
# #infection %>% group_by(ResidentId) %>% summarise(count=n()) 
# 
# single_res <- filter(infection, ResidentId == 1611661470) #%>% select(!Sequence)
# 
# infection <- infection %>% mutate(Day = as.Date(Day))
# wide_infection <- infection %>% #select(!Sequence) %>% 
#   pivot_wider(id_cols = c("ResidentId", "Day", "Month"),
#               names_from = "Element", 
#               values_from = "Value",
#               values_fill = NA) %>% arrange(ResidentId, Day)
# names(wide_infection)
# #wide_infection <- wide_infection %>% mutate(ViralTestResultDate=as.Date(ViralTestResultDate))
# 
# filter(wide_infection, ResidentId==1611661326)
# 
# summary(wide_infection$Day) # some early dates - i.e. 2016
# 
# wide_infection_tests <- wide_infection #%>% filter(!is.na(ViralTestStatus))
# 
# # some data has test result but no test result date
# # all test result date data has correponding result though
# no_result_date <- wide_infection %>% filter(!is.na(ViralTestStatus)) %>% filter(is.na(ViralTestResultDate))
# 
# # check reporting of dates
# wide_infection_time_test <- wide_infection_tests %>% mutate(time = as.numeric(ViralTestResultDate-Day))
# wide_infection_time_test %>% filter(time > 10)



###### COVID Testing Data ######
tests <- read_delim("ST files/CovidTests_20220520.csv", delim=";")
#tests <- read.csv("CovidTests_20220325.csv", sep=";")
#tests <- read.csv("CovidTests_20220115.csv", sep=";")
head(tests)
total_tests<-tests %>% group_by(ResidentId) %>% summarise(tests=n())
total_tests$tests %>% summary() # average resident took about 16 tests (not including possibility of multiple tests with same result)

tests <- tests %>% mutate(CollectionDate = as.Date(CollectionDate), ReceivedDate = as.Date(ReceivedDate))
# Keep only clear results - no FP or Inconclusive results
tests_clear <- tests %>% filter(Result %in% c("Positive", "Negative"))

# Some residents have multiple tests on a given day with clear results 
multiple_tests <- tests_clear %>% group_by(ResidentId, CollectionDate) %>% filter(n()>1)
one_result_day <- tests_clear %>% group_by(ResidentId, CollectionDate) %>% filter(n()==1)
filter(tests_clear, ResidentId == multiple_tests$ResidentId[1] & CollectionDate == multiple_tests$CollectionDate[1])

# 5/31/22 if multiple tests on same day (antigen and pcr), keep pcr result -
## if any missing test type and conflicting results, exclude both/all tests collected on that day
tests_clear_with_type <- tests_clear %>% mutate(pcr = ifelse(grepl("RNA|NA|PCR", Details), T, F), 
                                                antigen = ifelse(grepl("POC|Antigen", Details), T, F)) 
keep_pcr_if_multiple <- tests_clear_with_type %>% 
  group_by(ResidentId, CollectionDate) %>% mutate(keep=ifelse(n()==1|(n()>1&pcr), T, F)) %>% mutate(p_a=any(pcr)&any(antigen))

keep_pcr_if_multiple %>% filter(n()>1 & !p_a) # excluding 832 (<0.05%) tests because not all tests on the same day had a description of the test type and results were conflicting
keep_pcr_if_multiple %>% filter(n()>1 & p_a) # excluding 1696 (<0.1%) rapid tests because of assumed false negative/false positive

final_tests <- keep_pcr_if_multiple %>% filter(n()==1|(keep&p_a)) #2,932,228 tests per resident/day included
final_tests <- final_tests %>% filter(n()==1|!all(pcr))
final_tests %>% select(!c(Institution, keep, p_a)) %>% rename("Day"="CollectionDate") %>% write_csv("cleaned_data/complete_testing_data.csv")

# # 4/15/22 update to work on more edge cases in new 3/15/22 data
# multiple_tests <- multiple_tests %>% mutate(pcr = ifelse(grepl("RNA|NA|PCR", Details), T, F),
#                                             antigen = ifelse(grepl("POC|Antigen", Details), T, F), 
#                                             both = pcr&antigen) 
# keep_pos_only_if_multiple <- multiple_tests %>% mutate(keep = ifelse((pcr&Result=="Positive")|(pcr&Result=="Negative"), T, F))
# # remove if there are both positive and negative results for pcr tests or unknown on a single day (222 collection dates)
# keep_pos_only_if_multiple <- keep_pos_only_if_multiple %>% filter(unique(keep) %>% length()>1)
# # keep pcr result if there are multiple tests, mark if antigen test is negative but pcr is positive
# keep_pos_only_if_multiple <- keep_pos_only_if_multiple %>% filter(pcr) %>% left_join(keep_pos_only_if_multiple %>% summarise(antigen_negative = ifelse(any(pcr&Result=="Positive")&any(antigen&Result=="Negative"), T,F)))
# keep_pos_only_if_multiple <- keep_pos_only_if_multiple %>% select(!keep)
# 
# one_result_day <- one_result_day %>% mutate(pcr = ifelse(grepl("RNA|NA|PCR", Details), T, F),
#                                             antigen = ifelse(grepl("POC|Antigen", Details), T, F), 
#                                             both = pcr&antigen, 
#                                             antigen_negative = F) 
# 
# all_tests_Cleaned <- one_result_day %>% rbind(keep_pos_only_if_multiple)
# 
# setwd("D:/stan5/code_ST")
# write_csv(all_tests_Cleaned %>% rename("Day"="CollectionDate") %>% select(!c(Institution)), "march-data/complete_testing_data.csv", append=F)
