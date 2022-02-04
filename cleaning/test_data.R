# Sophia Tan 1/27/22
# Combining CDCR COVID Infection and Testing Data through 1/15/22 


setwd("D:/CDCR Data/14 January 15 2022")

library(tidyverse)


###### COVID Infection Data ######
infection <- read.csv("CovidInfection_20220115.csv", sep=";")
head(infection)

infection %>% group_by(ResidentId) %>% summarise(count=n()) 

single_res <- filter(infection, ResidentId == 1611661470) %>% select(!Sequence)

infection <- infection %>% mutate(Day = as.Date(Day))
wide_infection <- infection %>% select(!Sequence) %>% 
  pivot_wider(id_cols = c("ResidentId", "Day", "Month"),
              names_from = "Element", 
              values_from = "Value",
              values_fill = NA) %>% arrange(ResidentId, Day)
names(wide_infection)
wide_infection <- wide_infection %>% mutate(ViralTestResultDate=as.Date(ViralTestResultDate))

filter(wide_infection, ResidentId==1611661326)

summary(wide_infection$Day) # some early dates - i.e. 2016

wide_infection_tests <- wide_infection %>% filter(!is.na(ViralTestStatus))

# some data has test result but no test result date
# all test result date data has correponding result though
no_result_date <- wide_infection %>% filter(!is.na(ViralTestStatus)) %>% filter(is.na(ViralTestResultDate))

# check reporting of dates
wide_infection_time_test <- wide_infection_tests %>% mutate(time = as.numeric(ViralTestResultDate-Day))
wide_infection_time_test %>% filter(time > 10)



###### COVID Testing Data ######
tests <- read.csv("CovidTests_20220115.csv", sep=";")
head(tests)
total_tests<-tests %>% group_by(ResidentId) %>% summarise(tests=n())
total_tests$tests %>% summary() # average resident took about 16 tests (not including possibility of multiple tests with same result)

tests <- tests %>% mutate(CollectionDate = as.Date(CollectionDate), ReceivedDate = as.Date(ReceivedDate))
# Keep only clear results - no FP or Inconclusive results
tests_clear <- tests %>% filter(Result %in% c("Positive", "Negative"))

# Some residents have multiple tests on a given day with clear results 
multiple_tests <- tests_clear %>% group_by(ResidentId, CollectionDate) %>% mutate(count=n()) %>% filter(count!=1)
filter(tests_clear, ResidentId == multiple_tests$ResidentId[1] & CollectionDate == multiple_tests$CollectionDate[1])

keep_pos_only_if_multiple <- tests_clear %>% arrange(Result) %>% group_by(ResidentId, CollectionDate) %>% 
  summarise(Result=last(Result), ReceivedDate=last(ReceivedDate), Institution=last(Institution), 
            Details=last(Details))

nrow(keep_pos_only_if_multiple)
avg_days_testing <- keep_pos_only_if_multiple %>% group_by(ResidentId) %>% summarise(num_testing_days = n())
avg_days_testing$num_testing_days %>% summary()



###### Complete Testing and Infection Data ######
# join cleaned infection data and testing data - 
tests_inf <- wide_infection %>% full_join(keep_pos_only_if_multiple, by=c("ResidentId", "Day"="CollectionDate"))
 
# all tests should match received date
# test here have multiple tests in 1 day - if all negative, keep first received result
filter(tests_inf, ReceivedDate != ViralTestResultDate)
tests_inf <- tests_inf %>% mutate(ReceivedDate = if_else(ReceivedDate != ViralTestResultDate & Result == ViralTestStatus, 
                                         ViralTestResultDate, ReceivedDate))

setwd("D:/code_ST")
write_csv(tests_inf %>% select(!grep("Viral", names(.))), "complete_testing_data.csv", append=F)
