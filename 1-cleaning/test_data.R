# Sophia Tan 2/26/24
# Combining CDCR COVID Infection and Testing Data through 5/26/23 
# Analysis uses testing and infection data 3/1/20-12/15/22 

source(here::here("config.R"))

setwd("D:/CCHCS_premium/CDCR Data/May 26 2023 Data/")

###### COVID Testing Data ######
tests <- read_delim("CovidTests_20230526.csv", delim=";")
head(tests)

tests <- tests %>% mutate(CollectionDate = as.Date(CollectionDate), ReceivedDate = as.Date(ReceivedDate)) #4,117,203 tests
tests %>% filter(Result %in% c("FalsePositive","Inconclusive")) # 18,708 tests FP or inconclusive

# Keep only clear results - no FP or Inconclusive results
tests_clear <- tests %>% filter(Result %in% c("Positive", "Negative")) %>% 
  mutate(pcr = ifelse(grepl("RNA|NA|PCR", Details), T, F), 
         antigen = ifelse(grepl("POC|Antigen", Details), T, F),
         unknown = !(pcr|antigen)) %>% 
  group_by(ResidentId, CollectionDate)


# Some residents have multiple tests on a given day with clear and conflicting results 
multiple_tests <- tests_clear %>% filter(n()>1) # only 4195 resident-days
one_result_day <- tests_clear %>% filter(n()==1)


# if multiple tests on same day (antigen and pcr), keep pcr result -
## if any missing test type and conflicting results, exclude both/all tests collected on that day
keep_pcr_if_multiple <- multiple_tests %>% 
  filter(all(!unknown)) %>%  # 435 resident-days have missing test types
  filter(!all(pcr)) %>% # 197 resident-days have pcr tests with conflicting results
  filter(pcr) # 92 residents have no pcr test (conflicting antigen tests)

# combine all tests 
final_tests <- rbind(one_result_day, keep_pcr_if_multiple) %>% arrange(ResidentId, CollectionDate) #4,093,576 tests per resident/day included
# number of tests
nrow(final_tests) 

# total tests per resident
total_tests <- final_tests %>% group_by(ResidentId) %>% summarise(tests=n())
total_tests$tests %>% summary() # average resident took on average 23 tests (if accepting testing and not including possibility of multiple tests with same result)


# label testing data with infections (i.e. resident might be test positive multiple times during 1 infection)
# keep residents with any positive test
infections <- final_tests %>% arrange(ResidentId, CollectionDate) %>% filter(any(Result=="Positive",na.rm=T))

# diffDay_first_inf represents num days since day of first positive PCR test
# diffDay_between_tests represents num days since last positive PCR test
label_infections <- infections %>% 
  select(ResidentId, CollectionDate, Result) %>% 
  group_by(ResidentId) %>% 
  filter(Result=="Positive") %>% 
  mutate(num_pos = 1:n(), 
         diffDay_first_inf = as.numeric(CollectionDate-first(CollectionDate)),
         diffDay_between_tests = c(0, diff(CollectionDate)))

# remove multiple tests for a single infection (time between tests <90 days)
# redefine new reinfection as positive test >90 days after previous infection 
removed_mult_tests <- label_infections %>% 
  filter(diffDay_first_inf==0 | (diffDay_first_inf > 90 & diffDay_between_tests > 90))

removed_mult_tests <- removed_mult_tests %>% mutate(num_pos=1:n()) %>% select(!c(diffDay_first_inf, diffDay_between_tests))


# num_pos column used to label the number of infections a resident has had over time (num_pos = 1 is resident's first infection/has had one previous infection)
final_tests_labelinf <- final_tests %>% left_join(removed_mult_tests)
final_tests_labelinf <- final_tests_labelinf %>% arrange(ResidentId, CollectionDate) %>% group_by(ResidentId) %>% fill(num_pos, .direction = "down")
final_tests_labelinf <- final_tests_labelinf %>% replace_na(list(num_pos=0))


# save full and cleaned test and infection data
# 5/13 change columns being saved - keep received date
final_tests_labelinf %>% select(!c(Institution, Details)) %>% rename("Day"="CollectionDate") %>% 
  write_csv("D:/CCHCS_premium/st/cleaned-data/complete_testing_data051324.csv")

infections <- final_tests_labelinf %>% group_by(ResidentId, num_pos) %>% summarise_all(first) %>% filter(num_pos>0)  %>% 
  select(!c(Institution, Details)) %>% rename("Day"="CollectionDate") 
infections %>% write_csv("D:/CCHCS_premium/st/cleaned-data/infection_data051324.csv")


# descriptive data on test turnaround time
study_period <- final_tests_labelinf %>% filter(CollectionDate >= "2021-12-15" & CollectionDate <= "2022-12-15")
((study_period %>% ungroup() %>% mutate(time=(ReceivedDate-CollectionDate)%>%as.numeric()))$time %>% table())/nrow(study_period)*100


# # plot testing over time
# tests_plot <- final_tests %>% 
#   mutate(week=difftime(CollectionDate, as.Date("2020-03-01"), units="weeks")%>%as.numeric()%>%round()) 
# 
# tests_plot_group <- tests_plot %>% group_by(week) %>% 
#   summarise(CollectionDate=min(CollectionDate), resident_tests=n())
# 
# tests_plot_group %>% 
#   ggplot(aes(as.POSIXct(CollectionDate), resident_tests)) + 
#   geom_line() + 
#   scale_x_datetime("Time",date_breaks = "1 month", expand = c(0,0)) + 
#   scale_y_continuous("Total weekly resident-day tests") + 
#   theme(axis.text.x = element_text(angle=90))
