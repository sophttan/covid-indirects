# Sophia Tan 1/28/22
# Combine cleaned vaccination and infection/testing data 1/15/22 

rm(list=ls())

setwd("D:/stan5/code_ST/march-data/")

library(readr)
library(tidyverse)

test_inf <- read_csv("complete_testing_data.csv")
vacc <- read_csv("cleaned_vaccination_data.csv")

d <- test_inf %>% 
  full_join(vacc, by=c("ResidentId", "Day" = "Date")) %>% 
  arrange(ResidentId, Day) 

# 260,032 residents included over time in this dataset
# 3 residents have vaccine records but no testing/infection data (IDs 1619718382, 1627977166, 1635328030)
vacc[!(vacc$ResidentId %in% (test_inf$ResidentId %>% unique())),] %>% group_by(ResidentId)
d <- d %>% group_by(ResidentId) #%>% select(!Institution)

# 55,426 residents were infected at least once (Similar to report by CalProtect, which includes less data (less time))
d %>% filter(any(Result=="Positive"))
#d <- d %>% mutate(antigen_negative = ifelse(is.na(antigen_negative) & !is.na(Result), F, antigen_negative))

# fill in missing vaccine dose data
d_filled <- d %>% fill(num_dose, .direction = "down") %>% 
  fill(max_dose, full_vacc, booster_add_dose, booster_add_dose_mixed, incomplete, .direction = "downup") %>% 
  replace_na(list(num_dose=0, max_dose=0, full_vacc=0, booster_add_dose=0, booster_add_dose_mixed=0, incomplete=0)) 
d_filled[1:200,] %>% view()

# only 146,679 residents have been tested at some point, 113,353 have never been tested (or no record)
tests_vacc <- d_filled %>% filter(any(!is.na(Result)))
infections <- tests_vacc %>% filter(any(Result=="Positive"))


# diffDay_first_inf represents num days since day of first positive PCR test
# diffDay_between_tests represents num days since last positive PCR test
label_infections <- infections %>% select(ResidentId, Day, Result) %>% 
  filter(Result=="Positive") %>% mutate(num_pos = 1:n(), 
                                        diffDay_first_inf = as.numeric(Day-Day[1]),
                                        diffDay_between_tests = c(0, diff(Day)))

# remove multiple tests for a single infection (time between tests <90 days)
# redefine new reinfection as positive test >90 days after previous infection 
removed_mult_tests <- label_infections %>% 
  filter(diffDay_first_inf==0 | (diffDay_first_inf > 90 & diffDay_between_tests > 90))

# there are some residents with infections <90 days apart that tested negative in between positive tests
# some residents with positive tests >90 days apart without recorded tests in between (could be sustained PCR positivity and not reinfection)
tests_under_90 <- label_infections %>% 
  filter(max(num_pos) > 1) 

inf_subset <- infections %>% select(ResidentId, Day, Result, Details)
check_neg_test <- filter(inf_subset, ResidentId %in% tests_under_90$ResidentId %>% unique())
check_neg_test <- check_neg_test %>% full_join(tests_under_90, c("ResidentId", "Day", "Result"))
check_neg_test2 <- check_neg_test %>% fill(num_pos, .direction="down") %>% 
  filter(!is.na(num_pos)) %>% filter(num_pos < max(num_pos))
# if time between infections is <90 days, must have negative PCR test(s) between positive tests to be considered reinfection
has_neg_test <- check_neg_test2 %>% group_by(ResidentId, num_pos) %>% 
  summarise(Day=first(Day), neg_test=any(Result=="Negative" & grepl("RNA", Details))) %>%
  mutate(num_pos=num_pos+1)
combined <- tests_under_90 %>% full_join(has_neg_test %>% select(!Day), by=c("ResidentId", "num_pos")) 
combined <- combined %>% filter(neg_test == T | is.na(neg_test))

# include residents and infections that meet either criteria for reinfection
full_mult_inf <- removed_mult_tests %>% full_join(combined, c("ResidentId", "Day")) %>% mutate(num_pos=1:n()) %>%
  select(ResidentId, Day, num_pos, neg_test) %>% mutate(Result="Positive")

# check multiple tests
joined <- label_infections %>% full_join(full_mult_inf, c("ResidentId", "Day")) %>%
  select(ResidentId, Day, num_pos.x, num_pos.y, diffDay_first_inf, diffDay_between_tests, neg_test) 
joined %>% filter(any(is.na(num_pos.y))) %>% view()



# add vaccination data to test data
unique_vacc_infections <- tests_vacc %>% 
  right_join(full_mult_inf %>% select(!c(neg_test)), 
             by=c("ResidentId", "Day")) #%>% replace_na(list(Month=202201))
total_inf_time <- unique_vacc_infections %>% group_by(Day) %>% summarise(inf=n())
total_inf_time %>% ggplot(aes(Day, inf)) + geom_line() + ylab("Confirmed cases (monthly)") + xlab("Day")

first_inf <- filter(unique_vacc_infections, num_pos==1)
first_inf_unvacc <- filter(first_inf, num_dose==0) # 50680 residents were infected when unvaccinated
first_inf_partvacc <- filter(first_inf, num_dose==1 & full_vacc == 2) # 1088 infected with 1 dose of an mRNA vaccine

# breakthroughs
first_inf_fullvacc <- filter(first_inf, num_dose > 0 & num_dose==full_vacc) # 2347 
first_inf_booster_vacc <- filter(first_inf, num_dose > full_vacc) # 1311

# residents that experienced multiple confirmed infections 
re_inf <- filter(unique_vacc_infections, max(num_pos) > 1) # 1,622 experienced reinfections
re_inf %>% filter(num_pos > 1 & num_dose==0) # 795 of individuals who experienced 1+ infections were unvaccinated

re_inf %>% filter((num_dose>0&num_pos==1)|(num_dose>0&num_pos>1)) %>%
  select(ResidentId, Day, num_pos)

has_2_inf <- re_inf %>% filter(max(num_pos)==2) 

more_than_2_inf <- re_inf %>% filter(max(num_pos)>2) # 31 experienced multiple reinfections
more_than_2_inf %>% group_by(ResidentId) %>% summarise(inf=n(), max_dose=first(max_dose), full_vacc=first(full_vacc)) %>% 
  filter(full_vacc==0) %>% nrow() # 7 out of 31 are unvaccinated

pre_vacc <- unique_vacc_infections %>% filter(Day < "2020-12-07")
post_vacc <- unique_vacc_infections %>% filter(Day >= "2020-12-07")
filter(post_vacc, num_dose==0) %>% nrow()# 50680 residents were infected when unvaccinated
filter(post_vacc, num_dose==1 & full_vacc == 2) # 1088 infected with 1 dose of an mRNA vaccine
filter(post_vacc, num_dose > 0 & num_dose==full_vacc)
filter(post_vacc, num_dose > full_vacc)


vacc_infections_removed_extra_inf <- tests_vacc %>% 
  full_join(full_mult_inf %>% select(!c(neg_test, Result)), 
            by=c("ResidentId", "Day")) #%>% replace_na(list(Month=202201))

write_csv(vacc_infections_removed_extra_inf, "has_testing_data_aggregated_infections.csv")
