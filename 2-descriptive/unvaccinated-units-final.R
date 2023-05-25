# Sophia Tan 1/13/23, updated 5/16/23
# Identify units that meet different vaccine requirements

rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)

d <- read_csv("allunits_noincarcreq_vaccination_analysis.csv")
testing <- read_csv("testing_vacc_clean.csv")
duration <- read_csv("housing_duration.csv")

# exclude units if resident's don't co-reside for at least 2 weeks
over_14 <- d %>% filter(duration >= 14)

# keep units where at least 1 resident is unvaccinated
any_unvacc_over14 <- over_14 %>% filter(any(both_unvacc|one_unvacc))

# any_unvacc_over14 %>% ggplot(aes(duration)) + geom_histogram() + 
#   scale_x_continuous("Duration of co-residence (days)",limits=c(0,365),
#                      expand=c(0,0)) + scale_y_continuous("Number of units",expand=c(0,0))
# any_unvacc_over14$duration %>% as.numeric() %>% summary()
# 
# p1 <- group_room_2_final %>% ggplot(aes(Institution)) + geom_bar() + 
#   scale_x_continuous(limits=c(1,35),breaks=1:35,labels=1:35, expand=c(0,0)) + 
#   scale_y_continuous("Number of units") 
# 
# p2 <- any_unvacc_over14 %>% ggplot(aes(Institution)) + geom_bar() + 
#   scale_x_continuous(limits=c(1,35),breaks=1:35,labels=1:35, expand=c(0,0)) + 
#   scale_y_continuous("Number of units") 
# 
# library(patchwork)
# p1/p2


# make sure no units have prior infection within the last 90 days
any_unvacc_over14_noinf <- any_unvacc_over14 %>% filter(!prior_inf_90)
any_unvacc_over14_noinf <- any_unvacc_over14_noinf %>% ungroup() %>% mutate(label=1:n())

# check testing data
residents <- c(any_unvacc_over14_noinf$ResidentId.1, any_unvacc_over14_noinf$ResidentId.2) %>% unique()
testing <- testing %>% filter(ResidentId %in% residents) %>% 
  group_by(ResidentId) %>% 
  filter(Day >= "2021-12-15")
testing <- testing %>% left_join(duration%>%select(ResidentId, last)%>%mutate(last=last+1))

testing_only <- testing %>% filter(!Result%>%is.na()) %>% 
  select(ResidentId, Result, Day, last) %>% 
  rename("Test" = "Day")
omicron_testing <- testing_only %>% group_by(ResidentId)

# omicron_testing%>%summarise(tests=n())%>%filter(tests<75)%>%
#   ggplot(aes(tests))+
#   geom_histogram()+xlab("Number of tests")
# (omicron_testing%>%summarise(tests=n()))$tests%>%summary()
# omicron_testing%>%ggplot(aes(Test))+geom_histogram()+xlab("Time")+ylab("Tests over time")

omicron_testing_summary <- omicron_testing %>% 
  mutate(time=as.numeric(difftime(last, "2021-12-15"))+1) %>%
  summarise(tests=n(), rate_testing=tests/first(time)*30)

# omicron_testing_summary$rate_testing%>%summary()
# omicron_testing_summary%>%
#   ggplot(aes(rate_testing))+
#   geom_histogram()+xlab("Testing rate (tests per 30 days)")

omicron_testing_summary1 <- omicron_testing_summary%>%filter(rate_testing>=1)

avg_1_test <- any_unvacc_over14_noinf %>% 
  mutate(test1=ResidentId.1 %in% omicron_testing_summary1$ResidentId,
         test2=ResidentId.2 %in% omicron_testing_summary1$ResidentId) %>%
  filter(test1|test2)

avg_1_test <- avg_1_test %>% filter(both_unvacc|(one_unvacc&((test1&vacc.1==0)|(test2&vacc.2==0))))

matching <- avg_1_test %>% rowwise() %>%
  mutate(primary=case_when(one_unvacc&vacc.1==0~ResidentId.1,
                           one_unvacc&vacc.2==0~ResidentId.2,
                           both_unvacc&test1&!test2~ResidentId.1,
                           both_unvacc&!test1&test2~ResidentId.2,
                           T~sample(c(ResidentId.1, ResidentId.2), 1)))

matching%>%select(label, one_unvacc, both_unvacc, ResidentId.1, vacc.1, test1, ResidentId.2, vacc.2, test2, primary)

matching <- matching %>% 
  mutate(secondary = ifelse(primary==ResidentId.1, ResidentId.2, ResidentId.1),
         inf.primary=ifelse(primary==ResidentId.1, inf.1, inf.2), 
         inf.secondary=ifelse(primary==ResidentId.1, inf.2, inf.1), 
         inf.primary=ifelse(inf.primary, 1, 0), 
         inf.secondary=ifelse(inf.secondary, 1, 0))

# if requiring that the secondary resident have prior infection
matching <- matching %>% filter(inf.primary & inf.secondary)

matching <- matching %>% 
  mutate(treatment = ifelse(both_unvacc, 1, 0))

matching %>% group_by(treatment) %>% summarise(n=n())

matching <- matching %>% rowwise() %>% mutate(adjusted_start=first+5,
                                              adjusted_end=min(as.Date("2022-12-15"), last + 5))

write_csv(matching, "full_data_prematching_noincarcreq_vaccination052223.csv")

