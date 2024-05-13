# Sophia Tan 5/22/23
# Identify units that meet different vaccine requirements by dose

rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)

d <- read_csv("allunits_noincarcreq_vaccinationdose_analysis081423.csv")
testing <- read_csv("testing_vacc_clean.csv")
duration <- read_csv("housing_duration.csv")

# exclude units if resident's don't co-reside for at least 2 weeks
over_14 <- d %>% filter(duration >= 14)

# make sure no units have prior infection within the last 90 days
over14_noinf <- over_14 %>% filter(!prior_inf_90)
over14_noinf <- over14_noinf %>% ungroup() %>% mutate(label=1:n())

# check testing data
residents <- c(over14_noinf$ResidentId.1, over14_noinf$ResidentId.2) %>% unique()
testing <- testing %>% filter(ResidentId %in% residents) %>% 
  group_by(ResidentId) %>% 
  filter(Day >= "2021-12-15")
testing <- testing %>% left_join(duration%>%select(ResidentId, last)%>%mutate(last=last+1))

testing_only <- testing %>% filter(!Result%>%is.na()) %>% 
  select(ResidentId, Result, Day, last) %>% 
  rename("Test" = "Day")
omicron_testing <- testing_only %>% group_by(ResidentId)

omicron_testing_summary <- omicron_testing %>% 
  mutate(time=as.numeric(difftime(last, "2021-12-15"))+1) %>%
  summarise(tests=n(), rate_testing=tests/first(time)*30)

omicron_testing_summary1 <- omicron_testing_summary%>%filter(rate_testing>=1)

avg_1_test <- over14_noinf %>% 
  mutate(test1=ResidentId.1 %in% omicron_testing_summary1$ResidentId,
         test2=ResidentId.2 %in% omicron_testing_summary1$ResidentId) %>%
  filter(test1|test2)

avg_1_test <- avg_1_test %>% rowwise() %>%
  mutate(primary=case_when(test1&!test2~ResidentId.1,
                           !test1&test2~ResidentId.2,
                           test1&test2~sample(c(ResidentId.1, ResidentId.2), 1)))

avg_1_test%>%select(label, one_unvacc, both_unvacc, ResidentId.1, vacc.1, inf.1, test1, ResidentId.2, vacc.2, inf.2, test2, primary)

matching <- avg_1_test %>% 
  mutate(secondary = ifelse(primary==ResidentId.1, ResidentId.2, ResidentId.1),
         inf.primary=ifelse(primary==ResidentId.1, inf.1, inf.2), 
         inf.secondary=ifelse(primary==ResidentId.1, inf.2, inf.1), 
         vacc.primary=ifelse(primary==ResidentId.1, num_dose_grouped.1, num_dose_grouped.2),
         vacc.secondary=ifelse(primary==ResidentId.1, num_dose_grouped.2, num_dose_grouped.1),
         vacc.primary.doses=ifelse(primary==ResidentId.1, num_dose_adjusted.1, num_dose_adjusted.2),
         vacc.secondary.doses=ifelse(primary==ResidentId.1, num_dose_adjusted.2, num_dose_adjusted.1))

matching <- matching %>% 
  mutate(treatment = ifelse(vacc.secondary==0, 1, 0))

matching <- matching %>% filter(inf.primary&inf.secondary)

matching %>% group_by(vacc.primary, vacc.secondary) %>% summarise(n=n())

matching$treatment %>% table()

# matching <- matching %>% rowwise() %>% mutate(adjusted_start=first+5,
#                                               adjusted_end=min(as.Date("2022-12-15"), last + 5))

write_csv(matching, "allvacc_full_data_prematching_relaxincarceration_priorinf_bydose_081423v2.csv")

