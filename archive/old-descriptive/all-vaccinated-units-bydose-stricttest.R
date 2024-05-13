# Sophia Tan 1/13/23, updated 5/16/23
# Identify units that meet different vaccine requirements

rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)

d <- read_csv("allunits_vaccinationdose_analysis052323.csv")
testing <- read_csv("testing_vacc_clean.csv")
duration <- read_csv("housing_duration.csv")

# exclude units if resident's don't co-reside for at least 2 weeks 
over_14 <- d %>% filter(duration >= 14)

# keep units where at least 1 resident is unvaccinated
# any_unvacc_over14 <- over_14 %>% filter(any(both_unvacc|one_unvacc))

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
any_unvacc_over14_noinf <- over_14 %>% filter(!prior_inf_90)
any_unvacc_over14_noinf <- any_unvacc_over14_noinf %>% ungroup() %>% mutate(label=1:n())

residents <- c(any_unvacc_over14_noinf$ResidentId.1, any_unvacc_over14_noinf$ResidentId.2) %>% unique()
testing <- testing %>% filter(ResidentId %in% residents) %>% 
  group_by(ResidentId) %>% 
  filter(Day >= "2021-12-15")
testing_only <- testing %>% filter(!Result%>%is.na()) %>% 
  select(ResidentId, Result, Day) %>% 
  rename("Test" = "Day")

units_testing1 <- any_unvacc_over14_noinf %>% 
  left_join(testing_only, by=c("ResidentId.1"="ResidentId")) %>% 
  filter(Test >= first & Test <= last) %>% rename("Test.1" = "Test")
units_testing1 %>% select(Institution, RoomId, first, last, ResidentId.1, Test.1)

# test breaking apart 
a <- units_testing1 %>% select(label, ResidentId.1, first,last,duration,Test.1) %>%
  group_by(label) %>% 
  mutate(diff_test=diff(c(first(first),Test.1))) 

a <- a %>% group_by(label) %>% mutate(new_chunk=diff_test>14)
chunks <- a %>% filter(Test.1==first(Test.1)|new_chunk) %>% select(label, Test.1) %>% mutate(chunks=1:n())
a <- a %>% left_join(chunks, by=c("label", "Test.1")) 
a <- a %>% arrange(label, Test.1) %>% fill(chunks, .direction="down")
a_final <- a %>% group_by(label, chunks) %>% 
  mutate(first_chunked = first(Test.1),
         last_chunked = last(Test.1))%>% 
  summarise_all(first) %>% 
  mutate(duration_testing=last_chunked-first_chunked) %>% 
  group_by(label) %>% 
  filter(duration_testing==max(duration_testing)) %>% 
  filter(duration_testing > 0) %>% filter(chunks==first(chunks)) %>% 
  select(label, ResidentId.1, first_chunked, last_chunked, duration_testing)


units_testing2 <- any_unvacc_over14_noinf %>% left_join(testing_only, by=c("ResidentId.2"="ResidentId")) %>% 
  filter(Test >= first & Test <= last) %>% rename("Test.2" = "Test")
units_testing2 %>% select(Institution, RoomId, first, last, ResidentId.2, Test.2)

# test breaking apart 
a2 <- units_testing2 %>% select(label, ResidentId.2, first,last,duration,Test.2) %>%
  group_by(label) %>% 
  mutate(diff_test=diff(c(first(first),Test.2))) 

a2 <- a2 %>% group_by(label) %>% mutate(new_chunk=diff_test>14)
chunks <- a2 %>% filter(Test.2==first(Test.2)|new_chunk) %>% select(label, Test.2) %>% mutate(chunks=1:n())
a2 <- a2 %>% left_join(chunks, by=c("label", "Test.2")) 
a2 <- a2 %>% arrange(label, Test.2) %>% fill(chunks, .direction="down")
a2_final <- a2 %>% group_by(label, chunks) %>% 
  mutate(first_chunked = first(Test.2),
         last_chunked = last(Test.2))%>% 
  summarise_all(first) %>% 
  mutate(duration_testing=last_chunked-first_chunked) %>% 
  group_by(label) %>% 
  filter(duration_testing==max(duration_testing)) %>% 
  filter(duration_testing > 0) %>% filter(chunks==first(chunks)) %>% 
  select(label, ResidentId.2, first_chunked, last_chunked, duration_testing)

has_test <- any_unvacc_over14_noinf %>% 
  mutate(test1=label %in% a_final$label,
         test2=label %in% a2_final$label) %>%
  filter(test1|test2)

has_test <- has_test %>% 
  left_join(a_final %>% select(label, first_chunked, last_chunked, duration_testing), by="label") %>%
  left_join(a2_final %>% select(label, first_chunked, last_chunked, duration_testing), by="label", suffix = c(".1", ".2")) %>%
  rowwise() %>%
  mutate(primary=case_when(test1&!test2~ResidentId.1,
                           !test1&test2~ResidentId.2,
                           test1&test2&duration_testing.1==duration_testing.2~sample(c(ResidentId.1, ResidentId.2), 1),
                           test1&test2&duration_testing.1>duration_testing.2~ResidentId.1,
                           T~ResidentId.2),
         secondary=ifelse(primary==ResidentId.1, ResidentId.2, ResidentId.2))

matching <- has_test %>% 
  mutate(first = if_else(primary==ResidentId.1, first_chunked.1, first_chunked.2),
         last = if_else(primary==ResidentId.1, last_chunked.1, last_chunked.2),
         inf.primary=ifelse(primary==ResidentId.1, inf.1, inf.2), 
         inf.secondary=ifelse(primary==ResidentId.1, inf.2, inf.1), 
         vacc.primary=ifelse(primary==ResidentId.1, num_dose_adjusted.1, num_dose_adjusted.2),
         vacc.secondary=ifelse(primary==ResidentId.1, num_dose_adjusted.2, num_dose_adjusted.1))

matching <- matching %>% select(!c(test1, test2, duration_testing.1, duration_testing.2))

matching <- matching %>% 
  mutate(treatment = ifelse(vacc.secondary==0, 1, 0))

matching$treatment %>% table()


write_csv(matching, "full_data_prematching_allvacc_stricttesting.csv")

