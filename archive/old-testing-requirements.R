# Sophia Tan 1/13/23, updated 5/16/23
# Identify units that meet different vaccine requirements

rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)

d <- read_csv("allunits_noincarcreq_vaccinationdose_analysis052423.csv")
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


treatment <- any_unvacc_over14_noinf %>% filter(one_unvacc) %>% 
  filter((label %in% a_final$label|vacc.1>0) & (label %in% a2_final$label|vacc.2>0)) %>%
  mutate(primary = case_when(vacc.1==0~ResidentId.1, 
                             vacc.2==0~ResidentId.2)) %>% 
  mutate(inf.primary = ifelse(vacc.1==0, num_pos.1>0, num_pos.2>0),
         inf.secondary = ifelse(vacc.1>0, num_pos.1>0, num_pos.2>0)) %>% 
  left_join(a_final, by=c("label", "primary"="ResidentId.1")) %>% 
  left_join(a2_final, by=c("label", "primary"="ResidentId.2")) %>% 
  mutate(first_chunked.x=as.character(first_chunked.x), 
         first_chunked.y=as.character(first_chunked.y),
         last_chunked.x=as.character(last_chunked.x),
         last_chunked.y=as.character(last_chunked.y)) %>%
  replace_na(list(first_chunked.x="", first_chunked.y="",
                  last_chunked.x="", last_chunked.y="")) %>% 
  mutate(first_chunked=paste0(first_chunked.x, first_chunked.y)%>%as.Date(), 
         last_chunked=paste0(last_chunked.x, last_chunked.y)%>%as.Date())



control <- any_unvacc_over14_noinf %>% filter(both_unvacc) %>% 
  filter(label %in% a_final$label | label %in% a2_final$label) 

controlboth <- control %>% filter(label %in% a_final$label & label %in% a2_final$label) %>%
  left_join(a_final, by=c("label", "ResidentId.1"="ResidentId.1")) %>% 
  left_join(a2_final, by=c("label", "ResidentId.2"="ResidentId.2")) %>% 
  rowwise() %>% 
  mutate(primary=case_when(duration_testing.x>duration_testing.y~ResidentId.1,
                           duration_testing.x<duration_testing.y~ResidentId.2,
                           T~sample(c(ResidentId.1, ResidentId.2), size = 1))) %>%
  mutate(first_chunked=case_when(primary==ResidentId.1~first_chunked.x, 
                                  T~first_chunked.y),
         last_chunked=case_when(primary==ResidentId.1~last_chunked.x, 
                                T~last_chunked.y)) %>%
  mutate(inf.primary = case_when(primary==ResidentId.1~num_pos.1,
                                 primary==ResidentId.2~num_pos.2), 
         inf.secondary = case_when(primary==ResidentId.1~num_pos.2,
                                   primary==ResidentId.2~num_pos.1)) %>% select(names(treatment))

controlone <- control %>% filter(!label %in% controlboth$label)

controlone <- controlone %>%
  rowwise() %>% 
  mutate(both_test=label%in%a_final$label&label%in%a2_final$label) %>%
  mutate(primary = case_when(label %in% a_final$label & !label %in% a2_final$label~ResidentId.1,
                             label %in% a2_final$label & !label %in% a_final$label~ResidentId.2,
                             label %in% a_final$label & label %in% a2_final$label~sample(c(ResidentId.1, ResidentId.2), size = 1))) %>%
  mutate(inf.primary = case_when(primary==ResidentId.1~num_pos.1,
                                 primary==ResidentId.2~num_pos.2), 
         inf.secondary = case_when(primary==ResidentId.1~num_pos.2,
                                   primary==ResidentId.2~num_pos.1)) %>% 
  left_join(a_final, by=c("label", "primary"="ResidentId.1")) %>% 
  left_join(a2_final, by=c("label", "primary"="ResidentId.2")) %>% 
  mutate(first_chunked.x=as.character(first_chunked.x), 
         first_chunked.y=as.character(first_chunked.y),
         last_chunked.x=as.character(last_chunked.x),
         last_chunked.y=as.character(last_chunked.y)) %>%
  replace_na(list(first_chunked.x="", first_chunked.y="",
                  last_chunked.x="", last_chunked.y="")) %>% 
  mutate(first_chunked=paste0(first_chunked.x, first_chunked.y)%>%as.Date(), 
         last_chunked=paste0(last_chunked.x, last_chunked.y)%>%as.Date()) %>% select(names(treatment))

matching <- treatment %>% rbind(controlboth, controlone) %>% 
  mutate(secondary = ifelse(primary==ResidentId.1, ResidentId.2, ResidentId.1),
         first = first_chunked,
         last = last_chunked,
         inf.primary=ifelse(primary==ResidentId.1, inf.1, inf.2), 
         inf.secondary=ifelse(primary==ResidentId.1, inf.2, inf.1), 
         inf.primary=ifelse(inf.primary, 1, 0), 
         inf.secondary=ifelse(inf.secondary, 1, 0))

matching

matching <- matching %>% 
  mutate(treatment = ifelse(both_unvacc, 1, 0))

matching %>% group_by(treatment) %>% summarise(n=n())

write_csv(matching, "full_data_prematching_noincarcreq_unvacc_stricttesting.csv")

