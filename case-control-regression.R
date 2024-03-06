# Sophia Tan 3/4/24
# Match infections and controls

rm(list=ls())
gc() 

library(tidyverse)
library(readr)
library(lubridate)
library(survival)

matched <- read_csv("D:/CCHCS_premium/st/indirects/matched_building_withagerisk_030424.csv")
matched_keys <- matched %>% group_by(key, subclass) %>% group_keys() %>% mutate(group = 1:n())
matched <- matched %>% left_join(matched_keys) %>% mutate(id=1:n())

inf <- read_csv("D:/CCHCS_premium/st/cleaned-data/infection_data022624.csv") %>% filter(CollectionDate <= "2022-12-15") %>% 
  select(ResidentId, CollectionDate) %>% rename(last.inf.roommate=CollectionDate)
vaccine <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/complete_vaccine_data121523.csv") %>% 
  filter(Date <= "2022-12-15") %>% select(ResidentId, Date_offset, num_dose) %>%
  rename(last.vacc.roommate=Date_offset,
         dose.roommate=num_dose)

matched_inf_roommate <- matched %>% left_join(inf, by=c("Roommate"="ResidentId")) %>%
  group_by(id) %>% 
  filter(last.inf.roommate%>%is.na()|all(last.inf.roommate>=test.Day)|last.inf.roommate<test.Day) %>%
  mutate(last.inf.roommate=if_else(last.inf.roommate>=test.Day, NA, last.inf.roommate)) %>% 
  summarise_all(last) %>%
  mutate(has.prior.inf.roommate=if_else(last.inf.roommate%>%is.na(), 0, 1))

matched_infvacc_roommate <- matched_inf_roommate %>% left_join(vaccine, by=c("Roommate"="ResidentId")) %>%
  group_by(id) %>% 
  filter(last.vacc.roommate%>%is.na()|all(last.vacc.roommate>=test.Day)|last.vacc.roommate<test.Day) %>%
  mutate(last.vacc.roommate=if_else(last.vacc.roommate>=test.Day, NA, last.vacc.roommate)) %>% 
  summarise_all(last) %>%
  mutate(has.vacc.roommate.binary=if_else(last.vacc.roommate%>%is.na(), 0, 1))

matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(variant = case_when(test.Day<="2022-05-14"~1,
                             test.Day<="2022-08-14"~2,
                             T~3))

model <- glm(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + 
               has.prior.inf + num_dose + level + age + age.roommate + risk + risk.roommate +
               factor(Institution) + factor(variant), data=matched_infvacc_roommate, family="binomial")
summary(model)


model <- clogit(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + factor(variant) + strata(group), data=matched_infvacc_roommate)
summary(model)

model <- clogit(case ~ has.vacc.roommate.binary*has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + factor(variant) + strata(group), data=matched_infvacc_roommate)
summary(model)

# descriptive statistics on roommates
# vaccine
matched_infvacc_roommate %>% ggplot(aes(has.vacc.roommate.binary, group=factor(case), fill=factor(case))) + geom_bar(position="identity", alpha=0.5) + 
  scale_fill_discrete("Control v. case") + 
  scale_x_continuous("Roommate's vaccine status (binary)", breaks=c(0, 1))

matched_infvacc_roommate %>% ggplot(aes(dose.roommate, group=factor(case), fill=factor(case))) + geom_bar(position="identity", alpha=0.5) + 
  scale_fill_discrete("Control v. case") + 
  scale_x_continuous("Roommate's vaccine status (number of doses)", breaks=c(0, 1))

matched_infvacc_roommate %>% ggplot(aes(as.POSIXct(last.vacc.roommate), group=factor(case), fill=factor(case))) + geom_histogram(position="identity", alpha=0.5) + 
  scale_fill_discrete("Control v. case") + 
  scale_x_datetime("Date of roommate's most recent vaccine", date_breaks = "1 month") + 
  theme(axis.text.x = element_text(angle=90))


# infection
matched_infvacc_roommate %>% ggplot(aes(has.prior.inf.roommate, group=factor(case), fill=factor(case))) + geom_bar(position="identity", alpha=0.5) + 
  scale_fill_discrete("Control v. case") + 
  scale_x_continuous("Roommate's prior infection status", breaks=c(0, 1))

matched_infvacc_roommate %>% ggplot(aes(as.POSIXct(last.inf.roommate), group=factor(case), fill=factor(case))) + geom_histogram(position="identity", alpha=0.5) + 
  scale_fill_discrete("Control v. case") +
  scale_x_datetime("Date of roommate's most recent infection", date_breaks = "1 month") + 
  theme(axis.text.x = element_text(angle=90))


# descriptive statistics on cases over time
matched_inf_roommate %>% filter(case==1) %>% ggplot(aes(as.POSIXct(test.Day))) + geom_histogram() +
  scale_x_datetime("Number of cases included in study population", date_breaks = "1 month") + 
  theme(axis.text.x = element_text(angle=90))


# descriptive statistics on case/control
# vaccine
matched_infvacc_roommate %>% ggplot(aes(num_dose, group=factor(case), fill=factor(case))) + geom_bar(position="identity", alpha=0.5) + 
  scale_fill_discrete("Control v. case") + 
  scale_x_continuous("Case/control vaccine status (number of doses)", breaks=c(0:5))

matched_infvacc_roommate %>% ggplot(aes(as.POSIXct(Date_offset), group=factor(case), fill=factor(case))) + geom_histogram(position="identity", alpha=0.5) + 
  scale_fill_discrete("Control v. case") + 
  scale_x_datetime("Case/control most recent vaccine", date_breaks = "1 month") + 
  theme(axis.text.x = element_text(angle=90))


# infection
matched_infvacc_roommate %>% ggplot(aes(has.prior.inf, group=factor(case), fill=factor(case))) + geom_bar(position="identity", alpha=0.5) + 
  scale_fill_discrete("Control v. case") + 
  scale_x_continuous("Case/control prior infection status", breaks=c(0, 1))

matched_infvacc_roommate %>% has.prior.inf.roommate==1&has.vacc.roommate.binary==1


matched_infvacc_roommate %>% mutate(roommate.status=case_when(has.prior.inf.roommate==0&has.vacc.roommate.binary==0~0,
                                                              has.prior.inf.roommate==1&has.vacc.roommate.binary==0~1,
                                                              has.prior.inf.roommate==0&has.vacc.roommate.binary==1~2,
                                                              has.prior.inf.roommate==1&has.vacc.roommate.binary==1~3)) %>%
  ggplot(aes(roommate.status, group=factor(case), fill=factor(case))) + geom_bar(position="identity", alpha=0.5) + 
  scale_fill_discrete("Control v. case") + 
  scale_x_continuous("", breaks=0:3,labels = c("No inf/No vacc", "Inf/No vacc", "No inf/Vacc", "Inf/Vacc"))
  
matched_infvacc_roommate %>% mutate(inf.time=case_when(has.prior.inf.roommate==0~0,
                                                       test.Day-last.inf.roommate>90~1,
                                                       test.Day-last.inf.roommate>14~2,
                                                       T~3)) %>%
  ggplot(aes(inf.time, group=factor(case), fill=factor(case))) + geom_bar(position="identity", alpha=0.5) + 
  scale_fill_discrete("Control v. case") + 
  scale_x_continuous("", breaks=0:2,labels = c("No inf", "No recent inf", "Recent inf"))


remove_recent_roommate <- matched_infvacc_roommate %>% filter(has.prior.inf.roommate==0|test.Day-last.inf.roommate>14) 

model <- glm(case ~ has.vacc.roommate.binary*has.prior.inf.roommate + 
               has.prior.inf + num_dose + level + age + age.roommate + risk + risk.roommate +
               factor(Institution) + factor(variant), data=remove_recent_roommate, family="binomial")
summary(model)


model <- clogit(case ~ has.vacc.roommate.binary*has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + factor(variant) + strata(group), data=remove_recent_roommate)
summary(model)
