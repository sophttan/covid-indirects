# Sophia Tan 3/4/24
# Match infections and controls

rm(list=ls())
gc() 

library(tidyverse)
library(readr)
library(lubridate)
library(survival)

matched <- read_csv("D:/CCHCS_premium/st/indirects/matched_building_3_7days-roommate.csv")
matched_keys <- matched %>% group_by(key, subclass) %>% group_keys() %>% mutate(group = 1:n())
matched <- matched %>% left_join(matched_keys) %>% mutate(id=1:n())

inf <- read_csv("D:/CCHCS_premium/st/cleaned-data/infection_data022624.csv") %>% filter(CollectionDate <= "2022-12-15") %>%
  select(ResidentId, CollectionDate) %>% rename(last.inf.roommate=CollectionDate)
matched_inf_roommate <- matched %>% left_join(inf, by=c("Roommate"="ResidentId")) %>%
  group_by(id) %>% 
  filter(last.inf.roommate%>%is.na()|all(test.Day-last.inf.roommate<14)|test.Day-last.inf.roommate>=14) %>%
  mutate(last.inf.roommate=if_else(is.na(last.inf.roommate)|test.Day-last.inf.roommate<14, NA, last.inf.roommate)) %>%
  summarise_all(last)

vaccine <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/complete_vaccine_data121523.csv") %>% 
  filter(Date <= "2022-12-15") %>% select(ResidentId, Date_offset, num_dose, full_vacc) %>%
  rename(last.vacc.roommate=Date_offset,
         dose.roommate=num_dose,
         full_vacc.roommate=full_vacc)

matched_infvacc_roommate <- matched_inf_roommate %>% left_join(vaccine, by=c("Roommate"="ResidentId")) %>%
  group_by(id) %>% 
  filter(last.vacc.roommate%>%is.na()|all(last.vacc.roommate>=test.Day)|last.vacc.roommate<test.Day) %>%
  mutate(last.vacc.roommate=if_else(last.vacc.roommate>=test.Day, NA, last.vacc.roommate)) %>% 
  summarise_all(last) %>%
  mutate(has.vacc.roommate.binary=if_else(last.vacc.roommate%>%is.na(), 0, 1)) %>%
  mutate(dose.roommate.adjusted = case_when(last.vacc.roommate%>%is.na()~0,
                                            dose.roommate<full_vacc.roommate~1,
                                            dose.roommate==full_vacc.roommate~2,
                                            dose.roommate-full_vacc.roommate==1~3,
                                            dose.roommate-full_vacc.roommate>1~4))

matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(variant = case_when(test.Day<="2022-05-14"~1,
                             test.Day<="2022-08-14"~2,
                             T~3))

matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(has.prior.inf.roommate=if_else(last.inf.roommate%>%is.na(), 0, 1))

model <- glm(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + 
               has.prior.inf + num_dose_adjusted + level + age + age.roommate + risk + risk.roommate +
               factor(Institution) + factor(variant), data=matched_infvacc_roommate, family="binomial")
summary(model)

model <- clogit(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + factor(variant) + strata(group), data=matched_infvacc_roommate)
summary(model)
tbl_regression(model, exp=T, include = c("has.vacc.roommate.binary", "has.prior.inf.roommate","age","age.roommate","risk","risk.roommate"), 
               label = c("has.vacc.roommate.binary"="Roommate vaccination (binary)",
                         "has.prior.inf.roommate"="Roommate prior infection (binary)", 
                         "age"="Case/control age (years)", 
                         "age.roommate"="Roommate age (years)", 
                         "risk"="Case/control risk for severe COVID-19", 
                         "risk.roommate"="Roommate risk for severe COVID-19"))

model <- clogit(case ~ dose.roommate.adjusted + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + factor(variant) + strata(group), data=matched_infvacc_roommate)
summary(model)
tbl_regression(model, exp=T, include = c("dose.roommate.adjusted", "has.prior.inf.roommate","age","age.roommate","risk","risk.roommate"), 
               label = c("dose.roommate.adjusted"="Roommate vaccination (doses)",
                         "has.prior.inf.roommate"="Roommate prior infection (binary)", 
                         "age"="Case/control age (years)", 
                         "age.roommate"="Roommate age (years)", 
                         "risk"="Case/control risk for severe COVID-19", 
                         "risk.roommate"="Roommate risk for severe COVID-19"))

model <- clogit(case ~ has.vacc.roommate.binary*has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + factor(variant) + strata(group), data=matched_infvacc_roommate)
summary(model)

model <- clogit(case ~ dose.roommate.adjusted*has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + factor(variant) + strata(group), data=matched_infvacc_roommate)
summary(model)
tbl_regression(model, exp=T, include = c("dose.roommate.adjusted", "has.prior.inf.roommate", "dose.roommate.adjusted:has.prior.inf.roommate"), 
               label = c("dose.roommate.adjusted"="Roommate vaccination (doses)",
                         "has.prior.inf.roommate"="Roommate prior infection (binary)",
                         "dose.roommate.adjusted:has.prior.inf.roommate"="Interaction: doses and infection"))


matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(time_since_vacc = (test.Day-Date_offset)%>%as.numeric(),
         time_since_vacc.roommate = (test.Day-last.vacc.roommate) %>% as.numeric()) %>%
  mutate(time_since_vacc_cut=cut(time_since_vacc, breaks=c(0, 90, 182, 365, Inf), right = F),
         time_since_vacc_cut.roommate=cut(time_since_vacc.roommate, breaks=c(0, 90, 182, 365, Inf), right = F)) 

levels(matched_infvacc_roommate$time_since_vacc_cut)<-c(levels(matched_infvacc_roommate$time_since_vacc_cut), "None") 
matched_infvacc_roommate$time_since_vacc_cut[is.na(matched_infvacc_roommate$time_since_vacc_cut)] <- "None"
matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(time_since_vacc_cut = factor(time_since_vacc_cut, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf")))

levels(matched_infvacc_roommate$time_since_vacc_cut.roommate)<-c(levels(matched_infvacc_roommate$time_since_vacc_cut.roommate), "None") 
matched_infvacc_roommate$time_since_vacc_cut.roommate[is.na(matched_infvacc_roommate$time_since_vacc_cut.roommate)] <- "None"
matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(time_since_vacc_cut.roommate = factor(time_since_vacc_cut.roommate, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))

matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(time_since_inf.roommate = (test.Day-last.inf.roommate) %>% as.numeric()) %>%
  mutate(time_since_inf_cut.roommate=cut(time_since_inf.roommate, breaks=c(0, 90, 182, 365, Inf), right = F)) 
levels(matched_infvacc_roommate$time_since_inf_cut.roommate)<-c(levels(matched_infvacc_roommate$time_since_inf_cut.roommate), "None") 
matched_infvacc_roommate$time_since_inf_cut.roommate[is.na(matched_infvacc_roommate$time_since_inf_cut.roommate)] <- "None"
matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(time_since_inf_cut.roommate = factor(time_since_inf_cut.roommate, levels=c("None","[0,90)", "[90,182)","[182,365)","[365,Inf)")))

matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(latest=pmax(last.inf.roommate, last.vacc.roommate,na.rm=T)) %>%
  mutate(time_since_infvacc.roommate = (test.Day-latest)%>%as.numeric()) %>%
  mutate(time_since_infvacc_cut.roommate=cut(time_since_infvacc.roommate, breaks=c(0, 90, 182, 365, Inf), right = F)) 
levels(matched_infvacc_roommate$time_since_infvacc_cut.roommate)<-c(levels(matched_infvacc_roommate$time_since_infvacc_cut.roommate), "None") 
matched_infvacc_roommate$time_since_infvacc_cut.roommate[is.na(matched_infvacc_roommate$time_since_infvacc_cut.roommate)] <- "None"
matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(time_since_infvacc_cut.roommate = factor(time_since_infvacc_cut.roommate, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))


model <- glm(case ~ time_since_vacc_cut.roommate + time_since_inf_cut.roommate + 
               has.prior.inf + num_dose_adjusted + level + age + age.roommate + risk + risk.roommate +
               factor(Institution) + factor(variant), data=matched_infvacc_roommate, family="binomial")
summary(model)

model <- clogit(case ~ time_since_inf_cut.roommate + has.vacc.roommate.binary + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)
summary(model)

tbl_regression(model, exp=T, include = c("time_since_inf_cut.roommate"), 
               label = c("time_since_inf_cut.roommate"="Roommate: time since last infection"))


model <- clogit(case ~ dose.roommate.adjusted*time_since_vacc_cut.roommate + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + factor(variant) + strata(group), data=matched_infvacc_roommate)
summary(model)

tbl_regression(model, exp=T, include = c("time_since_vacc_cut.roommate"), 
               label = c("time_since_vacc_cut.roommate"="Roommate: time since last vaccine"))

model <- glm(case ~ time_since_vacc_cut.roommate +has.prior.inf.roommate + 
               has.prior.inf + num_dose_adjusted + level + age + age.roommate + risk + risk.roommate +
               factor(Institution) + factor(variant), data=matched_infvacc_roommate, family="binomial")
summary(model)

model <- glm(case ~ time_since_inf_cut.roommate*has.vacc.roommate.binary + 
               has.prior.inf + num_dose_adjusted + level + age + age.roommate + risk + risk.roommate +
               factor(Institution) + factor(variant), data=matched_infvacc_roommate, family="binomial")
summary(model)

model <- glm(case ~ time_since_infvacc_cut.roommate + 
               has.prior.inf + num_dose_adjusted + level + age + age.roommate + risk + risk.roommate +
               factor(Institution) + factor(variant), data=matched_infvacc_roommate, family="binomial")
summary(model)

model <- clogit(case ~ time_since_infvacc_cut.roommate + 
                  age + age.roommate + risk + risk.roommate + factor(variant) + strata(group), data=matched_infvacc_roommate)
summary(model)

tbl_regression(model, exp=T, include = c("time_since_infvacc_cut.roommate"), 
               label = c("time_since_infvacc_cut.roommate"="Roommate: time since most recent vaccine or infection"))


model <- clogit(case ~ time_since_vacc_cut.roommate*has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + factor(variant) + strata(group), data=matched_infvacc_roommate)
summary(model)

model <- clogit(case ~ time_since_inf_cut.roommate*has.vacc.roommate.binary + 
                  age + age.roommate + risk + risk.roommate + factor(variant) + strata(group), data=matched_infvacc_roommate)
summary(model)

# descriptive statistics on roommates
matched_infvacc_roommate %>% ggplot(aes(risk.roommate, group=factor(case), fill=factor(case))) + geom_bar(position="identity", alpha=0.5) + 
  scale_fill_discrete("Control v. case") + 
  scale_x_continuous("Roommate's risk for severe COVID-19")
matched_infvacc_roommate %>% ggplot(aes(age.roommate, group=factor(case), fill=factor(case))) + geom_histogram(position="identity", alpha=0.5) + 
  scale_fill_discrete("Control v. case") + 
  scale_x_continuous("Roommate's age")

within_match <- matched_infvacc_roommate %>% group_by(group) %>% arrange(group, case) %>%
  summarise(time_since_vacc = abs(time_since_vacc[1]-time_since_vacc[2]),
            time_since_inf = abs(time_since_inf[1]-time_since_inf[2]),
            vacc = dose.roommate.adjusted[1]-dose.roommate.adjusted,
            risk=abs(risk.roommate[1]-risk.roommate[2]),
            age=abs(age.roommate[1]-age.roommate[2]))
within_match %>% ggplot(aes(time_since_vacc)) + geom_histogram(position="identity") + 
  scale_x_continuous("Absolute difference in risk for severe COVID-19 in matched roommates")
within_match %>% ggplot(aes(time_since_inf)) + geom_histogram(position="identity") + 
  scale_x_continuous("Absolute difference in risk for severe COVID-19 in matched roommates")
within_match %>% ggplot(aes(risk)) + geom_bar(position="identity") + 
  scale_x_continuous("Absolute difference in risk for severe COVID-19 in matched roommates")
within_match %>% ggplot(aes(age)) + geom_bar(position="identity") + 
  scale_x_continuous("Absolute difference in age for severe COVID-19 in matched roommates")

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
matched_infvacc_roommate %>% filter(case==1) %>% ggplot(aes(as.POSIXct(test.Day))) + geom_histogram() +
  scale_x_datetime("Number of cases included in study population", date_breaks = "1 month") + 
  theme(axis.text.x = element_text(angle=90))


# descriptive statistics on case/control
# vaccine
matched_infvacc_roommate %>% ggplot(aes(num_dose_adjusted, group=factor(case), fill=factor(case))) + geom_bar(position="identity", alpha=0.5) + 
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
                                                       T~2)) %>%
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





# mechanism 
relevant<- matched_infvacc_roommate %>% select(id, group, ResidentId, num_dose_adjusted, test.Day, case, Roommate, last.inf.roommate, last.vacc.roommate, dose.roommate.adjusted, has.vacc.roommate.binary, has.prior.inf.roommate)
relevant

relevant %>% group_by(case, has.vacc.roommate.binary|has.prior.inf.roommate) %>% summarise(n=n())
# check_test <- relevant %>% group_by(group) %>% filter(any(case==0&has.vacc.roommate.binary==1)&any(case==1&has.vacc.roommate.binary==0))

relevant %>% group_by(case) %>% summarise(n=n())
relevant %>% group_by(case, num_dose_adjusted, has.vacc.roommate.binary) %>% summarise(n=n()) %>% group_by(case) %>% mutate(prop=n/sum(n))

test_data <- read_csv("D:/CCHCS_premium/st/cleaned-data/complete_testing_data022624.csv") %>% select(ResidentId,Day,Result,num_pos)
check_test <- relevant %>% left_join(test_data, by=c("Roommate"="ResidentId")) %>% group_by(id)

check_test %>% 
  filter(all(Day>test.Day|test.Day-Day>=14)) %>% summarise_all(first) %>% group_by(case, has.vacc.roommate.binary) %>% 
  summarise(n=n())%>%ungroup()%>%mutate(prop=n/c(615, 4319, 699, 4235))

check_test <- check_test %>% group_by(id) %>%
  filter((Day-test.Day<2 & test.Day-Day<14)|!any(Day-test.Day<2 & test.Day-Day<14)) %>% 
  select(id, group, case, Roommate, has.vacc.roommate.binary, test.Day, Day, Result, num_pos) %>%
  arrange(id, Result, Day) %>%
  summarise_all(last) %>% mutate(roommate_pos=Result=="Positive"&(Day-test.Day<2 & test.Day-Day<14)) 

check_test %>%
  group_by(case, has.vacc.roommate.binary, roommate_pos) %>% summarise(n=n()) %>%
  group_by(case, has.vacc.roommate.binary) %>% mutate(prop=n/sum(n))

check_test %>% filter(roommate_pos) %>% mutate(time_since=(test.Day-Day)%>%as.numeric()) %>% 
  ggplot(aes(time_since, group=factor(case), fill=factor(case))) + geom_bar(aes(y=..prop..), position="identity", alpha=0.5) + 
  scale_x_continuous("Days since roommate's positive test", breaks=seq(-1, 13)) +
  scale_y_continuous("Proportion") + 
  scale_fill_discrete("Control v. case")
