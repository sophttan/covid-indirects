# Sophia Tan 3/4/24
# Match infections and controls

rm(list=ls())
gc() 

library(tidyverse)
library(readr)
library(lubridate)
library(survival)

matched <- read_csv("D:/CCHCS_premium/st/indirects/matched_building_3_7days-12matching-042324.csv") 
matched_keys <- matched %>% 
  group_by(key, subclass) %>% group_keys() %>% mutate(group = 1:n())
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

matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(has.prior.inf.roommate=if_else(last.inf.roommate%>%is.na(), 0, 1))

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
  mutate(time_since_inf.roommate = (test.Day-(last.inf.roommate-14)) %>% as.numeric()) %>%
  mutate(time_since_inf_cut.roommate=cut(time_since_inf.roommate, breaks=c(0, 90, 182, 365, Inf), right = F)) 
levels(matched_infvacc_roommate$time_since_inf_cut.roommate)<-c(levels(matched_infvacc_roommate$time_since_inf_cut.roommate), "None") 
matched_infvacc_roommate$time_since_inf_cut.roommate[is.na(matched_infvacc_roommate$time_since_inf_cut.roommate)] <- "None"
matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(time_since_inf_cut.roommate = factor(time_since_inf_cut.roommate, levels=c("None","[0,90)", "[90,182)","[182,365)","[365,Inf)")))

matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(latest=pmax(last.inf.roommate-14, last.vacc.roommate, na.rm=T)) %>%
  mutate(time_since_infvacc.roommate = (test.Day-latest)%>%as.numeric()) %>%
  mutate(time_since_infvacc_cut.roommate=cut(time_since_infvacc.roommate, breaks=c(0, 90, 182, 365, Inf), right = F)) 
levels(matched_infvacc_roommate$time_since_infvacc_cut.roommate)<-c(levels(matched_infvacc_roommate$time_since_infvacc_cut.roommate), "None") 
matched_infvacc_roommate$time_since_infvacc_cut.roommate[is.na(matched_infvacc_roommate$time_since_infvacc_cut.roommate)] <- "None"
matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(time_since_infvacc_cut.roommate = factor(time_since_infvacc_cut.roommate, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))


format_results <- function(model) {
  res <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())
  
  res <- res %>% mutate(x=rownames(res))
  names(res) <- c("point", "lb", "ub", "x")
  
  res %>% select(x, point, lb, ub)
}


model <- clogit(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)
write_csv(format_results(model), here::here("results/no-time-match/main-results-binary.csv"))

model <- clogit(case ~ dose.roommate.adjusted + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)

main <- format_results(model)[2:6,]

model <- (summary(model)$coefficients)[1,]
or <- exp(model[1]*1:4)
low <- exp(model[1]*1:4-1.96*model[3])
high <- exp(model[1]*1:4+1.96*model[3])
dose_results <- cbind(or, low, high)
rownames(dose_results) <- c("dose.1", "dose.2", "dose.3", "dose.4")

dose_results <- dose_results %>% as.data.frame()
dose_results <- dose_results %>% mutate(x=rownames(dose_results))
names(dose_results) <- c("point", "lb", "ub", "x")  
dose_results <- dose_results %>% select(x, point, lb, ub)


write_csv(rbind(dose_results, main), here::here("results/no-time-match/main-results-dose.csv"))


inf_model <- clogit(case ~ time_since_inf_cut.roommate + has.vacc.roommate.binary + 
                      age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)

vacc_model <- clogit(case ~ time_since_vacc_cut.roommate + has.prior.inf.roommate + 
                       age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)

infvacc_model <- clogit(case ~ time_since_infvacc_cut.roommate + 
                          age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)

results <- rbind(format_results(inf_model) %>% mutate(inf_vacc="inf"),
                 format_results(vacc_model) %>% mutate(inf_vacc="vacc"),
                 format_results(infvacc_model) %>% mutate(inf_vacc="infvacc"))
write_csv(results, here::here("results/no-time-match/main-results-time.csv"))


# first 3 months vaccine
matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(time_since_vacc_cut2.roommate=cut(time_since_vacc.roommate, breaks=c(0, 30, 60, 90, 182, 365, Inf), right = F)) 

levels(matched_infvacc_roommate$time_since_vacc_cut2.roommate)<-c(levels(matched_infvacc_roommate$time_since_vacc_cut2.roommate), "None") 
matched_infvacc_roommate$time_since_vacc_cut2.roommate[is.na(matched_infvacc_roommate$time_since_vacc_cut2.roommate)] <- "None"
matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(time_since_vacc_cut2.roommate = factor(time_since_vacc_cut2.roommate, levels=c("None","[0,30)", "[30,60)", "[60,90)","[90,182)","[182,365)","[365,Inf)")))

model <- clogit(case ~ time_since_vacc_cut2.roommate + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)
write_csv(format_results(model), here::here("results/no-time-match/vacc-results-3months.csv"))


# bivalent
matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(bivalent = case_when(last.vacc.roommate%>%is.na()~0,
                              last.vacc.roommate-14<"2022-09-01"~1,
                              T~2) %>% factor(levels=0:2, labels=c("Unvacc", "Monovalent", "Bivalent")))
matched_infvacc_roommate %>% select(id, group, last.vacc.roommate, dose.roommate.adjusted, bivalent)

model <- clogit(case ~ bivalent + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)
format_results(model)

matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(bivalent_time = case_when(last.vacc.roommate%>%is.na()~"Unvacc",
                                   last.vacc.roommate-14>="2022-09-01"~"Bivalent",
                                   time_since_vacc.roommate<90~"Monovalent<90",
                                   time_since_vacc.roommate>=90~"Monovalent>=90") %>% factor(levels=c("Unvacc", "Monovalent<90", "Monovalent>=90", "Bivalent")))

model <- clogit(case ~ bivalent_time + time_since_inf_cut.roommate + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)
write_csv(format_results(model), here::here("results/no-time-match/main-results-bivalent.csv"))


matched_infvacc_roommate <- matched_infvacc_roommate %>% replace_na(list(time_since_inf=1000, time_since_vacc=1000))
model <- clogit(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + 
                  time_since_inf + time_since_vacc + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)
format_results(model)

inf_model <- clogit(case ~ time_since_inf_cut.roommate + has.vacc.roommate.binary + 
                      time_since_inf + time_since_vacc + 
                      age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)

vacc_model <- clogit(case ~ time_since_vacc_cut.roommate + has.prior.inf.roommate + 
                       time_since_inf + time_since_vacc + 
                       age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)

infvacc_model <- clogit(case ~ time_since_infvacc_cut.roommate + 
                          time_since_inf + time_since_vacc + 
                          age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)

results <- rbind(format_results(inf_model) %>% mutate(inf_vacc="inf"),
                 format_results(vacc_model) %>% mutate(inf_vacc="vacc"),
                 format_results(infvacc_model) %>% mutate(inf_vacc="infvacc"))
results


model <- clogit(case ~ dose.roommate.adjusted + has.prior.inf.roommate + 
                  time_since_inf + time_since_vacc + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)

main <- format_results(model)[2:6,]

model <- (summary(model)$coefficients)[1,]
or <- exp(model[1]*1:4)
low <- exp(model[1]*1:4-1.96*model[3])
high <- exp(model[1]*1:4+1.96*model[3])
dose_results <- cbind(or, low, high)
rownames(dose_results) <- c("dose.1", "dose.2", "dose.3", "dose.4")

dose_results <- dose_results %>% as.data.frame()
dose_results <- dose_results %>% mutate(x=rownames(dose_results))
names(dose_results) <- c("point", "lb", "ub", "x")
dose_results

