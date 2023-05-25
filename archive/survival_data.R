#m_adjusted <- m_adjusted %>% mutate(secondary=ifelse(primary==ResidentId.1, ResidentId.2, ResidentId.1))
m_adjusted %>% select(primary, secondary)

m_adjusted_testing <- m_adjusted %>% add_testing()

# use total eligible time
m_adjusted_testing_included <- m_adjusted_testing %>% 
  group_by(id) %>% 
  mutate(start=adjusted_start, end=last_chunked) %>% filter_testing() %>%
  # use absolute days since start of Omicron to create absolute intervals
  mutate(time=as.numeric(as.Date(start)-as.Date("2021-12-15"))) %>%
  # survival time is the absolute number of days since Omicron start
  mutate(survival_time=as.numeric(Day-as.Date("2021-12-15"))) %>% 
  # find final outcome 
  mutate(last_status = ifelse(any(Result=="Positive")&Result=="Positive", Result=="Positive", Day==last(Day))) %>% 
  filter(last_status) %>% summarise_all(first) %>% select(!last_status) %>% 
  mutate(treatment=abs(1-treatment))

final_surv <- m_adjusted_testing_included %>% 
  mutate(status=ifelse(Result=="Positive", 1, 3)) %>% 
  select(time, survival_time, status, treatment)
fit <- survfit(Surv(time=time, time2=survival_time, status, type="interval")~treatment, data = final_surv)
autoplot(fit) + 
  xlab("Time") + ylab("Survival") + 
  scale_fill_discrete(name=element_blank(), label=c("Control", "Treatment")) + 
  guides(color=F)

avg_inc_rate <- m_adjusted_testing_included %>% 
  group_by(treatment) %>% 
  mutate(time=as.numeric(end-start)) %>% 
  summarise(inc_rate=sum(Result=="Positive")/(n()*sum(time))*1000)
avg_inc_rate


# use matched time where we use maximum overlapped time
treatment <- m_adjusted %>% filter(treatment==0)
control <- m_adjusted %>% filter(treatment==1)

treatment_testing <- treatment %>% add_testing() %>% 
  mutate(start=int_start(intersection)%>%as.Date(), end=int_end(intersection)%>%as.Date())
treatment_time_test <- treatment_testing %>% 
  group_by(id) %>%
  filter_testing() %>% 
  #  mutate(time=as.numeric(as.Date(adjusted_start)-as.Date("2021-12-15"))+1) %>%
  mutate(survival_time=as.numeric(Day-start)) %>%
  mutate(last = ifelse(any(Result=="Positive")&Result=="Positive", Result=="Positive", Day==last(Day))) %>% 
  mutate(pos_first=first(Result)=="Positive") %>%
  filter(last) %>% summarise_all(first) %>% select(!last) %>% 
  mutate(survival_time=ifelse(Result!="Positive", as.numeric(end-start), survival_time))

control_testing <- treatment %>% select(subclass, id, intersection) %>% rename("id_treat"="id") %>% 
  left_join(control %>% select(!intersection), by="subclass") %>%
  mutate(first_intersection=int_start(intersection)%>%as.Date(), last_intersection=int_end(intersection)%>%as.Date()) %>%
  arrange(id, first_intersection, desc(intersect))
control_testing %>%  select(id, subclass, intersection, intersect) %>% arrange(id)

control_testing_unique <- control_testing %>% group_by(id) %>% distinct(id, first_intersection, .keep_all = T)

control_test_overlap <- control_testing %>% group_by(id) %>% 
  mutate(next_intersection=lag(intersection, 1)) %>% 
  mutate(overlap_next=!intersect(intersection, next_intersection) %>% is.na()) 

distinct_overlap <- control_test_overlap %>% filter(!overlap_next) %>% mutate(label=1:n())
control_test_overlap <- control_test_overlap %>% left_join(distinct_overlap)
control_test_overlap <- control_test_overlap %>% fill(label, .direction="down") %>% group_by(id, label) %>% 
  mutate(start=min(first_intersection)%>%as.Date(), end=max(last_intersection)%>%as.Date())

control_test_overlap %>% 
  select(id, id_treat, subclass, intersection, start, end, label) %>% head(20)

control_testing_unique <- control_test_overlap %>% group_by(id, label) %>% summarise_all(first)
control_time_test <- control_testing_unique %>% group_by(id, label) %>% add_testing() %>% filter_testing() %>% 
  mutate(survival_time=as.numeric(Day-start)) %>% 
  mutate(last = ifelse(any(Result=="Positive")&Result=="Positive", Result=="Positive", Day==last(Day))) %>%
  mutate(pos_first=first(Result)=="Positive") %>%
  filter(last) %>% summarise_all(first) %>% select(!last) %>% 
  mutate(survival_time=ifelse(Result!="Positive", as.numeric(end-start), survival_time))


control_time_test <- control_time_test %>% select(!c(id_treat, first_intersection, last_intersection, next_intersection, overlap_next))

# treatment_time_test <- treatment_time_test %>% 
#   left_join(control_test_overlap%>%select(id_treat,start,end), by=c("id"="id_treat")) %>% 
#   mutate(time1=as.Date(start.x)-as.Date(start.y), time2=as.Date(end.x)-as.Date(start.y)+1)
# treatment_time_test %>% select(id, subclass, intersection, start.y, end.y, time1, time2)


full <- treatment_time_test %>% 
  select(id, treatment, subclass, BuildingId,  primary, secondary, inf.primary, inf.secondary, start, end, survival_time, Result) %>% 
  rbind(control_time_test %>% select(id, treatment, subclass, BuildingId, primary, secondary, inf.primary, inf.secondary, start, end, survival_time, Result))

full <- full %>% mutate(treatment=1-treatment) %>% group_by(subclass) %>% filter(any(treatment==1)&any(treatment==0))
full$treatment%>%table()

full <- full %>% 
  mutate(status=ifelse(Result=="Positive", 1, 0)) 
fit <- survfit(Surv(survival_time, status, type="right")~treatment, data = full)
autoplot(fit) + 
  xlab("Time") + ylab("Survival") + 
  scale_fill_discrete(name=element_blank(), label=c("Control", "Treatment")) + 
  guides(color=F)

# avg incidence rate
full%>%group_by(treatment)%>%summarise(cases=sum(status), person_time=n()*mean(survival_time))%>%mutate(inc_rate=cases/person_time)
full%>%group_by(inf.secondary, treatment)%>%summarise(cases=sum(status), person_time=n()*mean(survival_time))%>%mutate(inc_rate=cases/person_time)


full_secondary <- full %>% 
  left_join(testing %>% select(ResidentId, Day, Result) %>% rename("Result2"="Result"), 
            by=c("secondary"="ResidentId")) %>% 
  group_by(id, start) %>% mutate(has_test=any(Day<=end&Day>=start,na.rm=T)) %>% 
  filter(!has_test|(Day<=end&Day>=start)) %>% filter(has_test|Day%>%is.na()|Day==first(Day)) %>%
  mutate(Day=if_else(!has_test, as.Date(NA), Day),
         Result2=if_else(!has_test, as.character(NA), Result2))
full_secondary <- full_secondary %>% 
  mutate(survival_time2=as.numeric(Day-start)) %>% 
  mutate(last = ifelse(any(Result2=="Positive",na.rm=T)&Result2=="Positive", Result2=="Positive", Day==last(Day))) %>%
  filter(last|!has_test) %>% summarise_all(first) %>% select(!last) %>% 
  mutate(survival_time2=ifelse(Result2!="Positive", as.numeric(end-start), survival_time2))
full_secondary%>%
  filter(!is.na(Result2)) %>%
  group_by(treatment)%>%
  summarise(n=unique(id)%>%length(), cases=sum(Result2=="Positive",na.rm=T), person_time=sum(survival_time2,na.rm=T))%>%
  mutate(inc_rate=cases/person_time)

full_secondary %>% filter(Result=="Negative"&Result2=="Positive") %>% select(survival_time, survival_time2) %>% 
  filter(survival_time2>=survival_time)
full_secondary %>% filter(Result=="Positive"&Result2=="Positive") %>% select(survival_time, survival_time2)
full_secondary %>% filter(Result=="Positive"&Result2=="Positive") %>% select(survival_time, survival_time2)

d <- read_csv("testing_vacc_clean.csv")
infections <- d %>% group_by(ResidentId, num_pos) %>% filter(num_pos>0) %>% select(ResidentId, Day, num_pos)
infections
vacc <- d %>% group_by(ResidentId) %>% filter(!Vaccine%>%is.na()) %>% select(ResidentId, Day, num_dose, Date_offset)

full_inf <- full_secondary %>% 
  left_join(infections%>%rename("inf1"="Day"), by=c("primary"="ResidentId")) %>% 
  filter(inf.primary==0|inf1<=start) %>% 
  mutate(inf1=if_else(inf.primary==0, as.Date(NA), inf1), num_inf1 = sum(!inf1%>%is.na())) %>% 
  group_by(id, start) %>% arrange(id, desc(inf1)) %>% 
  summarise_all(first) %>% 
  left_join(infections%>%rename("inf2"="Day"), by=c("secondary"="ResidentId")) %>%  
  filter(inf.secondary==0|inf2<=start) %>% 
  mutate(inf2=if_else(inf.secondary==0, as.Date(NA), inf2), num_inf2 = sum(!inf2%>%is.na())) %>%
  group_by(id, start) %>% arrange(id, desc(inf2)) %>% summarise_all(first)

full_inf

full_inf <- full_inf %>% mutate(days_inf1=as.numeric(start-inf1)+1, days_inf2=as.numeric(start-inf2)+1)

full_inf %>% group_by(treatment) %>% select(num_inf1, num_inf2, days_inf1, days_inf2) %>% 
  summarise_all(mean, na.rm=T) %>% 
  rbind(c(treatment=NA, (full_inf %>% ungroup() %>% select(num_inf1, num_inf2, days_inf1, days_inf2) %>% 
                           summarise_all(mean, na.rm=T))[1,]))

p <- full_inf %>% ggplot() + geom_bar(aes(num_inf1, fill="Primary resident"), position=position_dodge2(width = 0.5)) + 
  geom_bar(aes(num_inf2, fill="Secondary resident"), alpha=0.7) + 
  scale_x_continuous("Number of prior infections") + 
  scale_y_continuous("Count") + 
  theme(legend.title = element_blank(), 
        legend.position = "none")

p1 <- full_inf %>% ggplot() + geom_histogram(aes(days_inf1, ..density.., fill="Primary resident"), alpha=0.7) + 
  geom_histogram(aes(days_inf2, ..density.., fill="Secondary resident"), alpha=0.7) + 
  scale_x_continuous("Days since most recent prior infection") + 
  scale_y_continuous("Density") + 
  theme(legend.title = element_blank())

library(patchwork)
p+p1


full_testing_extend <- full %>% group_by(id, start) %>% 
  select(!c(survival_time, Result)) %>% mutate(end_5_days = end+5) %>% 
  left_join(testing %>% select(ResidentId, Day, Result), 
            by=c("primary"="ResidentId")) %>%
  filter(Day <= end_5_days & Day >= start) %>% 
  mutate(survival_time=as.numeric(Day-start)) %>% 
  mutate(last = ifelse(any(Result=="Positive")&Result=="Positive", Result=="Positive", Day==last(Day))) %>%
  filter(last) %>% summarise_all(first) %>% select(!last) %>% 
  mutate(survival_time=ifelse(Result!="Positive", as.numeric(end_5_days-start), survival_time))

full_testing_extend
full_testing_extend%>%
  group_by(treatment)%>%
  summarise(cases=sum(Result=="Positive"), person_time=sum(survival_time))%>%
  mutate(inc_rate=cases/person_time)

(full_secondary %>% filter(Result=="Positive"))$survival_time%>%summary()
full_secondary %>% filter(Result=="Positive") %>% 
  ggplot(aes(survival_time, group=treatment, fill=as.factor(treatment))) + 
  geom_histogram(aes(y=..density..), alpha=0.7, position="identity", binwidth = 2.5) + 
  scale_fill_discrete("Treatment") + 
  scale_x_continuous("Survival time until first positive test (days)") + 
  scale_y_continuous("Density")

secondary_vacc <- full %>% group_by(id, start) %>% 
  left_join(vacc%>%select(ResidentId, Date_offset, num_dose) %>% 
              rename("vacc_day"="Date_offset"), 
            by=c("secondary"="ResidentId")) %>% 
  filter(vacc_day <= start|treatment==0) %>% 
  arrange(id, start, desc(vacc_day)) %>% 
  summarise_all(first) %>% 
  mutate(vacc_day=ifelse(treatment==0, as.Date(NA), vacc_day))

secondary_vacc <- secondary_vacc %>% mutate(time_vacc=as.numeric(start-vacc_day+1))

secondary_vacc %>% ggplot(aes(time_vacc)) + geom_histogram(binwidth = 50) + 
  scale_x_continuous("Time since most recent vaccination (days)") + 
  scale_y_continuous("Count")

secondary_vacc$time_vacc%>%as.numeric()%>%summary()

secondary_vacc <- secondary_vacc %>% mutate(recent_vacc=time_vacc<=120)

secondary_vacc%>%group_by(recent_vacc, treatment)%>%summarise(n=n(), mean_time=mean(time_vacc), cases=sum(status), person_time=n()*mean(survival_time))%>%mutate(inc_rate=cases/person_time)


demo <- read_csv("demographic_data_clean.csv") %>% select(ResidentId, BirthYear)
full_age <- full %>% 
  left_join(demo, by=c("primary"="ResidentId")) %>% 
  mutate(age1=as.numeric(format(start, "%Y"))-BirthYear) %>% 
  select(!BirthYear) %>% 
  left_join(demo, by=c("secondary"="ResidentId")) %>% 
  mutate(age2=as.numeric(format(start, "%Y"))-BirthYear) 
full_age %>% ggplot() + 
  geom_histogram(aes(age1, y=..density.., fill="Primary resident"), alpha=0.7) + 
  geom_histogram(aes(age2, y=..density.., fill="Secondary resident"), alpha=0.5) + 
  scale_x_continuous("Age (years)") + 
  scale_y_continuous("Density") + 
  theme(legend.title = element_blank())

# control_testing <- control %>% left_join(testing %>% select(ResidentId, Day, Result), by=c("primary"="ResidentId")) %>% 
#   filter(Day >= adjusted_start & Day <= last_chunked)
# control_time_test <- control_testing %>% 
#   mutate(time=as.numeric(as.Date(adjusted_start)-as.Date("2021-12-15"))) %>% 
#   mutate(survival_time=as.numeric(Day-as.Date("2021-12-15"))) %>% 
#   mutate(last = ifelse(any(Result=="Positive")&Result=="Positive", Result=="Positive", Day==last(Day))) %>% 
#   filter(last) %>% summarise_all(first) %>% select(!last)
# 
# treatment_surv <- treatment_time_test %>% mutate(status=ifelse(Result=="Positive", 1, 0)) %>% select(time, survival_time, status)
# fit <- survfit(Surv(time, survival_time, status, type="interval")~1, data = treatment_surv)
# autoplot(fit) + 
#   xlab("Time") + ylab("Survival")
# 
# 
# 
# 
# control_testing <- treatment %>% summarise_all(first) %>% select(subclass, intersection) %>%
#   left_join(control %>% select(!intersection), by="subclass") %>%
#   mutate(first_intersection=int_start(intersection)) %>%
#   arrange(id, first_intersection, desc(intersect))
# control_testing %>%  select(id, subclass, intersection, intersect) %>% arrange(id)
# 
# control_testing_unique <- control_testing %>% group_by(id) %>% distinct(id, first_intersection, .keep_all = T) %>% group_by(id)
# 
# control_testing_unique %>% select(id, subclass, intersection, intersect) %>% arrange(id)
# 
# control_testing_unique <- control_testing_unique %>% left_join(testing %>% select(ResidentId, Day, Result), by=c("primary"="ResidentId")) %>%
#   filter(any(Day %within% intersection)) %>% filter(Day %within% intersection)
# 
# control_time_test <- control_testing_unique %>% mutate(survival_time=Day-as.Date(int_start(intersection))+1) %>%
#   mutate(last = ifelse(any(Result=="Positive")&Result=="Positive", Result=="Positive", Day==last(Day))) %>%
#   filter(last) %>% summarise_all(first) %>% select(!last)
# 
# control_time_test
# 
# final <- rbind(treatment_time_test, control_time_test%>%select(names(treatment_time_test))) %>% 
#   arrange(subclass, desc(treatment)) %>% group_by(subclass) %>% filter(any(treatment==1)&any(treatment==0)) %>%
#   mutate(treatment=abs(1-treatment))
# final
# 
# 
# final_surv <- final %>% mutate(status=ifelse(Result=="Positive", 1, 0)) %>% select(treatment, survival_time, status)
# fit <- survfit(coxph(Surv(survival_time, status)~treatment+frailty(subclass), data = final_surv))
# autoplot(fit) + 
#   scale_x_continuous("Time") + ylab("Survival") 
# 
# 
# # what control units test positive
# control_time_test %>% group_by(id) %>% filter(n()>1) %>% select(id, subclass, intersection, Day, Result, survival_time)