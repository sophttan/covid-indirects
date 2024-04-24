grouped <- matched_infvacc_roommate %>% group_by(group) %>% arrange(desc(case)) %>% 
  summarise(case=case, 
            age_diff=age[1]-age,
            risk_diff=risk[1]-risk,
            age_diff.roommate=age.roommate[1]-age.roommate,
            risk_diff.roommate=risk.roommate[1]-risk.roommate,
            vacc_diff=time_since_vacc[1]-time_since_vacc,
            inf_diff=time_since_inf[1]-time_since_inf) %>%
  filter(case!=1)


age <- ggplot(grouped) + 
  geom_histogram(aes(age_diff, y=..density..), binwidth = 5) + 
  scale_x_continuous("Difference between matched case and control", expand=c(0, 0)) + 
  scale_y_continuous("Density", expand=c(0, 0)) + 
  labs(subtitle="") + 
  theme(panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 

risk <- ggplot(grouped) + 
  geom_histogram(aes(risk_diff, y=..density..), binwidth = 1) + 
  scale_x_continuous("Difference between matched case and control", expand=c(0, 0)) + 
  scale_y_continuous("Density", expand=c(0, 0)) + 
  labs(subtitle="") + 
  theme(panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 

inf <- ggplot(grouped) + 
  geom_histogram(aes(inf_diff, y=..density..)) + 
  scale_x_continuous("Difference between matched case and control", expand=c(0, 0)) + 
  scale_y_continuous("Density", expand=c(0, 0)) + 
  labs(subtitle="D. Days since SARS-CoV-2 infection") + 
  theme(panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 

vacc <- ggplot(grouped) + 
  geom_histogram(aes(vacc_diff, y=..density..)) + 
  scale_x_continuous("Difference between matched case and control", expand=c(0, 0)) + 
  scale_y_continuous("Density", expand=c(0, 0)) + 
  labs(subtitle="") + 
  theme(panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 

matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(case=factor(case, levels=c(1,0), labels=c("Case", "Control")))

age_overall <- matched_infvacc_roommate %>% ggplot(aes(age, group=case, fill=case)) + 
  geom_histogram(aes(y = ..density..), binwidth=5, position="identity", alpha=0.5) + 
  scale_fill_discrete("") + 
  scale_x_continuous("Age", expand=c(0, 0)) + 
  scale_y_continuous("Density", expand=c(0, 0)) + 
  labs(subtitle="A. Age (years)") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 

risk_overall <- matched_infvacc_roommate %>% ggplot(aes(risk, group=factor(case), fill=factor(case))) + 
  geom_histogram(aes(y = ..density..), binwidth=1, position="identity", alpha=0.5) + 
  scale_fill_discrete("") + 
  scale_x_continuous("Risk for severe COVID-19", expand=c(0, 0)) + 
  scale_y_continuous("Density", expand=c(0, 0)) + 
  labs(subtitle="B. Risk") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 

inf_overall <- matched_infvacc_roommate %>% ggplot(aes(time_since_inf, group=factor(case), fill=factor(case))) + 
  geom_histogram(aes(y = ..density..), position="identity", alpha=0.5) + 
  scale_fill_discrete("") + 
  scale_x_continuous("Days", expand=c(0, 0)) + 
  scale_y_continuous("Density", expand=c(0, 0)) + 
  labs(subtitle="D. Days") +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 


vacc_overall <- matched_infvacc_roommate %>% ggplot(aes(time_since_vacc, group=factor(case), fill=factor(case))) +
  geom_histogram(aes(y = ..density..), position="identity", alpha=0.5) + 
  scale_fill_discrete("") + 
  scale_x_continuous("Days since COVID-19 vaccination", expand=c(0, 0)) + 
  scale_y_continuous("Density", expand=c(0, 0)) + 
  labs(subtitle="C. Days") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 


library(patchwork)
(age_overall + age)/(risk_overall + risk)/(vacc_overall + vacc)/(inf_overall + inf)
ggsave(here::here("figures/match_quality.jpg"), height=10, width=9, dpi=300)

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
  summarise(n=n())%>%ungroup()%>%mutate(prop=n/c(294, 4310, 382, 4222))

check_test <- check_test %>% mutate(has_test=any(test.Day-Day <= 14 & test.Day-Day>0,na.rm=T))
check_test_summary <- check_test %>% filter((!has_test&Day%>%is.na())|(!has_test&(Day==min(Day)))|(test.Day-Day <= 14 & test.Day-Day>0))
check_test_summary %>% group_by(id) %>% summarise_all(first) %>% group_by(case) %>% summarise(n=sum(has_test), has_test=mean(has_test))

check_test_summary %>% filter(has_test) %>% group_by(id) %>% mutate(has_pos=any(Result=="Positive")) %>% 
  summarise_all(first) %>% group_by(case) %>% summarise(n=n(), npos=sum(has_pos), has_pos=mean(has_pos))


check_test <- check_test %>% group_by(id) %>%
  filter((Day-test.Day<2 & test.Day-Day<14)|!any(Day-test.Day<2 & test.Day-Day<14)) %>% 
  select(id, group, case, Roommate, has.vacc.roommate.binary, test.Day, Day, Result, num_pos) %>%
  arrange(id, Result, Day) %>%
  summarise_all(last) %>% mutate(roommate_pos=Result=="Positive"&(Day-test.Day<2 & test.Day-Day<14)) 

check_test %>%
  group_by(case, roommate_pos) %>% summarise(n=n()) %>%
  group_by(case) %>% mutate(prop=n/sum(n))

check_test %>% filter(roommate_pos) %>% mutate(time_since=(test.Day-Day)%>%as.numeric()) %>% 
  ggplot(aes(time_since, group=factor(case), fill=factor(case))) + geom_bar(aes(y=..prop..), position="identity", alpha=0.5) + 
  scale_x_continuous("Days since roommate's positive test", breaks=seq(-1, 13)) +
  scale_y_continuous("Proportion") + 
  scale_fill_discrete("Control v. case")
