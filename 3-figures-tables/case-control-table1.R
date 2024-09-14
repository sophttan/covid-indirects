# Sophia Tan 3/4/24
# Evaluate plausibility of mechanism of indirect protection

source(here::here("config.R"))

data <- read_csv("D:/CCHCS_premium/st/indirects/case_control_postmatchprocessing091224.csv")

demo <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/demographic121523.csv")
demo

demo <- demo %>% mutate(Race=case_when(Race=="A"~"Asian or Pacific Islander",
                                       Race=="B"~"Black",
                                       Race=="H"|Race=="M"|Race=="C"~"Hispanic",
                                       Race=="I"~"American Indian/Alaskan Native",
                                       Race=="O"~"Other",
                                       Race=="W"~"White",
                                       Race=="U"~"Unknown"))

data <- data %>% 
  left_join(demo %>% select(!BirthYear), by="ResidentId") %>%
  left_join(demo %>% select(!BirthYear), by=c("Roommate"="ResidentId"), suffix=c("", ".roommate"))

data <- data %>% group_by(group) %>% mutate(weights=if_else(case==1|n()==2, 1, 0.5))

data_multiple_controls <- data %>% mutate(age2=age*weights,
                        num_dose_adjusted2=num_dose_adjusted*weights,
                        has.prior.inf2=has.prior.inf*weights,
                        time_since_vacc2=time_since_vacc*weights,
                        time_since_inf2=time_since_inf*weights)

data_multiple_controls %>% group_by(case) %>% mutate(sum_weights=sum(weights)) %>%
  group_by(case, num_dose_adjusted) %>% summarise(n=unique(group)%>%length(), perc=sum(weights)/first(sum_weights))

data_multiple_controls %>% group_by(case) %>% mutate(sum_weights=sum(weights)) %>%
  group_by(case, dose.roommate.adjusted) %>% summarise(n=sum(weights), perc=sum(weights)/first(sum_weights))

data_multiple_controls %>% group_by(case) %>% mutate(sum_weights=sum(weights)) %>%
  group_by(case, has.prior.inf.roommate) %>% summarise(n=sum(weights), perc=sum(weights)/first(sum_weights))

data_multiple_controls %>% group_by(case) %>% mutate(sum_weights=sum(weights)) %>%
  group_by(case, Sex) %>% summarise(n=sum(weights), perc=sum(weights)/first(sum_weights))

data_multiple_controls %>% group_by(case) %>% mutate(sum_weights=sum(weights)) %>%
  group_by(case, Sex.roommate) %>% summarise(n=sum(weights), perc=sum(weights)/first(sum_weights))

data_multiple_controls %>% group_by(case) %>% mutate(sum_weights=sum(weights)) %>%
  group_by(case, Race) %>% summarise(n=sum(weights), perc=sum(weights)/first(sum_weights))

data_multiple_controls %>% group_by(case) %>% mutate(sum_weights=sum(weights)) %>%
  group_by(case, Race.roommate) %>% summarise(n=sum(weights), perc=sum(weights)/first(sum_weights))

library(Hmisc)
data_multiple_controls %>% group_by(case) %>% 
  summarise(mean=sum(weights*age)/sum(weights), sd=sqrt(wtd.var(age, weights)))

data_multiple_controls %>% group_by(case) %>% 
  summarise(mean=sum(weights*age.roommate)/sum(weights), sd=sqrt(wtd.var(age.roommate, weights)))

data_multiple_controls %>% group_by(case) %>% 
  summarise(mean=sum(weights*risk)/sum(weights), sd=sqrt(wtd.var(risk, weights)))

data_multiple_controls %>% group_by(case) %>% 
  summarise(mean=sum(weights*risk.roommate)/sum(weights), sd=sqrt(wtd.var(risk.roommate, weights)))

data_multiple_controls %>% filter(num_dose_adjusted>0) %>% group_by(case) %>% 
  summarise(mean=sum(weights*time_since_vacc)/sum(weights), sd=sqrt(wtd.var(time_since_vacc, weights)))

data_multiple_controls %>% filter(dose.roommate.adjusted>0) %>% group_by(case) %>% 
  summarise(mean=sum(weights*time_since_vacc.roommate)/sum(weights), sd=sqrt(wtd.var(time_since_vacc.roommate, weights)))

data_multiple_controls %>% filter(has.prior.inf==1) %>% group_by(case) %>% 
  summarise(mean=sum(weights*time_since_inf)/sum(weights), sd=sqrt(wtd.var(time_since_inf, weights)))

data_multiple_controls %>% filter(has.prior.inf.roommate==1) %>% group_by(case) %>% 
  summarise(mean=sum(weights*time_since_inf.roommate)/sum(weights), sd=sqrt(wtd.var(time_since_inf.roommate, weights)))

residents <- tableone::CreateTableOne(vars=c("age", "Sex", "Race", "risk", "num_dose_adjusted", "time_since_vacc", 
                                             "has.prior.inf", "time_since_inf", "level"),
                                      factorVars = c("level", "num_dose_adjusted", "has.prior.inf"),
                                      strata="case",
                                      data=data, addOverall = F, ) 


roommates <- tableone::CreateTableOne(vars=c("age.roommate", "Sex.roommate", "Race.roommate", "risk.roommate", 
                                             "dose.roommate.adjusted", "time_since_vacc.roommate",
                                             "has.prior.inf.roommate", "time_since_inf.roommate"),
                                      factorVars = c("dose.roommate.adjusted", "has.prior.inf.roommate"),
                                      strata = c("case"),
                                      data=data, addOverall=F)

print(residents)[,c(2,1)] %>% rbind(print(roommates)[,c(2,1)]) %>% as.data.frame() %>% write.csv("D:/CCHCS_premium/st/covid-indirects/tables/demo.csv")

