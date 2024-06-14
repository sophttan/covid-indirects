# Sophia Tan 3/4/24
# Evaluate plausibility of mechanism of indirect protection

source(here::here("config.R"))

data <- read_csv("D:/CCHCS_premium/st/indirects/case_control_postmatchprocessing061324.csv")

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

residents <- tableone::CreateTableOne(vars=c("age", "Sex", "Race", "risk", "num_dose_adjusted", "time_since_vacc", 
                                             "has.prior.inf", "time_since_inf", "level"),
                                      factorVars = c("level", "num_dose_adjusted", "has.prior.inf"),
                                      strata="case",
                                      data=data, addOverall = F) 


roommates <- tableone::CreateTableOne(vars=c("age.roommate", "Sex.roommate", "Race.roommate", "risk.roommate", 
                                             "dose.roommate.adjusted", "time_since_vacc.roommate",
                                             "has.prior.inf.roommate", "time_since_inf.roommate"),
                                      factorVars = c("dose.roommate.adjusted", "has.prior.inf.roommate"),
                                      strata = c("case"),
                                      data=data, addOverall=F)

print(residents)[,c(2,1)] %>% rbind(print(roommates)[,c(2,1)]) %>% as.data.frame() %>% write.csv("D:/CCHCS_premium/st/covid-indirects/tables/demo.csv")

