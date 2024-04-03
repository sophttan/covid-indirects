matched_infvacc_roommate

library(tableone)

matched_infvacc_roommate %>% names()

demo <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/demographic121523.csv")
demo

demo <- demo %>% mutate(Race=case_when(Race=="A"~"Asian or Pacific Islander",
                                       Race=="B"~"Black",
                                       Race=="H"|Race=="M"|Race=="C"~"Hispanic",
                                       Race=="I"~"American Indian/Alaskan Native",
                                       Race=="O"~"Other",
                                       Race=="W"~"White",
                                       Race=="U"~"Unknown"))

matched_infvacc_roommate_demo <- matched_infvacc_roommate %>% 
  left_join(demo %>% select(!BirthYear), by="ResidentId") %>%
  left_join(demo %>% select(!BirthYear), by=c("Roommate"="ResidentId"), suffix=c("", ".roommate"))

residents <- tableone::CreateTableOne(vars=c("age", "Sex", "Race", "risk", "num_dose_adjusted", "time_since_vacc", 
                                             "has.prior.inf", "time_since_inf", "level"),
                                      factorVars = c("level", "num_dose_adjusted", "has.prior.inf"),
                                      strata="case",
                                      data=matched_infvacc_roommate_demo, addOverall = T) 


roommates <- tableone::CreateTableOne(vars=c("age.roommate", "Sex.roommate", "Race.roommate", "risk.roommate", 
                                             "dose.roommate.adjusted", "time_since_vacc.roommate",
                                             "has.prior.inf.roommate", "time_since_inf.roommate"),
                                      factorVars = c("dose.roommate.adjusted", "has.prior.inf.roommate"),
                                      strata = c("case"),
                                      data=matched_infvacc_roommate_demo, addOverall=T)

print(residents)[,1:3] %>% rbind(print(roommates)[,1:3]) %>% as.data.frame() %>% write.csv("D:/CCHCS_premium/st/covid-indirects/tables/demo.csv")
