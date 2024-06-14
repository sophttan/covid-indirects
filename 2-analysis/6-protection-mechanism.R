# Sophia Tan 3/4/24
# Evaluate plausibility of mechanism of indirect protection

source(here::here("config.R"))


data <- read_csv("D:/CCHCS_premium/st/indirects/case_control_postmatchprocessing061324.csv") %>% 
  select(id, group, ResidentId, num_dose_adjusted, test.Day, case, Roommate, 
         last.inf.roommate, last.vacc.roommate, dose.roommate.adjusted, has.prior.inf, 
         has.vacc.roommate.binary, has.prior.inf.roommate)

test_data <- read_csv("D:/CCHCS_premium/st/cleaned-data/complete_testing_data051324.csv") %>% select(ResidentId,Day,Result,num_pos)
check_test <- data %>% left_join(test_data, by=c("ResidentId"="ResidentId")) %>% group_by(id)

infection <- read_csv("D:/CCHCS_premium/st/cleaned-data/infection_data051324.csv")


# fill in
num_days <- 4

check_test_filtered <- check_test %>% mutate(has_test=any(test.Day-Day <= num_days & test.Day-Day>0,na.rm=T))
check_test_summary <- check_test_filtered %>% filter((!has_test&Day%>%is.na())|(!has_test&(Day==min(Day)))|(test.Day-Day <= num_days & test.Day-Day>0))
check_test_summary <- check_test_summary %>% mutate(Result=if_else(!has_test, NA, Result))
check_test_summary %>% group_by(id) %>% summarise_all(first) %>% group_by(case, has.vacc.roommate.binary) %>% summarise(n=sum(has_test), has_test=mean(has_test))

check_test_summary %>% group_by(id) %>% mutate(has_pos=any(Result=="Positive", na.rm=T)) %>% 
  summarise_all(first) %>% group_by(case, has.vacc.roommate.binary) %>% summarise(n=n(), npos=sum(has_pos), has_pos=mean(has_pos))

check_test_summary %>% mutate(has_either = if_else(has.vacc.roommate.binary==1|has.prior.inf.roommate==1, 1, 0)) %>%
  group_by(id) %>% 
  summarise_all(first) %>% 
  group_by(case, has_either) %>% summarise(n=n(), n2=sum(has_test), has_test=mean(has_test))

check_test_summary %>% mutate(has_either = if_else(num_dose_adjusted>0|has.prior.inf==1, 1, 0)) %>%
  group_by(id) %>% 
  summarise_all(first) %>% 
  group_by(case, has_either) %>% summarise(n=n(), n2=sum(has_test), has_test=mean(has_test))
  
