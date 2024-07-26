# Sophia Tan 2/26/24
# Identify infections and controls that meet study criteria

source(here::here("config.R"))

# load testing and infection data
inf_full <- read_csv("D:/CCHCS_premium/st/cleaned-data/infection_data051324.csv")
testing <- read_csv("D:/CCHCS_premium/st/cleaned-data/complete_testing_data051324.csv") 

# load housing data
housing <- NULL
for(i in 1:4){
  housing <- rbind(housing, read_csv(paste0("D:/CCHCS_premium/st/leaky/raw-data/housing_subset", i,".csv")))
}
# keep only pandemic housing data
housing <- housing %>% filter(Night>="2020-03-01") %>% filter(Night <= "2022-12-15")
gc()

# get list of residents that are eligible for inclusion based on housing requirement (must have been incarcerated by March 2020)
residents <- (housing %>% group_by(ResidentId))$ResidentId %>% unique() #177319 residents incarcerated at any point 3/1/2020-12/15/2022
included <- (housing %>% group_by(ResidentId) %>% filter(min(Night)<"2020-04-01"))$ResidentId %>% unique()


# filter housing data to be used to identify roommates
housing_relevant <- housing %>% filter(Night >= "2021-12-01")
rm(housing)
gc()

housing_relevant <- housing_relevant %>% group_by(Night, Institution, BuildingId, RoomId) %>% mutate(n=n())
housing_relevant <- housing_relevant %>% ungroup() %>% mutate(Day=Night+1) # housing defined by location at night - housing day is therefore morning/day after
gc()



#### housing requirements #### 
# fill in days for roommate definitions
# in the main analysis, we require that cases and controls co-reside with one person for the entire preceding period 3-6 days prior to test collection
# we conduct sensitivity analyses with 3 day, 0-6 days, and 6-9 days windows
min_days <- 3
max_days <- 6
num_days <- max_days-min_days+1



#### identify cases ####
# filter infections so only include confirmed infections during study period
inf <- inf_full %>% filter(Day>="2021-12-15" & Day<="2022-12-15") %>%
  select(ResidentId, num_pos, Day)
inf

# cases must have been incarcerated before 4/1/2020
inf_eligible <- inf %>% filter(ResidentId %in% included) # 23,307 eligible cases 

# join with housing data to check roommate requirements
inf_housing_full <- inf_eligible %>% full_join(housing_relevant, by=c("ResidentId")) %>% 
  rename("Day"="Day.y", "inf.Day"="Day.x") %>% 
  filter(inf.Day-Day<=max_days & inf.Day-Day>=min_days) %>% # filter for days within specified preceding period (i.e. 3-6 days)
  select(!c(Night)) %>% 
  group_by(ResidentId, num_pos) %>% 
  filter(n()==num_days) # make sure resident has housing data for all days in preceding period (125 missing any/all housing data)

# find nightly roommate(s) for each case
inf_housing_full_withroommate <- inf_housing_full %>% left_join(housing_relevant %>% rename("Roommate"="ResidentId"))
inf_housing_full_withroommate <- inf_housing_full_withroommate %>% filter(n==1 | Roommate != ResidentId) # keep row if resident is living alone, otherwise keep only roommates

inf_housing_full_withroommate <- inf_housing_full_withroommate %>% group_by(ResidentId, num_pos) %>%
  arrange(ResidentId, num_pos, Day)

inf_housing_full_withroommate %>% filter(any(n>2)) # 9693 excluded because they stay in rooms with more than 2 residents
inf_housing_full_withroommate %>% filter(all(n<=2)&any(n==1)) # 5493 excluded because they are in quarantine/isolation at any point during preceding period
  
inf_2 <- inf_housing_full_withroommate %>% 
  filter(all(n==2)) # case living with 1 other person over entire preceding period

cases_final <- inf_2 %>%
  filter(all(Roommate==first(Roommate))) %>% # make sure case lives with the same period over preceding period
  filter(all(Institution==first(Institution))) %>% # make sure case doesnt move buildings during preceding period since case/controls will be matched by building
  filter(all(BuildingId==first(BuildingId)))

cases_final <- cases_final %>% filter(first(Roommate) %in% included) # make sure roommate meets incarceration requirement

write_csv(cases_final, "D:/CCHCS_premium/st/indirects/cases6-9daysame-roommate-061324.csv")




#### identify controls ####
# tests during study period (includes positive and negative)
testing <- testing %>% group_by(ResidentId, num_pos) %>% mutate(last_inf=if_else(num_pos==0, NA, min(Day))) # new column for when most recent infection was
testing <- testing %>% filter(Day >= "2021-12-15" & Day <= "2022-12-15") 

# residents must have been incarcerated before 4/1/2020
testing_eligible <- testing %>% ungroup() %>% filter(ResidentId %in% included) #954227 tests among residents 

# control cannot have had an infection within the last 90 days
testing_eligible <- testing_eligible %>% filter(Result=="Negative") %>% 
  filter(num_pos==0|Day-last_inf>90) # no prior infection or infection > 90 days ago

# control also cannot have a new infection in the following 14 days after test collection
inf_full <- inf_full %>%
  select(ResidentId, Day) %>% rename(new.inf=Day)
remove <- testing_eligible %>% left_join(inf_full, "ResidentId") %>%
  filter(new.inf>Day & new.inf-Day<14) %>%
  select(ResidentId, Day) %>% mutate(remove=1)
testing_eligible <- testing_eligible %>% left_join(remove) %>% filter(remove%>%is.na()) %>% select(!remove)

testing_eligible  <- testing_eligible  %>% select(!c(Result, pcr, antigen, unknown, last_inf))

# data is too big to test all at once, so loop through tests in groups of 25000 negative tests
# keep track of reasons why controls are excluded for different study criteria
test_final <- NULL # has controls that meet all inclusion criteria
total_excluded_housing <- 0
total_excluded_isolation <- 0
total_excluded_group <- 0
total_excluded_movement <- 0
total_excluded_roommate <- 0

for(i in 0:floor(nrow(testing_eligible)/25000)) {
  gc()
  
  testing_sub <- testing_eligible[max(1,(i*25000)):min(((i+1)*25000-1),nrow(testing_eligible)),]
  n_total <- testing_sub %>% nrow()
  test_housing_full <- testing_sub %>% full_join(housing_relevant, by=c("ResidentId")) 
  
  test_housing_full <- test_housing_full %>% 
    rename("Day"="Day.y", "test.Day"="Day.x") %>%
    filter(test.Day-Day<=max_days & test.Day-Day>=min_days) %>% 
    group_by(ResidentId, test.Day) %>%
    filter(n()==num_days) %>%
    select(!c(Night)) 

  total_excluded_housing <- total_excluded_housing + n_total-(test_housing_full%>%group_keys()%>%nrow())
  test_housing_full_withroommate <- test_housing_full %>% left_join(housing_relevant %>% rename("Roommate"="ResidentId"))
  test_housing_full_withroommate <- test_housing_full_withroommate %>% filter(n==1 | Roommate != ResidentId)
  
  total_excluded_group <- total_excluded_group + (test_housing_full_withroommate %>%filter(any(n>2))%>%group_keys()%>%nrow())
  total_excluded_isolation <- total_excluded_isolation + (test_housing_full_withroommate %>%filter(all(n<=2)&any(n==1))%>%group_keys()%>%nrow())
  
  test_2 <- test_housing_full_withroommate %>% group_by(ResidentId, test.Day) %>% 
    filter(all(n==2)) %>% 
    arrange(ResidentId, test.Day, Day) 
  
  tests <- test_2 %>%
    filter(all(Roommate==first(Roommate))) %>%
    filter(all(Institution==first(Institution))) %>%
    filter(all(BuildingId==first(BuildingId)))
  
  total_excluded_movement <- total_excluded_movement + (test_2%>%group_keys()%>%nrow()) - (tests%>%group_keys()%>%nrow())
  
  total_excluded_roommate <- total_excluded_roommate + (tests %>%filter(!first(Roommate) %in% included)%>%group_keys()%>%nrow())
  
  tests <- tests %>% filter(first(Roommate) %in% included)

  test_final <- test_final %>% rbind(tests)
  
}


write_csv(test_final, "D:/CCHCS_premium/st/indirects/control6-9daysame-roommate-061324.csv")




#### combine case and control data ####
total <- rbind(cases_final %>% mutate(case=1) %>% rename("test.Day"="inf.Day"), 
               test_final %>% mutate(case=0))
total

total <- total %>% group_by(ResidentId, test.Day) 
total <- total %>% summarise_all(first)
total$case %>% table()

# label whether or not resident has prior infection
# num_pos marks the # of prior infections (+1 when resident has new infection) 
# if resident is a case, # prior infections = num_pos-1
total <- total %>% mutate(has.prior.inf=case_when(case==1&num_pos>1~1,
                                                  case==1&num_pos==1~0,
                                                  case==0&num_pos>0~1,
                                                  T~0))

# load covariate data  
vaccine <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/complete_vaccine_data121523.csv") 
security <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/cleaned_security_data021324.csv") 
demo <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/demographic121523.csv") %>% mutate(age=2022-BirthYear) %>% select(ResidentId, age)
risk <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/covid_risk_score012324.csv")
inf <- read_csv("D:/CCHCS_premium/st/cleaned-data/infection_data051324.csv") %>% filter(Day <= "2022-12-15") %>%
  select(ResidentId, Day) %>% rename(last.inf=Day)


# add in date of last infection for case/control
total_inf <- total %>% left_join(inf, "ResidentId") %>% 
  mutate(last.inf=if_else(is.na(last.inf)|last.inf>=test.Day, NA, last.inf)) %>%
  group_by(ResidentId, test.Day) %>%
  filter(all(last.inf%>%is.na())|!last.inf%>%is.na()) %>% 
  filter(all(last.inf%>%is.na())|last.inf==max(last.inf)) %>% distinct(ResidentId, test.Day, last.inf, .keep_all = T)


# add in date of last vaccine for case/control
total_vacc <- total_inf %>% group_by(ResidentId, test.Day) %>%
  left_join(vaccine%>%select(ResidentId, Date_offset, num_dose, full_vacc), by=c("ResidentId")) %>% 
  mutate(Date_offset=if_else(Date_offset>test.Day, NA, Date_offset)) 
total_vacc <- total_vacc %>% 
  filter(all(Date_offset%>%is.na())|!Date_offset%>%is.na()) %>% 
  filter(all(Date_offset%>%is.na())|Date_offset==max(Date_offset)) %>% distinct(ResidentId, test.Day, Date_offset, .keep_all = T)
total_vacc <- total_vacc %>% mutate(num_dose=if_else(Date_offset%>%is.na(), 0, num_dose))


# relabel vaccine doses (unvacc=0, partial vacc=1, complete primary=2, 1 booster=3, 2+ boosters=4)
total_vacc <- total_vacc %>% 
  mutate(num_dose_adjusted = case_when(num_dose==0~0,
                                       num_dose<full_vacc~1,
                                       num_dose==full_vacc~2,
                                       num_dose-full_vacc==1~3,
                                       num_dose-full_vacc>1~4))


# add columns for time since inf and time since vacc
total_vacc <- total_vacc %>% 
  mutate(time_since_vacc = (test.Day-Date_offset)%>%as.numeric(),
         time_since_inf = (test.Day-last.inf) %>% as.numeric()) 


# add in security level for case/control on date of test collection
total_vacc_security <- total_vacc %>% left_join(security)
total_vacc_security <- total_vacc_security %>% filter(Starting<=test.Day) %>% filter(Ending %>% is.na() | Ending > test.Day)
total_vacc_security <- total_vacc_security %>% select(!c(Starting, Ending))


# add in age data for case/control and for roommates
total_vacc_security_demo <- total_vacc_security %>% 
  left_join(demo, by=c("ResidentId")) %>% 
  left_join(demo, by=c("Roommate"="ResidentId"), suffix=c("", ".roommate"))


# add in risk data for case/control and for roommates
total_vacc_security_demo_risk <- total_vacc_security_demo %>% 
  left_join(risk, by=c("ResidentId")) %>% 
  filter(risk.start<=test.Day) %>% filter(risk.end > test.Day) %>%
  left_join(risk, by=c("Roommate"="ResidentId"), suffix=c("", ".roommate")) %>% 
  filter(risk.start.roommate<=test.Day) %>% filter(risk.end.roommate > test.Day)


# scale factors for distance based matching (mean 0, SD 1)
# time since vacc scale and time since inf scale weighted 2x
total_vacc_security_demo_risk <- total_vacc_security_demo_risk %>% ungroup() %>%
  mutate(time_since_vacc.scale = (scale(time_since_vacc, T, T)%>%as.vector())*2,
         time_since_inf.scale = (scale(time_since_inf, T, T)%>%as.vector())*2,
         risk.scale = scale(risk, T, T)%>%as.vector(),
         risk.roommate.scale = scale(risk.roommate, T, T)%>%as.vector(),
         age.scale = scale(age, T, T)%>%as.vector(),
         age.roommate.scale = scale(age.roommate, T, T)%>%as.vector())


total_vacc_security_demo_risk <- total_vacc_security_demo_risk %>% replace_na(list(time_since_vacc.scale=0, time_since_inf.scale=0))


write_csv(total_vacc_security_demo_risk, "D:/CCHCS_premium/st/indirects/case_control_prematch_6-9day061324.csv")
