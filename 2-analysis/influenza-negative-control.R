vaccine <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/complete_vaccine_data121523.csv") 
vaccine 
(vaccine %>% group_by(ResidentId) %>% filter(num_dose==1))%>% group_by(Vaccine, full_vacc) %>% summarise(n=n())
(vaccine %>% group_by(ResidentId) %>% filter(num_dose==1))$full_vacc %>% table()
(vaccine %>% group_by(ResidentId) %>% filter(num_dose==1)) %>% filter(grepl("Ad26", Vaccine)&grepl("mRNA", Vaccine))
c(20561, 78894, 22619)/122184

(vaccine %>% filter(num_dose==1) %>% filter(ResidentId %in% data$ResidentId))$Vaccine %>% table()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/CDCR Data/May 26 2023 Data/")

###### COVID Infection Data ######
vacc <-  read_delim("Immunization_20230526.csv", delim=";")
vacc$Vaccine%>%unique()

vacc_influenza <- vacc %>% filter(grepl("influenza", Vaccine))
vacc_influenza <- vacc_influenza %>% filter(Result=="Received")
vacc_influenza %>% filter(Date>="2020-01-01") %>% ggplot(aes(Date)) + geom_histogram()

vacc_influenza_period <- vacc_influenza %>% filter(Date <= "2022-12-15") %>% rename(flu.date=Date) %>% select(ResidentId, flu.date)

data <- read_csv("D:/CCHCS_premium/st/indirects/case_control_postmatchprocessing072224.csv")

data <- data %>% left_join(vacc_influenza_period, by=c("Roommate"="ResidentId"))

data <- data %>% group_by(id) %>% mutate(has.flu.roommate=any(!flu.date%>%is.na()) & any(flu.date<=test.Day)) %>% 
  filter(has.flu.roommate==F|flu.date<test.Day) %>%
  arrange(id, desc(flu.date)) %>% summarise_all(first)

data


# function to clean results 
format_results <- function(model) {
  res <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())
  
  res <- res %>% mutate(x=rownames(res))
  names(res) <- c("point", "lb", "ub", "x")
  
  res %>% select(x, point, lb, ub)
}


# main model - binary vaccine and binary infection status
model <- clogit(case ~ has.flu.roommate + 
                  age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                  # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                  strata(group), data=data)


# within 2 years
data <- data %>% mutate(flu.twoyears = if_else(has.flu.roommate&(test.Day-flu.date<=730&test.Day-flu.date>=0), T, F),
                        flu.oneyear = if_else(has.flu.roommate&(test.Day-flu.date<=365&test.Day-flu.date>=0), T, F))
data
two_year_model <- clogit(case ~ flu.twoyears + 
                  age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                  # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                  strata(group), data=data)


one_year_model <- clogit(case ~ flu.oneyear + 
                           age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                           # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                           strata(group), data=data)
write_csv(rbind(format_results(model)[1,],
                format_results(two_year_model)[1,],
                format_results(one_year_model)[1,]), 
          here::here("results/main/flu-results.csv"))

data %>% group_by(case) %>% summarise(has.flu.roommate=mean(has.flu.roommate), 
                                      flu.twoyears=mean(flu.twoyears),
                                      flu.oneyear=mean(flu.oneyear))

data %>% ggplot(aes(test.Day-flu.date, group=case, fill=factor(case))) + geom_histogram(aes(y=..density..), position="identity", alpha=0.5)
data %>% filter(test.Day-flu.date<=735&test.Day-flu.date>=0) %>%
  ggplot(aes(test.Day-flu.date, group=case, fill=factor(case))) + geom_histogram(aes(y=..density..), position="identity", alpha=0.5)
data %>% filter(test.Day-flu.date<=360&test.Day-flu.date>=0) %>%
  ggplot(aes(test.Day-flu.date, group=case, fill=factor(case))) + geom_histogram(aes(y=..density..), position="identity", alpha=0.5)


data <- data %>% mutate(time_since_flu = if_else(test.Day-flu.date<0, 0, ((test.Day-flu.date)%>%as.numeric())/30.412))
time_model <- clogit(case ~ time_since_flu + 
                           age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                           # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                           strata(group), data=data)

write_csv(rbind(format_results(model)[1,],
                format_results(two_year_model)[1,],
                format_results(one_year_model)[1,],
                format_results(time_model)[1,]), 
          here::here("results/main/flu-results.csv"))
