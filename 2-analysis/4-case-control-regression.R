# Sophia Tan 3/4/24
# Main regression results

source(here::here("config.R"))

data <- read_csv("D:/CCHCS_premium/st/indirects/case_control_postmatchprocessing_0-6day_061324.csv")
data <- data %>% mutate(time_since_inf_cut.roommate = factor(time_since_inf_cut.roommate, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))
data <- data %>% mutate(time_since_vacc_cut.roommate = factor(time_since_vacc_cut.roommate, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))
data <- data %>% mutate(time_since_infvacc_cut.roommate = factor(time_since_infvacc_cut.roommate, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))

# TO FILL IN
# change to reflect analysis being run (results folder name)
analysis <- "roommate-0-6day"
# uncomment if adjusting for time since infection and time since vaccine in case/control
# data <- data %>% replace_na(list(time_since_inf=1000, time_since_vacc=1000))


# function to clean results 
format_results <- function(model) {
  res <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())
  
  res <- res %>% mutate(x=rownames(res))
  names(res) <- c("point", "lb", "ub", "x")
  
  res %>% select(x, point, lb, ub)
}


# main model - binary vaccine and binary infection status
model <- clogit(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                  # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                  strata(group), data=data)
write_csv(format_results(model), here::here(paste0("results/", analysis, "/binary-results.csv")))


# dose dependent model
model <- clogit(case ~ dose.roommate.adjusted + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate +
                  # time_since_inf + time_since_vacc +
                  strata(group), data=data)

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

write_csv(rbind(dose_results, main), here::here(paste0("results/", analysis, "/dose-results.csv")))


# time since models
inf_model <- clogit(case ~ time_since_inf_cut.roommate + has.vacc.roommate.binary + 
                      age + age.roommate + risk + risk.roommate +
                      # time_since_inf + time_since_vacc +
                      strata(group), data=data)

vacc_model <- clogit(case ~ time_since_vacc_cut.roommate + has.prior.inf.roommate + 
                       age + age.roommate + risk + risk.roommate +
                       # time_since_inf + time_since_vacc +
                       strata(group), data=data)

infvacc_model <- clogit(case ~ time_since_infvacc_cut.roommate + 
                          age + age.roommate + risk + risk.roommate +
                          # time_since_inf + time_since_vacc +
                          strata(group), data=data)

results <- rbind(format_results(inf_model) %>% mutate(inf_vacc="inf"),
                 format_results(vacc_model) %>% mutate(inf_vacc="vacc"),
                 format_results(infvacc_model) %>% mutate(inf_vacc="infvacc"))
write_csv(results, here::here(paste0("results/", analysis, "/time-results.csv")))


# first 3 months vaccine
data <- data %>% 
  mutate(time_since_vacc_cut2.roommate=cut(time_since_vacc.roommate, breaks=c(0, 30, 60, 90, 182, 365, Inf), right = F)) 

levels(data$time_since_vacc_cut2.roommate)<-c(levels(data$time_since_vacc_cut2.roommate), "None") 
data$time_since_vacc_cut2.roommate[is.na(data$time_since_vacc_cut2.roommate)] <- "None"
data <- data %>% mutate(time_since_vacc_cut2.roommate = factor(time_since_vacc_cut2.roommate, levels=c("None","[0,30)", "[30,60)", "[60,90)","[90,182)","[182,365)","[365,Inf)")))

model <- clogit(case ~ time_since_vacc_cut2.roommate + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate +
                  # time_since_inf + time_since_vacc +
                  strata(group), data=data)
write_csv(format_results(model), here::here(paste0("results/", analysis, "/3months-results.csv")))


# bivalent
data <- data %>% 
  mutate(bivalent_time = case_when(last.vacc.roommate%>%is.na()~"Unvacc",
                                   last.vacc.roommate-14>="2022-09-01"~"Bivalent",
                                   time_since_vacc.roommate<90~"Monovalent<90",
                                   time_since_vacc.roommate>=90~"Monovalent>=90") %>% factor(levels=c("Unvacc", "Monovalent<90", "Monovalent>=90", "Bivalent")))

model <- clogit(case ~ bivalent_time + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate +
                  # time_since_inf + time_since_vacc +
                  strata(group), data=data)
write_csv(format_results(model), here::here(paste0("results/", analysis, "/bivalent-results.csv")))

data$bivalent_time %>% table()


# check interaction
model <- clogit(case ~ has.vacc.roommate.binary*has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate +
                  # time_since_inf + time_since_vacc +
                  strata(group), data=data)
summary(model) # interaction insignificant

# hybrid immunity model
data <- data %>% mutate(infvacc=case_when(has.vacc.roommate.binary==0&has.prior.inf.roommate==0~"none",
                                                                                  has.vacc.roommate.binary==1&has.prior.inf.roommate==1~"both",
                                                                                  has.vacc.roommate.binary==1~"vacc",
                                                                                  has.prior.inf.roommate==1~"inf"),
                                                                infvacc=factor(infvacc, levels=c("none", "vacc", "inf", "both"))) 

model <- clogit(case ~ infvacc + 
                  age + age.roommate + risk + risk.roommate +
                  # time_since_inf + time_since_vacc +
                  strata(group), data=data)
write_csv(format_results(model), here::here(paste0("results/", analysis, "/hybrid-results.csv")))



## other sensitivity analyses (run for main analysis only)
# adjust for time since inf and time since vacc of roommates together
model <- clogit(case ~ time_since_vacc_cut.roommate + time_since_inf_cut.roommate + 
                  age + age.roommate + risk + risk.roommate + 
                  strata(group), data=data)

write_csv(format_results(model), here::here(paste0("results/", analysis, "/time-bothinfvacc-results.csv")))


# preomicron v omicron prior infection protection
data <- data %>% 
  mutate(variant=case_when(last.inf.roommate<"2021-12-15"~"preomicron",
                           last.inf.roommate>="2021-12-15"~"omicron",
                           T~"") %>% factor()) %>%
  mutate(time_inf=paste0(time_since_inf_cut.roommate, variant) %>% 
           factor(levels=c("None", "[0,90)preomicron","[90,182)preomicron","[182,365)preomicron","[365,Inf)preomicron", 
                           "[0,90)omicron","[90,182)omicron","[182,365)omicron","[365,Inf)omicron")))

model <- clogit(case ~ time_inf + has.vacc.roommate.binary + 
                  age + age.roommate + risk + risk.roommate +
                  strata(group), data=data)

data$time_inf%>%table()

write_csv(format_results(model), here::here(paste0("results/", analysis, "/preomicron-omicron-results.csv")))
