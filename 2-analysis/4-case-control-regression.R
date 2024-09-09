# Sophia Tan 3/4/24
# Main regression results

source(here::here("config.R"))

data <- read_csv("D:/CCHCS_premium/st/indirects/case_control_postmatchprocessing061324.csv")
data <- data %>% mutate(time_since_inf_cut.roommate = factor(time_since_inf_cut.roommate, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))
data <- data %>% mutate(time_since_vacc_cut.roommate = factor(time_since_vacc_cut.roommate, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))
data <- data %>% mutate(time_since_infvacc_cut.roommate = factor(time_since_infvacc_cut.roommate, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))


data <- data %>% mutate(time_since_vacc_cut = factor(time_since_vacc_cut, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))
# data <- data %>% mutate(time_since_inf_cut = factor(time_since_inf_cut, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))

# TO FILL IN
# change to reflect analysis being run (results folder name)
analysis <- "main"
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
# dose dependent (categorical) model
data <- data %>% mutate(dose.roommate.cat = factor(dose.roommate.adjusted, levels=c(0,1,2,3,4)))
data$dose.roommate.cat %>% table()
model <- clogit(case ~ dose.roommate.cat + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate +
                  # time_since_inf + time_since_vacc +
                  strata(group), data=data)
format_results(model)

write_csv(format_results(model), here::here(paste0("results/", analysis, "/dose-results-categorical.csv")))

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



# results for 6months since last infection
data <- data %>% 
  mutate(time_since_inf_cut2.roommate=cut(time_since_inf.roommate, breaks=c(0, 182, 365, Inf), right = F)) 

levels(data$time_since_inf_cut2.roommate)<-c(levels(data$time_since_inf_cut2.roommate), "None") 
data$time_since_inf_cut2.roommate[is.na(data$time_since_inf_cut2.roommate)] <- "None"
data <- data %>% mutate(time_since_inf_cut2.roommate = factor(time_since_inf_cut2.roommate, levels=c("None","[0,182)","[182,365)","[365,Inf)")))

model <- clogit(case ~ time_since_inf_cut2.roommate + has.vacc.roommate.binary + 
                  age + age.roommate + risk + risk.roommate +
                  # time_since_inf + time_since_vacc +
                  strata(group), data=data)
write_csv(format_results(model), here::here(paste0("results/", analysis, "/6months-inf-results.csv")))



# main model - stratified
no_immunity <- data %>% filter(num_dose_adjusted==0&has.prior.inf==0)
no_immunity_model <- clogit(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                  # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                  strata(group), data=no_immunity)
format_results(no_immunity_model)

no_immunity_model_timevacc <- clogit(case ~ time_since_vacc_cut.roommate + has.prior.inf.roommate + 
                                age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                                # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                                strata(group), data=no_immunity)
format_results(no_immunity_model_timevacc)

no_immunity_model_timeinf <- clogit(case ~ has.vacc.roommate.binary + time_since_inf_cut.roommate + 
                               age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                               # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                               strata(group), data=no_immunity)
format_results(no_immunity_model_timeinf)

vacc_immunity <- data %>% filter(num_dose_adjusted>0&has.prior.inf==0)
vacc_model <- clogit(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                  # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                  strata(group), data=data %>% filter(num_dose_adjusted>0&has.prior.inf==0))
format_results(vacc_model)

vacc_model_timevacc <- clogit(case ~ time_since_vacc_cut.roommate + has.prior.inf.roommate + 
                       age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                       # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                       strata(group), data=vacc_immunity)
format_results(vacc_model_timevacc)

vacc_model_timeinf <- clogit(case ~ has.vacc.roommate.binary + time_since_inf_cut.roommate + 
                       age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                       # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                       strata(group), data=vacc_immunity)
format_results(vacc_model_timeinf)

inf_immunity <- data %>% filter(num_dose_adjusted==0&has.prior.inf==1)
inf_model <- clogit(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                  # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                  strata(group), data=inf_immunity)
format_results(inf_model)

inf_model_timevacc <- clogit(case ~ time_since_vacc_cut.roommate + has.prior.inf.roommate + 
                                age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                                # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                                strata(group), data=inf_immunity)
format_results(inf_model_timevacc)

inf_model_timeinf <- clogit(case ~ has.vacc.roommate.binary + time_since_inf_cut.roommate + 
                               age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                               # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                               strata(group), data=inf_immunity)
format_results(inf_model_timeinf)

hybrid_immunity <- data %>% filter(num_dose_adjusted>0&has.prior.inf==1)
hybrid_model <- clogit(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                  # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                  strata(group), data=hybrid_immunity)
format_results(hybrid_model)

hybrid_model_timevacc <- clogit(case ~ time_since_vacc_cut.roommate + has.prior.inf.roommate + 
                               age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                               # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                               strata(group), data=hybrid_immunity)
format_results(hybrid_model_timevacc)

hybrid_model_timeinf <- clogit(case ~ has.vacc.roommate.binary + time_since_inf_cut.roommate + 
                              age + age.roommate + risk + risk.roommate + # commented if running analysis with no adjustments for additional covariates
                              # time_since_inf + time_since_vacc + # commented unless running analysis with adjustment for time since vacc and/or inf of case/control
                              strata(group), data=hybrid_immunity)
format_results(hybrid_model_timeinf)

write_csv(rbind(format_results(no_immunity_model)[1:2,]%>%mutate(label="no_immunity", n=no_immunity%>%nrow()),
                format_results(vacc_model)[1:2,]%>%mutate(label="vacc_immunity", n=vacc_immunity%>%nrow()),
                format_results(inf_model)[1:2,]%>%mutate(label="inf_immunity", n=inf_immunity%>%nrow()),
                format_results(hybrid_model)[1:2,]%>%mutate(label="hybrid_immunity", n=hybrid_immunity%>%nrow())),
          here::here(paste0("results/", analysis, "/stratified-casecontrolimmunity-results.csv")))

write_csv(rbind(format_results(no_immunity_model_timevacc)[1:4,]%>%mutate(label="no_immunity", n=(no_immunity$time_since_vacc_cut.roommate%>%table())[2:5]),
                format_results(vacc_model_timevacc)[1:4,]%>%mutate(label="vacc_immunity", n=(vacc_immunity$time_since_vacc_cut.roommate%>%table())[2:5]),
                format_results(inf_model_timevacc)[1:4,]%>%mutate(label="inf_immunity", n=(inf_immunity$time_since_vacc_cut.roommate%>%table())[2:5]),
                format_results(hybrid_model_timevacc)[1:4,]%>%mutate(label="hybrid_immunity", n=(hybrid_immunity$time_since_vacc_cut.roommate%>%table())[2:5])),
          here::here(paste0("results/", analysis, "/stratified-casecontrolimmunity-results-vacc.csv")))

write_csv(rbind(format_results(no_immunity_model_timeinf)[2:5,]%>%mutate(label="no_immunity", n=(no_immunity$time_since_inf_cut.roommate%>%table())[2:5]),
                format_results(vacc_model_timeinf)[2:5,]%>%mutate(label="vacc_immunity", n=(vacc_immunity$time_since_inf_cut.roommate%>%table())[2:5]),
                format_results(inf_model_timeinf)[2:5,]%>%mutate(label="inf_immunity", n=(inf_immunity$time_since_inf_cut.roommate%>%table())[2:5]),
                format_results(hybrid_model_timeinf)[2:5,]%>%mutate(label="hybrid_immunity", n=(hybrid_immunity$time_since_inf_cut.roommate%>%table())[2:5])),
          here::here(paste0("results/", analysis, "/stratified-casecontrolimmunity-results-inf.csv")))


no_immunity_model_dose <- clogit(case ~ dose.roommate.adjusted + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate +
                  # time_since_inf + time_since_vacc +
                  strata(group), data=no_immunity)

vacc_immunity_model_dose <- clogit(case ~ dose.roommate.adjusted + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate +
                  # time_since_inf + time_since_vacc +
                  strata(group), data=vacc_immunity)

inf_immunity_model_dose <- clogit(case ~ dose.roommate.adjusted + has.prior.inf.roommate + 
                                     age + age.roommate + risk + risk.roommate +
                                     # time_since_inf + time_since_vacc +
                                     strata(group), data=inf_immunity)

hybrid_immunity_model_dose <- clogit(case ~ dose.roommate.adjusted + has.prior.inf.roommate + 
                                     age + age.roommate + risk + risk.roommate +
                                     # time_since_inf + time_since_vacc +
                                     strata(group), data=hybrid_immunity)

no_immunity_model_dose <- (summary(no_immunity_model_dose)$coefficients)[1,]
or <- exp(no_immunity_model_dose[1]*1:4)
low <- exp(no_immunity_model_dose[1]*1:4-1.96*no_immunity_model_dose[3])
high <- exp(no_immunity_model_dose[1]*1:4+1.96*no_immunity_model_dose[3])
dose_results <- cbind(or, low, high) %>% as.data.frame() %>% mutate(label="no_immunity")

vacc_immunity_model_dose <- (summary(vacc_immunity_model_dose)$coefficients)[1,]
or <- exp(vacc_immunity_model_dose[1]*1:4)
low <- exp(vacc_immunity_model_dose[1]*1:4-1.96*vacc_immunity_model_dose[3])
high <- exp(vacc_immunity_model_dose[1]*1:4+1.96*vacc_immunity_model_dose[3])
dose_results <- rbind(dose_results, cbind(or, low, high) %>% as.data.frame() %>% mutate(label="vacc_immunity"))

inf_immunity_model_dose <- (summary(inf_immunity_model_dose)$coefficients)[1,]
or <- exp(inf_immunity_model_dose[1]*1:4)
low <- exp(inf_immunity_model_dose[1]*1:4-1.96*inf_immunity_model_dose[3])
high <- exp(inf_immunity_model_dose[1]*1:4+1.96*inf_immunity_model_dose[3])
dose_results <- rbind(dose_results, cbind(or, low, high) %>% as.data.frame() %>% mutate(label="inf_immunity"))

hybrid_immunity_model_dose <- (summary(hybrid_immunity_model_dose)$coefficients)[1,]
or <- exp(hybrid_immunity_model_dose[1]*1:4)
low <- exp(hybrid_immunity_model_dose[1]*1:4-1.96*hybrid_immunity_model_dose[3])
high <- exp(hybrid_immunity_model_dose[1]*1:4+1.96*hybrid_immunity_model_dose[3])
dose_results <- rbind(dose_results, cbind(or, low, high) %>% as.data.frame() %>% mutate(label="hybrid_immunity"))

dose_results <- dose_results %>% as.data.frame()
dose_results <- dose_results %>% mutate(x=rep(c("dose.1", "dose.2", "dose.3", "dose.4"), 4))
names(dose_results) <- c("point", "lb", "ub", "label", "x")  
dose_results <- dose_results %>% select(x, point, lb, ub, label)

write_csv(dose_results, here::here(paste0("results/", analysis, "/stratified-casecontrolimmunity-results-dose.csv")))


data %>% ggplot(aes(time_since_inf)) + geom_histogram(aes(y=..density..)) + scale_x_continuous(limits=c(0,1000)) + scale_y_continuous(limits=c(0,0.005))
inf_immunity %>% ggplot(aes(time_since_inf)) + geom_histogram(aes(y=..density..)) + scale_x_continuous(limits=c(0, 1000)) + scale_y_continuous(limits=c(0,0.005))
hybrid_immunity %>% ggplot(aes(time_since_inf)) + geom_histogram(aes(y=..density..)) + scale_x_continuous(limits=c(0, 1000)) + scale_y_continuous(limits=c(0,0.005))

data %>% ggplot(aes(time_since_inf, group=factor(case), fill=factor(case))) + geom_histogram(aes(y=..density..), position="identity", alpha=0.5) + scale_x_continuous(limits=c(0,1000)) + scale_y_continuous(limits=c(0,0.005))
inf_immunity %>% ggplot(aes(time_since_inf, group=factor(case), fill=factor(case))) + geom_histogram(aes(y=..density..), position="identity", alpha=0.5) + scale_x_continuous(limits=c(0,1000)) + scale_y_continuous(limits=c(0,0.005))


data %>% ggplot(aes(time_since_vacc)) + geom_histogram(aes(y=..density..)) + scale_x_continuous(limits=c(0,1000)) + scale_y_continuous(limits=c(0,0.005))
vacc_immunity %>% ggplot(aes(time_since_vacc)) + geom_histogram(aes(y=..density..)) + scale_x_continuous(limits=c(0, 1000)) + scale_y_continuous(limits=c(0,0.005))


data <- data %>% 
  mutate(time_since_inf_cut=cut(time_since_inf, breaks=c(0, 90, 182, 365, Inf), right = F)) 

levels(data$time_since_inf_cut)<-c(levels(data$time_since_inf_cut), "None") 
data$time_since_inf_cut[is.na(data$time_since_inf_cut)] <- "None"
data <- data %>% mutate(time_since_inf_cut = factor(time_since_inf_cut, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))
