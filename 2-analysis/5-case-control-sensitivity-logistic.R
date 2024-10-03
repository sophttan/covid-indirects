# Sophia Tan
# use logistic regression for main model results instead of conditional logistic regression

source(here::here("config.R"))

data <- read_csv("D:/CCHCS_premium/st/indirects/case_control_postmatchprocessing091224.csv")
data <- data %>% mutate(time_since_inf_cut.roommate = factor(time_since_inf_cut.roommate, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))
data <- data %>% mutate(time_since_vacc_cut.roommate = factor(time_since_vacc_cut.roommate, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))
data <- data %>% mutate(time_since_infvacc_cut.roommate = factor(time_since_infvacc_cut.roommate, levels=c("None","[0,90)","[90,182)","[182,365)","[365,Inf)")))

# function to clean results 
format_results <- function(model) {
  res <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())
  
  res <- res %>% mutate(x=rownames(res))
  names(res) <- c("point", "lb", "ub", "x")
  
  res %>% select(x, point, lb, ub)
}

binary_model <- glm(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + 
                      num_dose_adjusted + has.prior.inf + 
                      age + age.roommate + risk + risk.roommate + 
                      factor(Institution) +  factor(BuildingId) + factor(level), 
                    data=data,  family = "binomial")


dose_model_fit <- glm(case ~ dose.roommate.adjusted + has.prior.inf.roommate + 
                    num_dose_adjusted + has.prior.inf + 
                    age + age.roommate + risk + risk.roommate +
                    factor(Institution) +  factor(BuildingId) + factor(level), 
                  data=data, family="binomial")

dose_model <- (summary(dose_model_fit)$coefficients)[2,]
or <- exp(dose_model[1]*1:4)
low <- exp(dose_model[1]*1:4-1.96*dose_model[2])
high <- exp(dose_model[1]*1:4+1.96*dose_model[2])
dose_results <- cbind(or, low, high)
rownames(dose_results) <- c("dose.1", "dose.2", "dose.3", "dose.4")

dose_results <- dose_results %>% as.data.frame()
dose_results <- dose_results %>% mutate(x=rownames(dose_results))
names(dose_results) <- c("point", "lb", "ub", "x")


inf_model <- glm(case ~ time_since_inf_cut.roommate + has.vacc.roommate.binary + 
                   num_dose_adjusted + has.prior.inf + 
                   age + age.roommate + risk + risk.roommate + 
                   factor(Institution) +  factor(BuildingId) + factor(level), 
                 data=data, family="binomial")

vacc_model <- glm(case ~ time_since_vacc_cut.roommate + has.prior.inf.roommate + 
                    num_dose_adjusted + has.prior.inf + 
                    age + age.roommate + risk + risk.roommate +                     
                    factor(Institution) +  factor(BuildingId) + factor(level), 
                  data=data, family="binomial")

infvacc_model <- glm(case ~ time_since_infvacc_cut.roommate + 
                       num_dose_adjusted + has.prior.inf + 
                       age + age.roommate + risk + risk.roommate +
                       factor(Institution) +  factor(BuildingId) + factor(level), 
                     data=data, family="binomial")

rbind(format_results(binary_model)[2:3,], dose_results, format_results(inf_model)[2:5,], format_results(vacc_model)[2:5,], format_results(infvacc_model)[2:5,]) %>%
  write_csv(here::here("results/logistic/full_results.csv"))


library(sandwich)
library(lmtest)

coef_binary_robust <- coeftest(binary_model, vcov. = vcovCL(binary_model, cluster = data$ResidentId))

coef_dose <- coeftest(dose_model_fit, vcov. = vcovCL(dose_model_fit, cluster = data$ResidentId))
coef_dose_robust <- coef_dose[2,]
or <- exp(coef_dose_robust[1]*1:4)
low <- exp(coef_dose_robust[1]*1:4-1.96*coef_dose_robust[2])
high <- exp(coef_dose_robust[1]*1:4+1.96*coef_dose_robust[2])
dose_robust <- cbind(or, low, high)
rownames(dose_robust) <- c("dose.1", "dose.2", "dose.3", "dose.4")

dose_robust <- dose_robust %>% as.data.frame()
dose_robust <- dose_robust %>% mutate(x=rownames(dose_robust))
names(dose_robust) <- c("point", "lb", "ub", "x")

coef_inf_robust <- coeftest(inf_model, vcov. = vcovCL(inf_model, cluster = data$ResidentId))
coef_vacc_robust <- coeftest(vacc_model, vcov. = vcovCL(vacc_model, cluster = data$ResidentId))
coef_infvacc_robust <- coeftest(infvacc_model, vcov. = vcovCL(infvacc_model, cluster = data$ResidentId))

rbind(format_results(coef_binary_robust)[2:3,], dose_robust, format_results(inf_model)[2:5,], format_results(vacc_model)[2:5,], format_results(infvacc_model)[2:5,]) %>%
  write_csv(here::here("results/logistic/full_results_robust.csv"))


