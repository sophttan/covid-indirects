model <- glm(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + age + age.roommate + risk + risk.roommate, data=matched_infvacc_roommate,  family = "binomial")
summary(model)

dose_model <- glm(case ~ dose.roommate.adjusted + has.prior.inf.roommate + 
                    age + age.roommate + risk + risk.roommate, data=matched_infvacc_roommate, family="binomial")

dose_model <- (summary(dose_model)$coefficients)[2,]
or <- exp(dose_model[1]*1:4)
low <- exp(dose_model[1]*1:4-1.96*dose_model[2])
high <- exp(dose_model[1]*1:4+1.96*dose_model[2])
dose_results <- cbind(or, low, high)
rownames(dose_results) <- c("dose.1", "dose.2", "dose.3", "dose.4")

dose_results <- ((1-dose_results)*100) %>% as.data.frame()
dose_results <- dose_results %>% mutate(x=rownames(dose_results))
names(dose_results) <- c("point", "lb", "ub", "x")

inf_model <- glm(case ~ time_since_inf_cut.roommate + has.vacc.roommate.binary + 
                   age + age.roommate + risk + risk.roommate, data=matched_infvacc_roommate, family="binomial")

vacc_model <- glm(case ~ time_since_vacc_cut.roommate + has.prior.inf.roommate + 
                    age + age.roommate + risk + risk.roommate, data=matched_infvacc_roommate, family="binomial")

infvacc_model <- glm(case ~ time_since_infvacc_cut.roommate + 
                       age + age.roommate + risk + risk.roommate, data=matched_infvacc_roommate, family="binomial")

rbind(format_results(model)[2:3,], dose_results, format_results(inf_model)[2:5,], format_results(vacc_model)[2:5,], format_results(infvacc_model)[2:5,]) %>%
  write_csv(here::here("results/logistic/full_results.csv"))
