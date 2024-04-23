model <- glm(case ~ time_since_vacc_cut.roommate + time_since_inf_cut.roommate + 
               has.prior.inf + num_dose_adjusted + level + age + age.roommate + risk + risk.roommate +
               factor(Institution) + factor(variant), data=matched_infvacc_roommate, family="binomial")
summary(model)

model <- clogit(case ~ time_since_inf_cut.roommate + has.vacc.roommate.binary + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)
summary(model)

tbl_regression(model, exp=T, include = c("time_since_inf_cut.roommate"), 
               label = c("time_since_inf_cut.roommate"="Roommate: time since last infection"))


model <- clogit(case ~ dose.roommate.adjusted*time_since_vacc_cut.roommate + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + factor(variant) + strata(group), data=matched_infvacc_roommate)
summary(model)

tbl_regression(model, exp=T, include = c("time_since_vacc_cut.roommate"), 
               label = c("time_since_vacc_cut.roommate"="Roommate: time since last vaccine"))

model <- glm(case ~ time_since_vacc_cut.roommate +has.prior.inf.roommate + 
               has.prior.inf + num_dose_adjusted + level + age + age.roommate + risk + risk.roommate +
               factor(Institution) + factor(variant), data=matched_infvacc_roommate, family="binomial")
summary(model)

model <- glm(case ~ time_since_inf_cut.roommate*has.vacc.roommate.binary + 
               has.prior.inf + num_dose_adjusted + level + age + age.roommate + risk + risk.roommate +
               factor(Institution) + factor(variant), data=matched_infvacc_roommate, family="binomial")
summary(model)

model <- glm(case ~ time_since_infvacc_cut.roommate + 
               has.prior.inf + num_dose_adjusted + level + age + age.roommate + risk + risk.roommate +
               factor(Institution) + factor(variant), data=matched_infvacc_roommate, family="binomial")
summary(model)

model <- clogit(case ~ time_since_infvacc_cut.roommate + 
                  age + age.roommate + risk + risk.roommate + factor(variant) + strata(group), data=matched_infvacc_roommate)
summary(model)

tbl_regression(model, exp=T, include = c("time_since_infvacc_cut.roommate"), 
               label = c("time_since_infvacc_cut.roommate"="Roommate: time since most recent vaccine or infection"))


model <- clogit(case ~ time_since_vacc_cut.roommate*has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + factor(variant) + strata(group), data=matched_infvacc_roommate)
summary(model)

model <- clogit(case ~ time_since_inf_cut.roommate*has.vacc.roommate.binary + 
                  age + age.roommate + risk + risk.roommate + factor(variant) + strata(group), data=matched_infvacc_roommate)
summary(model)
