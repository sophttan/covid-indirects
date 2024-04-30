model <- glm(case ~ time_since_vacc_cut.roommate + time_since_inf_cut.roommate + 
               has.prior.inf + num_dose_adjusted + level + age + age.roommate + risk + risk.roommate +
               factor(Institution) + factor(variant), data=matched_infvacc_roommate, family="binomial")
summary(model)

model <- clogit(case ~ time_since_vacc_cut.roommate + time_since_inf_cut.roommate + 
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


model <- clogit(case ~ has.vacc.roommate.binary*has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)
summary(model)

matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(infvacc=case_when(has.vacc.roommate.binary==0&has.prior.inf.roommate==0~"none",
                                                                                  has.vacc.roommate.binary==1&has.prior.inf.roommate==1~"both",
                                                                                  has.vacc.roommate.binary==1~"vacc",
                                                                                  has.prior.inf.roommate==1~"inf"),
                                                                infvacc=factor(infvacc, levels=c("none", "vacc", "inf", "both"))) 

model <- clogit(case ~ infvacc + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)
format_results(model)

matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(variant=case_when(last.inf.roommate<"2021-12-15"~"preomicron",
                           last.inf.roommate>="2021-12-15"~"omicron",
                           T~"") %>% factor()) %>%
  mutate(time_inf=paste0(time_since_inf_cut.roommate, variant) %>% 
           factor(levels=c("None", "[0,90)preomicron","[90,182)preomicron","[182,365)preomicron","[365,Inf)preomicron", 
                           "[0,90)omicron","[90,182)omicron","[182,365)omicron","[365,Inf)omicron")))
  

model <- clogit(case ~ time_inf + has.vacc.roommate.binary + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)
format_results(model)

