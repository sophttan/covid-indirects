# calculate facility specific incidence in the 7 days leading up to and including first days of infection
d <- read_csv("march-data/housing_inf_data_adjusted_roomtype1.csv")
d2 <- read_csv("march-data/housing_inf_data_adjusted_roomtype2.csv")
d <- d %>% bind_rows(d2)
rm(d2)
gc()
infections_total <- d %>% select(ResidentId, num_pos, Day, Institution) %>% filter(num_pos>=1) %>% 
  group_by(ResidentId, num_pos) %>% summarise_all(first) %>%
  group_by(ResidentId, num_pos)
calculate_inc <- function(institution, index_first_pos_test_date) {
  d_inst <- filter(d, Institution==institution & Day <= index_first_pos_test_date & index_first_pos_test_date - Day < 7)
  total_pop <- d_inst$ResidentId %>% unique() %>% length()
  infections_inst <- filter(infections_total, Institution==institution & Day <= index_first_pos_test_date & index_first_pos_test_date - Day < 7)
  total_inf <- infections_inst %>% nrow()
  total_inf/total_pop * 100000
}
vectorized_calculate_inc <- Vectorize(calculate_inc)
inst_day <- final %>% group_by(Institution, Day) %>% group_keys()
inst_day <- inst_day %>% mutate(incidence=vectorized_calculate_inc(Institution, Day))

final <- final %>% left_join(inst_day %>% mutate(Institution=as.factor(Institution)))

final <- final %>% mutate(incidence_log = log(incidence))

# add in dates of most recent vaccination
final <- final %>% left_join(vacc %>% select(ResidentId, num_dose, Date), 
                             by=c("index_id"="ResidentId", "index_prior_vacc_doses"="num_dose")) %>% rename("index_vacc_date"="Date")
final <- final %>% left_join(vacc %>% select(ResidentId, num_dose, Date),
                             by=c("contact_id"="ResidentId", "num_vacc_doses"="num_dose")) %>% rename("contact_date"="Date")

final %>% select(index_id, Day, index_prior_vacc_doses, index_vacc_date, contact_id, num_vacc_doses, contact_date)

# add in dates of most recent infection
final <- final %>% mutate(previous_inf = num_pos.y-1) %>% left_join(infections %>% select(!Institution) %>% rename("Day_prior_inf"="Day"), c("index_id"="ResidentId", "previous_inf"="num_pos"))

# estimate weeks since vaccination
final <- final %>% mutate(index_weeks_since_vacc = as.numeric(difftime(Day, index_vacc_date, units="weeks")), 
                          contact_weeks_since_vacc = as.numeric(difftime(Day, contact_date, units="weeks")))

# final <- final %>% mutate(index_weeks_since_vacc = ifelse(is.na(index_weeks_since_vacc), as.numeric(difftime(Day, "2020-12-01", units="weeks")), index_weeks_since_vacc),
#                           contact_weeks_since_vacc = ifelse(is.na(contact_weeks_since_vacc), as.numeric(difftime(Day, "2020-12-01", units="weeks")), contact_weeks_since_vacc))

final <- final %>% mutate(index_weeks_since_vacc_log = log(index_weeks_since_vacc),
                          contact_weeks_since_vacc_log = log(contact_weeks_since_vacc))
final <- final %>% mutate(index_id=as.factor(index_id))

final <- final %>% mutate(contact_has_vacc_or_inf = ifelse(num_vacc_doses>0|has_prior_inf==1, 1, 0))

library(lme4)
model <- glmer(contact_status ~ index_prior_vacc + 
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")
model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + #contact_has_vacc_or_inf + index_prior_vacc*contact_has_vacc_or_inf +
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")
model <- glmer(contact_status ~ index_has_vacc_or_inf +
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")
model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + index_prior_vacc*index_prior_inf +
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")
model <- glmer(contact_status ~ index_prior_vacc_doses + index_prior_inf + index_prior_vacc_doses*index_prior_inf+
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")


model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + index_prior_vacc*index_prior_inf+
                 contact_has_vacc_or_inf + 
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")

model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + 
                 has_prior_inf + num_vacc_doses + #incidence_log + 
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")
model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + 
                 has_prior_inf + num_vacc_doses + index_prior_vacc*index_prior_inf + 
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")


model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + 
                 has_prior_inf + num_vacc_doses + incidence_log  + (1|Institution) +
                 (1|index_id), data=final, weights=weights, family="poisson")

summary(model)

final <- final %>% mutate(contact_has_vacc =ifelse(num_vacc_doses>=1, 1, 0))
model <- glmer(contact_status ~  index_prior_vacc + index_prior_inf + 
                 has_prior_inf + contact_has_vacc + incidence_log  + (1|Institution) + 
                 (1|index_id), data=final, weights=weights, family="poisson")

summary(model)

model <- glmer(contact_status ~ index_has_vacc_or_inf + 
                 has_prior_inf + num_vacc_doses + incidence_log  + (1|Institution) + 
                 (1|index_id), data=final, weights=weights, family="poisson")

summary(model)

model <- glmer(contact_status ~ index_has_vacc_or_inf + 
                 has_prior_inf + contact_has_vacc + incidence_log  + (1|Institution) + 
                 (1|index_id), data=final, weights=weights, family="poisson")

summary(model)





model <- glmer(contact_status ~ index_prior_vacc*index_weeks_since_vacc_log + index_prior_inf + 
                 has_prior_inf + num_vacc_doses + incidence_log + #contact_weeks_since_vacc_log + 
                # Institution + (1|index_id), data=final, weights=weights, family="poisson")
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")

summary(model)



library(pscl)
library(sandwich)
fit <- zeroinfl(contact_status ~ index_prior_vacc + index_prior_inf + has_prior_inf + num_vacc_doses + incidence_log,
                data=final, weights=weights)
fit <- gam(list(contact_status ~ index_prior_vacc, #index_prior_inf + has_prior_inf + num_vacc_doses + incidence_log +
                 ~ index_prior_vacc),
                data=final, weights=weights, family=ziplss, method="REML")

fit <- gam(list(contact_status ~ index_prior_vacc + #index_prior_inf + has_prior_inf + 
                  num_vacc_doses + incidence_log, ~index_prior_vacc),
                  #s(index_id) + s(Institution), ~ index_prior_vacc),
           data=final, weights=weights, family=ziplss, method="REML")

summary(fit)


final <- final %>% left_join(demo_wide %>% mutate(ResidentId=as.factor(ResidentId)), by=c("index_id"="ResidentId")) 
final$age <- as.numeric(final$Day %>% format("%Y")) - as.numeric(final$BirthYear)

model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + 
                 has_prior_inf + num_vacc_doses + incidence_log + age + 
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")

summary(model)


model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + index_prior_vacc*index_prior_inf+
                 num_vacc_doses+has_prior_inf+incidence_log+
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")

summary(model)


final <- final %>% mutate(index_weeks_since_inf = as.numeric(difftime(Day, Day_prior_inf, units="weeks")))
final <- final %>% ungroup() %>% rowwise() %>% mutate(index_weeks_since_inf_vacc = min(index_weeks_since_inf, index_weeks_since_vacc, na.rm=T))
final <- final %>% mutate(index_weeks_since_inf_vacc = ifelse(index_weeks_since_inf_vacc %>% is.infinite(), NA, index_weeks_since_inf_vacc))

final <- final %>% mutate(index_weeks_since_inf_log = log(index_weeks_since_inf),
                          index_weeks_since_vacc_log = log(index_weeks_since_vacc),
                          index_weeks_since_inf_vacc_log = log(index_weeks_since_inf_vacc))

model <- glmer(contact_status ~ index_weeks_since_inf_log + index_weeks_since_vacc_log + 
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")
model <- glmer(contact_status ~ index_weeks_since_inf_vacc_log + 
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")

model <- glmer(contact_status ~ index_weeks_since_inf_log + index_prior_vacc_doses*index_weeks_since_vacc_log +
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")

model <- glmer(contact_status ~ index_weeks_since_inf_vacc_log + num_vacc_doses+has_prior_inf+
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")

model <- glmer(contact_status ~ num_vacc_doses*contact_weeks_since_vacc_log+has_prior_inf+
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")

summary(glmer(contact_status ~ index_weeks_since_inf_vacc_log + num_vacc_doses + has_prior_inf + 
                (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson"))


final <- final %>% replace_na(list(index_weeks_since_inf=104, index_weeks_since_vacc=104, index_weeks_since_inf_vacc=104))
final <- final %>% mutate(index_weeks_since_inf_log = log(index_weeks_since_inf),
                          index_weeks_since_vacc_log = log(index_weeks_since_vacc),
                          index_weeks_since_inf_vacc_log = log(index_weeks_since_inf_vacc))
model <- glmer(contact_status ~ index_weeks_since_inf_log + index_weeks_since_vacc_log + 
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")
model <- glmer(contact_status ~ index_weeks_since_inf_vacc_log + 
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")
summary(glmer(contact_status ~ index_has_vacc_or_inf+ index_weeks_since_inf_vacc_log + 
                (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson"))
summary(glmer(contact_status ~ index_prior_vacc + index_prior_inf + index_weeks_since_vacc_log + index_weeks_since_inf_log+
                (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson"))
summary(glmer(contact_status ~ index_prior_vacc + index_prior_inf + index_prior_vacc*index_prior_inf+ index_weeks_since_vacc_log + index_weeks_since_inf_log+
                (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson"))

final <- final %>% mutate(prior_vacc_inf_categorical = as.factor(case_when(index_prior_inf==0&index_prior_vacc==0~0,
                                                                  index_prior_vacc==1&index_prior_inf==0~1,
                                                                  index_prior_vacc==0&index_prior_inf==1~2,
                                                                  index_prior_vacc==1&index_prior_inf==1~3)))
summary(glmer(contact_status ~ prior_vacc_inf_categorical + num_vacc_doses + has_prior_inf +incidence_log + 
                (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson"))
