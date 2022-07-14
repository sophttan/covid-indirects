prior_inf_vacc_n <- final %>% group_by(index_prior_vacc, index_prior_inf) %>% summarise(count=n())
prior_inf_vacc_n$label <- c("no prior vacc or inf", "prior infection", "prior vaccination", "both prior vacc and inf")
prior_inf_vacc_n

final_with_label <- final %>% left_join(prior_inf_vacc_n %>% select(1, 2, 4)) %>% group_by(no) %>% summarise_all(first)
weeks <- final_with_label$week %>% unique()
weeks <- expand.grid(label=prior_inf_vacc_n$label, week=weeks)
pdf("D:/stan5/code_ST/CDCR-Calprotect-private/cases.pdf")
for (inst in final_with_label$Institution %>% unique()) {
  subset <- final_with_label %>% filter(Institution==inst) %>% group_by(label, week) %>% summarise(num_cases=unique(no)%>%length())
  print(subset %>% full_join(weeks) %>% replace_na(list(num_cases=0)) %>% 
    ggplot(aes(week, num_cases, color=label)) + geom_point() + geom_line() +
      scale_y_continuous("Number of index cases", limits = c(0,35)) + 
      theme(legend.title = element_blank()) + labs(title=inst))
}
dev.off()
model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + 
                 has_prior_inf + num_vacc_doses + incidence_log + Institution +  
                 (1|index_id), data=final, weights=weights, family="poisson")
model_parameters(
  model,
  vcov = merDeriv::vcov.glmerMod,
  vcov_args = list(cluster=~subclass)
)

pair_no <- final %>% left_join(prior_inf_vacc_n %>% select(1, 2, 4)) %>% group_by(Institution, label) %>% summarise(num_cases=n()) %>% spread(label, num_cases)
names(pair_no)[2:5] <- paste0("N: ", names(case_no)[2:5])
grouped_final <- final %>% left_join(prior_inf_vacc_n %>% select(1, 2, 4)) %>% 
  group_by(Institution, label) 
grouped_final %>% summarise(num_days = mean(num_days_in_contact)) %>% spread(label, num_days) %>%
  left_join(final_with_label %>% group_by(Institution) %>% summarise(num_cases=n())) %>% 
  select(Institution, num_cases, `no prior vacc or inf`, `prior infection`, `prior vaccination`, `both prior vacc and inf`) 
summary_inf_risk <- grouped_final %>% 
  summarise(inf_risk=round(mean(contact_status), 3)) %>% spread(label, inf_risk) %>%
  left_join(pair_no, "Institution") %>% 
  select(1,7,3,8,4,9,5,6,2) 
summary_inf_risk %>% arrange(desc(rowSums(.[,c(2,4,6,8)]))) %>% 
  write_csv("D:/stan5/code_ST/CDCR-Calprotect-private/infectiousness_institution_subgroup.csv")

has_more_than_10 <- grouped_final %>% group_by(Institution) %>% summarise(count=n()) %>% filter(count>=10)

model <- glm(contact_status ~ index_prior_vacc + index_prior_inf + 
               has_prior_inf + num_vacc_doses + incidence_log + Institution, data=final, weights=weights, family="poisson")
lmtest::coeftest(model, sandwich::vcovCL, cluster=~subclass)

model <- glm(contact_status ~ index_prior_vacc + index_prior_inf + index_prior_vacc*index_prior_inf+
                has_prior_inf + num_vacc_doses + incidence_log + Institution, data=final, weights=weights, family="poisson")
lmtest::coeftest(model, sandwich::vcovCL, cluster=~subclass)

model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + index_prior_vacc*index_prior_inf+
                has_prior_inf + num_vacc_doses + incidence_log + 
                 (1|index_id) + (1|Institution), data=final, weights=weights, family="poisson")



model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + 
                 has_prior_inf + num_vacc_doses + incidence_log + num_days_in_contact + Institution +
                 (1|index_id), data=final %>% filter(!Institution %in% c(13)), weights=weights, family="poisson")
model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + index_prior_vacc*index_prior_inf + 
                 has_prior_inf + num_vacc_doses + incidence_log + num_days_in_contact + Institution +
                 (1|index_id), data=final %>% filter(!Institution %in% c(13)), weights=weights, family="poisson")

model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + 
                 has_prior_inf + num_vacc_doses + incidence_log + num_days_in_contact + Institution +
                 (1|index_id), data=final %>% filter(Institution %in% has_more_than_10$Institution & Institution!=26), weights=weights, family="poisson")
model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + index_prior_vacc*index_prior_inf + 
                 has_prior_inf + num_vacc_doses + incidence_log + num_days_in_contact + Institution +
                 (1|index_id), data=final %>% filter(Institution %in% has_more_than_10$Institution & Institution!=26), weights=weights, family="poisson")

model <- glmer(contact_status ~ has_prior_inf + num_vacc_doses + incidence_log + Institution +
                 (1|index_id), data=final, weights=weights, family="poisson")
summary(model)


final <- final %>% mutate(contact_status_factor = as.factor(contact_status))
model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + 
                 has_prior_inf + num_vacc_doses + incidence_log  + 
                 (1|Institution) + (1|index_id), data=final%>% filter(Institution %in% has_more_than_10$Institution), weights=weights, family="binomial")
summary(model)
model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + index_prior_vacc*index_prior_inf+
                 has_prior_inf + num_vacc_doses + incidence_log  + 
                 Institution + (1|index_id), data=final%>% filter(Institution %in% has_more_than_10$Institution), weights=weights, family="binomial")
model <- update(model, start=getME(model, c("theta", "fixef")), control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=2e5)))
summary(model)
lmtest::coeftest(model, sandwich::vcovCL, cluster=~subclass)

model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + index_prior_vacc*index_prior_inf+
               has_prior_inf + num_vacc_doses + incidence_log + Institution + 
                 (1|index_id), data=final, weights=weights, family="binomial")
summary(model)

infections <- infections %>% left_join(infections_data %>% group_by(no) %>% summarise_all(first), c("ResidentId", "no"))
infections <- infections %>% left_join(vacc %>% select(ResidentId, num_dose, Date_offset), by=c("ResidentId", "num_dose"))
infections <- infections %>% mutate(num_dose_adjusted = ifelse(num_dose>0 & Day<Date_offset, num_dose-1, num_dose))

infections <- infections %>% rename("index_id"="ResidentId", 
                          "index_prior_vacc_doses"="num_dose_adjusted",
                          "contact_id"="contacts",
                          "contact_status"="neg_pos_contact")
infections <- infections %>% mutate(index_id = as.factor(index_id),
  index_prior_inf = ifelse(num_pos==1, 0, 1),
  index_prior_vacc = ifelse(index_prior_vacc_doses==0, 0, 1), 
  Institution = as.factor(Institution))
infections <- infections %>% mutate(index_prior_vacc_doses=ifelse(index_prior_vacc_doses>3, 3, index_prior_vacc_doses),
                          num_vacc_doses=ifelse(num_vacc_doses>3, 3, num_vacc_doses))

infections <- infections %>% mutate(index_has_vacc_or_inf=ifelse(index_prior_inf==1|index_prior_vacc==1, 1, 0))

inst_day <- infections %>% group_by(Institution, Day) %>% group_keys() %>% mutate(Institution=as.numeric(as.character(Institution)))
inst_day <- inst_day %>% mutate(incidence=vectorized_calculate_inc(Institution, Day))

infections <- infections %>% left_join(inst_day %>% mutate(Institution=as.factor(Institution)))

infections <- infections %>% mutate(incidence_log = log(incidence))
model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + 
                 has_prior_inf + num_vacc_doses + incidence_log + Institution +
                 (1|index_id), data=infections, family="poisson")
model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + index_prior_vacc*index_prior_inf+
                 has_prior_inf + num_vacc_doses + incidence_log + Institution +
                 (1|index_id), data=infections, family="poisson")



model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + 
                 has_prior_inf + num_vacc_doses + incidence_log + Institution +
                 (1|index_id), data=final, family="poisson")
model <- glmer(contact_status ~ index_prior_vacc + index_prior_inf + index_prior_vacc*index_prior_inf+
                 has_prior_inf + num_vacc_doses + incidence_log + Institution +
                 (1|index_id), data=infections, family="poisson")
