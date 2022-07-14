
inst_day <- read_csv("incidence_final.csv")
summary_housing <- read_csv("housing_duration.csv")
infections <- infections %>% left_join(summary_housing, by=c("contact_id"="ResidentId"))
infections_contact <- infections %>% filter(first.y<="2020-03-31")
infections_contact %>% group_by(no)

mult_contacts <- infections_contact %>% group_by(no) %>% filter(n()>1) 
set.seed(42)
mult_contacts <- mult_contacts[sample(1:nrow(mult_contacts)),]
mult_contacts_single <- mult_contacts %>% group_by(no) %>% summarise_all(first)
infections_unique <- infections_contact %>% group_by(no) %>% filter(n()==1) %>% summarise_all(first)
infections_unique <- infections_unique %>% rbind(mult_contacts_single)
infections_unique <- infections_unique %>% mutate(treatment = as.factor(ifelse(index_prior_vacc_doses==0, 1, 0)))

# inf_omicron_subset2 <- inf_omicron_subset %>% left_join(summary_housing, by=c("contact_id"="ResidentId"))
# (inf_omicron_subset2 %>% filter(first.y <= "2020-03-31"))$duration.y %>% hist()
# 
# inf_omicron_subset2 <- inf_omicron_subset2 %>% filter(first.y <= "2020-03-31") 
day_dist <- 30
distance_matrix <- dist(as.matrix(infections_unique%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()
m <- matchit(treatment ~ Day + Institution, data = infections_unique,
             method = "nearest", exact = "Institution", caliper = c(30),std.caliper = F,
             distance = distance_matrix, ratio=10, replace = F)
mout <- match.data(m)

mout <- mout %>% select(!c(Date_offset)) %>% 
  left_join(vacc %>% select(ResidentId, num_dose, Date_offset), 
            by=c("contact_id"="ResidentId", "num_vacc_doses"="num_dose")) %>% 
  mutate(num_dose_adjusted = ifelse(num_vacc_doses>0 & Day<Date_offset, num_vacc_doses-1, num_vacc_doses))
mout %>% group_by(num_dose_adjusted) %>% summarise(count=n(), risk=mean(contact_status))

mout <- mout %>% mutate(contact_vacc = ifelse(num_dose_adjusted>0, 1, 0))
model <- glm(contact_status ~ index_prior_vacc + index_prior_inf +
               contact_vacc+has_prior_inf+contact_vacc*has_prior_inf+incidence_log+factor(Institution), data=mout, weights=weights, family="poisson")
model <- coef_test(model, vcov = "CR2", cluster = mout$subclass) 
model

model <- glm(contact_status ~ index_prior_vacc + index_prior_inf +
               num_dose_adjusted+has_prior_inf+incidence_log+factor(Institution), data=mout, weights=weights, family="poisson")
model <- coef_test(model, vcov = "CR2", cluster = mout$subclass) 
model

model <- glm(contact_status ~ index_prior_vacc + index_prior_inf +
               num_dose_adjusted+has_prior_inf+num_dose_adjusted*has_prior_inf+incidence_log+factor(Institution), data=mout, weights=weights, family="poisson")
model <- coef_test(model, vcov = "CR2", cluster = mout$subclass) 
model
