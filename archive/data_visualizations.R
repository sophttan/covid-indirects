# visualizations
d <- read_csv("matched_data.csv")

d %>% mutate(index_prior_vacc = as.factor(index_prior_vacc)) %>% group_by(index_prior_vacc, num_vacc_doses) %>% summarise(count=n()) %>% 
  group_by(index_prior_vacc) %>% summarise(num_vacc_doses=num_vacc_doses,prop=count/sum(count)) %>% 
  ggplot(aes(x=num_vacc_doses, y=prop, fill=index_prior_vacc)) + stat_summary(geom="bar", position=position_dodge(.9)) +
  scale_y_continuous(name="proportion of contacts") + 
  xlab("close contact - number of vaccine doses") +
  scale_fill_discrete(name = "index case - prior vaccination") + 
  theme(legend.position = "bottom")

d %>% mutate(index_prior_vacc = as.factor(index_prior_vacc)) %>% group_by(index_prior_vacc) %>% 
  summarise(inf_risk=mean(contact_status)) %>% 
  ggplot(aes(index_prior_vacc, inf_risk)) + stat_summary(geom="bar") +
  scale_y_continuous(name="risk of infection in close contact") + 
  xlab("index case - prior vaccination")

d %>% mutate(index_prior_vacc_doses = as.factor(index_prior_vacc_doses)) %>% group_by(index_prior_vacc_doses) %>% 
  summarise(inf_risk=mean(contact_status)) %>% 
  ggplot(aes(index_prior_vacc_doses, inf_risk)) + stat_summary(geom="bar") +
  scale_y_continuous(name="risk of infection in close contact") + 
  xlab("index case - number of vaccine doses")

d %>% mutate(num_vacc_doses=ifelse(num_vacc_doses==4, 3, num_vacc_doses)) %>% group_by(index_prior_vacc, num_vacc_doses) %>% 
  summarise(inf_risk=mean(contact_status)) %>% ungroup() %>% mutate(index_prior_vacc=as.factor(index_prior_vacc), 
                                                                    num_vacc_doses=as.factor(num_vacc_doses)) %>%
  ggplot(aes(x=num_vacc_doses, y=inf_risk, fill=index_prior_vacc)) + stat_summary(geom="bar", position=position_dodge(.9)) +
  scale_y_continuous(name="risk of infection in close contact") + 
  xlab("close contact - number of vaccine doses") +
  scale_fill_discrete(name = "index case - prior vaccination") + 
  theme(legend.position = "bottom")


d %>% mutate(index_has_vacc_or_inf=as.factor(index_has_vacc_or_inf)) %>% 
  group_by(index_has_vacc_or_inf) %>% summarise(inf_risk = mean(contact_status)) %>%
  ggplot(aes(index_has_vacc_or_inf, inf_risk)) + stat_summary(geom="bar") +
  scale_y_continuous(name="risk of infection in close contact") + 
  xlab("index case - prior vaccination or infection")

d %>% group_by(index_prior_vacc, num_vacc_doses) %>% 
  summarise(inf_risk=mean(contact_status)) %>% ungroup() %>% mutate(index_prior_vacc=as.factor(index_prior_vacc), 
                                                                    num_vacc_doses=as.factor(num_vacc_doses)) %>%
  ggplot(aes(x=num_vacc_doses, y=inf_risk, fill=index_prior_vacc)) + stat_summary(geom="bar", position=position_dodge(.9)) +
  scale_y_continuous(name="risk of infection in close contact") + 
  xlab("close contact - number of vaccine doses") +
  scale_fill_discrete(name = "index case - prior vaccination or infection") + 
  theme(legend.position = "bottom")

d %>% group_by(index_has_vacc_or_inf, num_vacc_doses) %>% 
  summarise(inf_risk=mean(contact_status)) %>% ungroup() %>% mutate(index_has_vacc_or_inf=as.factor(index_has_vacc_or_inf), 
                                                                    num_vacc_doses=as.factor(num_vacc_doses)) %>%
  ggplot(aes(x=num_vacc_doses, y=inf_risk, fill=index_has_vacc_or_inf)) + stat_summary(geom="bar", position=position_dodge(.9)) +
  scale_y_continuous(name="risk of infection in close contact") + 
  xlab("close contact - number of vaccine doses") +
  scale_fill_discrete(name = "index case - prior vaccination or infection") + 
  theme(legend.position = "bottom")
