d <- read_csv("survival_data/allvacc_dose_infvacc072023.csv")
d <- d %>% mutate(InstBuild=as.factor(InstBuild), subclass=as.factor(subclass))
d <- d %>% mutate(vacc.primary.binary=(vacc.primary>0)%>%as.numeric())
d <- d %>% rowwise() %>% 
  mutate(time_since_inf_vacc.primary = min(time_since_inf.primary, time_since_vacc.primary, na.rm=T),
         time_since_inf_vacc.secondary = min(time_since_inf.secondary, time_since_vacc.secondary, na.rm=T))

d2 <- read_csv("survival_data/allvacc_dose_noincarcreq_priorinf_infvacc071223.csv")
d2 <- d2 %>% mutate(InstBuild=as.factor(InstBuild), subclass=as.factor(subclass))
d2 <- d2 %>% mutate(vacc.primary.binary=(vacc.primary>0)%>%as.numeric())
d2 <- d2 %>% rowwise() %>% 
  mutate(time_since_inf_vacc.primary = min(time_since_inf.primary, time_since_vacc.primary, na.rm=T),
         time_since_inf_vacc.secondary = min(time_since_inf.secondary, time_since_vacc.secondary, na.rm=T))



d%>%ggplot(aes(vacc.primary, y=..prop.., fill="Secondary"), alpha=0.5) + 
  geom_bar() +
  geom_bar(d=d2, aes(vacc.primary, y=..prop.., fill="Main"), alpha=0.5) +
  scale_x_continuous("Number of vaccine doses in primary resident", breaks=0:6) + 
  scale_y_continuous("Proportion") +
  scale_fill_discrete("Analysis")

d%>%ggplot(aes(vacc.secondary, y=..prop.., fill="Secondary"), alpha=0.5) + 
  geom_bar() +
  geom_bar(d=d2, aes(vacc.secondary, y=..prop.., fill="Main"), alpha=0.5) +
  scale_x_continuous("Number of vaccine doses in secondary resident", breaks=0:6) + 
  scale_y_continuous("Proportion") +
  scale_fill_discrete("Analysis")

t.test(d$vacc.primary, d2$vacc.primary) # no significant difference
t.test(d$vacc.secondary, d2$vacc.secondary) # significant difference - residents in main analysis have larger mean 

# sample size differences
d %>% group_by(treatment, inf.secondary) %>% summarise(n=n(), obs_time=sum(survival_time))
d2 %>% group_by(treatment, inf.secondary) %>% summarise(n=n(), obs_time=sum(survival_time))

d %>% group_by(vacc.primary.binary, inf.primary) %>% summarise(n=n(), obs_time=sum(survival_time))
d2 %>% group_by(vacc.primary.binary, inf.primary) %>% summarise(n=n(), obs_time=sum(survival_time))


d%>%filter(vacc.primary>0)%>%ggplot(aes(time_since_vacc.primary, fill="Secondary")) +
  geom_histogram(aes(y=..density..), alpha=0.5) + 
  geom_histogram(d=d2%>%filter(vacc.primary>0), aes(time_since_vacc.primary, y=..density.., fill="Main"), alpha=0.5) + 
  scale_x_continuous("Time since last vaccination") + 
  scale_y_continuous("Density") + 
  scale_fill_discrete(name="Vaccinated primary residents")
t.test((d%>%filter(vacc.primary>0))$time_since_vacc.primary, (d2%>%filter(vacc.primary>0))$time_since_vacc.primary)

d%>%filter(treatment>0)%>%ggplot(aes(time_since_vacc.secondary, fill="Secondary")) +
  geom_histogram(aes(y=..density..), alpha=0.5) + 
  geom_histogram(d=d2%>%filter(treatment>0), aes(time_since_vacc.secondary, y=..density.., fill="Main"), alpha=0.5) + 
  scale_x_continuous("Time since last vaccination") + 
  scale_y_continuous("Density") + 
  scale_fill_discrete(name="Vaccinated secondary residents")
t.test((d%>%filter(treatment>0))$time_since_vacc.secondary, (d2%>%filter(treatment>0))$time_since_vacc.secondary)

d%>%filter(inf.primary>0)%>%ggplot(aes(time_since_inf.primary, fill="Secondary")) +
  geom_histogram(aes(y=..density..), alpha=0.5) + 
  geom_histogram(d=d2%>%filter(inf.primary>0), aes(time_since_inf.primary, y=..density.., fill="Main"), alpha=0.5) + 
  scale_x_continuous("Time since last infection") + 
  scale_y_continuous("Density") + 
  scale_fill_discrete(name="Previously infected primary residents")
t.test((d%>%filter(inf.primary>0))$time_since_inf.primary, (d2%>%filter(inf.primary>0))$time_since_inf.primary)

d%>%filter(inf.secondary>0)%>%ggplot(aes(time_since_inf.secondary, fill="Secondary")) +
  geom_histogram(aes(y=..density..), alpha=0.5) + 
  geom_histogram(d=d2%>%filter(inf.secondary>0), aes(time_since_inf.secondary, y=..density.., fill="Main"), alpha=0.5) + 
  scale_x_continuous("Time since last infection") + 
  scale_y_continuous("Density") + 
  scale_fill_discrete(name="Previously infected secondary residents")
t.test((d%>%filter(inf.secondary>0))$time_since_inf.secondary, (d2%>%filter(inf.secondary>0))$time_since_inf.secondary)

lm(time_since_vacc.primary~time_since_inf.primary, data=d) %>% summary()

d %>% ggplot(aes(time_since_vacc.primary, time_since_inf.primary)) + geom_point() + 
  scale_x_continuous("Months since most recent vaccination") + 
  scale_y_continuous("Months since most recent confirmed infection")

d%>%ggplot(aes(time_since_inf.primary)) + geom_histogram() + scale_x_continuous("Months since most recent infection") + scale_y_continuous("Count")
d%>%filter(vacc.primary>0) %>% ggplot(aes(time_since_vacc.primary)) + geom_histogram() + scale_x_continuous("Months since most recent vaccination") + scale_y_continuous("Count")
d%>%ggplot(aes(time_since_inf_vacc.primary)) + geom_histogram() + scale_x_continuous("Months since most recent infection or vaccination") + scale_y_continuous("Count")

d %>% ggplot(aes(time_since_inf.secondary, group=as.factor(treatment), fill=as.factor(treatment))) + 
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5) + 
  scale_x_continuous("Time since most recent infection in secondary resident") + 
  scale_y_continuous("Density") +
  scale_fill_discrete("Treatment (vaccination)")

d %>% ggplot(aes(time_since_vacc.secondary, group=as.factor(inf.secondary), fill=as.factor(inf.secondary))) + 
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5) + 
  geom_histogram(d=d2, aes(y=..density.., fill="Main"), position="identity", alpha=0.3) + 
  scale_x_continuous("Time since most recent vaccination in secondary resident") + 
  scale_y_continuous("Density") +
  scale_fill_discrete("Prior infection")
t.test((d%>%filter(inf.secondary==0))$time_since_vacc.secondary, (d%>%filter(inf.secondary==1))$time_since_vacc.secondary)


d%>%ggplot(aes(time_since_inf_vacc.primary, fill="Secondary")) +
  geom_histogram(aes(y=..density..), alpha=0.5) + 
  geom_histogram(d=d2, aes(time_since_inf_vacc.primary, y=..density.., fill="Main"), alpha=0.5) + 
  scale_x_continuous("Time since most recent vaccination or infection in primary residents") + 
  scale_y_continuous("Density") + 
  scale_fill_discrete(name="Analysis")
t.test(d$time_since_inf_vacc.primary, d2$time_since_inf_vacc.primary)



d <- d %>% mutate(vacc_recent.primary = time_since_inf.primary > time_since_vacc.primary | (inf.primary==0 & vacc.primary>0),
                  vacc_recent.secondary = time_since_inf.secondary > time_since_vacc.secondary | (inf.secondary==0 & vacc.secondary>0)) %>% 
  replace_na(list(vacc_recent.primary=F, vacc_recent.secondary=F))
d2 <- d2 %>% mutate(vacc_recent.primary = time_since_inf.primary > time_since_vacc.primary | (inf.primary==0 & vacc.primaryhttp://127.0.0.1:14991/graphics/plot_zoom_png?width=480&height=353>0),
                  vacc_recent.secondary = time_since_inf.secondary > time_since_vacc.secondary | (inf.secondary==0 & vacc.secondary>0)) %>% 
  replace_na(list(vacc_recent.primary=F, vacc_recent.secondary=F))

d %>% filter(vacc.primary>0&inf.primary==1) %>% group_by(vacc_recent.primary) %>% summarise(prop=n()/nrow(.))
d2 %>% filter(vacc.primary>0&inf.primary==1) %>% group_by(vacc_recent.primary) %>% summarise(prop=n()/nrow(.))
d %>% filter(vacc.secondary>0&inf.secondary==1) %>% group_by(vacc_recent.secondary) %>% summarise(prop=n()/nrow(.))
d2 %>% filter(vacc.secondary>0&inf.secondary==1) %>% group_by(vacc_recent.secondary) %>% summarise(prop=n()/nrow(.))
