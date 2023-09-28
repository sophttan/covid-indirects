d <- read_csv("survival_data/allvacc_dose_noincarcreq_priorinf_infvacc081423.csv")
d_sum <- d %>% group_by(Institution, treatment) %>% summarise(n=n())

inst2 <- read_csv("matching_data_092223/institution2.csv")
inst3 <- read_csv("matching_data_092223/institution3.csv")
inst5 <- read_csv("matching_data_092223/institution5.csv")
inst6 <- read_csv("matching_data_092223/institution6.csv")
inst7 <- read_csv("matching_data_092223/institution7.csv")
inst8 <- read_csv("matching_data_092223/institution8.csv") %>% select(!c(both_unvacc.primary, both_unvacc.secondary, group))

new_sum <- rbind(inst2, inst3) %>% 
  mutate(treatment=factor(treatment, labels=c("Control", "Treatment")))


new_sum %>% ggplot(aes(time_since_inf.primary, group=treatment, fill=treatment)) + geom_histogram(position = "identity", alpha=0.5) + 
  xlab("Time since most recent infection primary resident") + 
  ylab("Count")


new_sum %>% ggplot(aes(time_since_inf.secondary, group=treatment, fill=treatment)) + geom_histogram(position = "identity", alpha=0.5) + 
  xlab("Time since most recent infection secondary resident") + 
  ylab("Count")

new_sum %>% filter(vacc.primary>0) %>% ggplot(aes(time_since_vacc.primary, group=treatment, fill=treatment)) + geom_histogram(position = "identity", alpha=0.5) + 
  xlab("Time since most recent vaccination primary resident") + 
  ylab("Count")

new_sum %>% ggplot(aes(age.primary, group=treatment, fill=treatment)) + geom_histogram(position = "identity", alpha=0.5) + 
  xlab("Time since most recent infection primary resident") + 
  ylab("Count")

new_sum %>% ggplot(aes(age.secondary, group=treatment, fill=treatment)) + geom_histogram(position = "identity", alpha=0.5) + 
  xlab("Time since most recent infection primary resident") + 
  ylab("Count")

new_sum %>% group_by(Institution, treatment) %>% summarise(n=n())

d_sum %>% inner_join(new_sum, by=c("Institution", "treatment"), suffix = c(".old", ".new"))
