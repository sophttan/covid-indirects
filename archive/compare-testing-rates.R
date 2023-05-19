testing <- read_csv("testing_vacc_clean.csv")

testing <- testing %>% filter(ResidentId %in% residents) %>% 
  group_by(ResidentId) %>% 
  mutate(first_dose=if_else(all(num_dose==0), as.Date(NA), first(Day[num_dose==1]))) %>%
  filter(Day >= "2021-12-15")


testing <- testing %>% left_join(duration%>%select(ResidentId, last)%>%mutate(last=last+1))
testing_novacc <- testing %>% filter(num_dose==0) %>% filter(!Result%>%is.na())
testing_vacc <- testing %>% filter(num_dose>0) %>% filter(!Result%>%is.na())

testing_novacc <- testing_novacc %>% select(ResidentId, Day, first_dose, last) %>% 
  mutate(time=ifelse(first_dose%>%is.na(), 
                     as.numeric(difftime(last,"2021-12-15"))+1, 
                     as.numeric(difftime(first_dose,"2021-12-15")))) %>% 
  summarise(rate_testing=n()/first(time)*30)

testing_vacc <- testing_vacc %>% select(ResidentId, Day, first_dose, last) %>% 
  mutate(time=ifelse(first_dose<"2021-12-15", 
                     as.numeric(difftime(last,"2021-12-15"))+1, 
                     as.numeric(difftime("2022-12-15",first_dose))+1)) %>% 
  summarise(rate_testing=n()/first(time)*30) 

ggplot() + geom_histogram(d=testing_novacc, aes(rate_testing, y=..density.., fill="Unvaccinated"), alpha=0.7) + 
  geom_histogram(d=testing_vacc, aes(rate_testing, y=..density.., fill="Vaccinated"), alpha=0.7) + 
  scale_x_continuous("Tests per 30 days during Omicron residence", limits=c(0, 15)) + scale_y_continuous("Density") + 
  theme(legend.title = element_blank())


few_tests <- omicron_testing%>%
  distinct(Test)%>%filter(n()<=10) 

few_tests%>%filter(ResidentId %in% (few_tests$ResidentId%>%unique())[seq(1, 1000, 100)])%>% 
  ungroup() %>% mutate(label=match(ResidentId, unique(ResidentId))) %>%
  ggplot(aes(Test, label, group=as.factor(ResidentId), color=as.factor(ResidentId))) + 
  geom_point() + geom_line() + 
  scale_color_discrete("ResidentId") + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

many_tests <- omicron_testing%>%group_by(ResidentId)%>% 
  distinct(Test)%>%filter(n()>=10) 

many_tests%>%ggplot(aes(Test)) + geom_histogram()

many_tests%>%filter(ResidentId %in% (many_tests$ResidentId%>%unique())[seq(1, 1000, 100)])%>% 
  ungroup() %>% mutate(label=match(ResidentId, unique(ResidentId))) %>%
  ggplot(aes(Test, label, group=as.factor(ResidentId), color=as.factor(ResidentId))) + 
  geom_point() + geom_line() + 
  scale_color_discrete("ResidentId") + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())