d <- vacc_infections_removed_extra_inf %>% filter(Day >= "2020-12-01")
pop <- d %>% group_keys() %>% nrow()

vacc <- vacc_infections_removed_extra_inf %>% filter(!is.na(Vaccine))
vacc <- vacc %>% mutate(booster_add_dose = ifelse(num_dose>full_vacc, 1, 0))
vacc <- vacc %>% mutate(Month=as.Date(format(Day, "%Y-%m-01")))
any_vacc <- vacc %>% filter(num_dose==1) %>% group_by(Month) %>% summarise(any=n()) %>% mutate(any_total = cumsum(any))
full_vacc <- vacc %>% filter(num_dose==full_vacc) %>% group_by(Month) %>% summarise(full=n()) %>% mutate(full_total=cumsum(full))
boosted <- vacc %>% filter(num_dose > full_vacc) %>% distinct(ResidentId, .keep_all = T) %>% group_by(Month) %>% summarise(boost=n()) %>% mutate(boosted_total=cumsum(boost))
boosted2 <- vacc %>% filter(num_dose - full_vacc > 1) %>% distinct(ResidentId, .keep_all = T) %>% group_by(Month) %>% summarise(boost2=n()) %>% mutate(boosted_total2=cumsum(boost2))

full_vacc_table <- any_vacc %>% full_join(full_vacc) %>% full_join(boosted) %>% full_join(boosted2)
#full_vacc_table <- full_vacc_table %>% full_join(dates) %>% replace_na(list(any_total=0, full_total=0, boosted_total=0))

full_vacc_table <- full_vacc_table %>% mutate(any_total=any_total/pop*100, 
                                              full_total=full_total/pop*100,
                                              boosted_total=boosted_total/pop*100, 
                                              boosted_total2=boosted_total2/pop*100)


p4 <- full_vacc_table %>% ggplot(aes(Month)) + 
  geom_line(aes(y=any_total, color="At least 1 dose")) + 
  geom_line(aes(y=full_total, color="Completed primary series")) +
  geom_line(aes(y=boosted_total, color="Received 1 booster dose")) + 
  geom_line(aes(y=boosted_total2, color="Received 2+ booster doses")) + 
  scale_y_continuous("Cumulative vaccination (%)", limits=c(0,100), expand = c(0,0)) + 
  scale_x_date("Time", breaks="month", date_labels = "%Y-%m") + 
  scale_color_brewer(palette="Dark2", direction=-1)+
  guides(color=guide_legend(title="Vaccine status")) +
  theme(panel.background = element_blank(), 
        legend.title=element_blank(),
        legend.key=element_blank(),
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line(), 
        axis.text.x = element_text(angle=90))
p4


all_inf <- vacc_infections_removed_extra_inf %>% group_by(ResidentId, num_pos) %>% filter(num_pos>=1) %>% summarise_all(first)
p1 <- all_inf %>% mutate(Month=as.Date(format(Day, "%Y-%m-01"))) %>% group_by(Month) %>% summarise(cases=n()) %>%
  ggplot(aes(Month, cases)) + 
  geom_line() + 
  scale_x_date("Time", breaks="month", expand=c(0.01,0), date_labels = "%Y-%m") + 
  scale_y_continuous("Monthly SARS-CoV-2\ninfections", expand = c(0,0)) +
  theme(panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle=90))

p1
all_inf %>% filter(Day >= "2021-12-15") %>% nrow()
