inf <- read_csv("D:/CCHCS_premium/st/cleaned-data/infection_data022624.csv")
testing <- read_csv("D:/CCHCS_premium/st/cleaned-data/complete_testing_data022624.csv") 


# overlay testing and infection data
inf <- inf %>% 
  mutate(week=difftime(CollectionDate, as.Date("2020-03-01"), units="weeks")%>%as.numeric()%>%round()) 

inf_plot_group <- inf %>% group_by(week) %>% 
  summarise(CollectionDate=min(CollectionDate), inf=n()) %>% 
  rename("Day"="CollectionDate")

tests_plot <- testing %>% 
  mutate(week=difftime(Day, as.Date("2020-03-01"), units="weeks")%>%as.numeric()%>%round()) 

tests_plot_group <- tests_plot %>% group_by(week) %>% 
  summarise(Day=min(Day), resident_tests=n())

colors <- c(RColorBrewer::brewer.pal(3, "Paired")[2], RColorBrewer::brewer.pal(8, "Dark2")[8])
inf <- inf_plot_group %>% 
  ggplot(aes(as.POSIXct(Day), inf)) + 
  geom_line(aes(color="Infections")) + 
  geom_line(data = tests_plot_group, aes(y=resident_tests/20, color="Tests")) +
  scale_y_continuous(name="Total weekly infections", 
                     expand = expansion(mult=c(0, 0.01)),
                     sec.axis = sec_axis(~.*20, name="Total weekly tests", breaks=seq(0, 100000, 25000))) + 
  scale_x_datetime("Time", date_labels = "%b %y", 
                   date_breaks = "2 month", expand = c(0.01,0.01), limits = as.POSIXct(c("2020-03-01", "2023-01-01"))) + 
  scale_color_manual(values=colors) + 
  labs(subtitle = "A") + 
  theme(panel.border = element_rect(fill=NA),
        axis.line.y.left = element_line(colour = colors[1]),
        axis.ticks.y.left =  element_line(colour = colors[1]),
        axis.text.y.left = element_text(color = colors[1]),
        axis.title.y.left = element_text(color=colors[1]),
        axis.line.y.right = element_line(color=colors[2]),
        axis.ticks.y.right =  element_line(colour = colors[2]),
        axis.text.y.right = element_text(color = colors[2]),
        axis.title.y.right = element_text(color=colors[2]),
        axis.text.x = element_text(angle=90),
        legend.position="none",
        panel.background = element_blank(),
        text=element_text(size=12, family="sans")) 
inf
ggsave("D:/CCHCS_premium/st/covid-indirects/figures/inf_test.jpg", width=10, height=5, dpi=300)


vaccines <- read_csv("D:/CCHCS_premium/st/leaky/cleaned-data/complete_vaccine_data121523.csv") %>% filter(Date<="2023-01-01")
vaccines
vacc_summary <- vaccines %>% group_by(Date) %>%
  summarise(any=sum(num_dose==1),
            full=sum(num_dose==full_vacc),
            one=sum(num_dose-full_vacc==1),
            two=sum(num_dose-full_vacc>1))

vacc_summary <- vacc_summary %>% full_join(data.frame(Date=seq(as.Date("2020-12-01"),as.Date("2023-01-01"),1))) %>% arrange(Date) %>%
  replace_na(list(any=0, full=0, one=0, two=0))

vacc_summary <- vacc_summary %>% mutate(any_cum=cumsum(any)/177319*100,
                                        full_cum=cumsum(full)/177319*100,
                                        one_cum=cumsum(one)/177319*100,
                                        two_cum=cumsum(two)/177319*100)

colors <- RColorBrewer::brewer.pal(9, "Purples")[c(9, 6, 4, 3)]
vacc <- vacc_summary %>% ggplot(aes(Date, any_cum)) + 
  geom_line(aes(color="Any vaccine")) + 
  geom_line(aes(y=full_cum, color="Primary series only")) + 
  geom_line(aes(y=one_cum, color="One booster")) + 
  geom_line(aes(y=two_cum, color="Two or more boosters")) +
  scale_x_date("Time", date_labels = "%b %y",date_breaks = "2 month", expand=c(0.01, 0.01), limits = as.Date(c("2020-03-01", "2023-01-01"))) + 
  scale_y_continuous("Vaccination coverage (%)", limits=c(0, 100), expand=c(0, 0)) +
  scale_color_manual(values=colors, breaks=c("Any vaccine", "Primary series only", "One booster", "Two or more boosters")) + 
  labs(subtitle="B") + 
  theme(legend.position = "bottom",
        legend.key=element_blank(),
        panel.border = element_rect(fill=NA),
        axis.text.x = element_text(angle=90),
        legend.title=element_blank(),
        panel.background = element_blank(),
        text=element_text(size=12, family="sans"))  

library(patchwork)
inf/vacc
ggsave("D:/CCHCS_premium/st/covid-indirects/figures/inf_test_vacc.jpg", width=8, height=7, dpi=300)
