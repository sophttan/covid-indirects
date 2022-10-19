# Sophia Tan 9/19/22
# Checking distributions

rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")
#setwd("D:/stan5/code_ST")

library(tidyverse)
library(broom)
library(readr)
library(MatchIt)
library(RColorBrewer)

colors<-brewer.pal(n=8, "Accent")[c(7,5)]

matched <- read_csv("matched_data_ps100722.csv")

matched <- matched %>% mutate(num_days_first_followup = difftime(first_followup%>%as.Date(), first_contact, units="days"),
                              num_days_last_followup = difftime(last_followup, first_contact, units="days"),
                              num_days_secondary = difftime(day_secondary_case, first_contact, units="days"))

setwd("/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/figures/appendix")

p1 <- matched %>% mutate(treatment=factor(treatment, levels=c(1,0), labels=c("No prior vaccination", "Prior vaccination"))) %>%
  ggplot(aes(num_tests_followup, group=treatment, fill=treatment)) +
  geom_bar(aes(y=..prop..), position ="dodge") + 
  scale_x_continuous(name="Number of follow up tests in close contacts", expand=c(0,0), breaks=seq(1,12,1)) + 
  scale_y_continuous("Proportion of close contacts", expand=c(0,0)) + 
  scale_fill_manual(values=colors, name="Prior vaccination in index case")+
  labs(title="Number of follow up tests among close contacts", subtitle="A")+
  theme(legend.title = element_text(),
        legend.position = "bottom",
        plot.title=element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())

p2 <- matched %>% filter(num_tests_followup==1) %>% mutate(treatment=factor(treatment, levels=c(1,0), labels=c("No prior vaccination", "Prior vaccination"))) %>%
  ggplot(aes(num_days_first_followup, group=treatment, fill=treatment)) +
  geom_bar(aes(y=..prop..), position = position_dodge2(padding=0, preserve="single")) + 
  scale_x_continuous(name="Number of days between first exposure and\n follow up test in close contacts", expand=c(0,0), breaks=seq(3,18)) + 
  scale_y_continuous("Proportion of close contacts", expand=c(0,0)) + 
  scale_fill_manual(values=colors, name="Prior vaccination in index case")+
  labs(title="\n\nTiming of follow up testing among close contacts",
       subtitle="Close contacts with 1 follow up test\nB")+
  theme(legend.title = element_text(),
        plot.title=element_text(hjust = 0.5),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())

p3 <- matched %>% filter(num_tests_followup > 1) %>% 
  mutate(treatment=factor(treatment, levels=c(1,0), labels=c("No prior vaccination", "Prior vaccination"))) %>%
  ggplot(aes(num_days_first_followup, group=treatment, fill=treatment)) +
  geom_bar(aes(y=..prop..), position = position_dodge2(padding=0, preserve="single")) + 
  scale_x_continuous(name="Number of days between first exposure and\nfirst follow up test in close contacts", expand=c(0,0), breaks=seq(3,18)) + 
  scale_y_continuous("Proportion of close contacts", expand=c(0,0)) + 
  scale_fill_manual(values=colors, name="Prior vaccination in index case")+
  labs(subtitle="Close contacts with >1 follow up tests\nC")+
  theme(legend.title = element_text(),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())

p4 <- matched %>% filter(num_tests_followup > 1) %>% 
  mutate(treatment=factor(treatment, levels=c(1,0), labels=c("No prior vaccination", "Prior vaccination"))) %>%
  ggplot(aes(num_days_last_followup, group=treatment, fill=treatment)) +
  geom_bar(aes(y=..prop..), position = position_dodge2(padding=0, preserve="single")) + 
  scale_x_continuous(name="Number of days between first exposure and\nlast follow up test in close contacts", expand=c(0,0), breaks=seq(3,18)) + 
  scale_y_continuous("Proportion of close contacts", expand=c(0,0)) + 
  scale_fill_manual(values=colors, name="Prior vaccination in index case")+
  labs(title=element_blank(), subtitle="\n\n")+
  theme(legend.title = element_text(),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())

library(gridExtra)
grid.arrange(patchworkGrob(p1/p2/(p3|p4) + 
                             plot_layout(guides="collect") & theme(legend.position = "bottom")), 
             left = "              Proportion of close contacts") %>%
  ggsave(filename="follow_up_testing.jpg", width=7, height=9)

p3 <- matched %>% filter(contact_status==1) %>% mutate(treatment=factor(treatment, levels=c(1,0), labels=c("No prior vaccination", "Prior vaccination"))) %>%
  ggplot(aes(num_days_secondary, group=treatment, fill=treatment)) +
  geom_bar(aes(y=..prop..), position=position_dodge2(padding = 0, preserve="single")) + 
  scale_x_continuous(name="Number of days between first exposure and\nfirst positive test in secondary cases", expand=c(0,0), breaks=seq(3,18)) + 
  scale_y_continuous("Proportion of close contacts", expand=c(0,0)) + 
  scale_fill_manual(values=colors, name="Prior vaccination in index case")+
  theme(legend.title = element_text(),
        legend.position = "bottom",
        panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())

p3 %>%
  ggsave(filename = "secondary_cases.jpg", width=6, height=4)

matched %>% mutate(treatment=factor(treatment, levels=c(1,0), labels=c("No prior vaccination", "Prior vaccination"))) %>%
  group_by(treatment) %>% select(num_days_in_contact,num_days_first_followup,num_days_last_followup,num_tests_followup,num_days_secondary) %>%
  mutate_all(as.numeric) %>% 
  summarise_all(mean, na.rm=T)
#  summarise_all(quantile, 0.75, na.rm=T)
matched$num_days_in_contact %>% summary()
matched$num_days_last_followup %>% as.numeric() %>% summary()
matched$num_tests_followup %>% as.numeric() %>% summary()
matched$num_days_secondary %>% as.numeric() %>% summary()

matched %>% mutate(as.numeric(num_days_last_followup), as.numeric(num_days_secondary)) %>% summary()

unvacc_exposure <- (matched %>% filter(contact_status==1) %>% filter(treatment==1))$num_days_secondary %>% as.numeric()
vacc_exposure <- (matched %>%  filter(contact_status==1) %>% filter(treatment==0))$num_days_secondary %>% as.numeric()
t.test(unvacc_exposure, vacc_exposure, var.equal = T)
