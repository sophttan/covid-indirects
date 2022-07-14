# main figure 1, summarizing total infections, omicron infections, and vaccination over time

rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")

library(tidyverse)
library(readr)
library(scales)
library(patchwork)
library(reshape2)

d <- read_csv("housing_inf_data.csv")
matched <- read_csv("matched_data.csv")
matched_summary <- matched %>%group_by(Institution) %>% summarise(count=n())

# total infections over time
all_inf <- d %>% group_by(ResidentId, num_pos) %>% filter(num_pos>=1) %>% summarise_all(first)
p1 <- all_inf %>% mutate(Month=as.Date(format(Day, "%Y-%m-01"))) %>% group_by(Month) %>% summarise(cases=n()) %>%
  ggplot(aes(Month, cases)) + 
  geom_rect(aes(xmin = as.Date("2021-12-01"), xmax = as.Date("2022-05-01"), ymin = 0, ymax = 20000), alpha = 0.005) +
  geom_line() + 
  xlab("Time")+
  scale_y_continuous("Monthly SARS-CoV-2\ninfections", expand = c(0,0), limits=c(0,20000), label=comma) +
  labs(subtitle="A")+
  theme(panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank())

p1

# omicron index cases stratified by 
matched <- matched %>% mutate(Month=as.Date(format(Day, "%Y-%m-01")))
matched_stratified <- matched %>% group_by(Month, index_prior_vacc, index_prior_inf) %>% 
  summarise(prim_cases=n()) %>% 
  arrange(index_prior_vacc, index_prior_inf, Month)

grid <- expand.grid(Month=unique(matched$Month), index_prior_vacc=0:1, index_prior_inf=0:1)
labels <- expand.grid(index_prior_vacc=0:1, index_prior_inf=0:1) %>% 
  mutate(label=c("No prior vaccination or infection", "Prior vaccination", "Prior infection", 
                 "Both prior vaccination and infection")%>%factor(levels=c("No prior vaccination or infection", "Prior vaccination", "Prior infection", 
                                                                           "Both prior vaccination and infection")))

library(RColorBrewer)
colors<-brewer.pal(n=8, "Accent")[c(8,5,2,1)]
matched_stratified <- matched_stratified %>% ungroup() %>% full_join(grid) %>% replace_na(list(prim_cases=0)) %>% left_join(labels)
p2 <- matched_stratified %>% ggplot(aes(Month, prim_cases, group=label, color=label)) + geom_line() + 
  scale_color_manual(values=colors)+
  scale_y_continuous("Monthly Omicron\nSARS-CoV-2 index cases", expand = c(0,0), label=comma) +
  scale_x_date(date_labels = "%Y-%m") +
  labs(subtitle="B")+  xlab("Time")+
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())
p2


#omicron infections over time by institution
infections <- d %>% group_by(ResidentId, num_pos) %>% filter(num_pos>=1) %>% summarise_all(first) %>% filter(Day >= "2021-12-15")

colors<-brewer.pal(n=9, "Blues")[c(8, 5)]

total_inf <- infections %>% filter(!Institution %>% is.na()) %>% 
  group_by(Institution) %>% summarise(count=n()) %>% 
  left_join(matched_summary, by="Institution") %>% 
  replace_na(list(count.y=0)) %>% 
  mutate(prop_total=count.x/nrow(infections)*100,prop_included=count.y/1261*100,
         prop_inc_tot=count.y/count.x*100) %>% 
  arrange(desc(prop_total)) %>% 
  mutate(Institution=Institution %>% factor(levels=Institution)) 

p3 <- total_inf %>% 
  ggplot(aes(reorder(Institution, -count.y), count.y)) + geom_bar(stat='identity', fill=colors[1]) + 
  scale_y_continuous("Omicron SARS-CoV-2\nindex cases", limits=c(0, 200), expand = c(0,0)) + 
  scale_x_discrete(name="Institution", labels=1:35) + 
  scale_fill_manual(values=colors[1]) + 
  labs(subtitle="C") + 
  theme(axis.text.x = element_text(angle=90),
        legend.title = element_blank(),
        legend.position = "right",
        legend.key = element_blank(),
        panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line(),
        axis.line.y.right = element_line())
p3


# vaccination data over time
vacc <- read_csv("cleaned_vaccination_data.csv")
nh <- read_delim("/Users/sophiatan/Documents/UCSF/ST files/NightlyHousing_20220520.csv",";")
nh <- nh %>% filter(Night >= "2020-12-07") 
gc()
# population for the purposes of this plot 
# is total number of residents that were incarcerated at any point 
# during the vaccination time period from 12/7/2020 to May 2022
pop <- (nh %>% filter(!is.na(Institution)))$ResidentId %>% unique() %>% length()

vacc <- vacc %>% mutate(booster_add_dose = ifelse(num_dose>full_vacc, 1, 0))
vacc <- vacc %>% mutate(Month=as.Date(format(Date, "%Y-%m-01")))
any_vacc <- vacc %>% filter(num_dose==1) %>% group_by(Month) %>% summarise(any=n()) %>% mutate(any_total = cumsum(any))
full_vacc <- vacc %>% filter(num_dose==full_vacc) %>% group_by(Month) %>% summarise(full=n()) %>% mutate(full_total=cumsum(full))
boosted <- vacc %>% filter(booster_add_dose==1) %>% group_by(Month) %>% summarise(boost=n()) %>% mutate(boosted_total=cumsum(boost))

dates <- all_inf %>% arrange(Day) %>% mutate(Month=as.Date(format(Day, "%Y-%m-01"))) %>%
  group_by(Month) %>% group_keys()
full_vacc_table <- any_vacc %>% full_join(full_vacc) %>% full_join(boosted)
full_vacc_table <- full_vacc_table %>% full_join(dates) %>% replace_na(list(any_total=0, full_total=0, boosted_total=0))

full_vacc_table <- full_vacc_table %>% mutate(any_total=any_total/pop*100, 
                                              full_total=full_total/pop*100,
                                              boosted_total=boosted_total/pop*100)


p4 <- full_vacc_table %>% ggplot(aes(Month)) + 
  geom_line(aes(y=any_total, color="At least 1 dose")) + 
  geom_line(aes(y=full_total, color="Completed primary series")) +
  geom_line(aes(y=boosted_total, color="Received booster dose")) + 
  geom_rect(aes(xmin = as.Date("2021-12-01"), xmax = as.Date("2022-05-01"), ymin = 0, ymax = 100), alpha = 0.005) +
  scale_y_continuous("Cumulative vaccination (%)", limits=c(0,100), expand = c(0,0)) + 
  scale_color_brewer(palette="Greens", direction=-1)+
  guides(color=guide_legend(title="Vaccine status")) +
  labs(subtitle="D")+  xlab("Time")+
  theme(panel.background = element_blank(), 
        legend.title=element_blank(),
        legend.key=element_blank(),
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())
p4


# save
((p1+p2)/p3/p4) %>% 
  ggsave(filename="/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/figures/main/summary_figure1.jpg", 
         width=10, height=9)


# build on p3, omicron infections
# what proportion of total Omicron infections were included as index cases?
suppfig <- total_inf %>% arrange(desc(count.y)) %>% mutate(Institution=factor(Institution, levels=Institution)) %>% 
  select(!c(prop_total, count.x, prop_included)) %>% mutate(prop_inc_tot=prop_inc_tot*7.5) %>%
  melt(key="Institution") %>% 
  mutate(variable=factor(variable, levels=c("count.y", "prop_inc_tot"))) %>% 
  ggplot(aes(Institution, value, fill=variable)) + geom_bar(stat='identity', position='dodge') + 
  scale_y_continuous("Omicron SARS-CoV-2 index cases", limits=c(0, 200), expand = c(0,0), 
                     sec.axis = sec_axis(~./7.5, name="Proportion of total SARS-CoV-2 infections\nincluded as index cases (%)")) + 
  scale_x_discrete(name="Institution", labels=1:35) + 
  scale_fill_manual(values= colors,  labels=c("Number of index cases", "Proportion of total infections")) + 
  #labs(subtitle="C") + 
  theme(axis.text.x = element_text(angle=90),
        legend.title = element_blank(),
        legend.position = "right",
        legend.key = element_blank(),
        panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line(colour = colors[1]),
        axis.ticks.y.left = element_line(color = colors[1]),
        axis.text.y.left = element_text(color = colors[1]),
        axis.title.y.left = element_text(color = colors[1]),
        axis.line.y.right = element_line(colour = colors[2]),
        axis.ticks.y.right = element_line(color = colors[2]),
        axis.text.y.right  = element_text(color = colors[2]),
        axis.title.y.right = element_text(color = colors[2]))
suppfig


suppfig %>% 
  ggsave(filename="/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/figures/supplementary/omicron_index_cases_institution.jpg", 
         width=8, height=4)

  