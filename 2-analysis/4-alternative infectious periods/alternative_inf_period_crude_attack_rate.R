# Sophia Tan 
# Unadjusted results of infectiousness under different infectious period definitions

rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")
#setwd("D:/stan5/code_ST")

library(tidyverse)
library(broom)
library(readr)
library(MatchIt)
library(RColorBrewer)

labels <- expand.grid(index_prior_vacc=0:1, index_prior_inf=0:1) %>% 
  mutate(label=c("No prior vaccination or infection", "Prior vaccination", "Prior infection", 
                 "Both prior vaccination and infection")%>%
           factor(levels=c("No prior vaccination or infection", "Prior vaccination", "Prior infection",
                           "Both prior vaccination and infection")))

binomial <- function(data) {
  data %>%
    rowwise %>%
    mutate(out = list(prop.test(x, count, conf.level=.95) %>%
                        tidy)) %>%
    ungroup %>%
    unnest(out) %>% select(!c(statistic, p.value, parameter, method, alternative))
}



matched <- read_csv("matched_data.csv")
matched_res<-matched %>% group_by(index_prior_vacc, index_prior_inf) %>% 
  summarise(count=n(), x=sum(contact_status), risk=mean(contact_status))

matched_res <- matched_res %>% binomial()
matched_res <- matched_res %>% left_join(labels) %>% mutate(group="5 days (main)")


matched_2_5infper <- read_csv("matched_data_alternative_inf_per2_5.csv")
matched_2_5infper_res<-matched_2_5infper %>% group_by(index_prior_vacc, index_prior_inf) %>% 
  summarise(count=n(), x=sum(contact_status), risk=mean(contact_status))

matched_2_5infper_res <- matched_2_5infper_res %>% binomial()
matched_2_5infper_res <- matched_2_5infper_res %>% left_join(labels) %>% mutate(group="5 days")


matched_2_7infper <- read_csv("matched_data_alternative_inf_per2_7.csv")
matched_2_7infper_res<-matched_2_7infper %>% group_by(index_prior_vacc, index_prior_inf) %>% 
  summarise(count=n(), x=sum(contact_status), risk=mean(contact_status))

matched_2_7infper_res <- matched_2_7infper_res %>% binomial()
matched_2_7infper_res <- matched_2_7infper_res %>% left_join(labels) %>% mutate(group="7 days")


total <- rbind(matched_res, matched_2_5infper_res, matched_2_7infper_res) %>% 
  mutate(group=factor(group, levels=c("5 days (main)", "5 days", "7 days"),
                      labels=c("5 day infectious period starting at first positive test",
                               "5 day infectious period starting 2 days prior to first positive test",
                               "7 day infectious period starting 2 days prior to first positive test")))


colors<-brewer.pal(n=8, "BrBG")[c(1,3,6)]
p <- total %>% 
  ggplot(aes(label, risk, group=group, color=group)) + 
  geom_point(position=position_dodge(0.5), size=2) + 
  geom_errorbar(position=position_dodge(0.5), aes(ymin=conf.low, ymax=conf.high), width=.4) + 
  scale_x_discrete(name="Prior vaccination and/or infection in index case", 
                   labels=c("No prior vaccination\nor infection", "Prior vaccination", 
                            "Prior infection", "Both prior vaccination\nand infection")) +
  scale_y_continuous(name="Attack rate in close contact", limits = c(0,.5), expand = c(0,0)) + 
  scale_color_manual(values=colors, name=element_blank()) + 
  theme(legend.position = "bottom",  
        legend.direction = "vertical",
        legend.key = element_blank(),
        panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())

p %>% ggsave(filename = "/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/figures/supplementary/alternative_infperiod_fig.jpg", width = 7, height=5)

