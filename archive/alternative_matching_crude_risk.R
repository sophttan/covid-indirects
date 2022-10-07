# Sophia Tan 6/24/22
# Unadjusted results of infectiousness under different matching specifications

rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")
#setwd("D:/stan5/code_ST")

library(tidyverse)
library(broom)
library(readr)
library(MatchIt)

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
matched_res <- matched_res %>% left_join(labels) %>% mutate(group="1:10 matching within 30 days")


matched_1_10_15 <- read_csv("matched_data_alternative1_10_15.csv")
matched_1_10_15_res<-matched_1_10_15 %>% group_by(index_prior_vacc, index_prior_inf) %>% 
  summarise(count=n(), x=sum(contact_status), risk=mean(contact_status))

matched_1_10_15_res <- matched_1_10_15_res %>% binomial()
matched_1_10_15_res <- matched_1_10_15_res %>% left_join(labels) %>% mutate(group="1:10 matching within 15 days")


matched_1_4_30 <- read_csv("matched_data_alternative1_4_30.csv")
matched_1_4_30_res<-matched_1_4_30 %>% group_by(index_prior_vacc, index_prior_inf) %>% 
  summarise(count=n(), x=sum(contact_status), risk=mean(contact_status))

matched_1_4_30_res <- matched_1_4_30_res %>% binomial()
matched_1_4_30_res <- matched_1_4_30_res %>% left_join(labels) %>% mutate(group="1:4 matching within 30 days")


nomatch <- read_csv("unmatched_all_covariates.csv")
nomatch_res<-nomatch %>% group_by(index_prior_vacc, index_prior_inf) %>% 
  summarise(count=n(), x=sum(contact_status), risk=mean(contact_status))

nomatch_res <- nomatch_res %>% binomial()
nomatch_res <- nomatch_res %>% left_join(labels) %>% mutate(group="No matching")



total <- rbind(matched_res, matched_1_10_15_res, matched_1_4_30_res, nomatch_res) %>% 
  mutate(group=factor(group, levels=c("1:10 matching within 30 days", "1:10 matching within 15 days", "1:4 matching within 30 days", "No matching")))

p <- total %>% 
  ggplot(aes(label, risk, group=group, color=group)) + 
  geom_point(position=position_dodge(0.5), size=2) + 
  geom_errorbar(position=position_dodge(0.5), aes(ymin=conf.low, ymax=conf.high), width=.4) + 
  scale_x_discrete(name="Prior vaccination and/or infection in index case", 
                   labels=c("No prior vaccination\nor infection", "Prior vaccination", 
                            "Prior infection", "Both prior vaccination\nand infection")) +
  scale_y_continuous(name="Attack rate in close contact", limits = c(0,.5), expand = c(0,0)) + 
  scale_color_brewer(guide = guide_legend(label.position = "right", title.position = "top"), 
                     type="div",
                     name=element_blank()) + 
  #  labs(subtitle = "A")+
  theme(legend.position = "right",  
        legend.key = element_blank(),
        panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line()) 

p %>% ggsave(filename = "/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/figures/supplementary/alternative_matching_specs_fig.jpg", width = 8, height=5)

