# compare datasets with and without requiring prior infection in residents
rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)

with <- read_csv("survival_data/allvacc_priorinf_infvacc071023.csv") %>%
  mutate(time_since_vacc.primary=if_else(vacc.primary==0, NA, time_since_vacc.primary),
         time_since_vacc.secondary=if_else(vacc.secondary==0, NA, time_since_vacc.secondary),
         time_since_inf.primary=if_else(inf.primary==0, NA, time_since_inf.primary),
         time_since_inf.secondary=if_else(inf.secondary==0, NA, time_since_inf.secondary))
without <- read_csv("survival_data/allvacc_infvacc071023.csv") %>% 
  mutate(time_since_vacc.primary=if_else(vacc.primary==0, NA, time_since_vacc.primary),
         time_since_vacc.secondary=if_else(vacc.secondary==0, NA, time_since_vacc.secondary),
         time_since_inf.primary=if_else(inf.primary==0, NA, time_since_inf.primary),
         time_since_inf.secondary=if_else(inf.secondary==0, NA, time_since_inf.secondary))
  

without %>% group_by(inf.primary, inf.secondary) %>% summarise(n=n())

with %>%
  group_by(inf.primary) %>% 
  summarise(risk.primary=mean(risk.primary), age.primary=mean(age.primary),
            time_since_inf.primary=mean(time_since_inf.primary), 
            time_since_vacc.primary=mean(time_since_vacc.primary))

with %>%
  group_by(inf.secondary) %>% 
  summarise(risk.secondary=mean(risk.secondary), age.secondary=mean(age.secondary),
            time_since_inf.secondary=mean(time_since_inf.secondary), 
            time_since_vacc.secondary=mean(time_since_vacc.secondary,na.rm=T))

with %>% group_by(inf.primary, vacc.primary) %>% summarise(n=n()) %>% mutate(prop=n/sum(n))
with %>% group_by(inf.secondary, vacc.secondary) %>% summarise(n=n()) %>% mutate(prop=n/sum(n))
without %>% group_by(inf.primary, vacc.primary) %>% summarise(n=n()) %>% mutate(prop=n/sum(n))
without %>% group_by(inf.secondary, vacc.secondary) %>% summarise(n=n()) %>% mutate(prop=n/sum(n))

without %>%
  group_by(inf.primary) %>% 
  summarise(risk.primary=mean(risk.primary), age.primary=mean(age.primary),
            time_since_inf.primary=mean(time_since_inf.primary,na.rm=T), 
            time_since_vacc.primary=mean(time_since_vacc.primary,na.rm=T))

without %>%
  group_by(inf.secondary) %>% 
  summarise(risk.secondary=mean(risk.secondary), age.secondary=mean(age.secondary),
            time_since_inf.secondary=mean(time_since_inf.secondary), 
            time_since_vacc.secondary=mean(time_since_vacc.secondary,na.rm=T))

ggplot(data=with %>% filter(vacc.primary>0), alpha= 0.7, color="grey", aes(time_since_vacc.primary, y=..density..)) +
  geom_histogram(position="identity") +   scale_color_discrete("Analysis with only previously\ninfected residents") + 
  geom_histogram(data=without %>% filter(vacc.primary>0), position="identity", alpha=0.7, 
                 aes(group=as.factor(inf.primary), fill=as.factor(inf.primary))) + 
  scale_x_continuous("Months since most recent vaccination") + 
  scale_y_continuous("Density") + 
  scale_fill_discrete("Prior infection", labels=c("No prior infection", "Prior infection")) 


ggplot(data=with %>% filter(vacc.secondary>0), alpha= 0.7, color="grey", aes(time_since_vacc.secondary, y=..density..)) + 
  geom_histogram(position="identity") +   scale_color_discrete("Analysis with only previously\ninfected residents") + 
  geom_histogram(data=without %>% filter(vacc.secondary>0), position="identity", alpha=0.7, 
                 aes(group=as.factor(inf.primary), fill=as.factor(inf.primary))) + 
  scale_x_continuous("Months since most recent vaccination") + 
  scale_y_continuous("Density") + 
  scale_fill_discrete("Prior infection", labels=c("No prior infection", "Prior infection")) 
