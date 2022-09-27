# clean comorbidity data to estimate covid risk among residents
rm(list=ls())

library(tidyverse)
library(readr)
library(lubridate)

setwd("/Users/sophiatan/Documents/UCSF/")

comorbidity <- read_delim("ST files/Comorbidity_20220520.csv",delim = ";")
covid_risk <- comorbidity %>% filter(Comorbidity=='CovidWeightedRisk')
covid_risk

covid_risk <- covid_risk %>% replace_na(list(Ending="2022-05-21")) %>%
  mutate(interval = interval(Starting, Ending-1), duration = time_length(interval, unit = "days"),
         Value=as.numeric(Value))

covid_score <- covid_risk %>% group_by(ResidentId) %>% summarise(avg = sum(Value*duration)/sum(duration))
covid_score %>% filter(avg <= 12) %>% ggplot(aes(avg)) + geom_histogram(aes(y=stat(count / sum(count))), bins=20) +  
  scale_x_continuous(name="COVID-19 risk score", expand=c(0,0)) + 
  scale_y_continuous("Proportion of residents", expand=c(0,0), limits=c(0,0.5)) + 
  theme(legend.title = element_text(),
        legend.position = "bottom",
        panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())

covid_risk
covid_risk %>% select(!c(Comorbidity, Month, Starting, Ending, duration)) %>% 
  write_csv("cleaned_data/covid_risk_score.csv")
