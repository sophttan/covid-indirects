# Sophia Tan 12/15/23
# updated 1/23/24
# Clean risk data 
# risk data is only available through 12/16/2022

rm(list=ls())
gc()

source(here::here("config.R"))

setwd("D:/CCHCS_premium/CDCR Data/Dec 16 2022 Data/")

comorbidity <- read_delim("Comorbidity_20221216.csv",delim = ";")
covid_risk <- comorbidity %>% filter(Comorbidity=='CovidWeightedRisk')
covid_risk

covid_risk <- covid_risk %>% replace_na(list(Ending=as.Date("2022-12-16"))) %>%
  mutate(interval = interval(Starting, Ending-1), duration = time_length(interval, unit = "days"),
         Value=as.numeric(Value))

# average resident risk score over time 
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

covid_risk %>% select(!c(Comorbidity, Month, interval, duration)) %>% 
  rename("risk"="Value", "risk.start"="Starting", "risk.end"="Ending") %>%
  write_csv("D:/CCHCS_premium/st/leaky/cleaned-data/covid_risk_score012324.csv")
