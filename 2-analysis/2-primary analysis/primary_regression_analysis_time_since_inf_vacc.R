# Test relationship between time since most recent vaccination or infection and infection in close contact
# Sophia Tan
rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")

library(clubSandwich)
library(tidyverse)
library(cowplot)
library(RColorBrewer)

# add in dates of most recent vaccination
vacc <- read_csv("cleaned_vaccination_data.csv")
d <- read_csv("matched_data_ps100722.csv")
d <- d %>% left_join(vacc %>% select(ResidentId, num_dose, Date), 
                     by=c("index_id"="ResidentId", "index_prior_vacc_doses"="num_dose")) %>% rename("index_vacc_date"="Date")

d <- d %>% mutate(index_weeks_since_vacc = as.numeric(difftime(Day, index_vacc_date, units="weeks")),
                  index_weeks_since_vacc_log = log(index_weeks_since_vacc))

# add in dates of most recent infection
inf_data <- read_csv("housing_inf_data072122.csv", col_select = c(ResidentId, num_pos, Day, Institution))
infections_total <- inf_data %>% filter(num_pos>=1) %>%
  group_by(ResidentId, num_pos) %>% summarise_all(first) %>%
  group_by(ResidentId, num_pos)
d <- d %>% mutate(previous_inf = num_pos-1) %>% left_join(infections_total %>% select(!Institution) %>% rename("Day_prior_inf"="Day"), c("index_id"="ResidentId", "previous_inf"="num_pos"))
d <- d %>% mutate(index_weeks_since_inf = as.numeric(difftime(Day, Day_prior_inf, units="weeks")))

# add in weeks since most recent vaccination or infection
d <- d %>% rowwise() %>% mutate(index_weeks_since_inf_vacc = min(index_weeks_since_inf, index_weeks_since_vacc, na.rm=T))
d <- d %>% mutate(index_weeks_since_inf_vacc = ifelse(index_weeks_since_inf_vacc %>% is.infinite(), NA, index_weeks_since_inf_vacc))
d <- d %>% mutate(index_weeks_since_inf_log = log(index_weeks_since_inf),
                  index_weeks_since_vacc_log = log(index_weeks_since_vacc),
                  index_weeks_since_inf_vacc_log = log(index_weeks_since_inf_vacc))

d <- d %>% mutate(Institution=as.factor(Institution),
                  index_id=as.factor(index_id))


# crude estimates of attack rate based on time since most recent vaccination
weeks_since_vacc<-d %>% filter(!index_weeks_since_vacc %>% is.na()) %>% 
  mutate(weeks=cut(index_weeks_since_vacc, c(0,5,10,15,20,30,40,50,Inf), include.lowest=T)) %>% 
  group_by(weeks) %>% summarise(x=sum(contact_status), count=n()) %>% rowwise() %>%
  mutate(out = list(prop.test(x, count, conf.level=.95) %>%
                      tidy)) %>%
  ungroup %>%
  unnest(out)

p <- weeks_since_vacc %>% ggplot(aes(factor(weeks), estimate)) + 
  geom_point(position=position_dodge(0.5), size=2) + 
  geom_errorbar(position=position_dodge(0.5), aes(ymin=conf.low, ymax=conf.high), width=.2) + 
  scale_x_discrete(name="Weeks since most recent vaccination", 
                   labels=c("<5", "5-10", "10-15", 
                            "15-20", "20-30", "30-40", "40-50", "\u226550")) + 
  scale_y_continuous(name="Attack rate in close contact", limits = c(0,0.6), expand = c(0,0)) + 
  #  labs(subtitle = "A")+
  theme(legend.position = "bottom",  
        legend.key = element_blank(),
        panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())

p %>% ggsave(filename="/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/figures/supplementary/attack_rate_time_since_vacc.jpg",
             width=6, height=4)


# function to estimate relative risks from coefficients
est_relative_risk <- function(data) {
  data %>% mutate(risk_red = round(100*(exp(beta*x)-1), 1),
                  lb = round(100*(exp(beta*x-2*SE*x)-1), 1),
                  ub = round(100*(exp(beta*x+2*SE*x)-1), 1))
}

format_table <- function(data) {
  data %>% 
    mutate(risk_red = paste0(risk_red, " (", lb, ", ", ub, ")")) %>%
    select(Coef, risk_red)
}

# relationship between time since most recent vaccination and infection in close contact
model <- glm(contact_status ~ index_prior_inf + index_weeks_since_vacc+num_days_in_contact+
               num_vacc_doses+has_prior_inf+incidence_log+Institution, data=d, weights=weights, family="poisson")
model <- coef_test(model, vcov = "CR2", cluster = d$subclass) %>% data.frame(row.names = NULL)
res <- model[3,]

# relationship between time since most recent infection and infection in close contact
model <- glm(contact_status ~ index_prior_vacc + index_weeks_since_inf+num_days_in_contact+
               num_vacc_doses+has_prior_inf+incidence_log+Institution, data=d, weights=weights, family="poisson")
model <- coef_test(model, vcov = "CR2", cluster = d$subclass) %>% data.frame(row.names = NULL)
model
res <- rbind(res, model[3,])

# relationship between time since most recent vaccination or infection and infection in close contact
model <- glm(contact_status ~ index_weeks_since_inf_vacc+num_days_in_contact+
               num_vacc_doses+has_prior_inf+incidence_log+Institution, data=d, weights=weights, family="poisson")
model <- coef_test(model, vcov = "CR2", cluster = d$subclass) %>% data.frame(row.names = NULL)
res <- rbind(res, model[2,])

res_tbl <- res %>% mutate(x=5)
res_tbl <- res_tbl %>% est_relative_risk()
res_tbl

res_tbl <- res_tbl %>% format_table()
res_tbl$Coef <- c("Time since last COVID-19 vaccine dose (per 5 weeks)",
                  "Time since most recent SARS-CoV-2 infection (per 5 weeks)",
                  "Time since most recent vaccine or infection (per 5 weeks)")
names(res_tbl) <- c("","Relative % change in attack rate of infection in close contact (95% CI)")
res_tbl %>% write_csv("/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/tables/time_since_vacc_inf_s5.csv")

