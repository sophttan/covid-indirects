rm(list=ls())
gc()

setwd("D:/stan5/code_ST/march-data/")

library(tidyverse)

d <- read_csv("housing_inf_data_adjusted_roomtype1.csv")
d2 <- read_csv("housing_inf_data_adjusted_roomtype2.csv")
d <- bind_rows(d, d2)
rm(d2)
gc()

d <- d %>% mutate(week = as.numeric(round(difftime(Day, as.Date("2020-03-01"), units = "week"))))
#d <- d %>% filter(Day>="2020-03-01")
d <- d %>% group_by(Institution, week)
# keep distinct residents 
most_recent_vacc_status <- d %>% arrange(desc(Day)) %>% distinct(ResidentId, .keep_all = T)
summary <- most_recent_vacc_status %>% summarise(total_res=n(), any_vacc=sum(num_dose>=1,na.rm=T), 
                                                 fully_vacc = sum(num_dose>=full_vacc & full_vacc>=1, na.rm=T),
                                                 boosted = sum(num_dose>full_vacc & full_vacc>=1, na.rm=T),
                                                 num_inf_vacc = sum(num_dose>=1 | num_pos>=1, na.rm=T))

infections <- d %>% filter(num_pos>=1) %>% group_by(ResidentId, num_pos) %>% summarise_all(first)
infections <- infections %>% mutate(week = as.numeric(round(difftime(Day, as.Date("2020-03-01"), units = "week"))))
infections <- infections %>% group_by(Institution, week) %>% summarise(cases = n())

summary <- summary %>% left_join(infections) 
summary <- summary %>% replace_na(list(cases=0)) 
summary <- summary %>% mutate(inc=cases/total_res*100000, any_vacc_perc=any_vacc/total_res*100, 
                              fully_vacc_perc=fully_vacc/total_res*100, boosted_perc=boosted/total_res*100,
                              inf_or_vacc_perc = num_inf_vacc/total_res*100)

summary <- summary %>% mutate(Institution=as.factor(Institution)) 
summary <- filter(summary, !is.na(Institution))

library(patchwork)
pdf("D:/stan5/code_ST/CDCR-Calprotect-private/incidence_by_facility_subset.pdf")
included_institutions <- c(1,3,4,5,6,7,13,16,19,20,21,22,23,24,25,26,27,28,29,30,31,33,34,35)
for (facility in included_institutions){ #unique(summary$Institution)) { 
  filtered <- summary %>% filter(Institution==facility)
  # plot vaccination over time in facility
  p1 <- filtered %>% filter(week>=30) %>% ggplot(aes(week)) + geom_line(aes(y=any_vacc_perc, color="at least 1 dose")) +
    geom_line(aes(y=fully_vacc_perc, color="fully vaccinated")) +
    geom_line(aes(y=boosted_perc, color="boosted")) +
    scale_y_continuous(name="Vaccinated (%)", limits = c(0,100)) +
    scale_x_continuous(name="weeks since 3/1/2020") + ggtitle(facility) +
    theme(legend.title = element_blank())
  p2 <- filtered %>% ggplot(aes(week, inc)) + 
    geom_line() + 
    scale_y_continuous(name = "Incidence per 100000 residents", limits = c(0, 30000)) + 
    scale_x_continuous(name="weeks since 3/1/2020")
  p3 <- filtered %>% ggplot(aes(week, inf_or_vacc_perc)) + geom_line() + 
    scale_y_continuous(name = "Previous infection or vaccination (%)", limits = c(0, 100)) + 
    scale_x_continuous(name="weeks since 3/1/2020")
  p4 <- filtered %>% ggplot(aes(week, total_res)) + geom_line() + 
    scale_y_continuous(name = "Facility population", limits = c(0, 6000)) + 
    scale_x_continuous(name="weeks since 3/1/2020")
  print((p1+p3)/(p2+p4))
}
dev.off()


calculate_inc <- function(institution, index_first_pos_test_date) {
  d_inst <- filter(d, Institution==institution & Day <= index_first_pos_test_date & index_first_pos_test_date - Day > 7)
  total_pop <- d_inst$ResidentId %>% unique() %>% length()
  infections_inst <- filter(infections, Institution==institution & Day <= index_first_pos_test_date & index_first_pos_test_date - Day > 7)
  total_inf <- infections_inst %>% nrow()
  total_inf/total_pop * 100000
}