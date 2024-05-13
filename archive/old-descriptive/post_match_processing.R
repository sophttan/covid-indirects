rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(lubridate)
library(tidyverse)

d1 <- read_csv("matching_data_071223/matching_data_allvacc_dose_infvacc072023_part1.csv")
d2 <- read_csv("matching_data_071223/matching_data_allvacc_dose_infvacc072023_part2.csv")
d <- d1 %>% 
  rbind(d2)
d <- d %>% mutate(id=1:n()) %>%
  group_by(Institution, subclass) %>% 
  mutate(subclass=cur_group_id())


d <- read_csv("matching_data_071223/matching_data_allvacc_groupeddoses_noincarcreq_priorinf_infvacc_081623.csv")

fix_intersection <- function(v) {
  v%>%str_extract_all("[0-9]{4}-[0-9]{2}-[0-9]{2}", simplify = T)
}

intersection <- fix_intersection(d$intersection)
d$start <- intersection[,1]%>%as.vector()%>%as.Date()
d$end <- intersection[,2]%>%as.vector()%>%as.Date()
d <- d %>% mutate(intersection=interval(start=start, end=end))

d <- d %>% group_by(subclass)

# use matched time where we use maximum overlapped time
treatment <- d %>% filter(treatment==0)
control <- d %>% filter(treatment==1)

maximize_obs_time_start <- function(start, end) {
  best <- as.Date(start[1])
  time <- 0
  for (i in 1:length(start)) {
    end_filtered <- end[start <= start[i] & end>start[i]]
    total_obs <- sum((end_filtered-start[i])%>%as.numeric())
    if(total_obs>time) {
      best <- as.Date(start[i])
      time <- total_obs
    }
  }
  as.Date(best)
}

maximize_cal_time_start <- function(start, end) {
  best <- as.Date(start[1])
  time <- 0
  for (i in 1:length(start)) {
    end_filtered <- end[start <= start[i] & end>start[i]]
    total_obs <- max((end_filtered-start[i])%>%as.numeric())
    if(total_obs>time) {
      best <- as.Date(start[i])
      time <- total_obs
    }
  }
  as.Date(best)
}

treatment <- treatment %>% 
  mutate(best_start=maximize_cal_time_start(start, end))

treatment_filter <- treatment %>% filter(start <= best_start & end > best_start)

treatment_filter <- treatment_filter %>% mutate(max_end = max(end))

treatment_summary <- treatment_filter %>% summarise(best_start=first(best_start), max_end=first(max_end))

control <- left_join(control, treatment_summary) %>% mutate(end=max_end)

control

all <- control %>% rbind(treatment_filter) %>% arrange(subclass, id)

all <- all %>% rename("group_start"="best_start")

all %>% group_by(treatment) %>% summarise(n=n(), observation=sum((end-group_start)%>%as.numeric()))

all <- all %>% rowwise() %>% mutate(final_start = group_start+5, final_end = min(as.Date("2022-12-15"), end + 5))
all %>% names()

plot_matches <- function(d, title="", subtitle="") {
  d <- d %>% mutate(subclass1=match(subclass, unique(subclass)))
  d <- d %>% group_by(subclass1) %>% 
    mutate(subclass1=subclass1*5-0.4*0:(n()-1)) %>% ungroup()
  
  p <- d %>%
    ggplot(aes(x = as.POSIXct(final_start), y = subclass1, 
               colour = as.factor(treatment))) +
    geom_point(aes(x = as.POSIXct(first)), size = 1.5, alpha=0.7, color="grey70") +
    geom_point(aes(x = as.POSIXct(last)), size = 1.5, alpha=0.7, color="grey70") +
    geom_segment(aes(x = as.POSIXct(first), xend = as.POSIXct(last), yend = subclass1, group = num_inf, color=as.factor(treatment)), alpha=0.7, color="grey70") +
    geom_point(size = 1) +
    geom_point(aes(x = as.POSIXct(final_end)), size = 1) +
    geom_segment(aes(xend = as.POSIXct(final_end), yend = subclass1, group = num_inf, color=as.factor(treatment))) +
    scale_x_datetime("Duration of co-residence", 
                     limits = c(as.POSIXct("2021-11-30"), as.POSIXct("2022-12-31")), 
                     date_breaks = "1 month", date_labels ="%b-%y", expand=c(0,0)) + 
    scale_color_discrete(name="Unit type", labels=c("Treatment", "Control")) +
    guides(color = guide_legend(reverse=TRUE)) + 
    labs(title=title, 
         subtitle=subtitle) + 
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
          axis.text.x = element_text(angle=90)) 
  
  p
}

pdf("D:/CCHCS_premium/st/indirects/testing/matching081623.pdf")
set.seed(70)
for (i in sample(all$subclass%>%unique(), 100)) {
  print(plot_matches(all %>% filter(subclass==i),
                     title=paste("Subclass", i),
                     subtitle="Matched by building, time, and number of vaccine doses in the primary resident"))
}
dev.off()

testing <- read_csv("testing_vacc_clean.csv") %>% 
  select(ResidentId, Day, Result, num_pos) %>% filter(!Result%>%is.na())

find_survival <- function(d) {
  add_testing <- function(d) {
    d %>% left_join(testing, by=c("primary"="ResidentId"))
  }
  
  filter_testing <- function(d) {
    d <- d %>% mutate(has_test=any(Day>=final_start&Day<=final_end))
    d %>% filter((Day >= final_start & Day <= final_end)|(!has_test&Day==first(Day))) %>% 
      mutate(Day=if_else(!has_test, as.Date(NA), Day), 
             Result=if_else(!has_test, as.character(NA), Result))
  }
  
  d %>% add_testing() %>% 
    group_by(id) %>% 
    filter_testing() %>%
    mutate(survival_time=as.numeric(Day-final_start)+1) %>%
    mutate(last = ifelse(any(Result=="Positive",na.rm=T)&Result=="Positive", Result=="Positive", Day==last(Day))) %>% 
    mutate(pos_first=first(Result)=="Positive") %>%
    filter(last|!has_test) %>% summarise_all(first) %>% select(!c(last), ) %>% 
    mutate(survival_time=ifelse(!has_test|Result!="Positive", as.numeric(final_end-final_start)+1, survival_time))
}

all_survival <- all %>% find_survival()

all_survival <- all_survival %>% mutate(treatment=1-treatment)
all_survival$treatment%>%table()

all_survival <- all_survival %>%
  unite("InstBuild", BuildingId:Institution, sep= "-", 
        remove = FALSE) %>%
  mutate(status=ifelse(!Result%>%is.na()&Result=="Positive", 1, 0), 
         InstBuild=as.factor(InstBuild),
         subclass=as.factor(subclass)) 

all_survival%>%
  group_by(treatment)%>%
  summarise(units=n(), cases=sum(status), person_time=n()*mean(survival_time))%>%
  mutate(inc_rate=cases/person_time*100000)

all_survival <- all_survival %>% 
  select(id, subclass, treatment, primary, secondary, Institution, BuildingId, InstBuild, inf.primary, inf.secondary, 
         vacc.primary, vacc.secondary, vacc.primary.grouped, 
         final_start, final_end, survival_time, status) %>%
  mutate(intersection=interval(final_start, final_end))

infections <- testing %>% group_by(ResidentId, num_pos) %>% 
  summarise_all(first) %>% filter(!is.na(num_pos)) %>% select(!Result)
all_survival_inf <- all_survival %>% 
  left_join(infections, by=c("primary"="ResidentId")) %>% 
  rename("infDay.primary"="Day") %>%
  group_by(id) 

all_survival_inf_clean <- all_survival_inf %>%
  filter(infDay.primary <= final_start|inf.primary==0) %>% 
  arrange(id, desc(infDay.primary)) %>% 
  summarise_all(first) %>% 
  mutate(time_since_inf.primary=(difftime(final_start, infDay.primary, units="days")%>%as.numeric())/30.417) %>%
  mutate(time_since_inf.primary=ifelse(inf.primary==0, NA, time_since_inf.primary))
all_survival_inf_clean     

all_survival_inf_clean <- all_survival_inf_clean %>%
  left_join(infections, by=c("secondary"="ResidentId")) %>% 
  rename("infDay.secondary"="Day") %>%
  group_by(id) %>%
  filter(infDay.secondary <= final_start|inf.secondary==0) %>% 
  arrange(id, desc(infDay.secondary)) %>% 
  summarise_all(first) %>% 
  mutate(time_since_inf.secondary=(difftime(final_start, infDay.secondary, units="days")%>%as.numeric())/30.417) %>%
  mutate(time_since_inf.secondary=ifelse(inf.secondary==0, NA, time_since_inf.secondary))

all_survival_inf_clean   

vacc <- read_csv("cleaned_vaccination_data.csv") %>% select(ResidentId, num_dose, Date)
all_survival_inf_clean_vacc <- all_survival_inf_clean %>% 
  left_join(vacc, by=c("primary"="ResidentId", "vacc.primary.doses"="num_dose")) %>% 
  rename("vaccday.primary"="Date")
all_survival_inf_clean_vacc <- all_survival_inf_clean_vacc %>% 
  left_join(vacc, by=c("secondary"="ResidentId", "vacc.secondary.doses"="num_dose")) %>% 
  rename("vaccday.secondary"="Date")
all_survival_inf_clean_vacc <- all_survival_inf_clean_vacc %>% 
  mutate(vaccday.primary=if_else(vacc.primary==0, as.Date(NA), vaccday.primary),
         vaccday.secondary=if_else(vaccday.secondary==0, as.Date(NA), vaccday.secondary)) %>% 
  mutate(time_since_vacc.primary = ((final_start-vaccday.primary)%>%as.numeric())/30.417,
         time_since_vacc.secondary = ((final_start-vaccday.secondary)%>%as.numeric())/30.417)

demo <- read_csv("demographic_data_clean.csv")
demo <- demo %>% mutate(age=2022-BirthYear)
demo

all_survival_inf_clean_vacc_demo <- all_survival_inf_clean_vacc %>% left_join(demo, by=c("primary"="ResidentId"))
all_survival_inf_clean_vacc_demo <- all_survival_inf_clean_vacc_demo %>% left_join(demo, by=c("secondary"="ResidentId"), suffix=c(".primary", ".secondary"))
all_survival_inf_clean_vacc_demo

risk <- read_csv("covid_risk_score.csv") %>% 
  filter(ResidentId %in% all_survival_inf_clean_vacc_demo$primary | ResidentId %in% all_survival_inf_clean_vacc_demo$secondary)
intersection <- fix_intersection(risk$interval)
risk$start <- intersection[,1]%>%as.vector()%>%as.Date()
risk$end <- intersection[,2]%>%as.vector()%>%as.Date()
risk <- risk %>% mutate(risk_interval=interval(start=start, end=end)) %>% select(!c(start, end, interval))

all_survival_inf_clean_vacc_demo_risk <- all_survival_inf_clean_vacc_demo %>% left_join(risk, by=c("primary"="ResidentId")) %>% 
  mutate(overlap_risk = intersect(intersection, risk_interval)) %>%
  filter(!is.na(overlap_risk)) %>% 
  mutate(days_risk=time_length(overlap_risk, unit = "day")+1) %>% 
  group_by(id) %>% 
  mutate(risk.primary=sum(days_risk*Value)/sum(days_risk)) %>% 
  summarise_all(first) %>%
  select(!c(risk_interval, overlap_risk, days_risk, Value))

all_survival_inf_clean_vacc_demo_risk <- all_survival_inf_clean_vacc_demo_risk %>% left_join(risk, by=c("secondary"="ResidentId")) %>% 
  mutate(overlap_risk = intersect(intersection, risk_interval)) %>%
  filter(!is.na(overlap_risk)) %>% 
  mutate(days_risk=time_length(overlap_risk, unit = "day")+1) %>% 
  group_by(id) %>% 
  mutate(risk.secondary=sum(days_risk*Value)/sum(days_risk)) %>% 
  summarise_all(first) %>%
  select(!c(risk_interval, overlap_risk, days_risk, Value))


write_csv(all_survival_inf_clean_vacc_demo_risk, "survival_data/allvacc_dose_noincarcreq_priorinf_infvacc081423v2.csv")

