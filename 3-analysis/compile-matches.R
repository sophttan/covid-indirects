d <- read_csv("survival_data/allvacc_dose_noincarcreq_priorinf_infvacc081423.csv")
d_sum <- d %>% group_by(Institution, treatment) %>% summarise(n=n())

setwd("D:/CCHCS_premium/st/indirects/cleaned-data/matching_data_092223")
files  <- list.files(pattern = "prematch_institution.+\\.csv")
tables <- lapply(files, read_csv)
prematched <- do.call(rbind , tables)

files  <- list.files(pattern = "^institution.+\\.csv")
tables <- lapply(files, read_csv)
matched <- do.call(rbind , tables)

library(smd)
summary_pre <- prematched %>% ungroup() %>% 
  summarize_at(
    .vars = c("time_since_inf.primary", "time_since_inf.secondary",
              "age.primary", "age.secondary",
              "risk.primary", "risk.secondary", "ps.secondary"),
    .funs = list(smd = ~ smd(., g = treatment)$estimate)) %>% 
  pivot_longer(cols=names(.),
               values_to = "smd before", 
               names_to = "variable")

summary_post <- matched %>% ungroup() %>% 
  summarize_at(
    .vars = c("time_since_inf.primary", "time_since_inf.secondary",
              "age.primary", "age.secondary",
              "risk.primary", "risk.secondary", "ps.secondary"),
    .funs = list(smd = ~ smd(., g = treatment)$estimate))%>% 
  pivot_longer(cols=names(.),
               values_to = "smd after", 
               names_to = "variable")

summary_pre%>%full_join(summary_post)%>%mutate_at(c("smd before","smd after"), round, digits=3) %>% view()

matched <- matched %>% 
  mutate(treatment=factor(treatment, labels=c("Control", "Treatment")))

prematched %>% group_by(treatment) %>% summarise(n=n())
matched %>% group_by(treatment) %>% summarise(n=n())

matched <- matched %>% group_by(Institution, BuildingId, subclass) %>% mutate(subclass=cur_group_id())
matched <- matched %>% arrange(subclass) %>% rowwise() %>%
  mutate(adjusted_start=first+5, adjusted_end=min(as.Date("2022-12-15"), last+5)) %>% 
  mutate(interval = interval(adjusted_start, adjusted_end)) %>%
  group_by(Institution, BuildingId, subclass) %>%
  mutate(intersect=intersect(interval[1], interval[2])) %>% 
  mutate(final_start=int_start(intersect) %>% as.Date(), final_end=int_end(intersect) %>% as.Date())
matched <- matched %>% ungroup() %>% mutate(id=1:n())

matched %>% ggplot(aes(time_since_inf.primary, group=treatment, fill=treatment)) + geom_histogram(position = "identity", alpha=0.5) + 
  xlab("Time since most recent infection primary resident") + 
  ylab("Count")
matched %>% group_by(treatment) %>% select(time_since_inf.primary) %>% 
  summarise_all(.funs = list(mean=mean, q1=~quantile(., probs = 0.25), q3=~quantile(., probs = 0.75)))

matched %>% ggplot(aes(time_since_inf.secondary, group=treatment, fill=treatment)) + geom_histogram(position = "identity", alpha=0.5) + 
  xlab("Time since most recent infection secondary resident") + 
  ylab("Count")
matched %>% group_by(treatment) %>% select(time_since_inf.secondary) %>% 
  summarise_all(.funs = list(mean=mean, q1=~quantile(., probs = 0.25), q3=~quantile(., probs = 0.75)))

matched %>% filter(vacc.primary>0) %>% ggplot(aes(time_since_vacc.primary, group=treatment, fill=treatment)) + geom_histogram(position = "identity", alpha=0.5) + 
  xlab("Time since most recent vaccination primary resident") + 
  ylab("Count")
matched %>% filter(vacc.primary>0) %>% group_by(treatment) %>% select(time_since_vacc.primary) %>% 
  summarise_all(.funs = list(mean=mean, q1=~quantile(., probs = 0.25), q3=~quantile(., probs = 0.75)))

matched %>% ggplot(aes(age.primary, group=treatment, fill=treatment)) + geom_histogram(position = "identity", alpha=0.5) + 
  xlab("Age primary resident") + 
  ylab("Count")
matched %>% group_by(treatment) %>% select(age.primary) %>% 
  summarise_all(.funs = list(mean=mean, q1=~quantile(., probs = 0.25), q3=~quantile(., probs = 0.75)))

matched %>% ggplot(aes(age.secondary, group=treatment, fill=treatment)) + geom_histogram(position = "identity", alpha=0.5) + 
  xlab("Age secondary resident") + 
  ylab("Count")
matched %>% group_by(treatment) %>% select(age.secondary) %>% 
  summarise_all(.funs = list(mean=mean, q1=~quantile(., probs = 0.25), q3=~quantile(., probs = 0.75)))

matched %>% ggplot(aes(ps.secondary, group=treatment, fill=treatment)) + geom_histogram(position = "identity", alpha=0.5) + 
  xlab("Propensity score in secondary resident") + 
  ylab("Count")
matched %>% group_by(treatment) %>% select(ps.secondary) %>% 
  summarise_all(.funs = list(mean=mean, q1=~quantile(., probs = 0.25), q3=~quantile(., probs = 0.75)))

matched_summary <- matched %>% group_by(Institution, treatment) %>% summarise(n=n())
d_sum <- d_sum %>% mutate(treatment = factor(treatment, labels=c("Control", "Treatment")))
d_sum %>% inner_join(matched_summary, by=c("Institution", "treatment"), suffix = c(".old", ".new"))


testing <- read_csv("D:/CCHCS_premium/st/indirects/cleaned-data/testing_vacc_clean.csv") %>% 
  select(ResidentId, Day, Result, num_pos) %>% filter(!Result%>%is.na())

risk <- read_csv("D:/CCHCS_premium/st/indirects/cleaned-data/covid_risk_score.csv")
fix_intersection <- function(v) {
  v%>%str_extract_all("[0-9]{4}-[0-9]{2}-[0-9]{2}", simplify = T)
}
intersection <- fix_intersection(risk$interval)
risk$start <- intersection[,1]%>%as.vector()%>%as.Date()
risk$end <- intersection[,2]%>%as.vector()%>%as.Date()
risk <- risk %>% mutate(risk_interval=interval(start=start, end=end)) %>% select(!c(start, end, interval))

find_survival <- function(tbl) {
  add_testing <- function(tbl) {
    tbl %>% left_join(testing, by=c("ResidentId.primary"="ResidentId"))
  }
  
  filter_testing <- function(tbl) {
    tbl <- tbl %>% mutate(has_test=any(Day>=final_start&Day<=final_end))
    tbl %>% filter((Day >= final_start & Day <= final_end)|(!has_test&Day==first(Day))) %>% 
      mutate(Day=if_else(!has_test, as.Date(NA), Day), 
             Result=if_else(!has_test, as.character(NA), Result))
  }
  
  tbl %>% add_testing() %>% 
    group_by(id_stable) %>% 
    filter_testing() %>%
    mutate(last = ifelse(any(Result=="Positive",na.rm=T)&Result=="Positive", Result=="Positive", Day==last(Day))) %>% 
    filter(last|!has_test) %>% summarise_all(first) %>% select(!c(last)) %>% 
    mutate(final_end = if_else(!has_test|Result!="Positive", final_end, Day)) %>%
    mutate(survival_time=as.numeric(final_end-final_start)+1) 
}

survival_data <- function(tbl) {
  tbl <- tbl %>% find_survival()
  
  tbl <- tbl %>%
    mutate(status=ifelse(!Result%>%is.na()&Result=="Positive", 1, 0), 
           Institution=as.factor(Institution),
           BuildingId=as.factor(BuildingId),
           subclass=as.factor(subclass)) 
  
  tbl <- tbl %>% left_join(risk, by=c("ResidentId.primary"="ResidentId")) %>% 
    mutate(overlap_risk = intersect(intersect, risk_interval)) %>%
    filter(!is.na(overlap_risk)) %>% 
    mutate(days_risk=time_length(overlap_risk, unit = "day")+1) %>% 
    group_by(id_stable) %>% 
    mutate(risk.primary=sum(days_risk*Value)/sum(days_risk)) %>% 
    summarise_all(first) %>%
    select(!c(risk_interval, overlap_risk, days_risk, Value))
  
  tbl <- tbl %>% left_join(risk, by=c("ResidentId.secondary"="ResidentId")) %>% 
    mutate(overlap_risk = intersect(intersect, risk_interval)) %>%
    filter(!is.na(overlap_risk)) %>% 
    mutate(days_risk=time_length(overlap_risk, unit = "day")+1) %>% 
    group_by(id_stable) %>% 
    mutate(risk.secondary=sum(days_risk*Value)/sum(days_risk)) %>% 
    summarise_all(first) %>%
    select(!c(risk_interval, overlap_risk, days_risk, Value))
}

prepare_month_survival_data <- function(tbl) {
  # join time dataset with data
  dtime <- tbl %>% cross_join(dates) %>% 
    filter(final_start <= month.first | (final_start >= month.first & final_start <= month.last)) %>%
    filter(final_end >= month.last | (final_end <= month.last & final_end >= month.first)) %>% 
    rowwise() %>%
    mutate(time1=(max(final_start, month.first)-final_start)%>%as.numeric(),
           time2=(min(final_end, month.last)-final_start)%>%as.numeric()) %>%
    mutate(time2=min(time2+1, first(survival_time))) %>%
    mutate(status=if_else(time2!=first(survival_time), 0, status)) %>% 
    filter(time1<=time2)
  
  dtime %>% mutate(month=factor(month, levels=0:3))
}

final_data <- survival_data(matched)

dates <- data.frame(date=seq(as.Date("2021-12-15"), as.Date("2022-12-15"), by=1))
# dates <- dates %>% mutate(day=0:(n()-1), month=floor(day/30.5)) %>% rowwise() %>% mutate(month=min(month, 11))
dates <- dates %>% mutate(month=floor((0:(n()-1))/91.5)) %>% rowwise() %>% 
  mutate(month=min(month, 11)) %>%
  group_by(month) %>% 
  filter(date==first(date)|date==last(date)) %>% 
  mutate(group=c("first", "last")) %>%
  pivot_wider(id_cols=month, names_from=group, names_prefix = "month.", values_from=date)

final_data_month <- prepare_month_survival_data(final_data)
final_data_month %>% group_by(id_stable)

final_data %>% group_by(treatment) %>% summarise(inc=sum(status)/sum(survival_time)*100000)
final_data
fit <- survfit(Surv(survival_time, status, type="right")~treatment, data = final_data)
autoplot(fit) + 
  xlab("Time (days)") + ylab("Survival (no SARS-CoV-2 infection)") + 
  scale_fill_discrete(name=element_blank(), labels=c("Control", "Treatment")) + 
  guides(color=F)


results <- coxph(Surv(time1, time2, status) ~ 
                   treatment + vacc.primary + 
                   time_since_inf.primary + time_since_inf.secondary + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   month + Institution +
                   frailty(subclass), 
                 data=final_data_month)
res <- (cox.zph(results))$table %>% as.data.frame() %>% mutate(var=row.names(.))
res

clean_results <- function(results) {
  summary(results)%>%coef()%>%as.data.frame()%>%
    mutate(lb=(coef-2*`se(coef)`)%>%exp(), ub=(coef+2*`se(coef)`)%>%exp(), coef=coef%>%exp()) %>%
    select(coef, lb, ub, p) %>% round(4)
}

results <- coxph(Surv(time1, time2, status) ~ 
                   treatment + vacc.primary + 
                   tt(time_since_inf.primary) + tt(time_since_inf.secondary) + 
                   age.primary + age.secondary + risk.primary + risk.secondary + 
                   month + 
                   frailty(subclass), 
                 data=final_data_month, 
                 tt=list(function(x,t,...){time_since_inf.primary<-x+t/30.417}, 
                         function(x,t,...){time_since_inf.secondary<-x+t/30.417}))
res <- clean_results(results) %>% mutate(var=row.names(.))
row.names(res) <- NULL
res
