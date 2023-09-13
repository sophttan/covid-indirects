# Sophia Tan 9/6/23

rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)
library(foreach)
library(doParallel)
library(survival)
library(ggfortify)
library(gtsummary)
library(rms)
library(MatchIt)
library(gtools)

registerDoParallel(4)

d <- read_csv("allvacc_full_data_prematching_relaxincarceration_priorinf_bydose_082523.csv") %>% 
  group_by(id) %>%
  mutate(both_test = all(test)) 

d_subset <- d %>% filter(Institution==2) 
buildings <- d_subset$BuildingId %>% unique()

risk <- read_csv("covid_risk_score.csv") %>% 
  filter(ResidentId %in% d_subset$ResidentId)
fix_intersection <- function(v) {
  v%>%str_extract_all("[0-9]{4}-[0-9]{2}-[0-9]{2}", simplify = T)
}
intersection <- fix_intersection(risk$interval)
risk$start <- intersection[,1]%>%as.vector()%>%as.Date()
risk$end <- intersection[,2]%>%as.vector()%>%as.Date()
risk <- risk %>% 
  mutate(risk_interval=interval(start=start, end=end)) %>% 
  select(!c(start, end, interval)) %>% rename("risk"="Value")
d_subset <- d_subset %>% left_join(risk, by=c("ResidentId")) %>% 
  filter(first%within%risk_interval) %>% 
  select(!c(risk_interval))
d_subset <- d_subset %>% mutate(time_since_inf = (difftime(first, Day_inf)%>%as.numeric())/30.417,
                                time_since_vacc = (difftime(first, Day_vacc)%>%as.numeric())/30.417) %>%
  replace_na(list(time_since_vacc=50))


generate_distance_matrix <- function(tbl) {
  # estimate propensity scores (age, COVID-19 risk, and prior infection history)
  distance_primary <- tbl %>% select(time_since_inf.primary, age.primary, risk.primary) %>% mutate_all(scale) %>% dist(diag = T, upper = T) %>% as.matrix()
  distance_primary <- (distance_primary - min(distance_primary))/(max(distance_primary)-min(distance_primary))
  
  distance_propensity <- dist(as.matrix(tbl%>%select(ps)), diag = T, upper = T) %>% as.matrix()
  distance_propensity <- (distance_propensity - min(distance_propensity))/(max(distance_propensity)-min(distance_propensity))
  
  total_distance <- distance_primary + distance_propensity

  overlap <- expand.grid(x=tbl$label,y=tbl$label) %>% 
    left_join(tbl %>% dplyr::select(label, first, duration_interval), by=c("x"="label")) %>% 
    left_join(tbl %>% dplyr::select(label, first, duration_interval), by=c("y"="label")) %>% 
    mutate(eligible=abs(first.x-first.y)<=6) %>%
    mutate(overlap=!(intersect(duration_interval.x, duration_interval.y)%>%time_length())%>%is.na())
  
  eligible_wide <- overlap%>% 
    dplyr::select(x,y,eligible) %>% 
    pivot_wider(id_cols = x, names_from = y, values_from = eligible)
  eligible_wide <- eligible_wide %>% 
    as.data.frame(row.names = .$x) %>% dplyr::select(!x)
  
  overlap_wide <- overlap%>% 
    dplyr::select(x,y,overlap) %>% 
    pivot_wider(id_cols = x, names_from = y, values_from = overlap)
  overlap_wide <- overlap_wide %>% 
    as.data.frame(row.names = .$x) %>% dplyr::select(!x)
  
  total_distance[!overlap_wide|!eligible_wide] <- 1000
  total_distance
}

filter_data <- function(tbl) {
  tbl <- tbl %>% mutate(label=1:n())
  tblselect <- tbl %>% select(label, treatment, duration_interval)
  control <- tblselect %>% filter(treatment==1) 
  treatment <- tblselect %>% filter(treatment==0) 
  
  cross_joined <- control %>% cross_join(treatment) %>% 
    mutate(intersection=time_length(intersect(duration_interval.x, duration_interval.y), "day")%>%as.numeric()+1)
  
  remove_controls <- cross_joined %>% 
    group_by(label.x) %>% 
    filter(all(intersection<7,na.rm=T)) %>% group_keys()
  
  remove_treatments <- cross_joined %>% 
    group_by(label.y) %>% 
    filter(all(intersection<7,na.rm=T)) %>% group_keys()
  
  tbl %>% filter(!label %in% remove_controls$label.x & !label %in% remove_treatments$label.y)
}

matching_specifications <- function(tbl) {
  a <- tbl %>% mutate(label=1:n())
  matchit(treatment ~ Institution + BuildingId + num_dose_grouped.primary + 
            age.primary + risk.primary + time_since_inf.primary +
            age.secondary + risk.secondary + time_since_inf.secondary,
          data = a,
          distance = generate_distance_matrix(a), 
          exact = treatment ~ Institution + BuildingId + num_dose_grouped.primary,
          ratio = 1, method="optimal")  %>%
    get_matches()
  
}

matching <- function(tbl) {
  m <- tbl %>% 
    matching_specifications() %>% 
    group_by(subclass) %>% 
    mutate(subclass=cur_group_id())
  
  m <- m %>% 
    group_by(subclass) %>% 
    arrange(subclass, desc(treatment)) %>% 
    filter(!intersect(first(duration_interval), duration_interval)%>%time_length()%>%is.na())%>%
    filter(abs(first(first)-first)<=6) %>% filter(n()>1) %>% 
    ungroup()
  
  if(nrow(m)==0){return()}
  
  m %>% mutate(id=1:n())
}

post_match_processing <- function(tbl) {
  tbl %>% group_by(subclass) %>% 
    mutate(overlap=intersect(duration_interval[1], duration_interval[2])) %>% 
    mutate(final_start = int_start(first(overlap))+5, 
           final_end = int_end(first(overlap))+5)
}

fix_assignment <- function(i, assignments, data_mixed, data_other) {
  matched <- matching(generate_assignment(i, assignments, data_mixed, data_other)%>% mutate(label=1:n())) 
  
  post_match_processing(matched) 
}

generate_assignment <- function(i, assignments, data_mixed, data_other) {
  assignment_full <- c(rbind(abs(assignments[i,]-1), assignments[i,]))
  a <- data_mixed %>% ungroup() %>% mutate(type=factor(assignment_full, labels =c("primary","secondary")))
  full <- a %>% rbind(data_other) 
  data <- full %>% as.data.frame() %>% 
    reshape(idvar = "id",
            timevar = "type",
            direction = "wide") %>% 
    mutate(both_has_test = test.primary & test.secondary) 
  
  data <- data %>% left_join(unit_info) %>% rename("id_stable"="id") 
  
  data <- data %>% ungroup() %>%
    mutate(treatment = ifelse(vacc.secondary==0, 1, 0)) %>% 
    mutate(duration_interval = interval(first, last)) 
  
  filtered <- data %>% filter_data() 
  
  filtered
}

test_assignment <- function(data) {
  if(nrow(data)==0){return()}

  ps <- glm(treatment ~ age.secondary + risk.secondary + 
              time_since_inf.secondary, data = data, family = binomial(link='logit'))
  data <- data %>% mutate(logodds = predict.glm(ps), ps=exp(logodds)/(1+exp(logodds)))
  
  matched <- matching(data%>% mutate(label=1:n())) 
  
  if(matched%>%is.null()){return()}
  
  processed <- post_match_processing(matched) %>% mutate(i=i)
  
  processed 
}

results <- NULL
for (building in buildings) {
  print(building)
  # remove treatment units that don't overlap with any possible control units
  # can do the same for any control units but they cannot overlap with any other units
  d_building <- d_subset %>% filter(BuildingId==building)
  
  if(n_groups(d_building)==1|all(d_building$vacc>0)){next}
  
  test <- d_building %>% 
    mutate(possible_control=all(vacc==0)|any(!vacc&!test)|(any(!vacc)&all(test))) %>% 
    summarise_all(first) %>%
    mutate(duration_interval=interval(first, last)) %>%
    select(id, num_dose_grouped, first, duration_interval, possible_control) 
  
  remove <- test %>% filter(!possible_control) %>% 
    cross_join(test%>%filter(possible_control)) %>% 
    mutate(eligible = abs(first.x-first.y), 
           overlap=(intersect(duration_interval.x, duration_interval.y)%>%time_length("day")%>%as.numeric())+1) %>% 
    group_by(id.x) %>% filter(all(overlap<1, na.rm=T) & all(eligible>6)) %>% group_keys()
  
  filter_subset <- test %>% cross_join(test) %>% 
    mutate(eligible = abs(first.x-first.y), 
           overlap=(intersect(duration_interval.x, duration_interval.y)%>%time_length("day")%>%as.numeric())+1) %>% 
    replace_na(list(overlap=0)) %>% 
    filter(id.x!=id.y) %>% 
    group_by(id.x) %>%
    mutate(remove_control=all(possible_control.x) & all(eligible>6)) %>%
    mutate(remove_treatment=all(!possible_control.x) & sum(possible_control.y & eligible <= 6)==0) %>%
    mutate(fix_control=all(possible_control.x) & sum(possible_control.y & eligible <= 6)==0) %>% summarise_all(first)
  
  remove <- filter_subset%>%filter(remove_control|remove_treatment) 
  fix <- filter_subset%>%filter(fix_control)
  d_building <- d_building %>% filter(!id %in% remove$id.x) %>% 
    mutate(test=if_else(id%in%fix$id.x & vacc==0, F, test), 
           both_test = if_else(id %in% fix$id.x, F, both_test))
  
  if(nrow(d_building)==0){next}
  
  unit_info <- d_building %>% select(id, Institution, BuildingId, RoomId, first, last, duration) %>% summarise_all(first)
  resident_info <- d_building %>% select(!names(unit_info)) 
  
  resident_info <- resident_info %>% 
    arrange(id, desc(test), num_dose_grouped) %>% 
    mutate(type=c("primary", "secondary"))
  
  all_mixed <- resident_info %>% filter(both_test&any(!vacc))
  all_other <- resident_info %>% filter(!id %in% all_mixed$id)
  
  res <- permutations(n=2,r=all_mixed%>%n_groups(),v=0:1,repeats.allowed=T)
  final_processed <- foreach(i=1:nrow(res), .packages=c("tidyverse","lubridate","MatchIt"), .combine=rbind) %dopar%  {
    for_matching <- generate_assignment(i, res, all_mixed, all_other)
    test_assignment(for_matching)
  }
  
  summary_data <- final_processed%>%group_by(i, subclass)%>%
    summarise(ps=abs(diff(ps))) %>% 
    group_by(i) %>%
    summarise(control=unique(subclass)%>%length(), meanps=mean(ps), ps=sum(ps)) 
  print(head(summary_data))
  
  optimum <- (summary_data%>%arrange(desc(control), ps))
  print(optimum)
  
  assignment <- c(rbind(abs(res[optimum,]-1), res[optimum,]))
  all_mixed <- all_mixed %>% ungroup() %>% mutate(type=factor(assignment, labels =c("primary","secondary")))
  all_updated <- all_mixed %>% rbind(all_other) %>% group_by(id)
  
  all_mixed <- all_updated %>% filter(both_test&all(vacc>0))
  all_other <- all_updated %>% filter(!id%in%(all_mixed$id)%>%unique())
  
  res <- permutations(n=2,r=all_mixed%>%n_groups(),v=0:1,repeats.allowed=T)
  final_processed4 <- foreach(i=1:nrow(res), .packages=c("tidyverse","lubridate","MatchIt"), .combine=rbind) %dopar%  {
    for_matching <- generate_assignment(i, res, all_mixed, all_other)
    test_assignment(for_matching)
  }
  
  summary_data4 <- final_processed4%>%group_by(i, subclass)%>%
    summarise(ps=abs(diff(ps))) %>% 
    group_by(i) %>%
    summarise(control=unique(subclass)%>%length(), meanps=mean(ps), ps=sum(ps)) 
  print(head(summary_data4))
  
  optimum <- (summary_data4%>%arrange(desc(control), meanps))
  print(optimum)
  
  results <- rbind(results, fix_assignment(optimum, res, all_mixed, all_other))
} 

for (a in head(optimum)$i) {
  print(plot_matches(final_processed%>%filter(i==a)%>%ungroup()))
}

a <- generate_assignment(1184, res, all_mixed, all_other)
ps <- glm(treatment ~ age.secondary + risk.secondary + time_since_inf.secondary, data = a, family = binomial(link='logit'))
a <- a %>% mutate(logodds = predict.glm(ps), ps=exp(logodds)/(1+exp(logodds)))
a <- a %>% mutate(label=1:n())
matched <- matchit(treatment ~ Institution + BuildingId + num_dose_grouped.primary + 
                     age.primary + risk.primary + time_since_inf.primary +
                     age.secondary + risk.secondary + time_since_inf.secondary + ps,
                   data = a,
                   distance = generate_distance_matrix(a), 
                   exact = treatment ~ Institution + BuildingId + num_dose_grouped.primary,
                   ratio = 1, method="optimal") 
summary(matched)
plot(summary(matched))
plot_matches <- function(d, title="", subtitle="") {
  d <- d %>% arrange(first, subclass) %>% mutate(subclass1=match(subclass, unique(subclass)))
  d <- d %>% group_by(subclass1) %>% 
    mutate(subclass1=subclass1*2-0.5*0:(n()-1)) %>% ungroup()
  
  p <- d %>%
    ggplot(aes(x = as.POSIXct(final_start), y = subclass1, 
               colour = as.factor(treatment))) +
    geom_point(aes(x = as.POSIXct(first)), size = 1.5, alpha=0.7, color="grey70") +
    geom_point(aes(x = as.POSIXct(last)), size = 1.5, alpha=0.7, color="grey70") +
    geom_segment(aes(x = as.POSIXct(first), xend = as.POSIXct(last), yend = subclass1, color=as.factor(treatment)), alpha=0.7, color="grey70") +
    geom_point(size = 1) +
    geom_point(aes(x = as.POSIXct(final_end)), size = 1) +
    geom_segment(aes(xend = as.POSIXct(final_end), yend = subclass1,  color=as.factor(treatment))) +
    geom_text(aes(x=as.POSIXct(first), label=id_stable), nudge_x = -10*3600*24, size=3) + 
    geom_text(aes(x=as.POSIXct(last), label=paste0(num_dose_grouped.primary, ", ", round(ps,2))), nudge_x = 18*3600*24, size=3) + 
    scale_x_datetime("Duration of co-residence", 
                     limits = c(as.POSIXct("2021-11-15"), as.POSIXct("2023-02-01")), 
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

plot_all_units <- function(d) {
  d %>%
    mutate(possible_control=all(vacc==0)|any(!vacc&!test)|(any(!vacc)&all(test))) %>% 
    summarise_all(first) %>%
    ungroup() %>% arrange(first, duration) %>% 
    mutate(label=1:n()) %>% 
    ggplot(aes(first, label, color=possible_control)) + 
    geom_point(aes()) + 
    geom_segment(aes(xend=last, yend=label)) + 
    geom_point(aes(x=last)) + 
    geom_text(aes(x=first, label=id), nudge_x = -10, size=3) + 
    scale_x_date("Time") + 
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
          axis.ticks.y = element_blank())
}
plot_all_units(d_building)

par(mfrow=c(1,1))
summary_data$control%>%table()%>%barplot(main="Variation from control unit assignment\nNumber of control units")
summary_data4$control%>%table()%>%barplot(main="Variation from treatment unit assignment")

summary_data$control_obs_time%>%as.numeric()%>%hist(main="Observation time control")
summary_data4$control_obs_time%>%as.numeric()%>%hist(main="")

summary_data$obs_time%>%as.numeric()%>%hist(main="Total observation time")
summary_data4$obs_time%>%as.numeric()%>%hist(main="")

