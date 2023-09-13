# Sophia Tan 8/16/23
# explore variability

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
  mutate(both_test_diff_vacc = all(test)&num_dose_grouped[1]!=num_dose_grouped[2])
d %>% mutate(control = all(vacc==0)|(!all(test)&sum(vacc==0&test)==1)) %>% 
  group_by(Institution, BuildingId) %>% summarise(control=sum(control)/2, n=n()/2, control/n) %>% view()

d_subset <- d %>% filter(Institution==2) %>% filter(BuildingId==-409899730)

plot_all_units <- function(d) {
  d %>%
    mutate(possible_control=any(vacc==0)&any(both_test_diff_vacc|(!test&!vacc))) %>% 
    summarise_all(first) %>%
    ungroup() %>% arrange(first, duration) %>% 
    mutate(label=1:n()) %>% 
    ggplot(aes(first, label, color=possible_control)) + 
    geom_point(aes()) + 
    geom_segment(aes(xend=last, yend=label)) + 
    geom_point(aes(x=last)) 
}

plot_all_units2 <- function(d) {
  d %>% arrange(first, duration) %>% 
    mutate(label=1:n()) %>% 
    ggplot(aes(first, label, color=factor(treatment))) + 
    geom_point(aes()) + 
    geom_segment(aes(xend=last, yend=label)) + 
    geom_point(aes(x=last)) 
}

# remove treatment units that don't overlap with any possible control units
# can do the same for any control units but they cannot overlap with any other units
test <- d_subset %>% 
  mutate(possible_control=any(vacc==0)&any(both_test_diff_vacc|(!test&!vacc))) %>% 
  summarise_all(first) %>%
  mutate(duration_interval=interval(first, last)) %>%
  select(id, duration_interval, possible_control) 
remove <- test %>% filter(!possible_control) %>% 
  cross_join(test%>%filter(possible_control)) %>% 
  mutate(overlap=(intersect(duration_interval.x, duration_interval.y)%>%time_length("day")%>%as.numeric())+1) %>% 
  group_by(id.x) %>% filter(all(overlap<7, na.rm=T)) %>% group_keys()

filter_subset <- test %>% cross_join(test) %>% 
  mutate(overlap=(intersect(duration_interval.x, duration_interval.y)%>%time_length("day")%>%as.numeric())+1) %>% 
  replace_na(list(overlap=0)) %>% 
  filter(id.x!=id.y) %>% 
  group_by(id.x) %>%
  mutate(remove_control=all(possible_control.x) & all(overlap<7)) %>%
  mutate(remove_treatment=all(!possible_control.x) & sum(possible_control.y & overlap>7)==0) %>%
  mutate(fix_control=all(possible_control.x) & sum(possible_control.y & overlap>7)==0) %>% summarise_all(first)

remove <- filter_subset%>%filter(remove_control|remove_treatment) 
fix <- filter_subset%>%filter(fix_control)
d_subset <- d_subset %>% filter(!id %in% remove$id.x) %>% 
  mutate(test=if_else(id%in%fix$id.x & vacc==0, F, test), 
         both_test_diff_vacc = if_else(id %in% fix$id.x, F, both_test_diff_vacc))

unit_info <- d_subset %>% select(id, Institution, BuildingId, RoomId, first, last, duration) %>% summarise_all(first)
resident_info <- d_subset %>% select(!names(unit_info)) 

generate_distance_matrix <- function(tbl) {
  overlap <- expand.grid(x=tbl$label,y=tbl$label) %>% 
    left_join(tbl %>% dplyr::select(label, duration_interval), by=c("x"="label")) %>% 
    left_join(tbl %>% dplyr::select(label, duration_interval), by=c("y"="label")) %>% 
    mutate(overlap=intersect(duration_interval.x,duration_interval.y), 
           duration_overlap=366-(time_length(overlap, unit = "day")+1))
  
  duration_overlap_wide <- overlap%>% 
    dplyr::select(x,y,duration_overlap) %>% 
    replace_na(list(duration_overlap=1000)) %>% 
    pivot_wider(id_cols = x, names_from = y, values_from = duration_overlap)
  duration_overlap_wide <- duration_overlap_wide %>% 
    as.data.frame(row.names = .$x) %>% dplyr::select(!x)
  
  duration_overlap_wide %>% as.matrix()
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
  matchit(treatment ~ Institution + BuildingId + duration_interval + num_dose_grouped.primary,
          data = a,
          distance = generate_distance_matrix(a), 
          exact = treatment ~ Institution + BuildingId + num_dose_grouped.primary, 
          ratio = 5, min.controls = 1, max.controls = 6, method="optimal") %>%
    get_matches()
  
}

matching <- function(tbl) {
  m <- tbl %>% 
    matching_specifications() %>% 
    group_by(Institution, subclass) %>% 
    mutate(subclass=cur_group_id())
  
  m %>% 
    group_by(subclass) %>% 
    arrange(subclass, desc(treatment)) %>% 
    mutate(intersection=intersect(first(duration_interval),duration_interval)) %>% 
    mutate(start=as.Date(int_start(intersection)), end=as.Date(int_end(intersection)),
           intersect=time_length(intersection,unit="day")+1) %>% 
    replace_na(list(intersect=0)) %>% 
    filter(treatment==1|intersect>=7) %>% filter(n()>1) %>% 
    ungroup() %>% 
    mutate(id=1:n())
}

post_match_processing <- function(tbl) {
  tbl <- tbl %>% group_by(subclass)
  treatment <- tbl %>% filter(treatment==0)
  control <- tbl %>% filter(treatment==1)
  
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
  
  control %>% rbind(treatment_filter) %>% arrange(subclass, id) %>% 
    rowwise() %>% 
    mutate(final_start = best_start+5, final_end = min(as.Date("2022-12-15"), end + 5))
}

test_assignment <- function(i, assignments, data_mixed, data_other) {
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
  
  if(nrow(filtered)==0){return()}
  
  matched <- matching(filtered%>% mutate(label=1:n())) 
  
  processed <- post_match_processing(matched) 
  
  processed %>% group_by(treatment) %>% summarise(n=n(), obs_time=sum(final_end-final_start+1)) %>% mutate(i=i)
}

all <- resident_info %>% 
  arrange(id, desc(test), num_dose_grouped) %>% 
  mutate(type=c("primary", "secondary"))

all_mixed <- all %>% filter(both_test_diff_vacc&any(!vacc))
all_other <- all%>% filter(!id %in% all_mixed$id)

res <- permutations(n=2,r=all_mixed%>%n_groups(),v=0:1,repeats.allowed=T)
final_processed <- foreach(i=1:nrow(res), .packages=c("tidyverse","lubridate","MatchIt"), .combine=rbind) %dopar%  {
  test_assignment(i, res, all_mixed, all_other)
}

summary_data <- final_processed%>%group_by(i)%>%
  summarise(control=n[2], treatment=n[1], n=sum(n), 
            control_obs_time=obs_time[2], obs_time=sum(obs_time)) 

summary_data %>% 
  ggplot(aes(control, obs_time)) + geom_point()
summary_data %>% 
  ggplot(aes(control, n)) + geom_point()
summary_data %>% 
  ggplot(aes(control, treatment)) + geom_point()


# check variability in vaccinated mixed units
optimum <- (summary_data%>%arrange(desc(obs_time)))$i[1]

assignment <- c(rbind(abs(res[optimum,]-1), res[optimum,]))
all_mixed <- all_mixed %>% ungroup() %>% mutate(type=factor(assignment, labels =c("primary","secondary")))
all_updated <- all_mixed %>% rbind(all_other) %>% group_by(id)

all_mixed <- all_updated %>% filter(both_test_diff_vacc&all(vacc>0))
all_other <- all_updated %>% filter(!id%in%(all_mixed$id)%>%unique())

res <- permutations(n=2,r=all_mixed%>%n_groups(),v=0:1,repeats.allowed=T)
final_processed4 <- foreach(i=1:nrow(res), .packages=c("tidyverse","lubridate","MatchIt"), .combine=rbind) %dopar%  {
  test_assignment(i, res, all_mixed, all_other)
}

summary_data4 <- final_processed4%>%group_by(i)%>%
  summarise(control=n[2], treatment=n[1], n=sum(n), 
            control_obs_time=obs_time[2], obs_time=sum(obs_time)) 

par(mfrow=c(3,2))
summary_data$control%>%table()%>%barplot(main="Variation from control unit assignment\nNumber of control units")
summary_data4$control%>%table()%>%barplot(main="Variation from treatment unit assignment")

summary_data$control_obs_time%>%as.numeric()%>%hist(main="Observation time control")
summary_data4$control_obs_time%>%as.numeric()%>%hist(main="")

summary_data$obs_time%>%as.numeric()%>%hist(main="Total observation time")
summary_data4$obs_time%>%as.numeric()%>%hist(main="")

