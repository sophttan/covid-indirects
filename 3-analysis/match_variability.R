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

registerDoParallel(4)

d <- read_csv("allvacc_full_data_prematching_relaxincarceration_priorinf_bydose_082523.csv") %>% group_by(id) 
  
d %>% mutate(both_has_test=all(test)) %>% group_by(Institution) %>% summarise(n=n()/2, both_has_test=sum(both_has_test)/2/n)
d %>% mutate(both_has_test=all(test), same_vacc=num_dose_grouped[1]==num_dose_grouped[2], diff_vacc_unvacc=!same_vacc&any(vacc==0)) %>% 
  group_by(Institution) %>% 
  summarise(n=n()/2, 
            both_test=sum(both_has_test)/2, 
            diff_vacc_both_test=sum(!same_vacc&both_has_test)/2, 
            diff_vacc_one_unvacc_both_test=sum(diff_vacc_unvacc&both_has_test)/2)

d_subset <- d %>% filter(Institution==35) 

unit_info <- d_subset %>% select(id, Institution, BuildingId, RoomId, first, last, duration) %>% summarise_all(first)
resident_info <- d_subset %>% select(!names(unit_info)) %>% arrange(id, desc(test)) 

generate_distance_matrix <- function(tbl) {
  overlap <- expand.grid(x=tbl$label,y=tbl$label) %>% 
    left_join(tbl %>% dplyr::select(label, duration_interval), by=c("x"="label")) %>% 
    left_join(tbl %>% dplyr::select(label, duration_interval), by=c("y"="label")) %>% 
    mutate(overlap=intersect(duration_interval.x,duration_interval.y), 
           duration_overlap=366-(time_length(overlap, unit = "day")+1))
  
  duration_overlap_wide <- overlap%>% 
    dplyr::select(x,y,duration_overlap) %>% replace_na(list(duration_overlap=1000)) %>% 
    pivot_wider(id_cols = x, names_from = y, values_from = duration_overlap)
  duration_overlap_wide <- duration_overlap_wide %>% as.data.frame(row.names = .$x) %>% dplyr::select(!x)
  
  duration_overlap_wide %>% as.matrix()
}

matching_specifications <- function(tbl) {
  # Iterate using the for loop from 1 to 5
  # And print the square of each number
  # Using parallelism
  foreach(set=c(35), .packages=c("dplyr","lubridate","MatchIt"), .combine=rbind) %do%  {
    a <- tbl %>% filter(Institution==set) %>% mutate(label=1:n())
    matchit(treatment ~ Institution + BuildingId + duration_interval + num_dose_grouped.primary,
            data = a,
            distance = generate_distance_matrix(a), 
            exact = treatment ~ Institution + BuildingId + num_dose_grouped.primary, 
            ratio = 5, min.controls = 1, max.controls = 6, method="optimal") %>%
      get_matches()
  }
}

matching <- function(tbl) {
  m <- matching_specifications(tbl) %>% group_by(Institution, subclass) %>% 
    mutate(subclass=cur_group_id())
  
  m <- m %>% group_by(subclass) %>% arrange(subclass, desc(treatment)) %>%
    mutate(intersect=time_length(intersect(first(duration_interval),duration_interval),unit="day")+1) %>% 
    replace_na(list(intersect=0)) %>% 
    mutate(include=any(intersect[2:n()]>0)) %>% 
    filter(include|treatment==0) %>%
    ungroup() %>% select(!c(id, subclass, weights)) %>% mutate(label=1:nrow(.))
  
  m <- matching_specifications(m) %>% group_by(Institution, subclass) %>% 
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

final_data_summary <- NULL
final_matched <- NULL
final_processed <- NULL
pdf("D:/CCHCS_premium/st/indirects/testing/match_variability.pdf")
for (i in 1:20) {
  print(i)
  resident_info_primary <- resident_info %>%  
    mutate(primary_first=if_else(!all(test), "primary", sample(c("primary", "secondary"), 1))) %>% 
    mutate(type=case_when(ResidentId==first(ResidentId)~primary_first,
                          first(primary_first)=="primary"~"secondary",
                          T~"primary")) 
  
  data <- resident_info_primary %>% as.data.frame() %>% 
    reshape(idvar = "id",
            timevar = "type",
            direction = "wide") %>% 
    mutate(both_has_test = test.primary & test.secondary) %>% 
    select(!primary_first.secondary) %>% rename("primary_first"="primary_first.primary")
  
  data <- unit_info %>% left_join(data) %>% rename("id_stable"="id") 
  
  data <- data %>% ungroup() %>%
    mutate(treatment = ifelse(vacc.secondary==0, 1, 0)) %>% 
    mutate(duration_interval = interval(first, last)) 
  
  data_summary <- data %>% group_by(Institution, treatment) %>% 
    summarise(n=n()) %>% 
    mutate(i=i) #, prop=sum(primary_first=="primary"&both_has_test)/sum(both_has_test))
  
  matched <- matching(data) %>% mutate(i=i)
  
  processed <- post_match_processing(matched) %>% mutate(i=i)
  
  matched %>% filter(BuildingId==-409899730) %>% 
    mutate(star=both_has_test&(vacc.primary!=vacc.secondary&(vacc.primary==0|vacc.secondary==0))) %>%
    plot_matches(title=i) %>% print()
  
  final_data_summary <- rbind(final_data_summary, data_summary)
  final_matched <- rbind(final_matched, matched)
  final_processed <- rbind(final_processed, processed)
}
dev.off()

final_data_summary %>%
  pivot_wider(id_cols = "i", names_from = "treatment", values_from = "n") %>% summary()

final_matched %>% filter(subclass==1) %>% 
  select(i, id, subclass, id_stable, both_has_test, 
         ResidentId.primary, ResidentId.secondary, vacc.primary, vacc.secondary) %>% 
  group_by(i)

plot_matches <- function(d, title="", subtitle="") {
  d <- d %>% mutate(subclass1=match(subclass, unique(subclass)))
  d <- d %>% group_by(subclass1) %>% 
    mutate(subclass1=subclass1*6-0.9*0:(n()-1)) %>% ungroup()
  
  p <- d %>%
    ggplot(aes(x = as.POSIXct(first), y = subclass1, 
               colour = as.factor(treatment))) +
    geom_point(size = 1) +
    geom_point(aes(x = as.POSIXct(last)), size = 1) +
    geom_segment(aes(xend = as.POSIXct(last), yend = subclass1)) +
    geom_text(aes(label=paste(id_stable, star)), nudge_x = -50*3600*24, size=3) + 
    scale_x_datetime("Duration of co-residence", 
                     limits = c(as.POSIXct("2021-09-01"), as.POSIXct("2022-12-31")), 
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

matched %>% filter(BuildingId==-409899730) %>% 
  mutate(star=both_has_test&(vacc.primary!=vacc.secondary&(vacc.primary==0|vacc.secondary==0))) %>%
  plot_matches()

data %>% nrow()
data %>% filter(both_has_test) %>% nrow()
data %>% filter(both_has_test&vacc.primary!=vacc.secondary) %>% nrow()
data %>% filter(BuildingId==-409899522) %>% 
  filter(id_stable==4208|id_stable==4209)

final_matched %>% group_by(i, subclass) %>% filter(BuildingId==-409899522)
final_matched %>% filter(BuildingId==-409899522) %>% group_by(i) %>% select(i, subclass, id_stable, treatment, vacc.primary) %>% 
  filter(any(id_stable==12285|id_stable==12290)) %>% view()

final_matched %>% filter(BuildingId==-409899522) %>% group_by(i) %>% select(i, subclass, id_stable, treatment, vacc.primary) %>% 
  filter(all(id_stable!=12285&id_stable!=12290))


a <- final_matched%>%filter(BuildingId==-409899730)%>%
  filter(treatment==1)%>%#filter(!(both_has_test&vacc.primary!=vacc.secondary)) %>%
  select(i, id_stable)
mysplit <- split(a$id_stable, a$i)
crossprod(table(stack(mysplit)))


a <- final_matched%>%filter(BuildingId==-409899730)%>%
  filter(treatment==0)%>%#filter(!(both_has_test&vacc.primary!=vacc.secondary)) %>%
  select(i, id_stable)
mysplit <- split(a$id_stable, a$i)
crossprod(table(stack(mysplit)))
