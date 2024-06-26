rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(lubridate)
library(tidyverse)
library(MatchIt)
#library(ggbrace)
library(patchwork)

for_matching <- read_csv("allvacc_full_data_prematching_relaxincarceration_priorinf_bydose_052423.csv")

# for_matching <- for_matching %>% ungroup() %>% mutate(label=1:nrow(.))
# vacc <- read_csv("cleaned_vaccination_data.csv")
# for_matching <- for_matching %>% left_join(vacc %>% select(ResidentId, num_dose, Date_offset),
#                                            by=c("secondary"="ResidentId"))
# 
# for_matching <- for_matching %>% group_by(label) %>% 
#   mutate(new_vacc=Date_offset > adjusted_start & Date_offset < adjusted_end) %>% 
#   filter(treatment==1|Date_offset<=last) %>% 
#   arrange(label, desc(Date_offset))
# 
# for_matching %>% select(label, primary, secondary, treatment, first, last, Date_offset, new_vacc)
# 
# unvacc <- for_matching %>% filter(treatment==1) %>% distinct(label, .keep_all = T) %>% select(!c(num_dose, Date_offset, new_vacc))
# vacc_before <- for_matching %>% filter(treatment==0 & all(!new_vacc)) %>% distinct(label, .keep_all = T)
# vacc_during <- for_matching %>% filter(treatment==0 & any(new_vacc))
# 
# vacc_before <- vacc_before %>% 
#   mutate(Date_end = Date_offset + 90) %>% 
#   mutate(eligible = adjusted_start <= Date_end) %>% 
#   rowwise() %>% 
#   mutate(vacc_start = max(Date_offset, adjusted_start), 
#          vacc_end=(min(Date_end, adjusted_end))) 
# 
# vacc_before %>% 
#   select(label, treatment, eligible, adjusted_start, adjusted_end, Date_offset, Date_end, vacc_start, vacc_end)
# 
# vacc_before_final <- vacc_before %>% filter(eligible)
# 
# vacc_during %>% 
#   select(label, treatment, adjusted_start, adjusted_end, Date_offset, num_dose)
# 
# vacc_during <- vacc_during %>%
#   mutate(Date_end = Date_offset + 90) %>% 
#   mutate(eligible = adjusted_start <= Date_end)
# 
# vacc_during_final <- vacc_during %>% filter(eligible) %>% 
#   mutate(overlap = lead(Date_end, 1)>=Date_offset & lead(Date_end, 1)<=Date_end) %>%
#   mutate(keep = !overlap%>%is.na()|n()==1) %>% 
#   rowwise() %>%
#   mutate(vacc_start = max(Date_offset, adjusted_start), 
#          vacc_end=(min(Date_end, adjusted_end))) %>%
#   group_by(label) %>% 
#   mutate(last_start = lead(vacc_start, 1), 
#          last_end = lead(vacc_end, 1)) %>% 
#   mutate(vacc_start=if_else(overlap&!is.na(overlap), last_start, vacc_start)) %>% 
#   filter(keep)
# 
# vacc_during_final <- vacc_during_final %>% distinct(label, vacc_start, vacc_end, .keep_all = T) %>% 
#   mutate(d=vacc_end-vacc_start) %>% arrange(label, vacc_start, desc(d)) %>% summarise_all(first)
# 
# 
# for_matching <- unvacc %>% rbind(vacc_before_final %>% 
#                                    select(!c(adjusted_start, adjusted_end)) %>% 
#                                             rename("adjusted_start"="vacc_start", "adjusted_end"="vacc_end") %>% 
#                                    select(names(unvacc)),
#                                  vacc_during_final %>% 
#                                    select(!c(adjusted_start, adjusted_end)) %>% 
#                                    rename("adjusted_start"="vacc_start", "adjusted_end"="vacc_end") %>% 
#                                    select(names(unvacc)))

generate_distance_matrix <- function(d) {
  overlap <- expand.grid(x=d$label,y=d$label) %>% 
    left_join(d %>% select(label, duration_interval), by=c("x"="label")) %>% 
    left_join(d %>% select(label, duration_interval), by=c("y"="label")) %>% 
    mutate(overlap=intersect(duration_interval.x,duration_interval.y), 
           duration_overlap=366-(time_length(overlap, unit = "day")+1))
  
  overlap_wide <- overlap%>% 
    select(x,y,overlap) %>% pivot_wider(id_cols = x, names_from = y, values_from = overlap)
  overlap_wide <- overlap_wide %>% as.data.frame(row.names = .$x) %>% select(!x)
  
  duration_overlap_wide <- overlap%>% 
    select(x,y,duration_overlap) %>% replace_na(list(duration_overlap=1000)) %>% 
    pivot_wider(id_cols = x, names_from = y, values_from = duration_overlap)
  duration_overlap_wide <- duration_overlap_wide %>% as.data.frame(row.names = .$x) %>% select(!x)
  
  #duration_overlap_wide[duration_overlap_wide > 352] <- Inf
  
  duration_overlap_wide %>% as.matrix()
}

plot_all_units <- function(d) {
  p <- d %>% 
    ggplot(aes(x = int_start(duration_interval), y = label, colour = as.factor(treatment))) +
    geom_segment(aes(xend = int_end(duration_interval), yend = label, color=as.factor(treatment))) +
    geom_point(size = 1) +
    geom_point(aes(x = int_end(duration_interval)), size = 1) +
    scale_x_datetime("Duration of co-residence", date_breaks = "1 month", date_labels ="%b-%y") + 
    geom_text(aes(label=label), nudge_x = -60*60*24*14, color="black") + 
    # stat_brace(data=test_b, 
    #            aes(x=as.POSIXct("2023-01-15"), y=row, group=num_inf, label=num_inf), color="black",
    #            rotate=90, labelsize=3, bending = 0.7) + 
    scale_color_discrete(name="Unit type", labels=c("Treatment", "Control")) +
    guides(color = guide_legend(reverse=TRUE)) + 
    labs(#title=paste("Institution 2", "Building", b),
      subtitle="All units") + 
    theme(#legend.position = "none", 
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),axis.ticks.y = element_blank(), 
      axis.text.x = element_text(angle=90)) 
  
  p
}

plot_matches <- function(d, title="", subtitle="") {
  d <- d %>% arrange(inf.primary, subclass)
  d <- d %>% ungroup() %>% mutate(subclass1=match(subclass, unique(subclass)))
  d <- d %>% group_by(subclass1) %>% 
    mutate(subclass1=subclass1*5-0.4*0:(n()-1)) %>% ungroup()
  
  p <- d %>%
    ggplot(aes(x = int_start(duration_interval), y = subclass1, 
               colour = as.factor(treatment))) +
    geom_segment(aes(xend = int_end(duration_interval), yend = subclass1, group = num_inf, color=as.factor(treatment))) +
    geom_point(size = 1) +
    geom_point(aes(x = int_end(duration_interval)), size = 1) +
    scale_x_datetime("Duration of co-residence", 
                     limits = c(as.POSIXct("2021-12-15"), as.POSIXct("2022-12-15")), 
                     date_breaks = "1 month", date_labels ="%b-%y") + 
    scale_color_discrete(name="Unit type", labels=c("Treatment", "Control")) +
    guides(color = guide_legend(reverse=TRUE)) + 
    labs(title=title, 
         subtitle=subtitle) + 
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
          axis.text.x = element_text(angle=90)) 
  
  p
}


for_matching <- for_matching %>% rowwise() %>% mutate(duration_interval = interval(first, last)) 
for_matching <- for_matching %>% ungroup() %>% mutate(label=1:nrow(.))
for_matching <- for_matching %>% mutate(treatment=(vacc.primary==0)%>%as.numeric())

# demo <- read_csv("demographic_data_clean.csv")
# demo <- demo %>% mutate(age=2022-BirthYear)
# demo
# 
# for_matching <- for_matching %>% left_join(demo, by=c("primary"="ResidentId"))
# for_matching <- for_matching %>% left_join(demo, by=c("secondary"="ResidentId"), suffix=c(".primary", ".secondary"))
# for_matching <- for_matching %>% mutate(Race.primary=ifelse(Race.primary=="C"|Race.primary=="M", "H", Race.primary),
#                   Race.secondary=ifelse(Race.secondary=="C"|Race.secondary=="M", "H", Race.secondary), 
#                   Race.primary=case_when(Race.primary=="A"~"Asian or Pacific Islander",
#                                          Race.primary=="B"~"Black",
#                                          Race.primary=="H"~"Hispanic",
#                                          Race.primary=="I"~"American Indian/Alaskan Native",
#                                          Race.primary=="O"~"Other",
#                                          Race.primary=="W"~"White"),
#                   Race.secondary=case_when(Race.secondary=="A"~"Asian or Pacific Islander",
#                                            Race.secondary=="B"~"Black",
#                                            Race.secondary=="H"~"Hispanic",
#                                            Race.secondary=="I"~"American Indian/Alaskan Native",
#                                            Race.secondary=="O"~"Other",
#                                            Race.secondary=="W"~"White"))

# for_matching <- for_matching %>% 
#   left_join(infections, by=c("primary"="ResidentId")) %>% 
#   rename("infDay.primary"="Day") %>% group_by(label)
# 
# for_matching <- for_matching %>% 
#   filter(infDay.primary <= adjusted_start) %>% 
#   arrange(label, desc(infDay.primary)) %>% 
#   summarise_all(first) %>% 
#   mutate(time_since_inf.primary=difftime(adjusted_start, infDay.primary, units="days")%>%as.numeric())
# for_matching     
# 
# for_matching <- for_matching %>%
#   left_join(infections, by=c("secondary"="ResidentId")) %>% 
#   rename("infDay.secondary"="Day") %>%
#   group_by(label) %>%
#   filter(infDay.secondary <= adjusted_start) %>% 
#   arrange(label, desc(infDay.secondary)) %>% 
#   summarise_all(first) %>% 
#   mutate(time_since_inf.secondary=(difftime(adjusted_start, infDay.secondary, units="days")%>%as.numeric()))
# for_matching 

# for_matching1 <- for_matching%>%filter(Institution%in%1:22)
# for_matching2 <- for_matching%>%filter(Institution%in%23:36)%>% mutate(label=1:nrow(.))

first_match <- matchit(treatment ~ Institution + BuildingId + duration_interval + 
                         vacc.secondary + inf.primary + inf.secondary, 
                       data = for_matching,
                       distance = generate_distance_matrix(for_matching), 
                       exact = treatment ~ Institution + BuildingId + vacc.secondary + inf.primary + inf.secondary,
                       ratio = 5, min.controls = 1, max.controls = 6, method="optimal")
m <- first_match %>% get_matches() %>% arrange(subclass)

filtered_matches <- m %>% group_by(subclass) %>% arrange(subclass, desc(treatment)) %>%
  mutate(intersect=time_length(intersect(first(duration_interval),duration_interval),unit="day")+1) %>% 
  replace_na(list(intersect=0)) %>% 
  mutate(include=any(intersect[2:n()]>0)) %>% 
  filter(include|treatment==0) %>%
  ungroup() %>% select(!c(id, subclass, weights)) %>% mutate(label=1:nrow(.))

match_adjusted <- matchit(treatment ~ Institution + BuildingId + duration_interval + 
                            vacc.secondary + inf.primary + inf.secondary, 
                          data = filtered_matches,
                          distance = generate_distance_matrix(filtered_matches), 
                          exact = treatment ~ Institution + BuildingId + vacc.secondary + inf.primary + inf.secondary, 
                          ratio = 5, min.controls = 1, max.controls = 6, method="optimal") 

m_adjusted <- match_adjusted %>% 
  get_matches() %>% 
  group_by(subclass) %>% 
  arrange(subclass, desc(treatment)) %>% 
  mutate(intersection=intersect(first(duration_interval),duration_interval)) %>% 
  mutate(intersect=time_length(intersection,unit="day")+1) %>% 
  replace_na(list(intersect=0)) %>% 
  filter(treatment==1|intersect>=7) %>% filter(n()>1)

m_adjusted%>%nrow()
m_adjusted%>%group_by(treatment, inf.primary, inf.secondary)%>%summarise(n=n())
m_adjusted$treatment%>%table()

# print plots of matches
pdf("D:/CCHCS_premium/st/indirects/testing/matching_bydose_071223.pdf")
keys <- m_adjusted %>% group_by(subclass) %>% group_keys() #, BuildingId
for (i in 1:100) {
  print(plot_matches(m_adjusted %>% filter(subclass==keys$subclass[i]),
                     title=paste("Subclass", keys$subclass[i]),
                     subtitle="Matched by building, time, and number of vaccine doses in the primary resident"))
}
dev.off()

write_csv(m_adjusted, "matching_data_071223/matching_data_primaryres_080323.csv")
