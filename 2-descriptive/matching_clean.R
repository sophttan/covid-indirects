rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(lubridate)
library(tidyverse)
library(MatchIt)
library(ggbrace)
library(patchwork)

for_matching <- read_csv("full_data_prematching_040423.csv")

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
  d <- d %>% arrange(subclass)
  d_keys <- d %>% group_by(subclass) %>% group_keys() %>% mutate(subclass1=1:n())
  d <- d %>% left_join(d_keys) %>%
    group_by(subclass) %>% fill(subclass1, .direction="down") %>% 
    group_by(subclass1) %>% 
    mutate(subclass1=subclass1*5-0.4*0:(n()-1)) %>% ungroup()
  
  p <- d %>%
    ggplot(aes(x = int_start(duration_interval), y = subclass1, 
               colour = as.factor(treatment))) +
    geom_segment(aes(xend = int_end(duration_interval), yend = subclass1, group = num_inf, color=as.factor(treatment))) +
    geom_point(size = 1) +
    geom_point(aes(x = int_end(duration_interval)), size = 1) +
    scale_x_datetime("Duration of co-residence", date_breaks = "1 month", date_labels ="%b-%y") + 
    scale_color_discrete(name="Unit type", labels=c("Treatment", "Control")) +
    guides(color = guide_legend(reverse=TRUE)) + 
    labs(title=title, 
         subtitle=subtitle) + 
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
          axis.text.x = element_text(angle=90)) 
  
  p
}

  
for_matching <- for_matching %>% rowwise() %>% mutate(duration_interval = interval(adjusted_start, last_chunked)) 
for_matching <- for_matching %>% ungroup() %>% mutate(label=1:nrow(.))

match <- matchit(treatment ~ Institution + BuildingId + duration_interval + inf.primary,# + inf.secondary, 
                 data = for_matching,
                 distance = generate_distance_matrix(for_matching), 
                 exact = treatment ~ Institution + BuildingId + inf.primary,# + inf.secondary,
                 ratio = 5, min.controls = 1, max.controls = 6, method="optimal")
m <- match %>% get_matches() %>% arrange(subclass)

filtered_matches <- m %>% group_by(subclass) %>% arrange(subclass, desc(treatment)) %>%
  mutate(intersect=time_length(intersect(first(duration_interval),duration_interval),unit="day")+1) %>% 
  replace_na(list(intersect=0)) %>% 
  mutate(include=any(intersect[2:n()]>0)) %>% 
  filter(include|treatment==0) %>%
  ungroup() %>% select(!c(id, subclass, weights)) %>% mutate(label=1:nrow(.))

match_adjusted <- matchit(treatment ~ Institution + BuildingId + duration_interval + inf.primary,# + inf.secondary,
                          data = filtered_matches,
                          distance = generate_distance_matrix(filtered_matches), 
                          exact = treatment ~ Institution + BuildingId + inf.primary,# + inf.secondary, 
                          ratio = 5, min.controls = 1, max.controls = 6, method="optimal") 

m_adjusted <- match_adjusted %>% 
  get_matches() %>% 
  group_by(subclass) %>% 
  arrange(subclass, desc(treatment)) %>% 
  mutate(intersection=intersect(first(duration_interval),duration_interval)) %>% 
  mutate(intersect=time_length(intersection,unit="day")+1) %>% 
  replace_na(list(intersect=0)) %>% 
  filter(treatment==1|intersect>=7) %>% filter(n()>1)

#testing data
testing <- read_csv("complete_testing_data.csv")
m_adjusted_testing <- m_adjusted %>% left_join(testing %>% select(ResidentId, Day), by=c("primary"="ResidentId")) %>% 
  group_by(id) %>% 
  summarise(subclass=first(subclass), include=any(Day %within% intersection)) 

(m_adjusted %>% filter(treatment!=1))$intersect %>% hist()


m_adjusted%>%nrow()
m_adjusted$treatment%>%table()

pdf("D:/CCHCS_premium/st/indirects/testing/matching_040423/matching_full.pdf")
keys <- m_adjusted %>% group_by(Institution) %>% group_keys() #, BuildingId
for (i in 1:nrow(keys)) {
  print(plot_matches(m_adjusted %>% filter(Institution==keys$Institution[i]),
                     title=paste("Institution", keys$Institution[i], "BuildingId", keys$BuildingId[i]), 
                     subtitle="Matched by building, time, and prior infection in the primary resident"))
}
dev.off()

write_csv(m_adjusted, "matching_data_040423/matched.csv")
