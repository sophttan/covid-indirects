for_matching <- read_csv("full_data_prematching.csv")

write_csv(filtered_matches, "matching.csv")


test <- any_unvacc_over30 %>% filter(Institution==2) %>% arrange(BuildingId)
test <- test %>% mutate(treatment = ifelse(both_unvacc, 1, 0),
                        adjusted_start = first+7)
test <- test %>% rowwise() %>% mutate(duration_interval = interval(adjusted_start, last)) 
test <- test %>% ungroup() %>% mutate(label=1:nrow(.))

overlap <- expand.grid(x=test$label,y=test$label) %>% 
  left_join(test %>% select(label, duration_interval), by=c("x"="label")) %>% 
  left_join(test %>% select(label, duration_interval), by=c("y"="label")) %>% 
  mutate(overlap=intersect(duration_interval.x,duration_interval.y), 
         duration_overlap=366-(time_length(overlap, unit = "day")+1))

overlap_wide <- overlap%>% 
  select(x,y,overlap) %>% pivot_wider(id_cols = x, names_from = y, values_from = overlap)
overlap_wide <- overlap_wide %>% as.data.frame(row.names = .$x) %>% select(!x)

duration_overlap_wide <- overlap%>% 
  select(x,y,duration_overlap) %>% replace_na(list(duration_overlap=Inf)) %>% 
  pivot_wider(id_cols = x, names_from = y, values_from = duration_overlap)
duration_overlap_wide <- duration_overlap_wide %>% as.data.frame(row.names = .$x) %>% select(!x)

test$treatment %>% table()

first_control <- (test %>% filter(treatment==1))[1,]
first_control$label

filter(test, as.vector(duration_overlap_wide[first_control$label[1],] >= 14)) %>% view()

roomtypes <- d %>% group_by(Institution, RoomId) %>% summarise(RoomType=unique(RoomType))
test <- test %>% left_join(roomtypes)

test %>% arrange(num_inf, first, duration) %>% mutate(row=1:n()) %>% 
  ggplot(aes(x = int_start(duration_interval), y = row, colour = as.factor(treatment))) +
  geom_segment(aes(xend = int_end(duration_interval), yend = row, color=as.factor(treatment))) +
  geom_point(size = 1) +
  geom_point(aes(x = int_end(duration_interval)), size = 1) +
  scale_x_datetime("Duration of co-residence", date_breaks = "1 month", date_labels ="%b-%y") + 
  scale_color_discrete(name="Unit type", labels=c("Treatment", "Control")) +
  guides(color = guide_legend(reverse=TRUE)) + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),axis.ticks.y = element_blank(), 
        axis.text.x = element_text(angle=90)) 

k <- 5
library(MatchIt)
# if <14 day overlap mark as infinite
duration_overlap_wide[duration_overlap_wide > 352] <- Inf
match1 <- matchit(treatment ~ Institution + RoomType + duration_interval, data = test,
                  distance = duration_overlap_wide %>% as.matrix(), exact = treatment ~ Institution + RoomType, 
                  ratio = k, method="optimal")
m1 <- match1 %>% get_matches() %>% arrange(subclass)

match2 <- matchit(treatment ~ BuildingId + RoomType + duration_interval, data = test,
                  distance = duration_overlap_wide %>% as.matrix(), exact = treatment ~ BuildingId + RoomType, 
                  ratio = k, method="optimal")
m2 <- match2 %>% get_matches() %>% arrange(subclass)

match3 <- matchit(treatment ~ BuildingId + RoomType + duration_interval + num_inf, data = test,
                  distance = duration_overlap_wide %>% as.matrix(), exact = treatment ~ BuildingId + RoomType + num_inf, 
                  ratio = k, min.controls = 1, max.controls = 6, method="optimal")
m3 <- match3 %>% get_matches() %>% arrange(subclass)

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
    select(x,y,duration_overlap) %>% replace_na(list(duration_overlap=Inf)) %>% 
    pivot_wider(id_cols = x, names_from = y, values_from = duration_overlap)
  duration_overlap_wide <- duration_overlap_wide %>% as.data.frame(row.names = .$x) %>% select(!x)
  
  duration_overlap_wide[duration_overlap_wide > 352] <- Inf
  
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

for (b in unique(test$BuildingId)) {
  test_b <- test %>% filter(Institution==2 & BuildingId==b)
  for (i in unique(test_b$num_inf)) {
    t <- test_b %>% filter(num_inf==1) %>% 
      arrange(first, duration) %>%
      mutate(label=1:nrow(.))
    
    tryCatch(
      {
        matchit(treatment ~ BuildingId + duration_interval + num_inf, data = t,
                distance = generate_distance_matrix(t), exact = treatment ~ BuildingId + num_inf, 
                ratio = 5, min.controls = 1, max.controls = 6, remove.unmatchables = T, method="optimal")
      },
      error = function(e){
        print(b)
        print(i)
      }
    )
  }
}


t <- test %>% filter(BuildingId==-107909874 & num_inf==2) %>% 
  arrange(first, duration) %>% ungroup() %>% 
  mutate(label=1:nrow(.))
plot_all_units(t)

matchit(treatment ~ BuildingId + duration_interval + num_inf, data = t,
        distance = generate_distance_matrix(t), exact = treatment ~ BuildingId + num_inf, 
        ratio = 5, min.controls = 1, max.controls = 6, remove.unmatchables = T, method="optimal") %>% 
  get_matches()


# m %>% group_by(label) %>% summarise(count=n())
# m %>% group_by(subclass) %>% summarise(count=n())
# m %>% view()

filtered_matches <- m3 %>% group_by(subclass) %>% 
  select(treatment,label,RoomId, ResidentId.1, num_pos.1, vacc.1, ResidentId.2, num_pos.2, vacc.2, first, last, duration, duration_interval) %>% 
  mutate(intersect=time_length(intersect(first(duration_interval),duration_interval),unit="day")+1) %>% 
  filter(intersect >= 14) %>% filter(n()>1)


# filtered_matches %>% group_by(subclass) %>% summarise(count=n()-1) %>% 
#   ggplot(aes(count)) + geom_histogram(bins=15) + 
#   scale_x_continuous("Number of possible matches for control units", expand=c(0,0)) + 
#   scale_y_continuous("Number of control units", limits=c(0,10), expand=c(0,0))
m %>% arrange(subclass) %>% mutate(subclass1=as.numeric(subclass)*5-0.4*0:(n()-1)) %>%
  ggplot(aes(x = int_start(duration_interval), y = subclass1, 
             colour = as.factor(treatment))) +
  geom_segment(aes(xend = int_end(duration_interval), yend = subclass1, color=as.factor(treatment))) +
  geom_point(size = 1) +
  geom_point(aes(x = int_end(duration_interval)), size = 1) +
  scale_x_datetime("Duration of co-residence")+ scale_color_discrete(name="Unit type", labels=c("Treatment", "Control")) +
  guides(color = guide_legend(reverse=TRUE)) + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),axis.ticks.y = element_blank())


pdf("D:/CCHCS_premium/st/indirects/testing/matching.pdf")
for (b in (m3$BuildingId %>% unique())) {
  test_b <- test %>% filter(num_inf==1) %>%
    filter(BuildingId==b) %>% arrange(num_inf, first, duration) %>% mutate(row=1:n())
  
  t1 <- test_b %>% select(label, row)
  m3_keys <- m3 %>% filter(BuildingId==b & num_inf==1) %>% filter(treatment==1) %>% arrange(first, duration) %>% 
    group_by(num_inf, subclass) %>% select(num_inf, subclass, first) %>% ungroup() %>% mutate(subclass1=1:n())
  m3_b <- m3 %>% filter(BuildingId==b & num_inf==1) %>% left_join(m3_keys) %>% 
    group_by(subclass) %>% fill(subclass1, .direction="down") %>% 
    group_by(subclass1) %>% 
    mutate(subclass1=subclass1*3-0.4*0:(n()-1)) %>% ungroup() %>% left_join(t1)
  
  t1 <- test_b %>% select(!c(Institution, RoomId, BuildingId, ResidentId.1, ResidentId.2, RoomType)) %>% 
    select(label, everything()) 
  
  p <- test_b %>% 
    ggplot(aes(x = int_start(duration_interval), y = row, colour = as.factor(treatment))) +
    geom_segment(aes(xend = int_end(duration_interval), yend = row, color=as.factor(treatment))) +
    geom_point(size = 1) +
    geom_point(aes(x = int_end(duration_interval)), size = 1) +
    scale_x_datetime("Duration of co-residence", date_breaks = "1 month", date_labels ="%b-%y") + 
    geom_text(aes(label=row), nudge_x = -60*60*24*14, color="black") + 
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
  
  m3_b_summ <- m3_b %>% group_by(num_inf) %>% filter(subclass1==min(subclass1) | subclass1==max(subclass1))
  p1 <- m3_b %>%
    ggplot(aes(x = int_start(duration_interval), y = subclass1, 
               colour = as.factor(treatment))) +
    geom_segment(aes(xend = int_end(duration_interval), yend = subclass1, group = num_inf, color=as.factor(treatment))) +
    geom_point(size = 1) +
    geom_point(aes(x = int_end(duration_interval)), size = 1) +
    geom_text(aes(label=row), nudge_x = -60*60*24*14, color="black") + 
    # stat_brace(data=m3_b_summ, 
    #            aes(x=as.POSIXct("2023-01-15"), y=subclass1, group=num_inf, label=num_inf), color="black",
    #            rotate=90, labelsize=3, bending = 0.7) + 
    scale_x_datetime("Duration of co-residence", date_breaks = "1 month", date_labels ="%b-%y") + 
    scale_color_discrete(name="Unit type", labels=c("Treatment", "Control")) +
    guides(color = guide_legend(reverse=TRUE)) + 
    labs(#title="", 
      subtitle="Matched by time") + 
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
          axis.text.x = element_text(angle=90)) 
  
  print(p|p1)
  
}
dev.off()



filtered_matches %>% filter(n()>1) %>% mutate(subclass1=as.numeric(subclass)-0.1*0:(n()-1)) %>%
  ggplot(aes(x = int_start(duration_interval), y = subclass1*10, colour = as.factor(treatment))) +
  geom_segment(aes(xend = int_end(duration_interval), yend = subclass1*10, color=as.factor(treatment))) +
  geom_point(size = 1) +
  geom_point(aes(x = int_end(duration_interval)), size = 1) +
  scale_x_datetime("Duration of co-residence")+ scale_color_discrete(name="Unit type", labels=c("Treatment", "Control")) +
  guides(color = guide_legend(reverse=TRUE)) + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),axis.ticks.y = element_blank())

filtered_matches %>% filter(subclass==6) %>% mutate(label=seq(66,1)) %>%
  ggplot(aes(x = int_start(duration_interval), y = label, colour = as.factor(treatment))) +
  geom_segment(aes(xend = int_end(duration_interval), yend = label), colour = "black") +
  geom_point(size = 1.5) +
  geom_point(aes(x = int_end(duration_interval)), size = 1.5) +
  scale_x_datetime("Duration of co-residence")+ scale_color_discrete(name="Unit type", labels=c("Treatment", "Control")) +
  guides(color = guide_legend(reverse=TRUE)) + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),axis.ticks.y = element_blank())

(filtered_matches %>% filter(treatment==0) %>% group_by(label) %>% summarise(count=n()))$count %>% hist()


library(lubridate)
for_matching <- any_unvacc_over30_noinf %>% mutate(treatment = ifelse(both_unvacc, 1, 0),
                                             adjusted_start = first+7)
for_matching <- for_matching %>% rowwise() %>% mutate(duration_interval = interval(adjusted_start, last)) 
for_matching <- for_matching %>% ungroup() %>% mutate(label=1:nrow(.))

overlap <- expand.grid(x=for_matching$label,y=for_matching$label) %>% 
  left_join(for_matching %>% select(label, duration_interval), by=c("x"="label")) %>% 
  left_join(for_matching %>% select(label, duration_interval), by=c("y"="label")) %>% 
  mutate(overlap=intersect(duration_interval.x,duration_interval.y), 
         duration_overlap=366-(time_length(overlap, unit = "day")+1))

overlap_wide <- overlap%>% 
  select(x,y,overlap) %>% pivot_wider(id_cols = x, names_from = y, values_from = overlap)
overlap_wide <- overlap_wide %>% as.data.frame(row.names = .$x) %>% select(!x)

duration_overlap_wide <- overlap%>% 
  select(x,y,duration_overlap) %>% replace_na(list(duration_overlap=Inf)) %>% 
  pivot_wider(id_cols = x, names_from = y, values_from = duration_overlap)
duration_overlap_wide <- duration_overlap_wide %>% as.data.frame(row.names = .$x) %>% select(!x)

for_matching$treatment %>% table()
#1:5 control to treatment ratio

library(MatchIt)
duration_overlap_wide[duration_overlap_wide > 352] <- Inf
match <- matchit(treatment ~ Institution + BuildingId + num_inf + duration_interval, 
                 data = for_matching,
                 distance = duration_overlap_wide %>% as.matrix(), 
                 exact = treatment ~ Institution + BuildingId + num_inf,
                 ratio = 5, min.controls = 1, max.controls = 6, method="optimal")
m <- match %>% get_matches() %>% arrange(subclass)
m %>% group_by(label) %>% summarise(count=n())
m %>% group_by(subclass) %>% summarise(count=n())
m %>% view()

filtered_matches <- m %>% group_by(subclass) %>% 
  select(treatment,label,Institution,BuildingId,RoomId, 
         ResidentId.1, num_pos.1, vacc.1, 
         ResidentId.2, num_pos.2, vacc.2, 
         num_inf,
         first, last, duration, duration_interval) %>% 
  mutate(subclass=as.numeric(subclass), 
         intersect=time_length(intersect(first(duration_interval),duration_interval),unit="day")+1) %>% 
  filter(intersect >= 14) %>% filter(n()>1)
filtered_matches %>% group_by(subclass) %>% summarise(count=n()) 

filtered_matches %>% nrow()

pdf("D:/CCHCS_premium/st/indirects/testing/matching_full.pdf")
keys <- filtered_matches %>% group_by(Institution, BuildingId) %>% group_keys()
for (i in 1:nrow(keys)) {
  m_b <- filtered_matches %>% filter(Institution==keys$Institution[i] & BuildingId==keys$BuildingId[i]) %>% 
    arrange(num_inf)
  m_b_keys <- m_b %>% group_by(num_inf, subclass) %>% group_keys() %>% mutate(subclass1=1:n())
  m_b <- m_b %>% left_join(m_b_keys) %>% 
    group_by(subclass) %>% fill(subclass1, .direction="down") %>% 
    group_by(subclass1) %>% 
    mutate(subclass1=subclass1*5-0.4*0:(n()-1)) %>% ungroup()
  
  for_matching_b <- for_matching %>% filter(Institution==keys$Institution[i] & BuildingId==keys$BuildingId[i]) %>% 
    arrange(num_inf, first, duration) %>% mutate(row=1:n())
  p <- for_matching_b %>% 
    ggplot(aes(x = int_start(duration_interval), y = row, colour = as.factor(treatment))) +
    geom_segment(aes(xend = int_end(duration_interval), yend = row, color=as.factor(treatment))) +
    geom_point(size = 1) +
    geom_point(aes(x = int_end(duration_interval)), size = 1) +
    scale_x_datetime("Duration of co-residence", date_breaks = "1 month", date_labels ="%b-%y") + 
    stat_brace(data=for_matching_b, 
               aes(x=as.POSIXct("2023-01-15"), y=row, group=num_inf, label=num_inf), color="black",
               rotate=90, labelsize=3, bending = 0.7) + 
    guides(color = guide_legend(reverse=TRUE)) + 
    labs(title=paste("Institution", keys$Institution[i], "Building", keys$BuildingId[i]),
         subtitle="All units") + 
    theme(legend.position = "none", 
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),axis.ticks.y = element_blank(), 
          axis.text.x = element_text(angle=90)) 
  
  m_b_summ <- m_b %>% group_by(num_inf) %>% filter(subclass1==min(subclass1) | subclass1==max(subclass1))
  p1 <- m_b %>%
    ggplot(aes(x = int_start(duration_interval), y = subclass1, 
               colour = as.factor(treatment))) +
    geom_segment(aes(xend = int_end(duration_interval), yend = subclass1, group = num_inf, color=as.factor(treatment))) +
    geom_point(size = 1) +
    geom_point(aes(x = int_end(duration_interval)), size = 1) +
    stat_brace(data=m_b_summ, 
               aes(x=as.POSIXct("2023-01-15"), y=subclass1, group=num_inf, label=num_inf), color="black",
               rotate=90, labelsize=3, bending = 0.7) + 
    scale_x_datetime("Duration of co-residence", date_breaks = "1 month", date_labels ="%b-%y") + 
    scale_color_discrete(name="Unit type", labels=c("Treatment", "Control")) +
    guides(color = guide_legend(reverse=TRUE)) + 
    labs(title="", 
         subtitle="Matched by building and prior infections") + 
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
          axis.text.x = element_text(angle=90)) 
  
  print(p|p1)
}
dev.off()
