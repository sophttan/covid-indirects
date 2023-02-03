# Sophia Tan 1/13/23
# exploring indirect effects

rm(list=ls())
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")

library(tidyverse)
library(readr)
library(lubridate)

d <- read_csv("complete-data.csv")

group_room <- d %>% filter(!is.na(Institution) & !is.na(RoomId)) %>% filter(Day >= "2021-12-15") %>% 
  group_by(Institution, RoomId, Day) 

# test <- group_room %>% summarise(num_res=n()) %>% 
#   mutate(less_than_5 = ifelse(num_res<=5, num_res, 0), less_than_2 = ifelse(num_res<=2, num_res, 0))
# test2 <- test %>% 
#   group_by(Day) %>% summarise(prop_people_5 = sum(less_than_5)/sum(num_res), prop_people_2 = sum(less_than_2)/sum(num_res))
# test2 %>% ggplot(aes(Day)) + geom_line(aes(y=prop_people_5, color="Proportion of residents in rooms of 5 or less people")) +
#   geom_line(aes(y=prop_people_2, color="Proportion of residents in rooms of 2 or less people")) 
room_size_over_time <- group_room %>% summarise(n=n()) %>% group_by(Day) %>% summarise(mean=mean(n))
ggplot(room_size_over_time, aes(Day, mean)) + geom_line() + 
  scale_x_date("Day", breaks="month", expand=c(0.05,0)) + 
  scale_y_continuous("Average room size", breaks=seq(2.1, 2.3, 0.025)) + 
  theme(axis.text.x = element_text(angle=90))

duration <- read_csv("housing_duration.csv")
duration <- duration %>% filter(first<="2020-03-31")
included <- duration$ResidentId %>% unique()

group_room_summary <- group_room %>% filter(n()==2)

residents <- group_room_summary %>% group_by(ResidentId) %>% group_keys()
residents <- residents %>% filter(ResidentId %in% included)
group_room_summary_entirepandemic <- group_room_summary %>% inner_join(residents)
group_room_summary_entirepandemic <- group_room_summary_entirepandemic %>% filter(n()==2)

group_room_2 <- group_room_summary_entirepandemic %>% mutate(num=as.factor(1:n())) %>%
  arrange(Day, Institution, RoomId)
group_room_2 <- group_room_2 %>%
  select(Institution, BuildingId, RoomId, Day, ResidentId, num, num_pos, num_dose_adjusted)
group_room_2 <- group_room_2 %>% mutate(vacc=ifelse(num_dose_adjusted>0,1,0))

group_room_2 <- group_room_2 %>% group_by(Institution, RoomId) %>% 
  filter(length(unique(BuildingId))==1) %>%
  as.data.frame()

building_room <- group_room_2 %>% group_by(Institution, RoomId, BuildingId) %>% group_keys()

group_room_2_wide <- group_room_2 %>% select(!BuildingId) %>%
  reshape(idvar = c("Institution", "RoomId", "Day"),
          timevar = "num",
          v.names = c("ResidentId", "num_pos", "num_dose_adjusted", "vacc"),
          direction = "wide")

# add building type
group_room_2_wide <- group_room_2_wide %>% left_join(building_room) 
group_room_2_wide <- group_room_2_wide %>% select(Institution, BuildingId, RoomId, Day, everything())

write_csv(group_room_2_wide, "wide_housing_2room.csv")
group_room_2_wide <- read_csv("wide_housing_2room.csv") 

group_room_2_wide_distinct <- group_room_2_wide %>%
  distinct(Institution,RoomId,ResidentId.1,ResidentId.2,
           vacc.1,vacc.2,.keep_all=T)

group_room_2_wide_distinct <- group_room_2_wide_distinct %>% 
  mutate(both_unvacc=(vacc.1==0&vacc.2==0),
         one_unvacc=(!both_unvacc&(vacc.1==0|vacc.2==0)),
         num_inf=case_when(
           is.na(num_pos.1)&is.na(num_pos.2)~0,
           is.na(num_pos.1)|is.na(num_pos.2)~1,
           !is.na(num_pos.1)&!is.na(num_pos.2)~2))

labeled <- group_room_2_wide_distinct %>% group_by(Institution, RoomId) %>% 
  mutate(label=1:n()) %>% ungroup() %>% 
  select(Institution, RoomId, Day, label)

group_room_2_wide <- group_room_2_wide %>% left_join(labeled) 
group_room_2_wide_summary <- group_room_2_wide %>% group_by(Institution, RoomId, ResidentId.1, ResidentId.2) %>%
  fill(label, .direction="down") %>% 
  group_by(Institution, RoomId, label) %>%
  summarise(first=first(Day), last=last(Day)) %>%
  mutate(duration=difftime(last, first, units="days")+1)

any_unvacc <- group_room_2_wide_summary %>% select(!label) %>% 
  left_join(group_room_2_wide_distinct, by=c("Institution", "RoomId", "first"="Day")) %>%
  rowwise() %>% filter(any(both_unvacc|one_unvacc))
any_unvacc


any_unvacc %>% ggplot(aes(duration)) + geom_histogram() + 
  scale_x_continuous("Duration of co-residence (days)",limits=c(0,365),
                     expand=c(0,0)) + scale_y_continuous("Number of units",expand=c(0,0))
any_unvacc$duration %>% as.numeric() %>% summary()


p1 <- any_unvacc %>% ggplot(aes(Institution)) + geom_bar() + 
  scale_x_continuous(limits=c(1,35),breaks=1:35,labels=1:35, expand=c(0,0)) + 
  scale_y_continuous("Number of units") 

p2 <- any_unvacc %>% filter(duration >= 30) %>% ggplot(aes(Institution)) + geom_bar() + 
  scale_x_continuous(limits=c(1,35),breaks=1:35,labels=1:35, expand=c(0,0)) + 
  scale_y_continuous("Number of units")

library(patchwork)
p1/p2

any_unvacc_over30 <- any_unvacc %>% filter(duration>=30) 
any_unvacc_over30 %>% ggplot(aes(duration)) + geom_histogram() + 
  scale_x_continuous("Duration of co-residence (days)",limits=c(0,365),
                     expand=c(0,0)) + scale_y_continuous("Number of units",expand=c(0,0))
any_unvacc_over30$duration %>% as.numeric() %>% summary()

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

test %>% arrange(first, duration) %>% mutate(row=1:n()) %>% 
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

k <- 3
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
                  ratio = k, method="optimal")
m3 <- match3 %>% get_matches() %>% arrange(subclass)
# m %>% group_by(label) %>% summarise(count=n())
# m %>% group_by(subclass) %>% summarise(count=n())
# m %>% view()

filtered_matches <- m %>% group_by(subclass) %>% 
  select(treatment,label,RoomId, ResidentId.1, num_pos.1, vacc.1, ResidentId.2, num_pos.2, vacc.2, first, last, duration, duration_interval) %>% 
  mutate(intersect=time_length(intersect(first(duration_interval),duration_interval),unit="day")+1) %>% 
  filter(intersect >= 14)


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
for (b in (m$BuildingId %>% unique())) {
  test_b <- test %>% filter(BuildingId==b)
  m2_b <- m2 %>% filter(BuildingId==b) %>% group_by(subclass) %>% 
    arrange(subclass) %>% mutate(subclass1=as.numeric(subclass)*5-0.4*(0:(n()-1)))
  
  p1 <- test_b %>% arrange(first, duration) %>% mutate(row=1:n()) %>% 
    ggplot(aes(x = int_start(duration_interval), y = row, colour = as.factor(treatment))) +
    geom_segment(aes(xend = int_end(duration_interval), yend = row, color=as.factor(treatment))) +
    geom_point(size = 1) +
    geom_point(aes(x = int_end(duration_interval)), size = 1) +
    scale_x_datetime("Duration of co-residence", date_breaks = "1 month", date_labels ="%b-%y") + 
    guides(color = guide_legend(reverse=TRUE)) + 
    labs(subtitle="All units") + 
    theme(legend.position = "none", 
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),axis.ticks.y = element_blank(), 
          axis.text.x = element_text(angle=90)) 
  p2 <- m2_b %>%
    ggplot(aes(x = int_start(duration_interval), y = subclass1, 
               colour = as.factor(treatment))) +
    geom_segment(aes(xend = int_end(duration_interval), yend = subclass1, color=as.factor(treatment))) +
    geom_point(size = 1) +
    geom_point(aes(x = int_end(duration_interval)), size = 1) +
    scale_x_datetime("Duration of co-residence", date_breaks = "1 month", date_labels ="%b-%y") + 
    scale_color_discrete(name="Unit type", labels=c("Treatment", "Control")) +
    guides(color = guide_legend(reverse=TRUE)) + 
    labs(subtitle="Matched by building") + 
    theme(legend.position = "none", 
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),axis.ticks.y = element_blank(), 
          axis.text.x = element_text(angle=90)) 
  p3 <- m3 %>% filter(BuildingId==b) %>% arrange(subclass) %>% 
    left_join(m2_b %>% select(label, subclass), by="label") %>%
    group_by(subclass.x) %>% 
    mutate(subclass=ifelse(treatment==1, subclass.y, NA)) %>% 
    fill(subclass, .direction="down") %>% 
    mutate(subclass1=as.numeric(subclass)*5-0.4*0:(n()-1)) %>%
    ggplot(aes(x = int_start(duration_interval), y = subclass1, 
               colour = as.factor(treatment))) +
    geom_segment(aes(xend = int_end(duration_interval), yend = subclass1, color=as.factor(treatment))) +
    geom_point(size = 1) +
    geom_point(aes(x = int_end(duration_interval)), size = 1) +
    scale_x_datetime("Duration of co-residence", date_breaks = "1 month", date_labels ="%b-%y") + 
    scale_y_continuous(limits=c(min(m2_b$subclass1), max(m2_b$subclass1))) +
    scale_color_discrete(name="Unit type", labels=c("Treatment", "Control")) +
    guides(color = guide_legend(reverse=TRUE)) + 
    labs(subtitle="Matched by building and prior infections") + 
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),axis.ticks.y = element_blank(), 
          axis.text.x = element_text(angle=90)) 
  library(patchwork)
  print(p1|p2|p3)
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
for_matching <- any_unvacc_over30 %>% mutate(treatment = ifelse(both_unvacc, 1, 0),
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

for_matching <- for_matching %>% left_join(roomtypes)

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


write_csv(for_matching, "full_data_prematching.csv")
write_csv(filtered_matches, "matching.csv")
