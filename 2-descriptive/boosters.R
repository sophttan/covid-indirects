g_data <-read_csv("group_room_data_2_entireperiod.csv")

vacc <- read_csv("cleaned_vaccination_data.csv")

vacc_new <- vacc %>% group_by(ResidentId) %>% 
  mutate(booster_add_dose=num_dose-full_vacc)
vacc_new <- vacc_new %>% mutate(booster_add_dose=ifelse(booster_add_dose<0, 0, booster_add_dose))
vacc_new <- vacc_new %>% mutate(within30_days = if_else(booster_add_dose>0, Date + 90, as.Date(NA)))
vacc_new                                

with_booster <- g_data %>% select(ResidentId, Day, num_dose, num_dose_adjusted, num_pos, RoomId, RoomType, Institution, FacilityId, BuildingId, inf_90_days) %>%
  group_by(ResidentId) %>% fill(num_dose_adjusted=)
  left_join(vacc_new %>% select(ResidentId, num_dose, booster_add_dose, within30_days), 
            by=c("ResidentId", "num_dose_adjusted"="num_dose"))

# label residents in rooms
group_room_2 <- with_booster %>% group_by(Institution, RoomId, Day) %>% mutate(num=as.factor(1:n())) %>%
  arrange(Institution, RoomId, Day)
group_room_2 <- group_room_2 %>% mutate(recent_booster=booster_add_dose>0&within30_days-Day<=90)
group_room_2

# keep residents with clear building reporting
# some rooms have multiple building labels within the same institution
group_room_2 <- group_room_2 %>% group_by(Institution, RoomId) %>%
  filter(length(unique(BuildingId))==1) %>%
  as.data.frame()

# get dataset for BuildingIds based on RoomId and Institution
building_room <- group_room_2 %>% group_by(Institution, RoomId, BuildingId) %>% group_keys()

# reshape dataset to be wide so each row represents a room-day instead of a resident-day over time
group_room_2_wide <- group_room_2 %>% select(!c(BuildingId, num_dose, booster_add_dose, within30_days)) %>%
  reshape(idvar = c("Institution", "RoomId", "Day"),
          timevar = "num",
          v.names = c("ResidentId", "num_pos", "num_dose_adjusted", "recent_booster", "inf_90_days"),
          direction = "wide")

# add building type
group_room_2_wide <- group_room_2_wide %>% left_join(building_room)
group_room_2_wide <- group_room_2_wide %>% select(Institution, BuildingId, everything())

group_room_2_wide <- group_room_2_wide %>% arrange(Institution, BuildingId, RoomId, Day)

group_room_2_wide %>% select(Institution, BuildingId, RoomId, Day, ResidentId.1, ResidentId.2) %>% head(20)

# make sure residents co-reside for continuous 30 days (no skipped days)
# make sure both residents have not had prior infection within the last 90 days
g <- group_room_2_wide %>% group_by(Institution, RoomId, ResidentId.1, ResidentId.2) %>% 
  mutate(cont = c(0, diff(Day)), more_1_day = cont==0|cont>1) 

g1 <- g %>% filter(more_1_day) %>% select(Day) %>% mutate(unit_label=1:n())

g <- g %>% left_join(g1) 
g <- g %>% select(!c(cont, more_1_day))
g <- g %>% fill(unit_label, .direction="down")

group_room_2_wide_distinct <- g %>%
  distinct(Institution,RoomId,ResidentId.1,ResidentId.2,unit_label,
           num_dose_adjusted.1, num_dose_adjusted.2, recent_booster.1, recent_booster.2,.keep_all=T)

group_room_2_wide_distinct <- group_room_2_wide_distinct %>% 
  replace_na(list(inf_90_days.1=F, inf_90_days.2=F)) %>% 
  mutate(num_inf=case_when(
           is.na(num_pos.1)&is.na(num_pos.2)~0,
           is.na(num_pos.1)|is.na(num_pos.2)~1,
           !is.na(num_pos.1)&!is.na(num_pos.2)~2),
         prior_inf_90 = inf_90_days.1|inf_90_days.2) 

labeled <- group_room_2_wide_distinct %>% ungroup() %>%
  mutate(label=1:nrow(.)) %>% 
  select(Institution, BuildingId, RoomId, Day, label)

group_room_2_wide <- group_room_2_wide %>% left_join(labeled) 
group_room_2_wide_summary <- group_room_2_wide %>% 
  fill(label, .direction="down") %>% 
  group_by(Institution, BuildingId, RoomId, label) %>%
  summarise(first=first(Day), last=last(Day)) %>%
  mutate(duration=difftime(last, first, units="days")+1)

group_room_2_final <- group_room_2_wide_summary %>% select(!label) %>% 
  left_join(group_room_2_wide_distinct, by=c("Institution", "BuildingId", "RoomId", "first"="Day")) %>%
  select(Institution, BuildingId, RoomId, RoomType, everything())

# exclude units if resident's don't co-reside for at least 2 weeks
over_14 <- group_room_2_final %>% filter(duration >= 14)
over14_noinf <- over_14 %>% filter(!prior_inf_90)

over14_noinf_booster <- over14_noinf %>% rowwise() %>% filter(recent_booster.1|recent_booster.2)
over14_noinf_booster %>% filter(!(recent_booster.1&recent_booster.2))

# check testing data
residents <- c(over14_noinf$ResidentId.1, over14_noinf$ResidentId.2) %>% unique()
testing <- read_csv("complete_testing_data.csv")
testing <- testing %>% filter(ResidentId %in% residents) %>% select(ResidentId, Day) %>% rename("Test" = "Day")

over14_noinf <- over14_noinf %>% select(!unit_label) %>% ungroup() %>% mutate(label=1:n())

units_testing1 <- over14_noinf %>% 
  left_join(testing, by=c("ResidentId.1"="ResidentId")) %>% 
  filter(Test >= first & Test <= last) %>% rename("Test.1" = "Test")
units_testing1 %>% select(Institution, RoomId, first, last, ResidentId.1, Test.1)

# test breaking apart 
a <- units_testing1 %>% select(label, ResidentId.1, first,last,duration,Test.1) %>%
  group_by(label) %>% 
  mutate(diff_test=diff(c(first(first),Test.1))) 

a <- a %>% mutate(first_chunked = case_when(first(diff_test)<=14~first,
                                            T~first(Test.1)),
                  last_chunked=case_when(last(last)-last(Test.1)<=14~last,
                                         T~last(Test.1)), 
                  adjusted_start=case_when(first==first_chunked~first+5,
                                           first!=first_chunked~first_chunked))
a <- a %>% mutate(diff_test=diff(c(first(first_chunked),Test.1))) %>% 
  mutate(break_chunk=ifelse(diff_test>14,1,NA)) %>% 
  filter(all(diff_test[2:n()]<=14))

a %>% filter(first!=first_chunked|last!=last_chunked)


units_testing2 <- over14_noinf %>% left_join(testing, by=c("ResidentId.2"="ResidentId")) %>% 
  filter(Test >= first & Test <= last) %>% rename("Test.2" = "Test")
units_testing2 %>% select(Institution, RoomId, first, last, ResidentId.2, Test.2)

# test breaking apart 
a2 <- units_testing2 %>% select(label, ResidentId.2, first,last,duration,Test.2) %>%
  group_by(label) %>% 
  mutate(diff_test=diff(c(first(first),Test.2))) 

a2 <- a2 %>% mutate(first_chunked = case_when(first(diff_test)<=14~first,
                                              T~first(Test.2)),
                    last_chunked=case_when(last(last)-last(Test.2)<=14~last,
                                           T~last(Test.2)), 
                    adjusted_start=case_when(first==first_chunked~first+5,
                                             first!=first_chunked~first_chunked))
a2 <- a2 %>% mutate(diff_test=diff(c(first(first_chunked),Test.2))) %>% 
  mutate(break_chunk=ifelse(diff_test>14,1,NA)) %>% 
  filter(all(diff_test[2:n()]<=14))


a <- a %>% group_by(label) %>% select(label, ResidentId.1, adjusted_start, last_chunked) %>% 
  summarise_all(first) %>%
  mutate(duration_chunked=last_chunked-adjusted_start)
a2 <- a2 %>% group_by(label) %>% select(label, ResidentId.2, adjusted_start, last_chunked) %>% 
  summarise_all(first) %>%
  mutate(duration_chunked=last_chunked-adjusted_start)

has_testing <- over14_noinf %>% 
  replace_na(list(recent_booster.1=F, recent_booster.2=F))%>%
  mutate(has_test.1 = label %in% a$label,
         has_test.2 = label %in% a2$label)
no_testing <- has_testing %>% filter(!(has_test.1|has_test.2))

has_testing <- has_testing %>% filter(has_test.1|has_test.2)

has_testing <- has_testing %>% filter(!(recent_booster.1&recent_booster.2))

treatment <- has_testing %>% filter(recent_booster.1|recent_booster.2)
control <- has_testing %>% filter(!recent_booster.1&!recent_booster.2)

treatment <- treatment %>%
  filter((has_test.1&!recent_booster.1)|(has_test.2&!recent_booster.2)) %>%
  mutate(primary = case_when(recent_booster.1~ResidentId.2,
                             recent_booster.2~ResidentId.1)) %>%
  mutate(recent_booster.primary = ifelse(primary==ResidentId.1, recent_booster.1, recent_booster.2),
         recent_booster.secondary = ifelse(primary==ResidentId.1, recent_booster.2, recent_booster.1)) %>%
  mutate(vacc.primary = ifelse(primary==ResidentId.1, num_dose_adjusted.1, num_dose_adjusted.2),
         vacc.secondary = ifelse(primary==ResidentId.1, num_dose_adjusted.2, num_dose_adjusted.1)) %>%
  mutate(inf.primary = ifelse(primary==ResidentId.1, num_pos.1>0, num_pos.2>0),
         inf.secondary = ifelse(primary==ResidentId.1, num_pos.2>0, num_pos.1>0)) %>% 
  left_join(a, by=c("label", "primary"="ResidentId.1")) %>% 
  left_join(a2, by=c("label", "primary"="ResidentId.2")) %>% 
  mutate(adjusted_start.x=as.character(adjusted_start.x), 
         adjusted_start.y=as.character(adjusted_start.y),
         last_chunked.x=as.character(last_chunked.x),
         last_chunked.y=as.character(last_chunked.y)) %>%
  replace_na(list(adjusted_start.x="", adjusted_start.y="",
                  last_chunked.x="", last_chunked.y="")) %>% 
  mutate(adjusted_start=paste0(adjusted_start.x, adjusted_start.y)%>%as.Date(), 
         last_chunked=paste0(last_chunked.x, last_chunked.y)%>%as.Date())


controlboth <- control %>% filter(has_test.1&has_test.2) %>% 
  left_join(a, by=c("label", "ResidentId.1"="ResidentId.1")) %>% 
  left_join(a2, by=c("label", "ResidentId.2"="ResidentId.2")) %>% 
  rowwise() %>% 
  mutate(primary=case_when(duration_chunked.x>duration_chunked.y~ResidentId.1,
                           duration_chunked.x<duration_chunked.y~ResidentId.2,
                           T~sample(c(ResidentId.1, ResidentId.2), size = 1))) %>%
  mutate(recent_booster.primary = ifelse(primary==ResidentId.1, recent_booster.1, recent_booster.2),
         recent_booster.secondary = ifelse(primary==ResidentId.1, recent_booster.2, recent_booster.1)) %>%
  mutate(vacc.primary = ifelse(primary==ResidentId.1, num_dose_adjusted.1, num_dose_adjusted.2),
         vacc.secondary = ifelse(primary==ResidentId.1, num_dose_adjusted.2, num_dose_adjusted.1)) %>%
  mutate(adjusted_start=case_when(primary==ResidentId.1~adjusted_start.x, 
                                  T~adjusted_start.y),
         last_chunked=case_when(primary==ResidentId.1~last_chunked.x, 
                                T~last_chunked.y)) %>%
  mutate(inf.primary = case_when(primary==ResidentId.1~num_pos.1,
                                 primary==ResidentId.2~num_pos.2), 
         inf.secondary = case_when(primary==ResidentId.1~num_pos.2,
                                   primary==ResidentId.2~num_pos.1)) %>% select(names(treatment))

controlone <- control %>% filter(!label %in% controlboth$label) %>%
  rowwise() %>% 
  mutate(primary = case_when(label %in% a$label & !label %in% a2$label~ResidentId.1,
                             label %in% a2$label & !label %in% a$label~ResidentId.2)) %>%
  mutate(inf.primary = case_when(primary==ResidentId.1~num_pos.1,
                                 primary==ResidentId.2~num_pos.2), 
         inf.secondary = case_when(primary==ResidentId.1~num_pos.2,
                                   primary==ResidentId.2~num_pos.1)) %>% 
  left_join(a, by=c("label", "primary"="ResidentId.1")) %>% 
  left_join(a2, by=c("label", "primary"="ResidentId.2")) %>% 
  mutate(recent_booster.primary = ifelse(primary==ResidentId.1, recent_booster.1, recent_booster.2),
         recent_booster.secondary = ifelse(primary==ResidentId.1, recent_booster.2, recent_booster.1)) %>%
  mutate(vacc.primary = ifelse(primary==ResidentId.1, num_dose_adjusted.1, num_dose_adjusted.2),
         vacc.secondary = ifelse(primary==ResidentId.1, num_dose_adjusted.2, num_dose_adjusted.1)) %>%
  mutate(adjusted_start.x=as.character(adjusted_start.x), 
         adjusted_start.y=as.character(adjusted_start.y),
         last_chunked.x=as.character(last_chunked.x),
         last_chunked.y=as.character(last_chunked.y)) %>%
  replace_na(list(adjusted_start.x="", adjusted_start.y="",
                  last_chunked.x="", last_chunked.y="")) %>% 
  mutate(adjusted_start=paste0(adjusted_start.x, adjusted_start.y)%>%as.Date(), 
         last_chunked=paste0(last_chunked.x, last_chunked.y)%>%as.Date()) %>% select(names(treatment))

treatment$treatment <- 0
controlboth$treatment <- 1
controlone$treatment <- 1
matching <- treatment %>% rbind(controlboth, controlone) %>% 
  replace_na(list(inf.primary=0, inf.secondary=0))

write_csv(matching, "boosters_full_data_prematching_031523.csv")

both_have_testing <- has_testing %>% filter(has_test.1&has_test.2)
one_has_testing <- has_testing %>% filter(!label %in% both_have_testing$label)

one_has_testing <- one_has_testing %>% 
  filter((recent_booster.1&recent_booster.2)|(has_test.1&!recent_booster.1)|(has_test.2&!recent_booster.2))

#has_testing %>% filter(recent_booster.1|recent_booster.2)

one_has_testing <- one_has_testing %>% 
  mutate(primary = case_when(has_test.1~ResidentId.1,
                             has_test.2~ResidentId.2)) %>%
  mutate(recent_booster.primary = ifelse(primary==ResidentId.1, recent_booster.1, recent_booster.2),
         recent_booster.secondary = ifelse(primary==ResidentId.1, recent_booster.2, recent_booster.1)) %>%
  mutate(inf.primary = ifelse(primary==ResidentId.1, num_pos.1>0, num_pos.2>0),
         inf.secondary = ifelse(primary==ResidentId.1, num_pos.2>0, num_pos.1>0)) %>% 
  left_join(a, by=c("label", "primary"="ResidentId.1")) %>% 
  left_join(a2, by=c("label", "primary"="ResidentId.2")) %>% 
  mutate(adjusted_start.x=as.character(adjusted_start.x), 
         adjusted_start.y=as.character(adjusted_start.y),
         last_chunked.x=as.character(last_chunked.x),
         last_chunked.y=as.character(last_chunked.y)) %>%
  replace_na(list(adjusted_start.x="", adjusted_start.y="",
                  last_chunked.x="", last_chunked.y="")) %>% 
  mutate(adjusted_start=paste0(adjusted_start.x, adjusted_start.y)%>%as.Date(), 
         last_chunked=paste0(last_chunked.x, last_chunked.y)%>%as.Date())

both_have_testing <- both_have_testing %>% 
  left_join(a, by=c("label", "ResidentId.1"="ResidentId.1")) %>% 
  left_join(a2, by=c("label", "ResidentId.2"="ResidentId.2")) %>% 
  rowwise() %>% 
  mutate(primary=case_when(recent_booster.1==recent_booster.2&duration_chunked.x>duration_chunked.y~ResidentId.1,
                           recent_booster.1==recent_booster.2&duration_chunked.x<duration_chunked.y~ResidentId.2,
                           recent_booster.1==recent_booster.2~sample(c(ResidentId.1, ResidentId.2), size=1),
                           recent_booster.1~ResidentId.2,
                           recent_booster.2~ResidentId.1,
                           T~sample(c(ResidentId.1, ResidentId.2), size = 1))) %>%
  mutate(adjusted_start=case_when(primary==ResidentId.1~adjusted_start.x, 
                                  T~adjusted_start.y),
         last_chunked=case_when(primary==ResidentId.1~last_chunked.x, 
                                T~last_chunked.y)) %>%
  mutate(inf.primary = case_when(primary==ResidentId.1~num_pos.1,
                                 primary==ResidentId.2~num_pos.2), 
         inf.secondary = case_when(primary==ResidentId.1~num_pos.2,
                                   primary==ResidentId.2~num_pos.1)) %>% select(names(one_has_testing))


matching <- both_have_testing %>% rbind(one_has_testing)

matching <- matching %>% 
  mutate(treatment = ifelse(both_unvacc, 1, 0))

matching$treatment %>% table()

write_csv(matching, "full_data_prematching_030823.csv")

