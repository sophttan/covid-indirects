units_testing1 <- any_unvacc_over14_noinf %>% 
  left_join(testing, by=c("ResidentId.1"="ResidentId")) %>% 
  filter(Test >= first & Test <= last) %>% rename("Test.1" = "Test")
units_testing1 %>% select(Institution, RoomId, first, last, ResidentId.1, Test.1)

# test breaking apart 
a <- units_testing1 %>% select(label, ResidentId.1, first,last,duration,Test.1) %>%
  group_by(label) %>% 
  mutate(diff_test=diff(c(first(first),Test.1))) 

a <- a %>% group_by(label) %>% mutate(new_chunk=diff_test>14)
chunks <- a %>% filter(Test.1==first(Test.1)|new_chunk) %>% select(label, Test.1) %>% mutate(chunks=1:n())
a <- a %>% left_join(chunks, by=c("label", "Test.1")) 
a <- a %>% arrange(label, Test.1) %>% fill(chunks, .direction="down")
a_final <- a %>% group_by(label, chunks) %>% 
  mutate(first_chunked = case_when(first(diff_test)<=14~first,
                                   T~first(Test.1)),
         adjusted_start=case_when(first==first_chunked~first+5,
                                  first!=first_chunked~first_chunked),
         last_chunked = last(Test.1))%>% 
  summarise_all(first) %>% 
  mutate(duration_testing=last_chunked-first_chunked) %>% 
  group_by(label) %>% 
  filter(duration_testing==max(duration_testing)) %>% 
  filter(duration_testing > 0) %>% filter(chunks==first(chunks)) %>% 
  select(label, ResidentId.1, first_chunked, adjusted_start, last_chunked, duration_testing)


units_testing2 <- any_unvacc_over14_noinf %>% left_join(testing, by=c("ResidentId.2"="ResidentId")) %>% 
  filter(Test >= first & Test <= last) %>% rename("Test.2" = "Test")
units_testing2 %>% select(Institution, RoomId, first, last, ResidentId.2, Test.2)

# test breaking apart 
a2 <- units_testing2 %>% select(label, ResidentId.2, first,last,duration,Test.2) %>%
  group_by(label) %>% 
  mutate(diff_test=diff(c(first(first),Test.2))) 

a2 <- a2 %>% group_by(label) %>% mutate(new_chunk=diff_test>14)
chunks <- a2 %>% filter(Test.2==first(Test.2)|new_chunk) %>% select(label, Test.2) %>% mutate(chunks=1:n())
a2 <- a2 %>% left_join(chunks, by=c("label", "Test.2")) 
a2 <- a2 %>% arrange(label, Test.2) %>% fill(chunks, .direction="down")
a2_final <- a2 %>% group_by(label, chunks) %>% 
  mutate(first_chunked = case_when(first(diff_test)<=14~first,
                                   T~first(Test.2)),
         adjusted_start=case_when(first==first_chunked~first+5,
                                  first!=first_chunked~first_chunked),
         last_chunked = last(Test.2))%>% 
  summarise_all(first) %>% 
  mutate(duration_testing=last_chunked-first_chunked) %>% 
  group_by(label) %>% 
  filter(duration_testing==max(duration_testing)) %>% 
  filter(duration_testing > 0) %>% filter(chunks==first(chunks)) %>% 
  select(label, ResidentId.2, first_chunked, adjusted_start, last_chunked, duration_testing)


treatment <- any_unvacc_over14_noinf %>% filter(one_unvacc) %>% 
  filter((label %in% a_final$label|vacc.1>0) & (label %in% a2_final$label|vacc.2>0)) %>%
  mutate(primary = case_when(vacc.1==0~ResidentId.1, 
                             vacc.2==0~ResidentId.2)) %>% 
  mutate(inf.primary = ifelse(vacc.1==0, num_pos.1>0, num_pos.2>0),
         inf.secondary = ifelse(vacc.1>0, num_pos.1>0, num_pos.2>0)) %>% 
  left_join(a_final, by=c("label", "primary"="ResidentId.1")) %>% 
  left_join(a2_final, by=c("label", "primary"="ResidentId.2")) %>% 
  mutate(adjusted_start.x=as.character(adjusted_start.x), 
         adjusted_start.y=as.character(adjusted_start.y),
         last_chunked.x=as.character(last_chunked.x),
         last_chunked.y=as.character(last_chunked.y)) %>%
  replace_na(list(adjusted_start.x="", adjusted_start.y="",
                  last_chunked.x="", last_chunked.y="")) %>% 
  mutate(adjusted_start=paste0(adjusted_start.x, adjusted_start.y)%>%as.Date(), 
         last_chunked=paste0(last_chunked.x, last_chunked.y)%>%as.Date())



control <- any_unvacc_over14_noinf %>% filter(both_unvacc) %>% 
  filter(label %in% a_final$label | label %in% a2_final$label) 

controlboth <- control %>% filter(label %in% a_final$label & label %in% a2_final$label) %>%
  left_join(a_final, by=c("label", "ResidentId.1"="ResidentId.1")) %>% 
  left_join(a2_final, by=c("label", "ResidentId.2"="ResidentId.2")) %>% 
  rowwise() %>% 
  mutate(primary=case_when(duration_testing.x>duration_testing.y~ResidentId.1,
                           duration_testing.x<duration_testing.y~ResidentId.2,
                           T~sample(c(ResidentId.1, ResidentId.2), size = 1))) %>%
  mutate(adjusted_start=case_when(primary==ResidentId.1~adjusted_start.x, 
                                  T~adjusted_start.y),
         last_chunked=case_when(primary==ResidentId.1~last_chunked.x, 
                                T~last_chunked.y)) %>%
  mutate(inf.primary = case_when(primary==ResidentId.1~num_pos.1,
                                 primary==ResidentId.2~num_pos.2), 
         inf.secondary = case_when(primary==ResidentId.1~num_pos.2,
                                   primary==ResidentId.2~num_pos.1)) %>% select(names(treatment))

controlone <- control %>% filter(!label %in% controlboth$label)

controlone <- controlone %>%
  rowwise() %>% 
  mutate(both_test=label%in%a_final$label&label%in%a2_final$label) %>%
  mutate(primary = case_when(label %in% a_final$label & !label %in% a2_final$label~ResidentId.1,
                             label %in% a2_final$label & !label %in% a_final$label~ResidentId.2,
                             label %in% a_final$label & label %in% a2_final$label~sample(c(ResidentId.1, ResidentId.2), size = 1))) %>%
  mutate(inf.primary = case_when(primary==ResidentId.1~num_pos.1,
                                 primary==ResidentId.2~num_pos.2), 
         inf.secondary = case_when(primary==ResidentId.1~num_pos.2,
                                   primary==ResidentId.2~num_pos.1)) %>% 
  left_join(a_final, by=c("label", "primary"="ResidentId.1")) %>% 
  left_join(a2_final, by=c("label", "primary"="ResidentId.2")) %>% 
  mutate(adjusted_start.x=as.character(adjusted_start.x), 
         adjusted_start.y=as.character(adjusted_start.y),
         last_chunked.x=as.character(last_chunked.x),
         last_chunked.y=as.character(last_chunked.y)) %>%
  replace_na(list(adjusted_start.x="", adjusted_start.y="",
                  last_chunked.x="", last_chunked.y="")) %>% 
  mutate(adjusted_start=paste0(adjusted_start.x, adjusted_start.y)%>%as.Date(), 
         last_chunked=paste0(last_chunked.x, last_chunked.y)%>%as.Date()) %>% select(names(treatment))

matching <- treatment %>% rbind(controlboth, controlone) %>% 
  mutate(inf.primary=ifelse(inf.primary%>%is.na(), 0, 1), 
         inf.secondary=ifelse(inf.secondary%>%is.na(), 0, 1))