filter_testing <- function(d) {
  d <- d %>% mutate(has_test=any(Day>=start&Day<=end))
  d %>% filter((Day >= start & Day <= end)|(!has_test&Day==first(Day))) %>% 
    mutate(Day=if_else(!has_test, as.Date(NA), Day), 
           Result=if_else(!has_test, as.character(NA), Result))
}

# use matched time where we use maximum overlapped time
treatment <- m_adjusted %>% filter(treatment==0)
control <- m_adjusted %>% filter(treatment==1)

treatment_testing <- treatment %>% add_testing() %>% 
  mutate(start=int_start(intersection)%>%as.Date(), end=int_end(intersection)%>%as.Date())
treatment_time_test <- treatment_testing %>% 
  group_by(id) %>%
  filter_testing() %>% 
  mutate(survival_time=as.numeric(Day-start)+1) %>%
  mutate(last = ifelse(any(Result=="Positive",na.rm=T)&Result=="Positive", Result=="Positive", Day==last(Day))) %>% 
  mutate(pos_first=first(Result)=="Positive") %>%
  filter(last|!has_test) %>% summarise_all(first) %>% select(!last) %>% 
  mutate(survival_time=ifelse(!has_test|Result!="Positive", as.numeric(end-start)+1, survival_time))

control_testing <- treatment %>% select(subclass, id, intersection) %>% rename("id_treat"="id") %>% 
  left_join(control %>% select(!intersection), by="subclass") %>%
  mutate(first_intersection=int_start(intersection)%>%as.Date(), last_intersection=int_end(intersection)%>%as.Date()) %>%
  arrange(id, first_intersection, desc(intersect))
control_testing %>%  select(id, subclass, intersection, intersect) %>% arrange(id)

control_testing_unique <- control_testing %>% group_by(id) %>% distinct(id, first_intersection, .keep_all = T)

control_test_overlap <- control_testing %>% group_by(id) %>% 
  mutate(next_intersection=lag(intersection, 1)) %>% 
  mutate(overlap_next=!intersect(intersection, next_intersection) %>% is.na()) 

distinct_overlap <- control_test_overlap %>% filter(!overlap_next) %>% mutate(label=1:n())
control_test_overlap <- control_test_overlap %>% left_join(distinct_overlap)
control_test_overlap <- control_test_overlap %>% fill(label, .direction="down") %>% group_by(id, label) %>% 
  mutate(start=min(first_intersection)%>%as.Date(), end=max(last_intersection)%>%as.Date())

control_test_overlap %>% 
  select(id, id_treat, subclass, intersection, start, end, label) %>% head(20)

control_testing_unique <- control_test_overlap %>% group_by(id, label) %>% summarise_all(first)
control_time_test <- control_testing_unique %>% group_by(id, label) %>% 
  add_testing() %>% 
  filter_testing() %>% 
  mutate(survival_time=as.numeric(Day-start)+1) %>% 
  mutate(last = ifelse(any(Result=="Positive",na.rm=T)&Result=="Positive", Result=="Positive", Day==last(Day))) %>%
  mutate(pos_first=first(Result)=="Positive") %>%
  filter(!has_test|last) %>% summarise_all(first) %>% select(!last) %>% 
  mutate(survival_time=ifelse(!has_test|Result!="Positive", as.numeric(end-start)+1, survival_time))

control_time_test <- control_time_test %>% select(!c(id_treat, first_intersection, last_intersection, next_intersection, overlap_next))


full <- treatment_time_test %>% 
  select(id, treatment, subclass, weights, BuildingId, primary, secondary, vacc.primary, vacc.secondary, inf.primary, inf.secondary, start, end, survival_time, Result) %>% 
  rbind(control_time_test %>% select(id, treatment, subclass, weights, BuildingId, primary, secondary, vacc.primary, vacc.secondary, inf.primary, inf.secondary, start, end, survival_time, Result))

full <- full %>% group_by(subclass) %>% filter(any(treatment==1)&any(treatment==0))
full <- full %>% mutate(weights=ifelse(sum(treatment==1)>1&treatment==1, sum(treatment==1), 1))
full <- full %>% mutate(treatment=1-treatment)
full$treatment%>%table()

full <- full %>% 
  mutate(status=ifelse(!Result%>%is.na()&Result=="Positive", 1, 0), 
         BuildingId=as.factor(BuildingId),
         subclass=as.factor(subclass)) 

full%>%group_by(treatment)%>%summarise(units=length(unique(id)), cases=sum(status), person_time=n()*mean(survival_time))%>%mutate(inc_rate=cases/person_time)

# full <- full %>% mutate(time_since_start=as.numeric(difftime(start, "2021-12-15", units="days")))
full <- full %>% group_by(treatment) %>% mutate(n=1:n()) %>% mutate(n=ifelse(treatment==0, n, NA))
full <- full %>% arrange(subclass, start) %>% ungroup() %>% 
  fill(n, .direction="down") %>%
  group_by(n) %>% 
  mutate(time_since_start_rel=start-first(start))

fit <- survfit(Surv(survival_time, status, type="right")~treatment, data = full)
autoplot(fit) + 
  xlab("Time") + ylab("Survival") + 
  scale_fill_discrete(name=element_blank(), label=c("Control", "Treatment")) + 
  guides(color=F)


results <- coxph(Surv(survival_time, status) ~ 
                   treatment + vacc.primary + inf.primary + inf.secondary + BuildingId + frailty(subclass), 
                 data=full)

tbl_regression(results, exp = TRUE) 


# vacc_check <- full %>% group_by(id) %>%
#   left_join(vacc %>% select(ResidentId, Date_offset, num_dose), by=c("secondary"="ResidentId")) %>% 
#   filter(Date_offset<=start) %>% 
#   arrange(id, desc(Date_offset)) %>% summarise_all(first)
# vacc_check <- vacc_check %>% mutate(time_since_vacc=start-Date_offset)
# vacc_check%>%ggplot(aes(time_since_vacc)) + geom_histogram()
