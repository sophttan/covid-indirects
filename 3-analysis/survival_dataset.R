m_adjusted <- m_adjusted %>% 
  select(id, subclass, Institution, BuildingId, RoomId, RoomType, 
         primary, inf.primary, treatment, adjusted_start, last_chunked, intersect, intersection)

m_adjusted_testing <- m_adjusted %>% left_join(testing %>% select(ResidentId, Day, Result), by=c("primary"="ResidentId")) %>%
  group_by(id)

treatment <- m_adjusted_testing %>% filter(treatment==0)
control <- m_adjusted_testing %>% filter(treatment==1)

treatment_testing <- treatment %>% filter(any(Day %within% intersection)) %>% filter(Day %within% intersection)
treatment_time_test <- treatment_testing %>% mutate(survival_time=Day-as.Date(int_start(intersection))) %>% 
  mutate(last = ifelse(any(Result=="Positive")&Result=="Positive", Result=="Positive", Day==last(Day))) %>% 
  filter(last) %>% summarise_all(first) %>% select(!last)

treatment_surv <- treatment_time_test %>% mutate(status=ifelse(Result=="Positive", 1, 0)) %>% select(survival_time, status)
fit <- survfit(Surv(survival_time, status)~1, data = treatment_surv)
autoplot(fit)

# control_testing <- treatment %>% summarise_all(first) %>% select(subclass, id, intersection) %>% 
#   left_join(control %>% select(!intersection), by="subclass") %>% 
#   select(names(treatment_testing)) %>% group_by(id.x) %>% 
#   filter(Day %within% intersection) %>% summarise_all(first) 
# control_testing
# 
# final <- rbind(treatment_testing, control_testing) %>% arrange(subclass, desc(treatment)) %>% group_by(subclass) %>% filter(n()>1)
# final
