matched_infvacc_roommate 

matched_infvacc_roommate <- matched_infvacc_roommate %>% group_by(group)

grouped <- matched_infvacc_roommate %>% group_by(group) %>% arrange(group, desc(case)) %>% 
  summarise(vacc_diff=diff(time_since_vacc),
            inf_diff=diff(time_since_inf))

grouped$vacc_diff%>%hist(main = "Difference in time since vaccine\nbetween matched case and control", xlab="Days")
grouped$vacc_diff%>%summary()

grouped$inf_diff%>%hist(main = "Difference in time since infection\nbetween matched case and control", xlab="Days")
grouped$inf_diff%>%summary()

matched_infvacc_roommate %>% group_by(group) %>% filter(dose.roommate.adjusted[1]!=dose.roommate.adjusted[2])
matched_infvacc_roommate %>% group_by(group) %>% filter(has.prior.inf.roommate[1]!=has.prior.inf.roommate[2])
matched_infvacc_roommate %>% group_by(group) %>% filter(has.vacc.roommate.binary[1]!=has.vacc.roommate.binary[2] & has.prior.inf.roommate[1]!=has.prior.inf.roommate[2])



inf <- read_csv("D:/CCHCS_premium/st/cleaned-data/infection_data022624.csv") %>% filter(CollectionDate <= "2022-12-15") %>%
  select(ResidentId, CollectionDate) %>% rename(new.inf=CollectionDate)
has_new_inf <- matched %>% left_join(inf, "ResidentId") %>%
  filter(new.inf>test.Day & new.inf-test.Day<14) 
has_new_inf %>% group_by(case) %>% summarise(n=n())

## try excluding from matching - check if results same if same include them and say that we did this analysis