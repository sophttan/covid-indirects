final_results <- NULL
for (i in 0:4) {
  d <- matched_infvacc_roommate %>% group_by(group) %>% filter(all(num_dose_adjusted==i))
  model <- clogit(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + 
                    age + age.roommate + risk + risk.roommate + strata(group), data=d)
  results <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())[1:2,]
  
  results <- (1-results)*100
  results <- results %>% mutate(x=rownames(results))
  results <- results %>% mutate(vacc=i)
  
  final_results <- rbind(final_results, results)
}

final_results


final_results <- NULL
for (i in 0:1) {
  d <- matched_infvacc_roommate %>% group_by(group) %>% filter(all(has.prior.inf==i))
  model <- clogit(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + 
                    age + age.roommate + risk + risk.roommate + strata(group), data=d)
  results <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())[1:2,]
  
  results <- (1-results)*100
  results <- results %>% mutate(x=rownames(results))
  results <- results %>% mutate(inf=i)
  
  final_results <- rbind(final_results, results)
}

final_results
