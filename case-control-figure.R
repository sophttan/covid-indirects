model <- clogit(case ~ has.vacc.roommate.binary + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)

results <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())[1:2,]
results

model <- clogit(case ~ dose.roommate.adjusted + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)
model <- (summary(model)$coefficients)[1,]
or <- exp(model[1]*1:4)
low <- exp(model[1]*1:4-1.96*model[3])
high <- exp(model[1]*1:4+1.96*model[3])
basic_results <- cbind(or, low, high)
rownames(basic_results) <- c("dose.1", "dose.2", "dose.3", "dose.4")
colnames(basic_results) <- colnames(results)

results <- rbind(results, basic_results)

model <- clogit(case ~ time_since_inf_cut.roommate + has.vacc.roommate.binary + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)
basic_results <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())[1:5,]

results <- rbind(results, basic_results)
results

model <- clogit(case ~ time_since_vacc_cut.roommate + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)
summary(model)

basic_results <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())[1:5,]

results <- rbind(results, basic_results)
results

results <- (1-results)*100
results <- results %>% mutate(x=rownames(results))

results <- results %>% mutate(inf_vacc=if_else(grepl("vacc|dose",x), "Vaccination", "Prior infection") %>% 
                                factor(levels=c("Vaccination", "Prior infection")), 
                              group=case_when(grepl("binary|prior.inf", x)~"Binary",
                                              grepl("dose", x)~"By dose",
                                              T~"By time")) %>%
  mutate(time=c("Any","Any","1","2","3","4",rep(c("<3 months", "3-6 months", "6-12 months", "1-2 years", "2+ years"), 2)),
         time=factor(time, levels=c("Any","1","2","3","4","<3 months", "3-6 months", "6-12 months", "1-2 years", "2+ years")))

library(ggh4x)
library(RColorBrewer)
p1 <- ggplot(results%>%filter(group!="By time"), aes(x=time, y=., color=inf_vacc)) + geom_point() + 
  geom_errorbar(aes(ymin=`97.5 %`, ymax=`2.5 %`), width=0.2) + 
  geom_hline(yintercept=0, linetype=2) + 
  facet_grid2(group~inf_vacc, scales="free_x", independent = "x", render_empty = F, switch = "y") + 
  scale_y_continuous(position = "left", "Indirect protection (%)") + 
  scale_color_brewer(palette = "Dark2") + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        legend.position = "none") 
p1

p2 <- ggplot(results%>%filter(group=="By time"), aes(x=time, y=., color=inf_vacc)) + geom_point() + 
  geom_errorbar(aes(ymin=`97.5 %`, ymax=`2.5 %`), width=0.2) + 
  geom_hline(yintercept=0, linetype=2) + 
  facet_grid2(group~inf_vacc, scale="free_x", independent = "x", render_empty = F, switch = "y") + 
  scale_y_continuous("Indirect protection (%)") + 
  scale_color_brewer(palette = "Dark2") + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        legend.position = "none") 

p1
p2
