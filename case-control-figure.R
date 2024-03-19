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
basic_results <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())[1:4,]

results <- rbind(results, basic_results)
results

model <- clogit(case ~ time_since_vacc_cut.roommate + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)
summary(model)

basic_results <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())[1:4,]

results <- rbind(results, basic_results)
results

model <- clogit(case ~ time_since_infvacc_cut.roommate + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)
summary(model)

basic_results <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())[1:4,]

results <- rbind(results, basic_results)
results

results <- (1-results)*100
results <- results %>% mutate(x=rownames(results))

results <- results %>% mutate(inf_vacc=case_when(grepl("infvacc", x)~"Most recent vaccination or infection",
                                                 grepl("vacc|dose", x)~"Vaccination",
                                                 T~"Prior infection") %>% 
                                factor(levels=c("Vaccination", "Prior infection", "Most recent vaccination or infection")), 
                              group=case_when(grepl("binary|prior.inf", x)~"Binary",
                                              grepl("dose", x)~"By dose",
                                              T~"By time")) %>%
  mutate(time=c("Any","Any","1","2","3","4",rep(c("<3", "3-6", "6-12", "12+"), 3)),
         time=factor(time, 
                     levels=c("Any","1","2","3","4","<3", "3-6", "6-12", "12+"),
                     labels=c("Any","Partially\nvaccinated","Primary\nseries only","1 booster","2+ boosters","<3", "3-6", "6-12", "12+"))) %>%
  mutate(value=if_else(inf_vacc=="Vaccination"&group!="By time", 1.5, NA))

library(ggh4x)
library(RColorBrewer)
p1 <- ggplot(results%>%filter(group!="By time"), aes(x=time, y=., color=inf_vacc)) + geom_point() + 
  geom_errorbar(aes(ymin=`97.5 %`, ymax=`2.5 %`), width=0.2) + 
  geom_hline(yintercept=0, linetype=2) +   
  geom_vline(aes(xintercept=value)) + 
  facet_grid2(~inf_vacc, space="free_x", scales="free_x", render_empty = F, switch = "y") + 
  scale_y_continuous(position = "left", "Indirect protection (%)") + 
  scale_color_brewer(palette = "Dark2") + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        text=element_text(size=14, family="sans")) 
p1
ggsave("D:/CCHCS_premium/st/covid-indirects/figures/figure2.jpg", width=8, height=4, dpi=300)

p2 <- ggplot(results%>%filter(group=="By time")%>%filter(!(inf_vacc=="Vaccination"&time=="2+ years")), aes(x=time, y=., color=inf_vacc)) + geom_point() + 
  geom_errorbar(aes(ymin=`97.5 %`, ymax=`2.5 %`), width=0.2) + 
  geom_hline(yintercept=0, linetype=2) + 
  facet_grid2(~inf_vacc, scale="free_x", independent = "x", render_empty = F, switch = "y") + 
  scale_x_discrete("Months") + 
  scale_y_continuous("Indirect protection (%)") + 
  scale_color_brewer(palette = "Dark2") + 
  theme_bw() + 
  theme(legend.position = "none") 
p2
