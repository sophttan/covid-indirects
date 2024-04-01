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


matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(time_since_vacc_cut2.roommate=cut(time_since_vacc.roommate, breaks=c(0, 30, 60, 90, 182, 365, Inf), right = F)) 

levels(matched_infvacc_roommate$time_since_vacc_cut2.roommate)<-c(levels(matched_infvacc_roommate$time_since_vacc_cut2.roommate), "None") 
matched_infvacc_roommate$time_since_vacc_cut2.roommate[is.na(matched_infvacc_roommate$time_since_vacc_cut2.roommate)] <- "None"
matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(time_since_vacc_cut2.roommate = factor(time_since_vacc_cut2.roommate, levels=c("None","[0,30)", "[30,60)", "[60,90)","[90,182)","[182,365)","[365,Inf)")))

model <- clogit(case ~ time_since_vacc_cut2.roommate + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)
summary(model)

basic_results <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())[1:3,]

results <- rbind(results, basic_results)
results


results <- (1-results)*100
results <- results %>% mutate(x=rownames(results))

results <- results %>% mutate(inf_vacc=case_when(grepl("infvacc", x)~"Most recent vaccination or infection",
                                                 grepl("vacc|dose", x)~"Vaccination",
                                                 T~"Prior infection"), 
                              group=case_when(grepl("binary|prior.inf", x)~"Binary",
                                              grepl("dose", x)~"By dose",
                                              T~"By time")) %>%
  mutate(time=c("Any","Any","1","2","3","4",rep(c("<3", "3-6", "6-12", "12+"), 3), "<1", "1-2", "2-3"),
         time=factor(time, 
                     levels=c("Any","1","2","3","4","<3", "3-6", "6-12", "12+",  "<1", "1-2", "2-3"),
                     labels=c("Any","Partially\nvaccinated","Primary\nseries only","1 booster","2+ boosters","<3", "3-6", "6-12", "12+", "<1", "1-2", "2-3"))) %>%
  mutate(value=if_else(inf_vacc=="Vaccination"&group!="By time", 1.5, NA))

results <- results %>% mutate(inf_vacc=if_else(group=="By time"&grepl("30|60",x), "First three months of vaccination", inf_vacc) %>%
                                factor(levels=c("Vaccination", "Prior infection", "Most recent vaccination or infection", "First three months of vaccination")))

library(ggh4x)
library(RColorBrewer)
p1 <- ggplot(results%>%filter(group!="By time"), aes(x=time, y=., color=inf_vacc)) + geom_point() + 
  geom_errorbar(aes(ymin=`97.5 %`, ymax=`2.5 %`), width=0.2) + 
  geom_hline(yintercept=0, linetype=2) +   
  geom_vline(aes(xintercept=value)) + 
  facet_grid2(~inf_vacc, space="free_x", scales="free_x", render_empty = F, switch = "y") + 
  scale_y_continuous(position = "left", "Indirect protection (%)") + 
  scale_color_brewer(palette = "Dark2") + 
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 
p1
ggsave("D:/CCHCS_premium/st/covid-indirects/figures/figure2.jpg", width=8, height=4, dpi=300)

p2 <- ggplot(results%>%filter(group=="By time"&inf_vacc!="First three months of vaccination")%>%
               mutate(inf_vacc=factor(inf_vacc, levels=c("Vaccination", "Prior infection", "Most recent vaccination or infection"))), 
             aes(x=time, y=., color=inf_vacc)) + geom_point() + 
  geom_errorbar(aes(ymin=`97.5 %`, ymax=`2.5 %`), width=0.2) + 
  geom_hline(yintercept=0, linetype=2) + 
  facet_grid2(~inf_vacc, scale="free_x", independent = "x", render_empty = F, switch = "y") + 
  scale_x_discrete("Months") + 
  scale_y_continuous("Indirect protection (%)") + 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 
p2
ggsave("D:/CCHCS_premium/st/covid-indirects/figures/figure3.jpg", width=11, height=4, dpi=300)

p2 <- ggplot(results%>%filter(group=="By time"&inf_vacc!="First three months of vaccination")%>%
               mutate(inf_vacc=factor(inf_vacc, levels=c("Vaccination", "Prior infection", "Most recent vaccination or infection"))), 
             aes(x=time, y=., color=inf_vacc)) + geom_point() + 
  geom_errorbar(aes(ymin=`97.5 %`, ymax=`2.5 %`), width=0.2) + 
  geom_hline(yintercept=0, linetype=2) + 
  facet_grid2(~inf_vacc, scale="free_x", independent = "x", render_empty = F, switch = "y") + 
  scale_x_discrete("Months") + 
  scale_y_continuous("Indirect protection (%)") + 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 
p2
ggsave("D:/CCHCS_premium/st/covid-indirects/figures/figure3.jpg", width=11, height=4, dpi=300)

p3 <- ggplot(results%>%filter(inf_vacc=="First three months of vaccination"),
             aes(x=time, y=., color=inf_vacc)) + geom_point() + 
  geom_errorbar(aes(ymin=`97.5 %`, ymax=`2.5 %`), width=0.2) + 
  geom_hline(yintercept=0, linetype=2) + 
  facet_grid2(~inf_vacc, scale="free_x", independent = "x", render_empty = F, switch = "y") + 
  scale_x_discrete("Months") + 
  scale_y_continuous("Indirect protection (%)") + 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 
p3
ggsave("D:/CCHCS_premium/st/covid-indirects/figures/figure4.jpg", width=4, height=4, dpi=300)

bivalent <- matched_infvacc_roommate

matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(bivalent = case_when(last.vacc.roommate%>%is.na()~0,
                              last.vacc.roommate<"2022-09-01"~1,
                              T~2) %>% factor(levels=0:2, labels=c("Unvacc", "Not-bivalent", "Bivalent")))
matched_infvacc_roommate %>% select(id, group, last.vacc.roommate, dose.roommate.adjusted, bivalent)

model <- clogit(case ~ bivalent + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)

results <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())

matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(bivalent_time = case_when(last.vacc.roommate%>%is.na()~"Unvacc",
                              last.vacc.roommate>="2022-09-01"&time_since_vacc.roommate<30~"Bivalent<30",
                              last.vacc.roommate>="2022-09-01"&time_since_vacc.roommate>=30~"Bivalent>=30",
                              time_since_vacc.roommate<30~"Not-bivalent<30",
                              time_since_vacc.roommate>=30~"Not-bivalent>=30") %>% factor(levels=c("Unvacc", "Not-bivalent<30", "Not-bivalent>=30", "Bivalent<30", "Bivalent>=30")))
matched_infvacc_roommate %>% select(id, group, last.vacc.roommate, dose.roommate.adjusted, bivalent_time)

model <- clogit(case ~ bivalent_time + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + strata(group), data=matched_infvacc_roommate)

results2 <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())
results2 <- results[1:2,]%>%rbind(results2[1:4,])
results2 <- (1-results2)*100

ggplot(results2%>%mutate(bivalent=c("Not bivalent","Bivalent","Not bivalent", "Not bivalent", "Bivalent","Bivalent"), 
                              time=c("Overall", "Overall", "<30", "30+", "<30","30+")%>%factor(levels=c("Overall","<30","30+"))), 
       aes(x=time, y=., group=bivalent, color=bivalent)) + geom_point(position = position_dodge(width=0.1)) + 
  geom_errorbar(aes(ymin=`97.5 %`, ymax=`2.5 %`), position=position_dodge(width=0.1), width=0.2) + 
  geom_hline(yintercept=0, linetype=2) + 
  scale_x_discrete("Days") + 
  scale_y_continuous("Indirect protection (%)") + 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 


ggplot(basic_results%>%mutate(time=c("<1", "1-2", "2-3")), 
       aes(x=time, y=., color="Vacc")) + geom_point(position = position_dodge(width=0.1)) + 
  geom_errorbar(aes(ymin=`97.5 %`, ymax=`2.5 %`), position=position_dodge(width=0.1), width=0.2) + 
  geom_hline(yintercept=0, linetype=2) + 
  scale_x_discrete("Months") + 
  scale_y_continuous("Indirect protection (%)") + 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 


m <- matched_infvacc_roommate %>% filter(all(test.Day>="2022-09-01")) %>% group_by(group2) 

m <- m %>% 
  mutate(time_since_vacc.roommate = (test.Day-last.vacc.roommate) %>% as.numeric()) %>%
  mutate(time_since_vacc_cut.roommate=cut(time_since_vacc.roommate, breaks=c(0, 30, 90, 182, 365, Inf), right = F)) 

levels(m$time_since_vacc_cut.roommate)<-c(levels(m$time_since_vacc_cut.roommate), "None") 
m$time_since_vacc_cut.roommate[is.na(m$time_since_vacc_cut.roommate)] <- "None"
m <- m %>% mutate(time_since_vacc_cut.roommate = factor(time_since_vacc_cut.roommate, levels=c("None","[0,30)","[30,90)","[90,182)","[182,365)","[365,Inf)")))

model <- clogit(case ~ bivalent + has.prior.inf.roommate + 
                  age + age.roommate + risk + risk.roommate + strata(group2), data=m)
summary(model)

