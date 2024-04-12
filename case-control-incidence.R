
library(patchwork)

inc <- read_csv("D:/CCHCS_premium/st/indirects/building_incidence.csv")
inc <- inc %>% rename("day.inc"="Day")

matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(test.Day.offset.early = test.Day-3, test.Day.offset.late = test.Day-17) %>%
  full_join(inc, by=c("Institution", "BuildingId")) %>% 
  mutate(inf=if_else(day.inc%>%is.na()|day.inc>test.Day.offset.early|day.inc<test.Day.offset.late, 0, inf)) %>% group_by(id) %>%
  mutate(inf=sum(inf)) %>% distinct(id, .keep_all = T)

matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(inf.cat=case_when(inf==0~0,
                                                                                  inf<=3~1,
                                                                                  T~2))

matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(time_since_vacc = (test.Day-Date_offset)%>%as.numeric(),
         time_since_vacc.roommate = (test.Day-last.vacc.roommate) %>% as.numeric()) %>%
  mutate(time_since_vacc_cut=cut(time_since_vacc, breaks=c(0,182,Inf), right = F),
         time_since_vacc_cut.roommate=cut(time_since_vacc.roommate, breaks=c(0,182,Inf), right = F)) 

levels(matched_infvacc_roommate$time_since_vacc_cut)<-c(levels(matched_infvacc_roommate$time_since_vacc_cut), "None") 
matched_infvacc_roommate$time_since_vacc_cut[is.na(matched_infvacc_roommate$time_since_vacc_cut)] <- "None"
matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(time_since_vacc_cut = factor(time_since_vacc_cut, levels=c("None","[0,182)","[182,Inf)")))

levels(matched_infvacc_roommate$time_since_vacc_cut.roommate)<-c(levels(matched_infvacc_roommate$time_since_vacc_cut.roommate), "None") 
matched_infvacc_roommate$time_since_vacc_cut.roommate[is.na(matched_infvacc_roommate$time_since_vacc_cut.roommate)] <- "None"
matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(time_since_vacc_cut.roommate = factor(time_since_vacc_cut.roommate, levels=c("None","[0,182)","[182,Inf)")))

matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(time_since_inf.roommate = (test.Day-last.inf.roommate) %>% as.numeric()) %>%
  mutate(time_since_inf_cut.roommate=cut(time_since_inf.roommate, breaks=c(0,182,Inf), right = F)) 
levels(matched_infvacc_roommate$time_since_inf_cut.roommate)<-c(levels(matched_infvacc_roommate$time_since_inf_cut.roommate), "None") 
matched_infvacc_roommate$time_since_inf_cut.roommate[is.na(matched_infvacc_roommate$time_since_inf_cut.roommate)] <- "None"
matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(time_since_inf_cut.roommate = factor(time_since_inf_cut.roommate, levels=c("None","[0,182)","[182,Inf)")))

matched_infvacc_roommate <- matched_infvacc_roommate %>% 
  mutate(latest=pmax(last.inf.roommate, last.vacc.roommate,na.rm=T)) %>%
  mutate(time_since_infvacc.roommate = (test.Day-latest)%>%as.numeric()) %>%
  mutate(time_since_infvacc_cut.roommate=cut(time_since_infvacc.roommate, breaks=c(0,182,Inf), right = F)) 
levels(matched_infvacc_roommate$time_since_infvacc_cut.roommate)<-c(levels(matched_infvacc_roommate$time_since_infvacc_cut.roommate), "None") 
matched_infvacc_roommate$time_since_infvacc_cut.roommate[is.na(matched_infvacc_roommate$time_since_infvacc_cut.roommate)] <- "None"
matched_infvacc_roommate <- matched_infvacc_roommate %>% mutate(time_since_infvacc_cut.roommate = factor(time_since_infvacc_cut.roommate, levels=c("None","[0,182)","[182,Inf)")))

final_results <- NULL
for (i in 0:2) {
  d <- matched_infvacc_roommate %>% group_by(group) %>% filter(all(inf.cat==i))
  model <- clogit(case ~ time_since_vacc_cut.roommate + has.prior.inf.roommate + 
                    age + age.roommate + risk + risk.roommate + strata(group), data=d)
  basic_results <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())[1:2,]

  model <- clogit(case ~ time_since_inf_cut.roommate + has.vacc.roommate.binary + 
                    age + age.roommate + risk + risk.roommate + strata(group), data=d)
  basic_results2 <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())[1:2,]
  
  model <- clogit(case ~ time_since_infvacc_cut.roommate + 
                    age + age.roommate + risk + risk.roommate + strata(group), data=d)
  basic_results3 <- (exp(coef(model))%>%cbind(exp(confint(model))) %>% as.data.frame())[1:2,]
  
  results <- rbind(basic_results, basic_results2, basic_results3)
  
  
  results <- (1-results)*100
  results <- results %>% mutate(x=rownames(results))
  results <- results %>% mutate(inf=i)
  results <- results %>% 
    mutate(inf_vacc=case_when(grepl("infvacc",x)~"Most recent inf or vacc",
                              grepl("vacc", x)~"Vaccination",
                              T~"Prior infection") %>% 
             factor(levels=c("Vaccination", "Prior infection","Most recent inf or vacc")), 
           group="By time") %>%
    mutate(time=rep(c("<6", "6+"),3),
           time=factor(time, levels=c("<6", "6+"))) 

  final_results <- rbind(final_results, results)
}


final_results <- final_results %>% mutate(inf=factor(inf, labels=c("No confirmed infections", "<4 infections", "4+ infections")))

p <- ggplot(final_results, aes(x=time, y=., color=inf_vacc)) + geom_point() + 
  geom_errorbar(aes(ymin=`97.5 %`, ymax=`2.5 %`), width=0.2) + 
  geom_hline(yintercept=0, linetype=2) + 
  facet_grid2(inf_vacc~inf, scale="free_x", independent = "x", render_empty = F) + 
  scale_x_discrete("Months") + 
  scale_y_continuous("Indirect protection (%)", breaks=seq(-50, 80, 25)) + 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.text.y = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 
p
ggsave("D:/CCHCS_premium/st/covid-indirects/figures/incidence.jpg", width=8, height=7, dpi=300)
