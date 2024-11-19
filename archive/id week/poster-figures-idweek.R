# Sophia Tan
# Code for making figures

source(here::here("config.R"))

# load all results for plotting
results <- rbind(read_csv(here::here("results/main/hybrid-results.csv"))[3,], 
                 read_csv(here::here("results/main/binary-results.csv"))[1:2,],
                 read_csv(here::here("results/main/dose-results.csv"))[1:4,],
                 (read_csv(here::here("results/main/time-results.csv"))[c(1:4, 10:13, 19:22),])%>%select(!inf_vacc),
                 read_csv(here::here("results/main/3months-results.csv"))[1:3,])

# label groups and categories
results <- results %>% mutate(inf_vacc=case_when(grepl("both", x)~"Hybrid",
                                                 grepl("infvacc", x)~"Most recent vaccination or infection",
                                                 grepl("vacc|dose", x)~"Vaccine",
                                                 T~"Infection"), 
                              group=case_when(grepl("binary|prior.inf|both", x)~"Binary",
                                              grepl("dose", x)~"By dose",
                                              T~"By time")) %>%
  mutate(time=c("Both", "Any","Any","1","2","3","4",rep(c("<3", "3-6", "6-12", "12+"), 3), "<1", "1-2", "2-3"),
         time=factor(time, 
                     levels=c("Any","Both","1","2","3","4","<3", "3-6", "6-12", "12+",  "<1", "1-2", "2-3"),
                     labels=c("Any","Both","Partially\nvaccinated","Primary\nseries","1 booster","2+ boosters","<3", "3-6", "6-12", "12+", "<1", "1-2", "2-3"))) %>%
  mutate(value=if_else(inf_vacc=="Vaccine"&group!="By time", 1.5, NA))

results <- results %>% mutate(inf_vacc=if_else(group=="By time"&grepl("30|60",x), 
                                               "First three months of vaccination", inf_vacc) %>%
                                factor(levels=c("Vaccine", "Infection", "Hybrid", "Most recent vaccination or infection", "First three months of vaccination")))

# convert OR to indirect protection
results <- results %>% mutate(point=(1-point)*100, 
                              lb=(1-lb)*100, 
                              ub=(1-ub)*100)

ggplot(results%>%filter(group=="By dose"), aes(x=time, y=point, color=inf_vacc)) + geom_point(size=3) + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.2, size=1) + 
  geom_hline(yintercept=0, linetype=2) +   
  facet_grid2(~inf_vacc, space="free_x", scales="free_x", render_empty = F, switch = "y") + 
  scale_y_continuous(position = "left", "Indirect protection (%)", limits=c(0, 40), breaks = seq(0,40,10)) + 
  scale_color_brewer(palette = "Dark2") + 
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_blank(),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=20, family="sans")) 
ggsave("figures/dose-poster.jpg", dpi=400, width=7.5, height=3.5)

ggplot(results%>%filter(group!="By time"&group=="Binary")%>%
         mutate(inf_vacc=c("Hybrid immunity", "Any vaccine", "Any prior infection")%>%
                  factor(levels=c("Any vaccine", "Any prior infection", "Hybrid immunity"))), 
       aes(x=time, y=point, color=inf_vacc)) + geom_point(size=3) + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.2, size=1) + 
  geom_hline(yintercept=0, linetype=2) +   
  facet_grid2(~inf_vacc, space="free_x", scales="free_x", render_empty = F, switch = "y") + 
  scale_y_continuous(position = "left", "Indirect protection (%)", limits=c(-1, 50), breaks = seq(0,50,10)) + 
  scale_color_brewer(palette = "Dark2") + 
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(size=20),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=25, family="sans")) 


# plot strength and durability of indirect protection
ggplot(results%>%filter(group=="By time"&inf_vacc!="First three months of vaccination")%>%
         mutate(inf_vacc=factor(inf_vacc, labels=c("Vaccine", "Infection", "Most recent vaccine\nor infection"))), 
       aes(x=time, y=point, color=inf_vacc)) + geom_point(size=3) + 
  geom_errorbar(aes(ymin=ub, ymax=lb), width=0.2,size=1) + 
  geom_hline(yintercept=0, linetype=2) + 
  facet_grid2(~inf_vacc, scale="free_x", independent = "x", render_empty = F, switch = "y") + 
  scale_x_discrete("Time since (months)") + 
  scale_y_continuous("Indirect protection (%)", limits=c(-3, 60), breaks = seq(0,60,10)) + 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(size=20),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=20, family="sans")) 
ggsave("figures/time-poster.jpg", dpi=400, width=8.5, height=4)

# bivalent
results <- read_csv(here::here("results/main/bivalent-results.csv"))[1:3,]
results <- results %>% mutate(point=(1-point)*100, 
                              lb=(1-lb)*100, 
                              ub=(1-ub)*100)

colors <- RColorBrewer::brewer.pal(11, "BrBG")[c(10,8)]
ggplot(results[1:3,]%>%mutate(bivalent=c("Monovalent", "Monovalent","Bivalent"), 
                              time=c("<3", "3+", "<3")%>%factor(levels=c("<3", "3+"))), 
       aes(x=time, y=point, group=bivalent, color=bivalent)) + geom_point(position = position_dodge(width=0.1)) + 
  geom_errorbar(aes(ymin=ub, ymax=lb), position=position_dodge(width=0.1), width=0.2) + 
  geom_hline(yintercept=0, linetype=2) + 
  scale_x_discrete("Months") + 
  scale_y_continuous("Indirect protection (%)", limits=c(-1, 75), breaks = seq(0,75,10)) + 
  scale_color_manual(values=colors) + 
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(size=20),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 


