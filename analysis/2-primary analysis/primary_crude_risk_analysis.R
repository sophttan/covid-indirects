# Estimate crude attack rate of Omicron SARS-CoV-2 infection in study population
# Sophia Tan
rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")


d <- read_csv("matched_data.csv")

# overall attack risk
overall <- data.frame(count=d%>%nrow(), x=d$contact_status %>% sum())
overall$risk <- overall$x/overall$count
overall <- overall %>%
  rowwise %>%
  mutate(out = list(prop.test(x, count, conf.level=.95) %>%
                      tidy)) %>%
  ungroup %>%
  unnest(out)

overall


priorvacc<-d %>% group_by(index_prior_vacc, index_prior_inf) %>% 
  summarise(count=n(), x=sum(contact_status), risk=mean(contact_status))
priorvacc<-priorvacc %>% rbind(d %>% group_by(index_prior_vacc) %>% 
                                 summarise(count=n(), x=sum(contact_status), risk=mean(contact_status)) %>% 
                                 mutate(index_prior_inf=-1)) #index_prior_inf == -1 if overall (not stratified by prior infection)

priorvacc <- priorvacc %>%
  rowwise %>%
  mutate(out = list(prop.test(x, count, conf.level=.95) %>%
                      tidy)) %>%
  ungroup %>%
  unnest(out)
priorvacc


summary_risk <- priorvacc %>% filter(index_prior_inf != -1) %>% 
  mutate(risk=paste0(round(risk*100, 1), " (", round(conf.low*100, 1), ", ", round(conf.high*100, 1), ")"),
         index_prior_vacc=factor(index_prior_vacc, levels=c(0,1), labels=c("No prior vaccination", "Prior vaccination")),
         index_prior_inf=factor(index_prior_inf, levels=c(0,1), labels=c("No prior infection", "Prior infection"))) %>% 
  select(index_prior_vacc, index_prior_inf, risk) %>% spread(index_prior_vacc, risk)
names(summary_risk)[1] <- "Attack rate (%) (95% CI)"
summary_risk

summary_risk %>% write_csv("/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/tables/crude_attack_rate_4x4_s1.csv")


d <- d %>% mutate(boosted=ifelse(index_prior_vacc_doses>full_vacc, 2, ifelse(index_prior_vacc_doses==0, 0, 1)))
# stratified by booster status, no vaccine, primary series, booster
primbooster<-d %>% group_by(boosted, index_prior_inf) %>% 
  summarise(count=n(), x=sum(contact_status), risk=mean(contact_status))
primbooster<-primbooster %>% rbind(d %>% group_by(boosted) %>% 
                                     summarise(count=n(), x=sum(contact_status), risk=mean(contact_status)) %>% 
                                     mutate(index_prior_inf=-1)) #index_prior_inf == -1 if overall (not stratified by prior infection)

primbooster <- primbooster %>%
  rowwise %>%
  mutate(out = list(prop.test(x, count, conf.level=.95) %>%
                      tidy)) %>%
  ungroup %>%
  unnest(out)

total <- priorvacc %>% mutate(boosted=ifelse(index_prior_vacc>0, 4, 0)) %>% select(!index_prior_vacc) %>% rbind(primbooster)
p <- total %>% 
  # boosted = 0 if not vaccinated, 1 if primary series, 2 if boosted, 4 if any prior vaccination
  mutate(boosted=factor(boosted, levels=c(0,4,1,2)),
         index_prior_inf=factor(index_prior_inf, levels=c(-1, 0, 1))) %>%
  ggplot(aes(factor(boosted), risk, group=index_prior_inf, color=index_prior_inf)) + 
  geom_point(position=position_dodge(0.5), size=2) + 
  geom_errorbar(position=position_dodge(0.5), aes(ymin=conf.low, ymax=conf.high), width=.4) + 
  scale_x_discrete(name="Prior vaccination in index case", 
                   labels=c("No prior\nvaccination", "Any prior\nvaccination", "Received\nprimary series", "Received\nbooster")) + 
  scale_y_continuous(name="Attack rate in close contact", limits = c(0,.5), expand = c(0,0)) + 
  scale_color_brewer(guide = guide_legend(label.position = "right", title.position = "top"), 
                     type="qual",
                     name=element_blank(), 
                     labels=c("Overall", "No prior infection", "Prior infection")) + 
  #  labs(subtitle = "A")+
  geom_vline(xintercept = 2.5, size=.1) + 
  theme(legend.position = "bottom",  
        legend.key = element_blank(),
        panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())

p %>% ggsave(filename = "/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/figures/main/unadjusted_attack_rate_figure2.jpg", 
             width = 5, height= 4)
