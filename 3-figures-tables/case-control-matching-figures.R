# Sophia Tan 3/4/24
# Plot descriptive data on match quality

source(here::here("config.R"))

data <- read_csv("D:/CCHCS_premium/st/indirects/case_control_postmatchprocessing091224.csv")

# summarise difference between cases and controls on covariates that were matched on
grouped <- data %>% group_by(group) %>% arrange(desc(case)) %>% 
  summarise(case=case, 
            age_diff=age[1]-age,
            risk_diff=risk[1]-risk,
            age_diff.roommate=age.roommate[1]-age.roommate,
            risk_diff.roommate=risk.roommate[1]-risk.roommate,
            vacc_diff=time_since_vacc[1]-time_since_vacc,
            inf_diff=time_since_inf[1]-time_since_inf) %>%
  filter(case!=1)


# plot within matched group differences
age <- ggplot(grouped) + 
  geom_histogram(aes(age_diff, y=..density..), binwidth = 5) + 
  scale_x_continuous("Difference between matched case and control", expand=c(0, 0)) + 
  scale_y_continuous("Density", expand=c(0, 0)) + 
  labs(subtitle="") + 
  theme(panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 

risk <- ggplot(grouped) + 
  geom_histogram(aes(risk_diff, y=..density..), binwidth = 1) + 
  scale_x_continuous("Difference between matched case and control", expand=c(0, 0)) + 
  scale_y_continuous("Density", expand=c(0, 0)) + 
  labs(subtitle="") + 
  theme(panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 

inf <- ggplot(grouped) + 
  geom_histogram(aes(inf_diff, y=..density..)) + 
  scale_x_continuous("Difference between matched case and control", expand=c(0, 0)) + 
  scale_y_continuous("Density", expand=c(0, 0)) + 
  labs(subtitle="") + 
  theme(panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 

vacc <- ggplot(grouped) + 
  geom_histogram(aes(vacc_diff, y=..density..)) + 
  scale_x_continuous("Difference between matched case and control", expand=c(0, 0)) + 
  scale_y_continuous("Density", expand=c(0, 0)) + 
  labs(subtitle="") + 
  theme(panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 



data <- data %>% mutate(case=factor(case, levels=c(1,0), labels=c("Case", "Control")))

# plot overall distributions 
age_overall <- data %>% ggplot(aes(age, group=case, fill=case)) + 
  geom_histogram(aes(y = ..density..), binwidth=5, position="identity", alpha=0.5) + 
  scale_fill_discrete("") + 
  scale_x_continuous("Age", expand=c(0, 0)) + 
  scale_y_continuous("Density", expand=c(0, 0)) + 
  labs(subtitle="A. Age (years)") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 

risk_overall <- data %>% ggplot(aes(risk, group=factor(case), fill=factor(case))) + 
  geom_histogram(aes(y = ..density..), binwidth=1, position="identity", alpha=0.5) + 
  scale_fill_discrete("") + 
  scale_x_continuous("Risk for severe COVID-19", expand=c(0, 0)) + 
  scale_y_continuous("Density", expand=c(0, 0)) + 
  labs(subtitle="B. Risk") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 

inf_overall <- data %>% ggplot(aes(time_since_inf, group=factor(case), fill=factor(case))) + 
  geom_histogram(aes(y = ..density..), position="identity", alpha=0.5) + 
  scale_fill_discrete("") + 
  scale_x_continuous("Days", expand=c(0, 0)) + 
  scale_y_continuous("Density", expand=c(0, 0)) + 
  labs(subtitle="D. Days since last SARS-CoV-2 infection") +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 


vacc_overall <- data %>% ggplot(aes(time_since_vacc, group=factor(case), fill=factor(case))) +
  geom_histogram(aes(y = ..density..), position="identity", alpha=0.5) + 
  scale_fill_discrete("") + 
  scale_x_continuous("Days", expand=c(0, 0)) + 
  scale_y_continuous("Density", expand=c(0, 0)) + 
  labs(subtitle="C. Days since COVID-19 vaccination") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border= element_rect(fill=NA),
        strip.text.x = element_text(face="bold", size=12),
        strip.background = element_rect(fill=NA,colour="black"),
        text=element_text(size=12, family="sans")) 


(age_overall + age)/(risk_overall + risk)/(vacc_overall + vacc)/(inf_overall + inf)
ggsave(here::here("figures/match_quality.jpg"), height=10, width=9, dpi=300)