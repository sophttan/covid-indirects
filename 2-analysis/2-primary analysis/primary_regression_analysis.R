# Estimate adjusted relative reduction in infectiousness associated with vaccination and/or prior infection
# Sophia Tan
rm(list=ls())
gc()
setwd("/Users/sophiatan/Documents/UCSF/cleaned_data/")

library(clubSandwich)
library(tidyverse)
library(cowplot)
library(RColorBrewer)

d <- read_csv("matched_data_ps092922.csv")

d <- d %>% mutate(Institution=as.factor(Institution),
                          index_id=as.factor(index_id))

# pre-specified model
model <- glm(contact_status ~ index_prior_vacc + index_prior_inf + num_days_in_contact +
               num_vacc_doses+has_prior_inf+incidence_log+Institution, data=d, weights=weights, family="poisson")
summary(model)

# find SE of linear combination of covariates (prior vaccination and prior infection)
error <- c(sqrt(t(c(0,1,1,rep(0,29))) %*% vcovCR(model, cluster = d$subclass, type="CR2") %*% c(0,1,1,rep(0,29))))

model <- coef_test(model, vcov = "CR2", cluster = d$subclass) %>% data.frame(row.names = NULL)
main_results <- rbind(model[2:4,], model[5,], model[5,], model[5,], model[6:7,])
main_results$x <- c(1,1,1,1,2,3,1,1)

# function to estimate relative risks from coefficients
est_relative_risk <- function(data) {
  data %>% mutate(risk_red = round(100*(exp(beta*x)-1), 1),
                  lb = round(100*(exp(beta*x-2*SE*x)-1), 1),
                  ub = round(100*(exp(beta*x+2*SE*x)-1), 1))
}

format_table <- function(data) {
  data %>% 
    mutate(risk_red = paste0(risk_red, " (", lb, ", ", ub, ")")) %>%
    select(Coef, risk_red)
}

main_results <- main_results %>% est_relative_risk()

main_results_tbl <- main_results %>% format_table()
main_results_tbl$Coef <- c("Prior vaccination only", "Prior infection only", 
                           "Number of days of exposure between index case and close contact",
                            "Number of vaccine doses 1 dose", "2 doses", "\u22653 doses", 
                            "Prior infection only",
                            "SARS-CoV-2 incidence in the 7 days preceding the positive test in the index case (natural log scale)")
main_results_tbl$group <- c("Index case", "", "","Close contact", "", "", "", "Institution")
main_results_tbl <- main_results_tbl %>% select(group, Coef, risk_red)
names(main_results_tbl) <- c("","","Relative % change in attack rate of infection in close contact (95% CI)")
main_results_tbl %>% write_csv("/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/tables/main_regression_s2.csv")


# sensitivity and exploratory analyses
vacc_def_results <- main_results[1,]
## test other vaccine definitions for index cases and contacts
# run with index case number of vaccine doses 
# reweigh matches based on different control group statuses
# keep matches the same, create 3 different control groups where each group is weighted separately
d <- d %>% group_by(subclass, index_prior_vacc_doses) %>% 
  mutate(dose_weights=ifelse(treatment==1,1,1/n()))
mean_weights <- d %>% filter(treatment==0) %>% group_by(index_prior_vacc_doses) %>% summarise(mean=mean(dose_weights))
d <- d %>% group_by(index_prior_vacc_doses) %>% 
  mutate(dose_weights=ifelse(treatment==1,1,dose_weights/(mean_weights)$mean[index_prior_vacc_doses]))

model <- glm(contact_status ~ index_prior_vacc_doses + index_prior_inf +num_days_in_contact+
               num_vacc_doses+has_prior_inf+incidence_log+Institution, data=d, weights=dose_weights, family="poisson")
model <- coef_test(model, vcov = "CR2", cluster = d$subclass) %>% data.frame(row.names=NULL)
res <- rbind(model[2,], model[2,], model[2,])
res$x <- c(1,2,3)
vacc_def_results <- vacc_def_results %>% rbind(res %>% est_relative_risk())

# contact has any vaccination
d <- d %>% mutate(contact_has_vacc=ifelse(num_vacc_doses>0, 1, 0))
model <- glm(contact_status ~ index_prior_vacc + index_prior_inf + num_days_in_contact +
               contact_has_vacc+has_prior_inf+incidence_log+Institution, data=d, weights=weights, family="poisson")
summary(model)
model <- coef_test(model, vcov = "CR2", cluster = d$subclass) %>% data.frame(row.names=NULL)
res <- model[5,]
res$x <- 1
vacc_def_results <- vacc_def_results %>% rbind(res %>% est_relative_risk())

# add in original results (contact vaccination status by number of doses)
vacc_def_results <- rbind(vacc_def_results, main_results[4:6,])

# contact has either any vaccination or infection
d <- d %>% mutate(contact_has_vacc_or_inf = ifelse(contact_has_vacc|has_prior_inf, 1, 0))
model <- glm(contact_status ~ index_prior_vacc + index_prior_inf + num_days_in_contact +
               contact_has_vacc_or_inf+incidence_log+Institution, data=d, weights=weights, family="poisson")
summary(model)
model <- coef_test(model, vcov = "CR2", cluster = d$subclass) %>% data.frame(row.names=NULL)
res <- model[5,]
res$x <- 1
vacc_def_results <- vacc_def_results %>% rbind(res %>% est_relative_risk())

vacc_def_results_tbl <- vacc_def_results %>% format_table()
vacc_def_results_tbl$Coef <- c(rep(c("Prior vaccination only", 
                           "Number of vaccine doses 1 dose", "2 doses", "\u22653 doses"), 2),
                           "Prior vaccination or infection")
vacc_def_results_tbl$group <- c("Index case", rep("",3), "Close contact", rep("",4))
vacc_def_results_tbl <- vacc_def_results_tbl %>% select(group, Coef, risk_red)
names(vacc_def_results_tbl) <- c("","","Relative % change in attack rate of infection in close contact (95% CI)")
vacc_def_results_tbl %>% write_csv("/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/tables/vaccine_def_s3.csv")



## testing interaction between prior vaccination and infection in the index case
model <- glm(contact_status ~ index_prior_vacc + index_prior_inf + index_prior_vacc*index_prior_inf+
               num_days_in_contact + num_vacc_doses+has_prior_inf+incidence_log+factor(Institution), data=d, weights=weights, family="poisson")
summary(model)
model <- coef_test(model, vcov = "CR2", cluster = d$subclass) %>% as.data.frame()

res <- model[c(2:3, 33),]
res$x <- 1
interaction_results <- res %>% est_relative_risk()

interaction_results_tbl <- interaction_results %>% format_table()
interaction_results_tbl$Coef <- c("Prior vaccination", 
                                  "Prior infection", 
                                  "Interaction between vaccination and prior infection")
interaction_results_tbl <- interaction_results_tbl %>% select(Coef, risk_red)
names(interaction_results_tbl) <- c("","Relative % change in attack rate of infection in close contact (95% CI)")
interaction_results_tbl %>% write_csv("/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/tables/interaction_vacc_inf_s4.csv")


# plot key results
results_plotting <- rbind(main_results[1:2,], vacc_def_results[2:4,]) 
# combination of vaccination and infection
comb_vacc_inf <- data.frame(Coef="both_inf_vacc", beta=sum(results_plotting$beta[1:2]), SE=error, x=1) %>% est_relative_risk() %>% 
  select(Coef, risk_red, lb, ub)
results_plotting <- rbind(results_plotting%>% select(Coef, risk_red, lb, ub), comb_vacc_inf, list("no_vacc_inf", 0, 0, 0))

colors<-brewer.pal(n=8, "Accent")[c(1,2,7,5)]
results_plotting <- results_plotting %>% mutate(label=factor(c("Prior\nvaccination", "Prior\ninfection",
                                                       "1 dose", "2 doses", "\u22653 doses",
                                                       "Both prior\nvaccination\nand infection",
                                                       "No prior\nvaccination\nor infection"), 
                                                     levels = c("No prior\nvaccination\nor infection", 
                                                                "Prior\nvaccination", "Prior\ninfection", 
                                                                "Both prior\nvaccination\nand infection",
                                                                "1 dose", "2 doses", "\u22653 doses")),
                                        group=c("v", "i", "v", "v", "v", "b", "n"))
p <- results_plotting %>% ggplot(aes(label, risk_red, color=group)) +  geom_hline(yintercept = 0) + 
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.1) + 
  scale_color_manual(values=colors) + 
  scale_y_continuous("Relative change in attack rate (%)", limits = c(-70, 5)) + 
  
  # geom_vline(xintercept = 1.5, size=.1, linetype="dashed") + 
  # geom_vline(xintercept = 4.5, size=.1) +
  geom_vline(xintercept = 4.5, size=.1) +
  xlab("Vaccination and/or prior infection in index case") +
  theme(legend.position = "none",
        panel.background = element_blank(), 
        axis.line.x.bottom = element_line(), 
        axis.line.y.left = element_line())
p

p %>% ggsave(filename="/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/figures/main/relative_risk_reduction_figure3.jpg", 
             width=6, height=4)

