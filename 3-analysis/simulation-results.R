library(tidyverse)

sample_sizes <- read_csv("results/sample_sizes_100.csv")
regression_res <- read_csv("results/regression_100.csv")
ph_test <- read_csv("results/ph_test_100.csv")

sample_sizes$units.0%>%summary()
sample_sizes$units.1%>%summary()

sample_sizes$obs_time.0%>%hist()
sample_sizes$obs_time.0%>%summary()

sample_sizes$obs_time.1%>%hist()
sample_sizes$obs_time.1%>%summary()

regression_coef <- regression_res %>% select(i, var, coef) %>% pivot_wider(id_cols=i, names_from = var, values_from = coef) %>% 
  select(!grep("Institution|frailty", names(.)))
regression_p <- regression_res %>% select(i, var, p) %>% pivot_wider(id_cols=i, names_from = var, values_from = p) %>% 
  select(!grep("Institution|frailty", names(.)))

regression_coef$treatment %>% hist(main="HR of treatment variable")
regression_coef$treatment %>% summary()

regression_p$treatment %>% hist(main="HR of treatment variable p-value")
regression_p$treatment %>% summary()

regression_coef %>% summary()

regression_p%>%summary()
ph_test %>% select(i, var, p) %>% pivot_wider(id_cols=i, names_from = var, values_from = p) %>% summary()
