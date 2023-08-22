library(tidyverse)

sample_sizes <- read_csv("results/sample_sizes_50.csv")
regression_res <- read_csv("results/regression_50.csv")
ph_test <- read_csv("results/ph_test_50.csv")

sample_sizes$units.0%>%hist()
sample_sizes$units.1%>%hist()

sample_sizes$obs_time.0%>%hist()
sample_sizes$obs_time.0%>%summary()

sample_sizes$obs_time.1%>%hist()
sample_sizes$obs_time.1%>%summary()

regression_coef <- regression_res %>% select(i, var, coef) %>% pivot_wider(id_cols=i, names_from = var, values_from = coef) %>% 
  select(!grep("Institution|frailty", names(.)))
regression_p <- regression_res %>% select(i, var, p) %>% pivot_wider(id_cols=i, names_from = var, values_from = p) %>% 
  select(!grep("Institution|frailty", names(.)))

regression_coef$treatment %>% hist()
regression_coef$treatment %>% summary()

regression_p$treatment %>% hist()
regression_p$treatment %>% summary()

regression_coef %>% summary()

ph_test %>% select(i, var, p) %>% pivot_wider(id_cols=i, names_from = var, values_from = p) %>% summary()
