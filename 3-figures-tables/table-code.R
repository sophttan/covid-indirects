# Sophia Tan
# Code for making results tables

source(here::here("config.R"))

make_tbl <- function(res, names, include_age_risk=T) {
  res <- res %>% mutate(`Odds ratio (OR) (95% CI)` = paste0(signif(point,3), " (", signif(lb,3), ", ", signif(ub,3), ")"))
  
  if(include_age_risk) {
    res$x <- c(names, "Age Case or control", "Age Roommate", 
               "Severe COVID-19 risk score Case or control", "Severe COVID-19 risk score Roommate")
  } else{
    res$x <- names
  }
  
  res %>% select(1, ncol(.)) %>% rename(" "=x)
}


# binary results
res <- read_csv(here::here("results/main/binary-results.csv"))
res                 

make_tbl(res, c("Vaccine-derived immunity (any)", "Infection-acquired immunity (any)")) %>% write_csv("tables/binary.csv")

# dose results
res <- read_csv(here::here("results/main/dose-results.csv"))
res                 

make_tbl(res, c("Vaccine-derived immunity (by dose) Partial vaccination",
                "Primary series alone",
                "One booster",
                "Two or more boosters",
                "Infection-acquired immunity (any)")) %>% write_csv("tables/dose.csv")


# waning results
res <- read_csv(here::here("results/main/time-results.csv"))
res                 

column <- c("Immunity (time) <3 months", "3 to <6 months", "6 to <12 months", "12+ months",
            "Vaccine-derived or Infection-acquired immunity (any)")

vacc <- make_tbl(res%>%filter(inf_vacc=="vacc"), column)
inf <- make_tbl(res%>%filter(inf_vacc=="inf"), column)[,2]
infvacc <- make_tbl(res%>%filter(inf_vacc=="infvacc"), column[1:4])[,2]

infvacc <- infvacc[1:4,] %>% rbind(list("")) %>% rbind(infvacc[5:8,])

tbl <- vacc %>% cbind(inf) %>% cbind(infvacc)
names(tbl) <- paste(c("","Vaccine", "Infection", "Vaccine or infection"), names(tbl))
tbl %>% write_csv("tables/time.csv")


# 3 month vaccine
res <- read_csv(here::here("results/main/3months-results.csv"))
res <- res[1:3,]                 

make_tbl(res, c("<1 months",
                "1 to <2 months",
                "2 to <3 months"), F) %>% 
  rename("Time since vaccine"=" ") %>% write_csv("tables/three_month_vacc.csv")


# bivalent
res <- read_csv(here::here("results/main/bivalent-results.csv"))
res <- res[1:3,]                 

make_tbl(res, c("Ancestral monovalent vaccine <3 months",
                "Ancestral monovalent vaccine 3+ months",
                "Bivalent vaccine <3 months"), F) %>% write_csv("tables/bivalent.csv")
