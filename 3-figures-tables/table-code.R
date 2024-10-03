# Sophia Tan
# Code for making results tables

source(here::here("config.R"))

make_tbl <- function(res, names, include_age_risk=T) {
  res <- res %>% mutate(`Odds ratio (OR) (95% CI)` = paste0(signif(point,3), " (", signif(lb,3), ", ", signif(ub,3), ")"))
  
  if(include_age_risk) {
    res$x <- c(names, "Age Case or control", "Age Roommate", 
               "Severe COVID-19 risk score Case or control", "Severe COVID-19 risk score Roommate")
  } else{
    res <- res %>% filter(!grepl("(age|risk)", x))
    res$x <- names
  }
  
  res %>% select(1, ncol(.)) %>% rename(" "=x)
}


# binary results
res <- read_csv(here::here("results/main/binary-results.csv"))
res                 

make_tbl(res, c("Vaccine-derived immunity (any)", "Infection-acquired immunity (any)")) %>% write_csv("tables/binary.csv")

# dose results
names <- c("Vaccine-derived immunity (by dose) Partial vaccination",
           "Primary series alone",
           "One booster",
           "Two or more boosters",
           "Infection-acquired immunity (any)") 
num <- make_tbl(read_csv(here::here("results/main/dose-results.csv")), names) 
cat <- make_tbl(read_csv(here::here("results/main/dose-results-categorical.csv")), names)
rbind(c(" ", "Doses (numeric)", "Doses (categorical)"), cbind(num[,1],num[,2],cat[,2])) %>% 
  write_csv("tables/dose.csv")


# hybrid results
res <- read_csv(here::here("results/main/hybrid-results.csv"))
res
names <- c("Immunity Only vaccine-derived immunity", 
           "Only infection-acquired immunity",
           "Both vaccine- and infection-acquired immunity (hybrid)")
make_tbl(res, names) %>% write_csv("tables/hybrid.csv")


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

# waning adjustment for both
res <- read_csv(here::here("results/main/time-bothinfvacc-results.csv"))
res                 

column <- c("Vaccine-derived immunity (time) <3 months", "3 to <6 months", "6 to <12 months", "12+ months",
            "Infection-acquired immunity (time) <3 months", "3 to <6 months", "6 to <12 months", "12+ months")
make_tbl(res, column, include_age_risk = F) %>% write_csv("tables/waning_bothinfvacc.csv")

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

# interaction
res <- read_csv(here::here("results/main/interaction-results.csv"))
res

tbl <- make_tbl(res, c(rep("Any vaccine",5), 
                       rep(c("<3 months", "3 to <6 months", "6 to <12 months", "12+ months"), 5)), 
                include_age_risk = F)
tbl$inf <- c("Any infection", 
             c("<3 months", "3 to <6 months", "6 to <12 months", "12+ months"),
             rep("Any infection", 4),
             rep(c("<3 months", "3 to <6 months", "6 to <12 months", "12+ months"), each=4))
tbl <- tbl %>% pivot_wider(id_cols=` `, names_from = "inf", values_from = `Odds ratio (OR) (95% CI)`)
tbl %>% write_csv("tables/interaction.csv")

# binary results
res <- read_csv(here::here("results/main/stratified-casecontrolimmunity-results.csv"))
res                 

cbind("Immunity of matched case and controls" = rep(c("No immunity",
                                                      "Only vaccine-derived immunity",
                                                      "Only infection-acquired immunity",
                                                      "Both vaccine- and infection-acquired immunity (hybrid)"), each=2),
  make_tbl(res, rep(c("Vaccine-derived immunity (any)", "Infection-acquired immunity (any)"), 4), include_age_risk = F)) %>%
  write_csv("tables/stratified-casecontrol.csv")



