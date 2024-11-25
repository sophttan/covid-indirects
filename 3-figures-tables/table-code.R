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

# results grouped
make_sensitivity <- function(name, filepath) {
  res <- read_csv(here::here(paste0(filepath,"binary-results.csv")))[1:2,] %>% 
    rbind(read_csv(here::here(paste0(filepath,"dose-results.csv")))[1:4,]) %>%
    rbind(read_csv(here::here(paste0(filepath,"time-results.csv")))%>%filter(grepl("time.+roommate",x)&inf_vacc%in%c("inf","vacc"))%>%select(!inf_vacc)) 
  tbl <- make_tbl(res, names=c(rep(c("Vaccine-derived immunity Any", "Infection-acquired immunity Any")),
                               rep(c("By dose Partial vaccination",
                                     "Primary series alone",
                                     "One booster",
                                     "Two or more boosters")),
                               rep(c("By time <3 months", "3 to <6 months", "6 to <12 months", "12+ months"), 2)), include_age_risk = F)
  tbl <- tbl %>% mutate(inf=c(c("vacc", "inf"),rep("vacc",4),rep("inf", 4), rep("vacc", 4))) %>% arrange(desc(inf))
  tbl <- tbl %>% select(!inf)
  names(tbl) <- c("", name)
  tbl
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


# pre-omicron v omicron
res <- read_csv(here::here("results/main/preomicron-omicron-results.csv"))[1:8,]
tbl <- make_tbl(res, rep(c("<3 months", "3 to <6 months", "6 to <12 months", "12+ months"), 2), 
                include_age_risk = F)
tbl$inf <- rep(c("pre-Omicron infection", "Omicron infection"), each=4)
tbl <- tbl %>% pivot_wider(id_cols=` `, names_from = "inf", values_from = `Odds ratio (OR) (95% CI)`)
tbl %>% write_csv("tables/preomicron-omicron.csv")


# stratified results
res <- read_csv(here::here("results/main/stratified-casecontrolimmunity-results.csv")) %>% 
  rbind(read_csv(here::here("results/main/stratified-casecontrolimmunity-results-dose.csv")) %>% mutate(n=NA)) %>%
  rbind(read_csv(here::here("results/main/stratified-casecontrolimmunity-results-vacc.csv"))) %>%
  rbind(read_csv(here::here("results/main/stratified-casecontrolimmunity-results-inf.csv"))) 
tbl <- make_tbl(res, names=c(rep(c("Vaccine-derived immunity Any", "Infection-acquired immunity Any"), 4),
                             rep(c("By dose Partial vaccination",
                                   "Primary series alone",
                                   "One booster",
                                   "Two or more boosters"), 4),
                             rep(c("By time <3 months", "3 to <6 months", "6 to <12 months", "12+ months"), 8)), include_age_risk = F)
tbl <- tbl %>% mutate(label=res$label, inf=c(rep(c("vacc", "inf"), 4),
                                             rep("vacc", 32), rep("inf", 16))) %>% arrange(desc(inf))
tbl <- tbl %>% pivot_wider(id_cols=c(` `, "inf"), names_from = "label", values_from = `Odds ratio (OR) (95% CI)`)
tbl <- tbl %>% select(!inf)
tbl <- cbind(tbl[,1], make_sensitivity("Main", "results/main/")[,2], tbl[,2:5])

names(tbl) <- c("", "All cases and controls", "No immunity","Only vaccine-derived immunity","Only infection-acquired immunity","Both vaccine- and infection-acquired immunity (hybrid)")
tbl %>% write_csv("tables/stratified-casecontrol.csv")


# housing sensitivity results grouped
tbl <- cbind(make_sensitivity("Days 3-6 (Main)", "results/main/"), 
             make_sensitivity("Day 3", "results/roommate-3day/")[,2], 
             make_sensitivity("Days 0-6", "results/roommate-0-6day/")[,2], 
             make_sensitivity("Days 6-9", "results/roommate-6-9day/")[,2])
tbl %>% write_csv("tables/housing-sensitivity.csv")


# matching/adjustment sensitivity results grouped
tbl <- cbind(make_sensitivity("Main", "results/main/"), 
             make_sensitivity("1:1 matching", "results/match11//")[,2], 
             make_sensitivity("Removing matching by time since vaccine and/or infection", "results/no-time-match/")[,2], 
             make_sensitivity("Adding statistical adjustment for time since vaccine and/or infection of case/control", "results/timeadj/")[,2],
             make_sensitivity("Removing statistical adjustment for age and risk", "results/noadj/")[,2])
tbl %>% write_csv("tables/match-statistical-sensitivity.csv")


# negative control flu results
res <- read_csv("results/main/flu-results.csv")
make_tbl(res, 
         c("Any influenza vaccine (lifetime)",
           "Influenza vaccine within 2 years",
           "Influenza vaccine within 1 year",
           "Time since influenza vaccine (months)"), F) %>% write_csv("tables/flu.csv")


# full results
res <- read_csv("results/logistic/full_results.csv")
res <- res %>% mutate(name=if_else(grepl("vacc|dose", x), "vacc", "inf")) %>% arrange(desc(name))
res <- res %>% filter(!grepl("infvacc", x)) %>% select(!name)
column <- c("Vaccine-derived immunity Any", "By dose Partial vaccination",
            "Primary series alone",
            "One booster",
            "Two or more boosters",
            "By time <3 months", "3 to <6 months", "6 to <12 months", "12+ months",
            "Infection-acquired immunity Any",
            "By time <3 months", "3 to <6 months", "6 to <12 months", "12+ months")

res_robust <- read_csv("results/logistic/full_results_robust.csv")
res_robust <- res_robust %>% mutate(name=if_else(grepl("vacc|dose", x), "vacc", "inf")) %>% arrange(desc(name))
res_robust <- res_robust %>% filter(!grepl("infvacc", x)) %>% select(!name)

# model sensitivity results grouped
tbl <- cbind(make_sensitivity("Conditional logistic regression (main)", "results/main/"), 
             make_tbl(res, column, F)[,2], 
             make_sensitivity("Conditional logistic regression without repeated residents", "results/no-repeat/")[,2], 
             make_tbl(res_robust, column, F)[,2])
names(tbl)[3]<-"Unconditional logistic regression"
names(tbl)[5]<-"Unconditional logistic regression with person-level clustering"
tbl %>% write_csv("tables/model-sensitivity.csv")

