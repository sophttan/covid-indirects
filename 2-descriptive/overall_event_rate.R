library(lubridate)
library(tidyverse)
library(MatchIt)
library(ggbrace)
library(patchwork)

setwd("D:/CCHCS_premium/st/indirects/cleaned-data")
testing <- read_csv("testing_vacc_clean.csv") %>% filter(!Result%>%is.na()) %>% 
  select(ResidentId, Day, Result, num_pos) %>% arrange(ResidentId, Day) %>% 
  group_by(ResidentId) %>% fill(num_pos, .direction="down")

clean_data <- function(d) {
  d %>% select(!c(adjusted_start.x, adjusted_start.y, last_chunked.x, last_chunked.y, duration_chunked.x, duration_chunked.y))
}

find_event_rate <- function(d) {
  d <- d %>% mutate(duration=last_chunked-adjusted_start+1)
  d_full <- d %>% left_join(t, by=c("primary"="ResidentId"))
  d_full_test <- d_full %>% filter(Test<=last_chunked & Test>=adjusted_start)
  
  num_events <- d_full_test %>% group_by(label) %>% 
    summarise(inf = length(unique(num_pos))-1, duration=first(duration))
  
  num_events
}

data <- read_csv("full_data_prematching_030823.csv")
data <- clean_data(data)
data

t <- testing %>% filter(ResidentId %in% data$primary) %>% rename("Test" = "Day")

results <- find_event_rate(data) 
results$duration %>% sum()
(results$inf %>% sum())/(results$duration %>% sum()) * 1000
