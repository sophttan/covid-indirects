# Sophia Tan 2/27/24
# Match infections and controls
# Primary analysis
# Exact matching on building and security level, time, infection and vaccine status (cases and controls only)
# Distance matching on age (case and control and roommates), risk (case and control and roommates), time since last infection and last vaccine (cases and controls only)

rm(list=ls())
gc() 

library(tidyverse)
library(readr)
library(lubridate)
library(MatchIt)

data <- read_csv("D:/CCHCS_premium/st/indirects/case_control_prematch.csv")

# function that generates pairwise distance matrix for cases and controls
# final distance matrix is n x n where n is number of rows of tbl (1 row per case/control)
# value in row 1, column 2 represents distance between resident in row 1 or tbl and resident in row 2 of tbl
# distance is Inf if match is not allowed (case and control are roommates or tests are >2 days apart)
# and otherwise distance is euclidean distance of scaled covariates 
# if include_time is T, distance includes time since infection and time since vaccination (primary analysis)
get_distance <- function(tbl, include_time=T) {
  
  # generate distance matrix based on all covariates
  if(include_time) {
    dist_tbl <- dist(tbl %>% select(time_since_inf.scale, time_since_vacc.scale, risk.scale, risk.roommate.scale, age.scale, age.roommate.scale), diag=T, upper=T) %>% as.matrix()
  } else {
    dist_tbl <- dist(tbl %>% select(risk.scale, risk.roommate.scale, age.scale, age.roommate.scale), diag=T, upper=T) %>% as.matrix()
  }

  tbl <- tbl %>% mutate(label=1:n()) %>% select(label, ResidentId, Roommate, test.Day) 
  
  # create table with n^2 number of rows (pairwise rows)
  tbl <- cross_join(tbl, tbl) %>% 
    mutate(roommates = ResidentId.x==Roommate.y, # T if residents are roommates
           eligible=abs(test.Day.x-test.Day.y)%>%as.numeric()) # absolute number of days between test collection
  
  # create test day eligibility matrix
  test_matrix <- tbl%>% 
    select(label.x,label.y,eligible) %>% 
    pivot_wider(id_cols = label.x, names_from = label.y, values_from = eligible) %>% as.data.frame() %>% select(!label.x)
  dist_tbl[test_matrix>2] <- Inf # set distance in distance matrix to be Inf if tests are more than 2 days apart
  
  # create roommate eligibility matrix
  roommate_matrix <- tbl%>% 
    select(label.x, label.y, roommates) %>% 
    pivot_wider(id_cols = label.x, names_from = label.y, values_from = roommates) %>% as.data.frame() %>% select(!label.x)
  dist_tbl[roommate_matrix==T] <- Inf # set distance in distance matrix to be Inf if residents are roommates
  
  dist_tbl
}


#### run matching #### 
# matching takes place iteratively for memory
# distance based matching takes place within exact matching strata (building and security level, vaccine status, prior infection)

# empty matched data
match <- NULL

# group data by exact strata
# summarise with number of controls and number of cases, give unique label (key) to strata
keys <- data %>% group_by(Institution, BuildingId, num_dose_adjusted, has.prior.inf, level) %>%
  summarise(control=sum(case!=1), case=sum(case==1)) %>% 
  ungroup() %>%
  mutate(key=1:n())

# remove strata where there are no controls or no cases (no matching possible)
keep <- keys %>% filter(control>0&case>0)
data <- data %>% left_join(keep %>% select(!c(control, case))) 


# matching ratio (can be changed)
# cases and control are matched 1:2 in primary analysis, matched 1:1 in sensitivity analysis
ratio <- 2 


# iterate to conduct distance based matching for each exact strata
for (i in keep$key) {
  gc()
  for_matching_inst <- data %>% filter(key==i) %>% ungroup()
  for_matching_inst <- for_matching_inst %>% arrange(desc(case))
  
  # get distance matrix
  distance_matrix <- get_distance(for_matching_inst)
  
  # check for number of cases and number of controls
  num_cases <- sum(for_matching_inst$case==1)
  num_controls <- sum(for_matching_inst$case==0)
  
  # check if there are any valid matches (any distance that is not Inf) and skip to next strata if no possible matches
  no_valid_matches <- all(distance_matrix[(num_cases+1):nrow(for_matching_inst),1:num_cases]%>%is.infinite())
  if(no_valid_matches) {next}
  
  ## edge cases that break matchit package function
  # if there is only 1 case, match all possible controls in order of distance (number of matches can be anywhere from 1 to ratio)
  if(num_cases==1) {
    for_matching_inst$distance <- distance_matrix[,1]
    
    num_match <- min(nrow(for_matching_inst%>%filter(!distance%>%is.infinite())), ratio+1)
    for_matching_inst <- (for_matching_inst %>% arrange(distance))[1:num_match,] %>% select(!distance)
    
    match <- rbind(match, cbind(id=1:num_match, subclass=rep(1, num_match), for_matching_inst))
    next
  }
  
  # if there is only 1 control, match control to closest case
  if(num_controls==1) {
    for_matching_inst$distance <- distance_matrix[,1]
    
    for_matching_inst <- (for_matching_inst %>% arrange(case, distance))[1:2,] %>% select(!distance)
    
    match <- rbind(match, cbind(id=1:2, subclass=c(1,1), for_matching_inst))
    next
  }
  
  # if >1 case and >1 control, use matchit function
  matched_data <- matchit(case ~ Institution + BuildingId + num_dose_adjusted + has.prior.inf + level,
                          data = for_matching_inst, 
                          distance = distance_matrix,
                          exact = case ~ Institution + BuildingId + num_dose_adjusted + has.prior.inf + level, 
                          ratio=ratio)
  
  print(matched_data %>% summary())
  match <- rbind(match, matched_data %>% get_matches() %>% select(!weights))
}

match


# check that case and controls have test within 2 days and case and controls are not roommates
match %>% group_by(key, subclass) %>% arrange(key, subclass, desc(case)) %>% 
  filter(any(abs(test.Day-first(test.Day))>2) | any(first(ResidentId)==Roommate))



# relabel match groups to be unique across all institutions (subclass currently 1:n within exact strata (keys))                                                                                 
matched_keys <- match %>% 
  group_by(key, subclass) %>% group_keys() %>% mutate(group = 1:n())
match <- match %>% left_join(matched_keys) %>% mutate(id=1:n())
   

# change file path based on matching specifications
# primary analysis saves as matched_building_3_7days-12matching-[date].csv
match %>% select(!c(n, Day, Night)) %>% write_csv("D:/CCHCS_premium/st/indirects/matched_building_3_7days-12matching-051724.csv")

