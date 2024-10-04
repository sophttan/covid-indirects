# Sophia Tan 2/27/24 - last updated 9/24/24
# Match infections and controls
# Primary analysis
# Exact matching on building and security level, time, infection and vaccine status (cases and controls only)
# Distance matching on age (case and control and roommates), risk (case and control and roommates), time since last infection and last vaccine (cases and controls only)

source(here::here("config.R"))

data <- read_csv("D:/CCHCS_premium/st/indirects/___")

# function that generates pairwise distance matrix for cases and controls
# final distance matrix is n x n where n is number of rows of tbl (1 row per case/control)
# value in row 1, column 2 represents distance between resident in row 1 of tbl and resident in row 2 of tbl
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
# ratio is maximum number of matches allowed for each case
# include_time should be T for main analyses to adjust for time since last vaccine or last infection - F in sensitivity analyses
run_main_matching <- function(data, ratio, include_time) {
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
  
  # iterate to conduct distance based matching for each exact strata
  for (i in keep$key) {
    gc()
    for_matching_inst <- data %>% filter(key==i) %>% ungroup()
    for_matching_inst <- for_matching_inst %>% arrange(desc(case))
    
    # get distance matrix
    distance_matrix <- get_distance(for_matching_inst, include_time = include_time)
    
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
      
      num_match <- min(nrow(for_matching_inst%>%filter(case==0&!distance%>%is.infinite())), ratio) 
      for_matching_inst <- (for_matching_inst %>% arrange(desc(case), distance))[1:(num_match+1),] %>% select(!distance)
      
      match <- rbind(match, cbind(id=1:(num_match+1), subclass=rep(1,num_match+1), for_matching_inst))
      next
    }
    
    # if there is only 1 control, match control to closest case
    if(num_controls==1) {
      for_matching_inst$distance <- distance_matrix[,num_cases+1]
      
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
}


# matching ratio (can be changed)
# cases and control are matched 1:2 in primary analysis, matched 1:1 in sensitivity analysis
match <- run_main_matching(data, 2, T)


# check that case and controls have test within 2 days and case and controls are not roommates
match %>% group_by(key, subclass) %>% arrange(key, subclass, desc(case)) %>% 
  filter(any(abs(test.Day-first(test.Day))>2) | any(first(ResidentId)==Roommate))


#### duplicate residents in matches ####
# some cases are matched to the same resident multiple times
# if the resident has multiple negative tests that meet matching criteria
duplicate_controls <- match %>% group_by(key, subclass) %>% 
  filter(length(unique(ResidentId))!=n()) 
duplicate_controls


# group data by exact strata
# summarise with number of controls and number of cases, give unique label (key) to strata
keys <- data %>% group_by(Institution, BuildingId, num_dose_adjusted, has.prior.inf, level) %>%
  summarise(control=sum(case!=1), case=sum(case==1)) %>% 
  ungroup() %>%
  mutate(key=1:n())

# remove strata where there are no controls or no cases (no matching possible)
keep <- keys %>% filter(control>0&case>0)
data <- data %>% left_join(keep %>% select(!c(control, case))) 

unmatched <- data %>% 
  left_join(match %>% select(key, ResidentId, test.Day) %>% mutate(matched=1)) %>% 
  filter(matched%>%is.na()) %>%
  ungroup() %>% select(!matched)


no_duplicates <- NULL
for (i in duplicate_controls$key%>%unique()) {
  gc()
  # keep only unmatched cases and controls 
  # valid matches (no repeated controls) are untouched (keep matching without replacement)
  for_matching_inst <- unmatched %>% 
    filter(key==i) 
  # add in all matches with duplicate controls
  for_matching_inst <- rbind(for_matching_inst, duplicate_controls %>% filter(key==i) %>% ungroup() %>% select(names(for_matching_inst)))
  for_matching_inst <- for_matching_inst %>% arrange(desc(case))
  
  # get distance matrix
  distance_matrix <- get_distance(for_matching_inst, include_time = T)
  
  # check for number of cases and number of controls
  num_cases <- sum(for_matching_inst$case==1)
  num_controls <- sum(for_matching_inst$case==0)

  # check for multiple eligible tests from same control
  # check distance matrix between cases and controls (ignore part of distance matrix that compares controls to each other)
  distance_matrix_cases_res <- cbind(ResidentId=for_matching_inst[(num_cases+1):nrow(for_matching_inst),]$ResidentId, distance_matrix[(num_cases+1):nrow(for_matching_inst),1:num_cases]) %>% as.data.frame()
  distance_matrix_cases_res <- distance_matrix_cases_res %>% 
    group_by(ResidentId) %>% 
    mutate_at(2:ncol(.),
              # change distance to inf if resident is eligible as control for a single case multiple times
              # if eligible multiple times, keep the test that is the best match
              function(x){if_else(1:n()==which.min(x),x,Inf)}) 
  
  # update distance in full matrix
  distance_matrix[(num_cases+1):nrow(for_matching_inst),1:num_cases] <- distance_matrix_cases_res[,2:ncol(distance_matrix_cases_res)]%>%as.matrix()
  distance_matrix[1:num_cases,(num_cases+1):nrow(for_matching_inst)] <- t(distance_matrix_cases_res[,2:ncol(distance_matrix_cases_res)]%>%as.matrix())
  
  ## edge cases that break matchit package function
  # if there is only 1 case, match all possible controls in order of distance (number of matches can be anywhere from 1 to ratio)
  if(num_cases==1) {
    for_matching_inst$distance <- distance_matrix[,1]
    for_matching_inst <- for_matching_inst %>% arrange(desc(case), distance) 
    
    num_match <- min(nrow(for_matching_inst%>%filter(case==0&!distance%>%is.infinite())), ratio) 
    for_matching_inst <- for_matching_inst[1:(num_match+1),] %>% select(!distance)
    
    no_duplicates <- rbind(no_duplicates, cbind(id=1:(num_match+1), subclass=rep(1,num_match+1), for_matching_inst))
    next
  }
  
  # if >1 case and >1 control, use matchit function
  matched_data <- matchit(case ~ Institution + BuildingId + num_dose_adjusted + has.prior.inf + level,
                          data = for_matching_inst, 
                          distance = distance_matrix,
                          exact = case ~ Institution + BuildingId + num_dose_adjusted + has.prior.inf + level, 
                          ratio=ratio)
  
  print(matched_data %>% summary())
  no_duplicates <- rbind(no_duplicates, matched_data %>% get_matches() %>% select(!weights))
}


# relabel match groups to be unique across all institutions (subclass currently 1:n within exact strata (keys))                                                                                 
full_match <- match %>% group_by(key, subclass) %>% 
  filter(length(unique(ResidentId))==n()) %>% mutate(duplicate=F) %>% 
  ungroup() %>%
  rbind(no_duplicates %>% mutate(duplicate=T))
matched_keys <- full_match %>%
  group_by(key, subclass, duplicate) %>% 
  group_keys() %>% mutate(group = 1:n())
full_match <- full_match %>% left_join(matched_keys) %>% mutate(id=1:n())


# change file path based on matching specifications
# primary analysis saves as matched_building_3_7days-12matching-[date].csv
full_match %>% select(!c(n, Day, Night)) %>% write_csv("D:/CCHCS_premium/st/indirects/matched_building___")

