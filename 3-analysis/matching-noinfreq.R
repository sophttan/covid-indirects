# Sophia Tan 9/6/23

rm(list=ls())
gc()
setwd("D:/CCHCS_premium/st/indirects/cleaned-data/")
source()

# set up parallelization
cl<-makeCluster(detectCores()-1)
registerDoParallel(cl)

d <- read_csv("allvacc_full_data_prematching_bydose_082523.csv") %>% 
  group_by(id) %>%
  mutate(fixed_assignment = any(test)&any(!test)) %>% 
  mutate(both_unvacc = all(vacc==0),
         fixed_control = both_unvacc|(fixed_assignment&any(vacc==0&!test)),
         possible_control= fixed_control|(any(!vacc)&all(test))) 

risk <- read_csv("covid_risk_score.csv") %>% 
  filter(ResidentId %in% d$ResidentId)
fix_intersection <- function(v) {
  v%>%str_extract_all("[0-9]{4}-[0-9]{2}-[0-9]{2}", simplify = T)
}
intersection <- fix_intersection(risk$interval)
risk$start <- intersection[,1]%>%as.vector()%>%as.Date()
risk$end <- intersection[,2]%>%as.vector()%>%as.Date()
risk <- risk %>% 
  mutate(risk_interval=interval(start=start, end=end)) %>% 
  select(!c(start, end, interval)) %>% rename("risk"="Value")

d <- d %>% left_join(risk, by=c("ResidentId")) %>% 
  filter(first%within%risk_interval) %>% 
  select(!c(risk_interval))

rm(risk) 
gc()

d <- d %>% mutate(time_since_inf = (difftime(first, Day_inf)%>%as.numeric())/30.417,
                  time_since_vacc = (difftime(first, Day_vacc)%>%as.numeric())/30.417) 

d <- d %>% ungroup()
ps <- glm(vacc ~ age + risk, data = d, family = binomial(link='logit'))
d <- d %>% mutate(logodds = predict.glm(ps), ps=exp(logodds)/(1+exp(logodds)))

d <- d %>%
  mutate(time_since_inf_scaled = scale(time_since_inf)%>%as.numeric(),
         ps_scaled = scale(ps)%>%as.numeric()) %>% group_by(id)




d_subset <- d %>% filter(Institution==3) %>% filter(id!=8823)
buildings <- d_subset$BuildingId %>% unique()

prematch <- NULL
results <- NULL
for (building in buildings) {
  print(building)
  # remove treatment units that don't overlap with any possible control units
  # can do the same for any control units but they cannot overlap with any other units
  d_building <- d_subset %>% filter(BuildingId==building) %>% arrange(first)
  
  if(n_groups(d_building)==1|all(d_building$vacc>0)){next}
  
  test <- d_building %>% 
    mutate(duration_interval=interval(first, last)) %>%
    select(id, num_dose_grouped, inf, both_unvacc, possible_control, test, first, duration_interval) %>%
    mutate(primary_vacc = list(num_dose_grouped[test]),
           primary_inf = list(inf[test]),
           secondary_inf = list(inf[!test])) %>%
    distinct(id, .keep_all = T) %>% select(!test)
  
  for_filtering <- test %>% cross_join(test) %>% 
    mutate(eligible = abs(first.x-first.y) <= 13, 
           overlap=(intersect(duration_interval.x, duration_interval.y)%>%time_length("day")%>%as.numeric())+1) %>% 
    replace_na(list(overlap=0)) %>% 
    filter(id.x!=id.y) %>% rowwise() %>%
    mutate(overlap_vacc = list(intersect(primary_vacc.x, primary_vacc.y)),
           possible_vacc_match = length(overlap_vacc)>0) %>%
    group_by(id.x)
  
  remove <- for_filtering%>%
    # remove controls if they don't have matches (by time or vaccine status)
    # remove treatments if they don't have matches (by time or vaccine status)
    summarise(remove_control = all(possible_control.x) & sum(eligible & possible_vacc_match)==0,
              remove_treatment = all(!possible_control.x) & sum(possible_control.y & eligible & possible_vacc_match)==0) %>%
    filter(remove_control|remove_treatment)
  
  # fix_control <- for_filtering %>% 
  #   # fix mixed units as control if they don't overlap with any units with 2 unvaccinated residents
  #   summarise(new_fix_control=all(possible_control.x) & sum(both_unvacc.y & eligible)==0) %>% 
  #   filter(new_fix_control)
  
  # fix_treatment <- for_filtering %>% 
  #   filter(!possible_control.x) %>% filter(!id.x %in% remove$id.x) %>%
  #   filter(possible_control.y & eligible & possible_vacc_match) %>% unnest(overlap_vacc) %>% 
  #   filter(unique(overlap_vacc)%>%length()==1) %>% select(!c(primary_vacc.x, primary_vacc.y, primary_inf.x, primary_inf.y, secondary_inf.x, secondary_inf.y)) %>% 
  #   summarise_all(first) %>% select(id.x, overlap_vacc)
  
  d_building <- d_building %>% filter(!id %in% remove$id.x) %>%
    mutate(both_test_diff_vacc_inf = all(test)&((num_dose_grouped[1]!=num_dose_grouped[2])|inf[1]!=inf[2]))
    # left_join(fix_treatment, by=c("id"="id.x")) %>% 
    # mutate(test=case_when(id%in%fix_control$id.x & !all(vacc==0) & vacc==0~F, 
    #                       # num_dose_grouped[1]!=num_dose_grouped[2] & overlap_vacc!=num_dose_grouped~F,
    #                       T~test), 
    #        both_test_diff_vacc_inf = all(test)&((num_dose_grouped[1]!=num_dose_grouped[2])|inf[1]!=inf[2])) 
    # select(!overlap_vacc)
  
  if(nrow(d_building)==0){next}
  
  groups <- d_building %>% summarise_all(first) %>% ungroup() %>% arrange(first) %>% mutate(diff=c(0, diff(first))) %>% select(id, first, diff)
  groups_summary <- groups %>% mutate(new=id==first(id)|diff>13) %>% filter(new) %>% mutate(group=1:n()) %>% select(id,group)
  d_building <- d_building %>% left_join(groups_summary) %>% ungroup() %>% fill(group, .direction="down") %>% group_by(id)
  
  prematch_building <- NULL
  building_results <- NULL
  for (g in 1:nrow(groups_summary)) {
    d_building_sub <- d_building %>% filter(group==g)
    
    unit_info <- d_building_sub %>% 
      select(id, Institution, BuildingId, RoomId, first, last, duration, group, fixed_assignment, fixed_control, possible_control) %>% 
      summarise_all(first)
    
    resident_info <- d_building_sub %>% select(!names(unit_info)) 
    
    unit_info <- unit_info %>% 
      select(!c(fixed_assignment, fixed_control, possible_control)) 
    
    resident_info <- resident_info %>% 
      arrange(id, desc(test), num_dose_grouped) %>% 
      mutate(type=c("primary", "secondary"))
    
    all_mixed <- resident_info %>% filter(both_test_diff_vacc_inf&any(!vacc))
    all_other <- resident_info %>% filter(!id %in% all_mixed$id)
    
    if (all_mixed%>%nrow()==0){
      best <- all_other%>%data_reshape()%>%filter_data()%>%test_assignment()
    } else {
      res <- permutations(n=2,r=all_mixed%>%n_groups(),v=0:1,repeats.allowed=T)
      print(nrow(res))
      
      fix_control <- all_mixed$id %>% unique()
      
      final_processed <- matrix()
      final_processed <- foreach(i=1:nrow(res), .packages=c("tidyverse","lubridate","MatchIt"), .combine=rbind) %dopar%  {
        gc()
        for_matching <- generate_assignment(i, res, all_mixed, all_other)%>%data_reshape()%>%filter_data()
        check <- test_assignment(for_matching) 
        if(!is.null(check)) {
          check <- check %>% group_by(subclass)%>% 
            summarise(diff_primary=abs(diff(time_since_inf_scaled.primary)),
                      diff_propensity=abs(diff(ps_scaled.secondary)),
                      diff_secondary=abs(diff(time_since_inf_scaled.secondary))) 
          check <- check %>% mutate(weight_primary=0.5,
                                    weight_propensity=case_when(diff_primary%>%is.na()&diff_secondary%>%is.na()~1,
                                                                diff_primary%>%is.na()|diff_secondary%>%is.na()~0.5,
                                                                T~0.25),
                                    weight_secondary=case_when(diff_primary%>%is.na()~0.5,
                                                               T~0.25)) %>%
            replace_na(list(diff_primary=0, diff_secondary=0)) %>%
            mutate(distance=weight_primary*diff_primary+weight_propensity*diff_propensity+weight_secondary*diff_secondary)%>%
            mutate(i=i) %>% group_by(i) %>% 
            summarise(control=n(), meandist=mean(distance), dist=sum(distance))}
      } 
      
      if(!final_processed%>%is.null()) {
        optimum <- (final_processed%>%arrange(desc(control), meandist))
        
        best <- generate_assignment(optimum$i[1], res, all_mixed, all_other)%>%
          data_reshape()%>%filter_data()%>%test_assignment()
        
        assignment <- c(rbind(abs(res[optimum$i[1],]-1), res[optimum$i[1],]))
        all_mixed <- all_mixed %>% ungroup() %>% 
          mutate(type=factor(assignment, labels=c("primary","secondary")))
        
        fix_control <- fix_control[!fix_control%in%best$id_stable]
        
      } 
      
      all_mixed <- all_mixed %>% 
        group_by(id) %>%
        mutate(type=if_else(id %in% fix_control & first(vacc)==0, c("secondary", "primary"), type))
    }
    
    all_updated <- all_mixed %>% rbind(all_other) %>% group_by(id)
    all_mixed <- all_updated %>% filter(both_test_diff_vacc_inf&(all(vacc>0)))
    all_other <- all_updated %>% filter(!id%in%(all_mixed$id)%>%unique())
    
    if (all_mixed%>%nrow()==0){
      best <- all_other%>%data_reshape()%>%filter_data()
      final <- best %>% test_assignment() 
      if(final%>%is.null()) {next}
      final <- final %>% post_match_processing()
    } else {
      res <- permutations(n=2,r=all_mixed%>%n_groups(),v=0:1,repeats.allowed=T)
      print(nrow(res))
      final_processed4 <- matrix()
      final_processed4 <- foreach(i=1:nrow(res), .packages=c("tidyverse","lubridate","MatchIt"), .combine=rbind) %dopar%  {
        gc()
        for_matching <- generate_assignment(i, res, all_mixed, all_other)%>%data_reshape()%>%filter_data()
        check <- test_assignment(for_matching) 
        if(!is.null(check)) {
          check <- check %>% group_by(subclass)%>% 
            summarise(diff_primary=abs(diff(time_since_inf_scaled.primary)),
                      diff_propensity=abs(diff(ps_scaled.secondary)),
                      diff_secondary=abs(diff(time_since_inf_scaled.secondary))) 
          check <- check %>% mutate(weight_primary=0.5,
                                    weight_propensity=case_when(diff_primary%>%is.na()&diff_secondary%>%is.na()~1,
                                                                diff_primary%>%is.na()|diff_secondary%>%is.na()~0.5,
                                                                T~0.25),
                                    weight_secondary=case_when(diff_primary%>%is.na()~0.5,
                                                               T~0.25)) %>%
            replace_na(list(diff_primary=0, diff_secondary=0)) %>%
            mutate(distance=weight_primary*diff_primary+weight_propensity*diff_propensity+weight_secondary*diff_secondary)%>%
            mutate(i=i) %>% group_by(i) %>% 
            summarise(control=n(), meandist=mean(distance), dist=sum(distance))}
      } 
      if(final_processed4 %>% is.null()) {next}
      optimum <- (final_processed4%>%arrange(desc(control), meandist))
      
      best <- generate_assignment(optimum$i[1], res, all_mixed, all_other)%>%data_reshape()%>%filter_data()
      final <- best %>% test_assignment() %>% post_match_processing()
    }
    
    prematch_building <- rbind(prematch_building, best)
    building_results <- rbind(building_results, final)
    
  }
  
  if(!building_results%>%is.null()){
    building_results <- building_results %>% group_by(group, subclass) %>% mutate(subclass=cur_group_id())
  }
  
  prematch <- rbind(prematch, prematch_building)
  results <- rbind(results, building_results)
} 

results <- results %>% group_by(Institution, BuildingId, subclass) %>% mutate(subclass=cur_group_id())
results %>% plot_matches()

(d_subset%>%summarise_all(first))$possible_control %>% table()
(prematch$treatment) %>% table()
results$treatment %>% table()

(d_subset%>%summarise_all(first))$duration%>%sum()
(prematch$duration) %>% sum()
(results$final_end-results$final_start+1) %>% sum()

library(smd)
prematch %>% ungroup() %>% 
  summarize_at(
    .vars = c("time_since_inf.primary", "time_since_inf.secondary",
              "age.primary", "age.secondary",
              "risk.primary", "risk.secondary"),
    .funs = list(smd = ~ smd(., na.rm = T, g = treatment)$estimate))

results %>% ungroup() %>% 
  summarize_at(
    .vars = c("time_since_inf.primary", "time_since_inf.secondary",
              "age.primary", "age.secondary",
              "risk.primary", "risk.secondary"),
    .funs = list(smd = ~ smd(., na.rm = T, g = treatment)$estimate))

write_csv(prematch, "matching_data_100623_anyinf/prematch_institution2.csv")
write_csv(results, "matching_data_100623_anyinf/institution2.csv")


d_subset %>% filter(id %in% results$id_stable[results$treatment==0]|!possible_control) %>%
  filter(first <= "2022-06-30") %>%
  plot_all_units()
d_subset %>% filter(!id %in% results$id_stable[results$treatment==0]|!possible_control) %>%
  filter(first <= "2022-06-30") %>%
  plot_all_units()
d_subset %>% filter(!id %in% results$id_stable[results$treatment==0]|!possible_control) %>%
  filter(first > "2022-06-30") %>%
  plot_all_units()


for (a in optimum$i[c(1:6)]) {
  print(plot_matches(final_processed%>%filter(i==a)%>%ungroup()))
  print(final_processed%>%filter(i==a)%>%filter(treatment==1))
}

a <- generate_assignment(305, res, all_mixed, all_other)
a <- a %>% mutate(label=1:n())
matched <- matchit(treatment ~ Institution + BuildingId + num_dose_grouped.primary + 
                     time_since_inf.primary + time_since_inf.secondary + ps_scaled.secondary +
                     age.primary + risk.primary + 
                     age.secondary + risk.secondary,
                   data = a%>%filter(num_dose_grouped.primary==1),
                   distance = generate_distance_matrix(a%>%filter(num_dose_grouped.primary==1)), 
                   exact = treatment ~ Institution + BuildingId + num_dose_grouped.primary,
                   ratio = 1) 
summary(matched)
plot(summary(matched))
plot_matches <- function(d, title="", subtitle="") {
  d <- d %>% arrange(subclass, treatment, first) %>% mutate(subclass1=match(subclass, unique(subclass)))
  d <- d %>% group_by(subclass1) %>% 
    mutate(subclass1=subclass1*5-0.5*0:(n()-1)) %>% ungroup()
  
  p <- d %>%
    ggplot(aes(x = as.POSIXct(final_start), y = subclass1, 
               colour = as.factor(treatment))) +
    geom_point(aes(x = as.POSIXct(first)), size = 1.5, alpha=0.7, color="grey70") +
    geom_point(aes(x = as.POSIXct(last)), size = 1.5, alpha=0.7, color="grey70") +
    geom_segment(aes(x = as.POSIXct(first), xend = as.POSIXct(last), yend = subclass1, color=as.factor(treatment)), alpha=0.7, color="grey70") +
    geom_point(size = 1) +
    geom_point(aes(x = as.POSIXct(final_end)), size = 1) +
    geom_segment(aes(xend = as.POSIXct(final_end), yend = subclass1,  color=as.factor(treatment))) +
    geom_text(aes(x=as.POSIXct(first), label=id_stable), nudge_x = -10*3600*24, size=3) + 
    geom_text(aes(x=as.POSIXct(last), label=paste(num_dose_grouped.primary, inf.primary, inf.secondary)), nudge_x = 18*3600*24, size=3) + 
    scale_x_datetime("Duration of co-residence", 
                     limits = c(as.POSIXct("2021-11-15"), as.POSIXct("2023-02-01")), 
                     date_breaks = "1 month", date_labels ="%b-%y", expand=c(0,0)) + 
    scale_color_discrete(name="Unit type", labels=c("Control", "Treatment")) +
    guides(color = guide_legend(reverse=TRUE)) + 
    labs(title=title, 
         subtitle=subtitle) + 
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
          axis.text.x = element_text(angle=90)) 
  
  p
}

plot_all_units <- function(d) {
  d %>% mutate(vacc = list(num_dose_grouped), inf=list(inf)) %>% 
    distinct(id, .keep_all = T) %>%
    ungroup() %>% arrange(BuildingId, first, duration, desc(test)) %>% 
    mutate(label=1:n()) %>% 
    ggplot(aes(first, label, color=possible_control)) + 
    geom_point(aes()) + 
    geom_segment(aes(xend=last, yend=label)) + 
    geom_point(aes(x=last)) + 
    geom_text(aes(x=first, label=id), nudge_x = -10, size=3) + 
    geom_text(aes(x=last, label=paste(vacc, inf)), nudge_x = 5, size=3) + 
    scale_x_date("Time") + 
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
          axis.ticks.y = element_blank())
}
plot_all_units(d_building)

par(mfrow=c(1,1))
summary_data$control%>%table()%>%barplot(main="Variation from control unit assignment\nNumber of control units")
summary_data4$control%>%table()%>%barplot(main="Variation from treatment unit assignment")

summary_data$control_obs_time%>%as.numeric()%>%hist(main="Observation time control")
summary_data4$control_obs_time%>%as.numeric()%>%hist(main="")

summary_data$obs_time%>%as.numeric()%>%hist(main="Total observation time")
summary_data4$obs_time%>%as.numeric()%>%hist(main="")

