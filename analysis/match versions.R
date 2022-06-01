day_dist <- 30

distance_matrix <- dist(as.matrix(infections_unique_omicron%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()
distance_matrix_inst <- dist(as.matrix(infections_unique_omicron%>%select(Institution)%>%mutate(Institution=as.numeric(Institution))), diag = T, upper = T) %>% as.matrix()
distance_matrix[distance_matrix>day_dist]<-Inf
distance_matrix_inst[distance_matrix_inst!=0]<-Inf
distance_matrix <- distance_matrix+distance_matrix_inst

inf_omicron_subset <- infections_unique_omicron %>% select(no, ResidentId, num_dose_adjusted, treatment, num_pos.y, Day, Institution)

m1 <- matchit(treatment ~ Day + Institution, data = inf_omicron_subset,
             method = "nearest", distance = distance_matrix, ratio=3, replace = F)
summary(m1)
m1$match.matrix
m1out <- match.data(m1)
final1 <- m1out %>% group_by(subclass) %>% arrange(desc(treatment)) %>% 
  filter(abs(Day-first(Day))<=day_dist & first(Institution)==Institution) %>% group_by(subclass) %>% filter(n()>1)

final1 %>% group_by(treatment) %>% summarise(count=n())


distance_matrix <- dist(as.matrix(infections_unique_omicron%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()
distance_matrix_inst <- dist(as.matrix(infections_unique_omicron%>%select(Institution)%>%mutate(Institution=as.numeric(Institution))), diag = T, upper = T) %>% as.matrix()
distance_matrix[distance_matrix>day_dist]<-10000
distance_matrix_inst[distance_matrix_inst!=0]<-10000
distance_matrix <- distance_matrix+distance_matrix_inst

m2 <- matchit(treatment ~ Day + Institution, data = inf_omicron_subset,
              method = "nearest", distance = distance_matrix, ratio=3, replace = F)
summary(m2)
m2$match.matrix
m2out <- match.data(m2)
final2 <- m2out %>% group_by(subclass) %>% arrange(desc(treatment)) %>% 
  filter(abs(Day-first(Day))>day_dist & first(Institution)!=Institution) %>% group_by(subclass) %>% filter(n()>1)

final2 %>% group_by(treatment) %>% summarise(count=n())


distance_matrix <- dist(as.matrix(infections_unique_omicron%>%select(Day)%>%mutate(Day=as.numeric(Day))), diag = T, upper = T) %>% as.matrix()
#distance_matrix[distance_matrix>day_dist]<-Inf
m3 <- matchit(treatment ~ Day + Institution, data = inf_omicron_subset,
              method = "nearest", exact = "Institution", #caliper = c(30),std.caliper = F,
              distance = distance_matrix, ratio=3, replace = F)
summary(m3)
m3$match.matrix
m3out <- match.data(m3)
final3 <- m3out %>% group_by(subclass) %>% arrange(desc(treatment)) %>% 
  filter(abs(Day-first(Day))<=day_dist) %>% group_by(subclass) %>% filter(n()>1)

final3 %>% group_by(treatment) %>% summarise(count=n())
final3 %>% arrange(subclass) %>% select(!c(ResidentId, num_pos.y)) %>% rename("index_case_no"="no", "index_num_doses"="num_dose_adjusted") %>% head(20)
