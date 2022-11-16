all <- read_csv("all_contacts_100722.csv")


possible <- all %>% group_by(no) %>% filter(all(multiple_inf==0, na.rm=T)) %>% ungroup() %>%
  filter(has_inf==0)

final <- possible %>% filter(has_neg_test==1&has_followup==1)
excluded <- possible %>% filter(has_neg_test==0|has_followup==0)
excluded %>% group_by(contacts) %>% filter(n()>1) %>% view()

set.seed(42)
final <- final[sample(1:nrow(final)),]
set.seed(42)
excluded <- excluded[sample(1:nrow(excluded)),]

contacts <- final %>% distinct(contacts, .keep_all = T) %>% mutate(group="included") %>% 
  rbind(excluded %>% distinct(contacts, .keep_all = T) %>% mutate(group="excluded")) 

contacts <- contacts %>% select(!c(ResidentId, num_pos, no, ...1)) 

library(lubridate)
demo <- read_csv("demographic_data_clean.csv")
covid_risk <- read_csv("covid_risk_score.csv")
covid_risk_subset <- covid_risk %>% filter(ResidentId %in% contacts$contacts) 
covid_risk_subset <- covid_risk_subset %>% rowwise() %>% mutate(first=interval %>% str_extract_all("[0-9]+-[0-9]+-[0-9]+"), 
                                                                interval=interval(first[1], first[2])) %>% select(!first)

contacts <- contacts %>% left_join(demo, by=c("contacts"="ResidentId")) %>% full_join(covid_risk_subset, by=c("contacts"="ResidentId")) %>% 
  filter(first_contact %>% lubridate::`%within%`(interval)) %>%
  rename("covid_risk"="Value") %>% select(!c(interval, first_contact))

contacts
contacts <- contacts %>% mutate(Age=2022-BirthYear)

contacts

vacc <- read_csv("cleaned_vaccination_data.csv")
vacc_primary <- vacc %>% group_by(ResidentId) %>% filter(num_dose <= full_vacc)
vacc_primary <- vacc_primary %>% summarise(full_vacc=first(full_vacc), primary_series=first(Vaccine), all_same=all(Vaccine==first(Vaccine)))
contacts <- contacts %>% left_join(vacc_primary, by=c("contacts"="ResidentId"))

library(tableone)
contacts <- contacts %>% mutate(primary_series=ifelse(num_vacc_doses==0|num_vacc_doses %>% is.na(), "Unvaccinated", primary_series),
                                boosted=ifelse(num_vacc_doses>full_vacc.y & full_vacc.y>0, 1, 0),
                          Race = case_when(Race=="A"~"Asian or Pacific Islander",
                                           Race=="B"~"Black",
                                           Race=="H"|Race=="M"~"Hispanic",
                                           Race=="I"~"American Indian/Alaskan Native",
                                           Race=="O"~"Other",
                                           Race=="W"~"White")) 

vars <- c("Sex", "Age", "Race", "covid_risk", "has_prior_inf")
catVars <- c("Sex", "Race", "has_prior_inf")
tbl1 <- CreateTableOne(vars = vars, strata = c("group"), data = contacts, factorVars = catVars)
tbl1_matrix <- print(tbl1)
tbl1_matrix <- tbl1_matrix[,c(1,2)]
tbl1_matrix

contacts <- contacts %>% mutate(status=case_when(num_vacc_doses>0&num_vacc_doses==full_vacc.y~"Fully vaccinated", 
                                                 num_vacc_doses>0&num_vacc_doses<full_vacc.y~"Partially vaccinated",
                                                 num_vacc_doses>0&num_vacc_doses>full_vacc.y~"Boosted",
                                                 num_vacc_doses==0~""), 
                          group=group %>% factor(levels=c("included", "excluded")))
prim_series <- contacts %>% group_by(group, primary_series) %>% summarise(n=n())
prim_series <- prim_series %>% group_by(group) %>% summarise(primary_series=primary_series, n=n, perc=signif(n/sum(n)*100,2))
stratified_status <- contacts %>% filter(primary_series != "Unvaccinated") %>% group_by(group, primary_series) %>% 
  summarise(status=status,count=n()) %>% 
  group_by(group, primary_series, status) %>% summarise(n=n(), perc=signif(n()/mean(count)*100, 2))
prim_series <- prim_series %>% mutate(status="") %>% rbind(stratified_status)

prim_series <- prim_series %>% mutate(primary_series = case_when(grepl("Ad26",primary_series)~"Ad26.COV2",
                                                                 grepl("BNT",primary_series)~"BNT162b2",
                                                                 grepl("1273",primary_series)~"mRNA-1273",
                                                                 primary_series=="Unvaccinated"~"Unvaccinated")) %>% 
  mutate(`N (%)` = paste0(n, " (", perc, "%)"), 
         status = paste(primary_series, status)) 

prim_series <- prim_series %>% select(!c(n, perc, primary_series)) %>% 
  pivot_wider(names_from=group, values_from=`N (%)`, values_fill = "-") %>%
  mutate(status=status %>% 
           factor(levels=c("Unvaccinated ", "Ad26.COV2 ", "Ad26.COV2 Fully vaccinated", "Ad26.COV2 Boosted",
                           "BNT162b2 ", "BNT162b2 Partially vaccinated", "BNT162b2 Fully vaccinated", "BNT162b2 Boosted",
                           "mRNA-1273 ", "mRNA-1273 Partially vaccinated", "mRNA-1273 Fully vaccinated", "mRNA-1273 Boosted"))) %>%
  arrange(status)


prim_series <- prim_series %>% data.frame(row.names = as.character(prim_series$status), check.names = F)
prim_series <- prim_series %>% select(!status)
names(prim_series) <- NULL
colnames(prim_series) <- colnames(tbl1_matrix)


tbl1_matrix %>% rbind(prim_series) %>% write.csv("/Users/sophiatan/Documents/UCSF/CDCR-CalProtect/tables/demographics_missingness_table.csv",)
