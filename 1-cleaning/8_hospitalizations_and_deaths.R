# estimate number of COVID-19 hospitalizations and deaths during Omicron wave

rm(list=ls())
gc()

setwd("D:/CCHCS_premium/CDCR Data/Dec 16 2022 Data")

# hospitalization data
hosp <- read.csv("CovidHospitalizations_20221216.csv", sep = ";")
hosp <- hosp %>% mutate(AdmissionDate=as.Date(AdmissionDate),
                        DischargeDate=as.Date(DischargeDate))
hosp %>% group_by(ResidentId) 
hosp %>% group_by(ResidentId) %>% filter(n()>1) %>% arrange(ResidentId)

hosp <- hosp %>% group_by(ResidentId) %>% arrange(ResidentId, AdmissionDate) %>% 
  mutate(diff=c(0, AdmissionDate[2:n()]-DischargeDate[1:n()-1]))
omicron_hosp <- omicron %>% left_join(hosp %>% 
                        filter(AdmissionDate >= "2021-12-15") %>% summarise_all(first))
omicron_hosp   
omicron_hosp %>% filter(!AdmissionDate %>% is.na())
31/22334


# any COVID-19 related deaths in Omicron wave?
loc <- read_delim("/Users/sophiatan/Documents/UCSF/ST files/LocationStatus_20220520.csv", delim = ";")
loc_covid <- loc %>% group_by(ResidentId) %>% arrange(ResidentId, Sequence) %>% filter(Day >= "2020-03-01")
died <- loc_covid%>%arrange(ResidentId)%>%filter(LocationStatus=='DiedInCustody')
