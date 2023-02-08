rm(list=ls())

setwd("D:/CCHCS_premium/CDCR Data/Dec 16 2022 Data")

library(readr)
library(tidyverse)

activity <- read_delim("Activity_20221216.csv", delim = ";")

activity %>% head()

activity$Day %>% summary()

activity <- activity %>% filter(Day >= "2021-12-15") %>% arrange(ResidentId, Day)

activity$ActivityType %>% unique()

# check jobs
jobs <- activity %>% filter(ActivityType=="Job"|ActivityType=="WORK")
jobs$Detail %>% unique()
jobs <- jobs %>% mutate(JobType = substring(Detail, 2, 4))
job_groups <- jobs %>% group_by(JobType) %>% 
  summarise(count=length(unique(Detail)), Detail=paste0(unique(Detail), collapse=""))

job_groups <- job_groups %>% mutate(Type = str_extract(Detail, "[^\\(\\)\\|]+"))
job_groups <- job_groups %>% mutate(Type = substring(Type, 5, length(Type)) %>% str_trim(side="both"))
job_groups %>% view()

job_groups_clean <- job_groups %>% select(JobType, Type)
job_groups_clean %>% view()

jobs <- jobs %>% left_join(job_groups_clean, by="JobType")
diff_jobs_1_day <- jobs %>% group_by(ResidentId, Day) %>% filter(!all(JobType==first(JobType)))
diff_jobs_1_day 
diff_jobs_1_day <- diff_jobs_1_day %>% arrange(Type) %>% mutate(Types=paste0(unique(Type), collapse="|"))
diff_jobs_1_day$Types %>% unique()

summary_jobs <- jobs %>% group_by(ResidentId) %>% arrange(Type) %>% 
  summarise(num_days=length(unique(Day)),
            num_types=length(unique(Type)),
            Types=paste0(unique(Type), collapse="|"))

summary_jobs$num_days %>% summary()

# most common jobs
jobs %>% group_by(ResidentId) %>% 
  summarise(Type=unique(Type)) %>% ungroup() %>% 
  group_by(Type) %>% summarise(count=n()) %>% arrange(desc(count))


jobs %>% select(ResidentId, Day, JobType, Type, Detail) %>% 
  write_csv('D:/CCHCS_premium/st/indirects/cleaned-data/job_omicron.csv')
