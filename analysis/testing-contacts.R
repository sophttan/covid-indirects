# Sophia Tan 3/8/22
# Test for loop code

setwd("D:/code_ST")
d <- read_csv("housing_inf_data.csv")
prim <- read_csv("potential-primary-cases/all_infections_less_than_8res.csv")

d %>% group_by(RoomId, Day) %>% summarise(residents=unique(ResidentId))

for (p in 1:nrow(prim)) {
  filter(d, )
}