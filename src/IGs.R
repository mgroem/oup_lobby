
library(tidyverse)
sgp <- read.csv("data/SGP_IGs_draft.csv")

sgp$count <- 1

sgp <- sgp[sgp$type=="Business Association"|sgp$type=="Citizen Group"|sgp$type=="Professional Group"|sgp$type=="Trade Union",]

sgp <- sgp %>%
  group_by(type) %>%
  summarize(count = n(),
            perc = n()/894)
