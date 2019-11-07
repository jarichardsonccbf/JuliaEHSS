library(readxl)
library(tidyverse)
library(lubridate)

osha <- read_excel("data/OSHA Export Example (Jason).xlsx", sheet = "Data") %>% 
  mutate(month = month(`Loss Date`),
         year = year(`Loss Date`)) %>% 
  filter(year == 2019)

osha.total <- osha %>% 
  group_by(Location, month) %>% 
  summarise(totals = n())
  
osha.work.loss <- osha %>% 
  filter(`Lost Work Days` > 0) %>% 
  group_by(Location, month) %>% 
  summarise(work_loss = n())

osha.count <- osha.total %>% 
  left_join(osha.work.loss, by = c("Location", "month"))
