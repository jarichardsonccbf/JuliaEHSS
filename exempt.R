library(readxl)
library(tidyverse)

source("locations.R")

exempt.hours <- read_excel("data/CCBF Exempt Hours_2019_10.xlsx", sheet = "Sheet1") %>% 
  left_join(exempt.locations, by = c("L4 Org Unit Name", "Personnel Area Desc")) %>% 
  group_by(Facility, Section) %>% 
  summarise(`Hours worked` = sum(`Total Hours`, na.rm = T)) %>% 
  drop_na()

all.loc <- exempt.locations %>% 
  select(-c(`L4 Org Unit Name`, `Personnel Area Desc`)) %>% 
  unique()

all.loc <- exempt.hours %>%  
  right_join(all.loc, by = c("Facility", "Section"))

exempt.totals <- all.loc %>% 
  group_by(Facility) %>% 
  summarise(`Hours worked` = sum(`Hours worked`, na.rm = T)) %>% 
  mutate(Section = "A") %>% 
  select(Facility, Section, `Hours worked`) %>% 
  mutate(Section = as.factor(Section))

exempt.all <- bind_rows(exempt.totals, all.loc) %>%
  arrange(Facility, Section) %>% 
  mutate(Section = recode(Section,
                          "A" = "Total")) %>% 
  ungroup() 

exempt.all[exempt.all == 0] <- NA

exempt.all <- rbind(as.data.frame(exempt.all),
  exempt.all %>% 
  ungroup() %>% 
  summarise(`Hours worked` = sum(`Hours worked`, na.rm = T)) %>% 
  mutate(Facility = "Total",
         Section = "Total") %>% 
  select(Facility, Section, `Hours worked`))

rm(list=ls()[! ls() %in% c("osha.all", "exempt.all")])
