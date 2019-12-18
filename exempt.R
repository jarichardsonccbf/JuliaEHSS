source("OSHA.R")

library(readxl)
library(tidyverse)

source("locations.R")

facility.section.hours <- read_excel("data/ExemptNov19.xlsx", sheet = "Sheet1") %>% 
  full_join(exempt.locations, by = c("L4 Org Unit Name", "Personnel Area Desc")) %>% 
  group_by(Facility, Section) %>% 
  summarise(`Hours worked` = sum(`Total Hours`, na.rm = T)) %>% 
  drop_na()

all.loc <- exempt.locations %>% 
  select(-c(`L4 Org Unit Name`, `Personnel Area Desc`)) %>% 
  unique()

facility.section.hours <- facility.section.hours %>%  
  right_join(all.loc, by = c("Facility", "Section"))

facility.totals <- facility.section.hours %>% 
  group_by(Facility) %>% 
  summarise(`Hours worked` = sum(`Hours worked`, na.rm = T)) %>% 
  mutate(Section = "A") %>% 
  select(Facility, Section, `Hours worked`) %>% 
  mutate(Section = as.factor(Section))

state.total <- facility.totals %>% 
  summarise(`Hours worked` = sum(`Hours worked`, na.rm = T)) %>% 
  mutate(Facility = "Total",
         Section = "Total") %>% 
  select(Facility, Section, `Hours worked`)


exempt.all <- bind_rows(facility.totals, facility.section.hours, state.total) %>%
  arrange(Facility, Section) %>% 
  mutate(Section = recode(Section,
                          "A" = "Total")) %>% 
  ungroup() 

exempt.all[exempt.all == 0] <- NA

source("locations.R")

read_excel("data/ExemptNov19.xlsx", sheet = "Sheet1") %>% 
  full_join(exempt.locations, by = c("L4 Org Unit Name", "Personnel Area Desc")) %>% 
  select(`L4 Org Unit Name`, `Personnel Area Desc`, Facility, Section) %>% 
  unique() %>% 
  subset(is.na(Facility)) %>% 
  ungroup() %>% 
  select(-c(Facility, Section)) %>% 
  drop_na() %>% 
  


rm(list=ls()[! ls() %in% c("osha.all", "exempt.all")])