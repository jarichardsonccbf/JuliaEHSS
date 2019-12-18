library(readxl)
library(tidyverse)
library(lubridate)

source("locations.R")

osha <- read_excel("data/OSHAnov19.xlsx", sheet = "Data")
osha <- osha[-1:-2,]

names(osha) <- as.matrix(osha[1, ])
osha <- osha[-1, ]

osha <- osha %>% 
  mutate(month = months(as.Date(as.numeric(osha$`Loss Date`), origin = '1899-12-30')),
         year = year(as.Date(as.numeric(osha$`Loss Date`), origin = '1899-12-30'))) %>% 
  filter(year == "2019",
         month == "November")


osha <- osha %>%
  left_join(cbcs.locations, by = c("Location"))
  
facility.section.or <- osha %>% 
  group_by(Facility, Section) %>% 
  summarise(OR = n())
  
facility.section.lt <- osha %>% 
  filter(`Lost Work Days` > 0) %>% 
  group_by(Facility, Section) %>% 
  summarise(LT = n())

facility.section <- facility.section.or %>% 
  left_join(facility.section.lt, by = c("Facility", "Section"))

facility.totals <- facility.section %>%
  droplevels() %>% 
  pivot_longer(cols = OR:LT, names_to = "incident.type") %>% 
  group_by(Facility, incident.type) %>% 
  summarise(total = sum(value, na.rm = T)) %>% 
  pivot_wider(names_from = incident.type, values_from = total) %>% 
  mutate(Section = "A") %>% 
  select(Facility, Section, OR, LT)

state.totals <- facility.totals %>%
  ungroup() %>% 
  summarise_at(vars (OR:LT), sum, na.rm = T) %>%
  mutate(Facility = "Total",
         Section = "Total") %>% 
  select(Facility, Section, OR, LT)

rm(list=ls()[! ls() %in% c("facility.section", "facility.totals", "state.totals")])

facility.section <- as.data.frame(facility.section)
facility.totals <- as.data.frame(facility.totals)
state.totals <- as.data.frame(state.totals)

osha.all <- rbind(facility.totals, facility.section, state.totals) %>%
  arrange(Facility, Section) %>% 
  mutate(Section = recode(Section,
                          "A" = "Total")) %>% 
  ungroup() 
 
osha.all[osha.all == 0] <- NA

rm(list=ls()[! ls() %in% c("osha.all")])
