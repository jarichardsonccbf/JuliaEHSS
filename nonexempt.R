source("exempt.R")

library(readxl)
library(tidyverse)

source("locations.R")

rm(exempt.locations, expenditures.locations, cbcs.locations)

non.exempt <- read_excel("data/NonExemptNov19.xlsx", sheet = "Details") %>% 
  filter(`Wagetype Desc.` != "3C Sick Pay",
         `Wagetype Desc.` != "3C Vacation Pay",
         `Wagetype Desc.` != "3C Vac Payout SupTx") %>% 
  droplevels() %>%
  rename(Cost.Center.Desc. = `Cost Center Desc.`) %>% 
  mutate(Cost.Center.Desc. = as.factor(Cost.Center.Desc.)) %>% 
  left_join(non.exempt.locations, by = "Cost.Center.Desc.")

facility.section.totals <- non.exempt %>% 
  group_by(Facility, Section) %>% 
  summarise(`Hours worked` = sum(Hours))

facility.totals <- facility.section.totals %>% 
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

non.exempt.all <- bind_rows(facility.totals, facility.section.totals, state.total) %>%
  arrange(Facility, Section) %>% 
  mutate(Section = recode(Section,
                          "A" = "Total")) %>% 
  ungroup() 

non.exempt.all[non.exempt.all == 0] <- NA
