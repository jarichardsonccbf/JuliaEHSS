library(readxl)
library(tidyverse)

source("locations.R")

non.exempt <- read_excel("data/NONEXEMPTCCBF EHSS hours report October 2019.xlsx", sheet = "Details") %>% 
  filter(`Wagetype Desc.` != "3C Sick Pay",
         `Wagetype Desc.` != "3C Vacation Pay",
         `Wagetype Desc.` != "3C Vac Payout SupTx") %>% 
  droplevels() %>%
  rename(Cost.Center.Desc. = `Cost Center Desc.`) %>% 
  mutate(Cost.Center.Desc. = as.factor(Cost.Center.Desc.)) %>% 
  left_join(non.exempt.locations, by = "Cost.Center.Desc.")

non.exempt.hours <- non.exempt %>% 
  group_by(Facility, Section) %>% 
  summarise(`Hours worked` = sum(Hours))

all.loc <- non.exempt.locations %>% 
  select(-c(Cost.Center.Desc.)) %>% 
  unique()

all.loc <- non.exempt.hours %>%  
  right_join(all.loc, by = c("Facility", "Section"))


non.exempt.totals <- all.loc %>% 
  group_by(Facility) %>% 
  summarise(`Hours worked` = sum(`Hours worked`, na.rm = T)) %>% 
  mutate(Section = "A") %>% 
  select(Facility, Section, `Hours worked`) %>% 
  mutate(Section = as.factor(Section))

non.exempt.all <- bind_rows(non.exempt.totals, all.loc) %>%
  arrange(Facility, Section) %>% 
  mutate(Section = recode(Section,
                          "A" = "Total")) %>% 
  ungroup() 

non.exempt.all[non.exempt.all == 0] <- NA

non.exempt.all <- rbind(as.data.frame(non.exempt.all),
                    non.exempt.all %>% 
                      ungroup() %>% 
                      summarise(`Hours worked` = sum(`Hours worked`, na.rm = T)) %>% 
                      mutate(Facility = "Total",
                             Section = "Total") %>% 
                      select(Facility, Section, `Hours worked`))


rm(list=ls()[! ls() %in% c("osha.all", "exempt.all", "non.exempt.all")])
