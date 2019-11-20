library(readxl)
library(tidyverse)

source("locations.R")

nonexempt <- read_excel("data/NONEXEMPTCCBF EHSS hours report October 2019.xlsx", sheet = "Details") %>% 
  filter(`Wagetype Desc.` != "3C Sick Pay",
         `Wagetype Desc.` != "3C Vacation Pay",
         `Wagetype Desc.` != "3C Vac Payout SupTx") %>% 
  droplevels() %>%
  rename(Cost.Center.Desc. = `Cost Center Desc.`) %>% 
  mutate(Cost.Center.Desc. = as.factor(Cost.Center.Desc.)) %>% 
  left_join(non.exempt.locations, by = "Cost.Center.Desc.")

nonexempt <- nonexempt %>% 
  group_by(Facility, Section) %>% 
  summarise(`Hours worked` = sum(Hours))

all.loc <- non.exempt.locations %>% 
  select(-c(Cost.Center.Desc.)) %>% 
  unique()

all.loc <- nonexempt %>%  
  right_join(all.loc, by = c("Facility", "Section"))


all.totals <- all.loc %>% 
  group_by(Facility) %>% 
  summarise(`Hours worked` = sum(`Hours worked`, na.rm = T)) %>% 
  mutate(Section = "A") %>% 
  select(Facility, Section, `Hours worked`) %>% 
  mutate(Section = as.factor(Section))

all <- bind_rows(all.totals, all.loc) %>%
  arrange(Facility, Section) %>% 
  mutate(Section = recode(Section,
                          "A" = "Total")) %>% 
  ungroup() 

osha.all[osha.all == 0] <- NA