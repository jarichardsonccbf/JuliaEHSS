library(readxl)
library(tidyverse)
library(lubridate)

source("locations.R")

osha <- read_excel("data/OSHA Export Example (Jason).xlsx", sheet = "Data") %>% 
  mutate(month = months(`Loss Date`),
         year = year(`Loss Date`)) %>% 
  filter(year == "2019",
         month == "November")

if("HEADQUARTERS - HQ" %in% osha$Location == TRUE) {
  
  "There is a HQ incident this month. Please convert 'HEADQUARTERS - HQ' to 'HEADQUARTERS - SALES' or 'HEADQUARTERS - ADMIN', save the file, and reupload."
  
} else {

osha <- osha %>%
  left_join(cbcs.locations, by = c("Location"))
  
osha.total <- osha %>% 
  group_by(Facility, Section) %>% 
  summarise(OR = n())
  
osha.work.loss <- osha %>% 
  filter(`Lost Work Days` > 0) %>% 
  group_by(Facility, Section) %>% 
  summarise(LT = n())

osha.count <- osha.total %>% 
  left_join(osha.work.loss, by = c("Facility", "Section"))

osha.totals <- osha.count %>% 
  pivot_longer(cols = OR:LT, names_to = "incident.type") %>% 
  group_by(Facility, incident.type) %>% 
  summarise(total = sum(value, na.rm = T)) %>% 
  pivot_wider(names_from = incident.type, values_from = total) %>% 
  mutate(Section = "A") %>% 
  select(Facility, Section, OR, LT)

osha.all <- rbind(osha.count, osha.totals) %>%
  arrange(Facility, Section) %>% 
  mutate(Section = recode(Section,
                          "A" = "Total")) %>% 
  ungroup() 
 
osha.all[osha.all == 0] <- NA

}

osha.all <- rbind(osha.all,
osha.all %>% 
  summarise_at(vars (OR:LT), sum, na.rm = T) %>% 
  mutate(Facility = "Total",
         Section = "Total") %>% 
  select(Facility, Section, OR, LT))

rm(list=ls()[! ls() %in% c("osha.all")])
