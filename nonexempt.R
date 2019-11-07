library(readxl)
library(tidyverse)

nonexempt <- read_excel("data/NONEXEMPTCCBF EHSS hours report October 2019.xlsx", sheet = "Details") %>% 
  filter(`Wagetype Desc.` != "3C Sick Pay",
         `Wagetype Desc.` != "3C Vacation Pay") %>% 
  droplevels()

# Does Julia filter out "Vac Payout SupTx" or just "Vacation Pay"

a <- nonexempt %>% 
  group_by(`Cost Center Desc.`) %>% 
  summarise(`Hours worked` = sum(Hours))
