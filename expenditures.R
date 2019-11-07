library(readxl)
library(tidyverse)

expend <- read_excel("data/Expenditures Billed_Susan Small.xlsx", skip = 1) %>% 
  head(-3)

expend %>% 
  group_by(`Cost Center`) %>% 
  summarise(`Hours worked` = sum(`View Billable Hours [ST/Hr]`))
