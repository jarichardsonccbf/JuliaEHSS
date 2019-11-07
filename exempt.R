library(readxl)
library(tidyverse)

exempt <- read_excel("data/CCBF Exempt Hours_2019_10.xlsx", sheet = "Sheet1")

a <- exempt %>% 
  group_by(`L4 Org Unit Name`) %>% 
  summarise(`Hours worked` = sum(`Total Hours`))
