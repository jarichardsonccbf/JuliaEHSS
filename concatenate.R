osha.exempt.non <- non.exempt.all %>% 
  full_join(exempt.all, by = c("Facility", "Section")) %>% 
  replace(is.na(.), 0) %>%
  mutate(`Hours worked` = `Hours worked.x` + `Hours worked.y`) %>% 
  select(-c(`Hours worked.x`, `Hours worked.y`)) %>% 
  full_join(osha.all, by = c("Facility", "Section")) %>% 
  arrange(Facility)
