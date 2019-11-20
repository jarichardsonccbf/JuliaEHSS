osha.all

osha.exempt <- all %>% 
  full_join(osha.all, by = c("Facility", "Section"))

