## check 9: compare admin denoms current yr vs prev yr ----
# flag changes in target population of >10%
check9 <- data %>%
  filter(Year %in% c(rev_yr-1, rev_yr)) %>%
  select(ISOCountryCode, Country, Year, Vaccine, admin_denom = ChildrenInTarget) %>%
  pivot_wider(., names_from = Year, values_from = admin_denom) %>%
  # calculate rel difference
  mutate(diff = .[[5]] - .[[4]],  
         diff_rel = round((diff / .[[4]]) * 100, 0),
         diff_rel_abs = abs(diff_rel)) %>%
  # filter where absolute relative difference >5%
  filter(diff_rel_abs > 10) %>%
  # group by all columns except vaccine
  group_by_at(vars(-Vaccine)) %>%
  # combine vaccines where all other column values are the same into one
  summarise(Vaccine = paste(Vaccine, collapse = ", ")) %>%
  relocate(Vaccine, .after = Country)  %>%
  mutate(check = str_glue("Admin denominator in {rev_yr} vs {rev_yr-1} (change >5%)")) %>%
  select(check, everything())


## clean table ----
# here we will show the largest relative difference in denom per country, noting that it can vary by antigen
fin_check09 <- check9 %>%
  group_by(ISOCountryCode) %>%
  filter(diff_rel_abs == max(diff_rel_abs)) %>%
  select(ISOCountryCode, diff_rel, Vaccine) %>% 
  distinct() %>%
  mutate(ch9_rel_diff_denom = case_when(abs(diff_rel) > 5 ~ paste0('denom rel diff >5% (',diff_rel,'%) | ', Vaccine),
                                        TRUE ~ NA_character_)) %>%
  select(-c(diff_rel, Vaccine))