## check 12: flag where DTP3 > DTP1 ----
check12 <- data %>%
  filter(Vaccine %in% c('dtp1','dtp3')) %>%
  select(ISOCountryCode, Year, Vaccine, WUENIC) %>%
  arrange(ISOCountryCode, Year, Vaccine) %>%
  pivot_wider(., names_from = Vaccine, values_from = WUENIC) %>%
  mutate(flag_yr = case_when(dtp3 > dtp1 ~ as.character(Year), TRUE ~ NA_character_),
         flag_diff = ifelse(!is.na(flag_yr), dtp3 - dtp1, NA_real_),
         flag = ifelse(!is.na(flag_yr), paste0(flag_yr, ' (', as.character(flag_diff), 'pp higher)'), NA_character_)) %>%
  filter(!is.na(flag_yr)) %>%
  mutate(check = "DTP3 > DTP1") %>%
  select(check, everything())


## clean table ----
fin_check12 <- check12 %>%
  select(ISOCountryCode, ch12_dtp3_gt_dtp1 = flag) %>%
  distinct()