## check 13: flag where IPVC > IPV1 ----
check13 <- data %>%
  filter(Vaccine %in% c('ipv1','ipvc')) %>%
  select(ISOCountryCode, Year, Vaccine, WUENIC) %>%
  arrange(ISOCountryCode, Year, Vaccine) %>%
  pivot_wider(., names_from = Vaccine, values_from = WUENIC) %>%
  mutate(flag_yr = case_when(ipvc > ipv1 ~ as.character(Year), TRUE ~ NA_character_),
         flag_diff = ifelse(!is.na(flag_yr), ipvc - ipv1, NA_real_),
         flag = ifelse(!is.na(flag_yr), paste0(flag_yr, ' (', as.character(flag_diff), 'pp higher)'), NA_character_)) %>%
  filter(!is.na(flag_yr)) %>%
  mutate(check = "IPVC > IPV1") %>%
  select(check, everything())


## clean table ----
fin_check13 <- check13 %>%
  select(ISOCountryCode, ch13_ipvc_gt_ipv1 = flag) %>%
  distinct()
