## check 7b: cross-check => DTP3, IPV1 ----
# dtp3 and ipv1 should be similar in some countries, particularly in countries that administer OPV
v1 <- "dtp3"
v2 <- "ipv1"
check7b <- compare_2vax_b(data, v1, v2) %>%
  mutate(check = "Cross-check DTP3 and IPV1") %>%
  select(check, everything()) 

# filter for the OPV-using countries
check7b_2 <- check7b %>%
  filter(ISOCountryCode %in% c(opv_list))


## clean table ----
fin_check07b <- check7b_2 %>%
  select(ISOCountryCode, diff) %>%
  mutate(ch7b_diff_dtp3_ipv1 = case_when(abs(diff) > 15 ~ paste0('>15pp diff between DTP3 and IPV1 (',diff,'pp)'),
                                         abs(diff) > 10 ~ paste0('>10pp diff between DTP3 and IPV1 (',diff,'pp)'),
                                         abs(diff) > 5 ~ paste0('>5pp diff between DTP3 and IPV1 (',diff,'pp)'),
                                         diff < 0 ~ paste0('DTP3 less than IPV1 (',diff,'pp)'),
                                         TRUE ~ NA_character_)) %>%
  filter(!is.na(ch7b_diff_dtp3_ipv1)) %>%
  select(-diff)