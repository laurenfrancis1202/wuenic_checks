## check 7d: cross-check => DTP3, IPVC (DTP-IPV combination using countries) ----
# schedule / combination vaccines
temp <- schedules %>%
  filter(vaccinecode %in% dtp_ipv_vaccine_list,
         schedulerounds == 3) %>%
  mutate(vaccine_round = str_glue("{vaccinecode} (dose {schedulerounds}, {ageadministered})")) %>%
  select(iso3c, vaccine_round) %>%
  distinct() %>%
  arrange(iso3c, vaccine_round) %>%
  group_by(iso3c) %>%
  mutate(n = seq(n())) %>%
  ungroup() %>%
  mutate(name = str_glue("v_{n}")) %>%
  select(-n) %>%
  pivot_wider(., names_from = name, values_from = vaccine_round)

# dtp3 and ipvc should be the same in countries that administer dtp-ipv in combination (usually hexa)
v1 <- "ipvc"
v2 <- "dtp3"

# calculate difference between dtp3 and ipvc
# flag differences
check7d <- compare_combination_2vax(data, v1, v2) %>% 
  filter(diff != 0)


check7d <- check7d %>%
  mutate(check = "Cross-check IPVC and DTP3 (combination)") %>%
  select(check, everything())

# add info on vaccine
check7d_2 <- check7d %>%
  left_join(temp, by =c('ISOCountryCode' = 'iso3c'))

# filter for the DTP-IPV countries
check7d <- check7d %>%
  filter(ISOCountryCode %in% comb_dtp3_ipvc_list)


## clean table ----
fin_check07d <- check7d %>%
  select(ISOCountryCode, diff) %>%
  mutate(ch7d_diff_dtp3_ipvc = case_when(diff > 0 ~ paste0('Combination IPVC != DTP3 (',diff,'pp higher)'),
                                         diff < 0 ~ paste0('Combination IPVC != DTP3 (',abs(diff),'pp lower)'),
                                         TRUE ~ NA_character_)) %>%
  filter(!is.na(ch7d_diff_dtp3_ipvc)) %>%
  select(-diff)
