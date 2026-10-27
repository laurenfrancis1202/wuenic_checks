## check 7c: cross-check => DTP1, IPV1 (DTP-IPV combination using countries) ----
# schedule / combination vaccines
temp <- schedules %>%
  filter(vaccinecode %in% dtp_ipv_vaccine_list,
         schedulerounds == 1) %>%
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

# dtp1 and ipv1 should be the same in countries that administer dtp-ipv in combination (usually hexa)
v1 <- "ipv1"
v2 <- "dtp1"

# calculate difference between dtp3 and ipvc
check7c <- compare_combination_2vax(data, v1, v2) %>% 
  filter(diff != 0)

# filter for the DTP-IPV countries (so this works for all countries as well as a single country)
check7c <- check7c %>%
  filter(ISOCountryCode %in% comb_dtp1_ipv1_list) %>%
  mutate(check = "Cross-check IPV1 and DTP1 (combination)") %>%
  select(check, everything())

# add info on vaccine
check7c_2 <- check7c %>%
  left_join(temp, by =c('ISOCountryCode' = 'iso3c'))


## clean table ----
fin_check07c <- check7c %>%
  select(ISOCountryCode, diff) %>%
  mutate(ch7c_diff_dtp1_ipv1 = case_when(diff > 0 ~ paste0('Combination IPV1 != DTP1 (',diff,'pp higher)'),
                                         diff < 0 ~ paste0('Combination IPV1 != DTP1 (',abs(diff),'pp lower)'),
                                         TRUE ~ NA_character_)) %>%
  filter(!is.na(ch7c_diff_dtp1_ipv1)) %>%
  select(-diff)
