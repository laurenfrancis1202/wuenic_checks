## check 7: cross-check => DTP1, IPV1 (DTP-IPV combination using countries) ----
# identify countries who administer DTP-IPV
sch_dtp_ipv <- schedules %>%
  filter(vaccinecode %in% c('DTAPHIBHEPBIPV','DTAPHIBIPV','DTAPIPV','DTAPHEPBIPV','DTWPHIBHEPBIPV'),
         schedulerounds == 1) %>%
  pull(iso3c) %>%
  unique()

# dtp1 and ipv1 should be similar in some countries, particularly in countries that administer dtp-ipv in combination
v1 <- "dtp1"
v2 <- "ipv1"

check7 <- compare_2vax_b(data, v1, v2) %>%
  mutate(check = "Cross-check DTP1 and IPV1") %>%
  select(check, everything())

# any countries with IPV1 > DTP1
check7_2 <- check7 %>%
  mutate(flag = case_when(ISOCountryCode %in% sch_dtp_ipv ~ "DTP-IPV combination vaccine",
                          TRUE ~ "NOT DTP-IPV combination vaccine"))

# filter for the DTP-IPV countries
check7 <- check7 %>%
  filter(ISOCountryCode %in% sch_dtp_ipv)


## clean table ----
fin_check07 <- check7 %>%
  select(ISOCountryCode, diff) %>%
  mutate(ch7_diff_dtp1_ipv1 = case_when(abs(diff) > 15 ~ paste0('>15pp diff between DTP1 and IPV1 (',diff,'pp)'),
                                        abs(diff) > 10 ~ paste0('>10pp diff between DTP1 and IPV1 (',diff,'pp)'),
                                        abs(diff) > 5 ~ paste0('>5pp diff between DTP1 and IPV1 (',diff,'pp)'),
                                        diff < 0 ~ paste0('DTP1 less than IPV1 (',diff,'pp)'),
                                        TRUE ~ NA_character_)) %>%
  filter(!is.na(ch7_diff_dtp1_ipv1)) %>%
  select(-diff)