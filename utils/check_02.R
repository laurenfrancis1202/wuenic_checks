## check 2: compare coverage between current and previous revision ----
check2 <- data %>%
  filter(Vaccine %in% c('dtp3')) %>%
  select(ISOCountryCode, Country, Vaccine, Year, WUENIC, WUENICPreviousRevision) %>%
  mutate(diff = WUENIC - WUENICPreviousRevision,
         diff_abs = abs(diff)) %>%
  arrange(desc(diff_abs)) %>%
  filter(diff_abs > 5)  %>%
  mutate(check = "Coverage in current vs previous revision") %>%
  select(check, everything())


## clean table ----
fin_check02 <- check2 %>%
  mutate(ch2 = case_when(diff_abs > 15 ~ paste0('>15pp diff (', diff, 'pp) | ', Vaccine, ' ', Year),
                         diff_abs > 10 ~ paste0('>10pp diff (', diff, 'pp) | ', Vaccine, ' ', Year),
                         diff_abs > 5 ~ paste0('>5pp diff (', diff, 'pp) | ', Vaccine, ' ', Year),
                         TRUE ~ NA_character_)) %>%
  select(ISOCountryCode, ch2_dtp3_rev1_vs_rev2 = ch2)