## check 4: cross-check => pcvc, Pol3, RotaC against DTP3 [vaccines with > 2 years of data] ----
# flag difference of >10%
vaccines <- c('dtp3','pcvc','rotac')
min_ndp  <- 2  # minimum number of data points RotaC and PCVc need in order to check as it's common that introduction period coverage is lower

check4 <- data %>%
  group_by(ISOCountryCode, Vaccine) %>%
  mutate(ndp = n_distinct(Year)) %>%
  ungroup() %>%
  filter(ndp > min_ndp) %>%
  select(-ndp) %>%
  filter(Vaccine %in% vaccines, Year == rev_yr) %>%
  select(Country, ISOCountryCode, Year, Vaccine, WUENIC) %>%
  pivot_wider(names_from = Vaccine, values_from = WUENIC) %>%
  # ensure missing vaccine columns exist
  { 
    df <- .
    for (v in vaccines) {
      if (!v %in% names(df)) df[[v]] <- NA_real_
    }
    df
  } %>%
  # calculate differences safely
  mutate(
    diff_dtp3_pcvc  = dtp3 - coalesce(pcvc, dtp3),
    diff_dtp3_rotac = dtp3 - coalesce(rotac, dtp3),
    diff_dtp_gt10   = as.integer(abs(diff_dtp3_pcvc) > 10 |
                                   abs(diff_dtp3_rotac) > 10),
    diff_dtp_gt15   = as.integer(abs(diff_dtp3_pcvc) > 15 |
                                   abs(diff_dtp3_rotac) > 15)
  ) %>%
  arrange(desc(diff_dtp_gt10), desc(diff_dtp_gt15)) %>%
  filter(diff_dtp_gt10 == 1) %>%
  mutate(check = "Cross-check PCVc and RotaC against DTP3 (when PCVc or RotaC are not new)") %>%
  select(check, everything())



## clean table ----
fin_check04 <- check4 %>%
  select(ISOCountryCode, diff_dtp3_pcvc, 
         diff_dtp3_rotac) %>%
  mutate(ch4_diff_dtp3_pcvc = case_when(abs(diff_dtp3_pcvc) > 15 ~ paste0('>15pp diff (', diff_dtp3_pcvc,'pp)'),
                                        abs(diff_dtp3_pcvc) > 10 ~ paste0('>10pp diff (', diff_dtp3_pcvc,'pp)'),
                                        abs(diff_dtp3_pcvc) > 5 ~ paste0('>5pp diff (', diff_dtp3_pcvc,'pp)'),
                                        TRUE ~ NA_character_),
         ch4_diff_dtp3_rotac = case_when(abs(diff_dtp3_rotac) > 15 ~ paste0('>15pp diff (', diff_dtp3_rotac,'pp)'),
                                         abs(diff_dtp3_rotac) > 10 ~ paste0('>10pp diff (', diff_dtp3_rotac,'pp)'),
                                         abs(diff_dtp3_rotac) > 5 ~ paste0('>5pp diff (', diff_dtp3_rotac,'pp)'),
                                         TRUE ~ NA_character_)) %>%
  select(-c(diff_dtp3_pcvc:diff_dtp3_rotac))
