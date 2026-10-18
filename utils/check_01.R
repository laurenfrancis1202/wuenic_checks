## check 1: compare country-vaccine estimates, current yr vs previous yr [vaccines with > 2 years of data] ----
min_ndp  <- 2  # minimum number of data points. It is common that vaccines just introduced will have large increases in intro period

check1 <- data %>%
  # count number of years each vaccine has data
  group_by(ISOCountryCode, Vaccine) %>%
  mutate(ndp = n_distinct(Year)) %>%
  ungroup() %>%
  # filter for vaccines with more than min_ndp datapoints
  filter(ndp > min_ndp) %>%
  select(-ndp) %>%
  filter(Year %in% c(rev_yr-1, rev_yr)
         # Vaccine %in% c('dtp3', 'mcv1')   # JUNE2026 : Remove filter so it runs for all vaccines
         ) %>%
  select(Country, ISOCountryCode:WUENIC) %>%
  arrange(ISOCountryCode, Year) %>%
  pivot_wider(., names_from = Year, values_from = WUENIC) %>%
  # latest year val - prev year val => col5 - col4
  mutate(diff = .[[5]] - .[[4]],  
         absolute_diff = abs(diff)) %>%  
  mutate(diff_gt10 = ifelse(absolute_diff > 10, 1, 0),
         diff_gt15 = ifelse(absolute_diff > 15, 1, 0)) %>%
  filter(diff_gt10 == 1) %>%
  mutate(check = str_glue("Vaccine coverage in {rev_yr} vs {rev_yr-1} (current revision)")) %>%
  select(check, everything())


## clean table ----
fin_check01 <- check1 %>%
  # filter(Vaccine=='dtp3') %>%
  select(ISOCountryCode, Vaccine, diff) %>%
  mutate(ch1= case_when(abs(diff) > 15 ~ paste0('>15pp diff (', diff, 'pp)'),
                        abs(diff) > 10 ~ paste0('>10pp diff (', diff, 'pp)'),
                        abs(diff) > 5 ~ paste0('>5pp diff (', diff, 'pp)')),
         Vaccine = paste0('ch1_', Vaccine)) %>%
  select(-diff) %>%
  pivot_wider(., names_from = Vaccine, values_from = ch1)
