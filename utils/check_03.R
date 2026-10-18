## check 3: cross-check => DTP3, Hib3, HepB3 ----
# identify countries who administer DTP-HIB-HEPB3
sch_dtp_hib_hepb <- schedules %>%
  filter(vaccinecode %in% c('DTAPHIBHEPBIPV','DTWPHIBHEPB','DTAPHIBIPV',
                            'DTAPHIB','DTAPHIBHEPB','DTAPHEPBIPV',
                            'DTWPHIB','DTWPHIBHEPBIPV','DTWPHEPB'),
         schedulerounds == 3) %>%
  select(ISOCountryCode = iso3c, schedule = vaccinecode, schedulerounds) %>%
  distinct() 


vaccines <- c('dtp3','hepb3','hib3')

# cross-check wuenic
check3 <- data %>%
  filter(Vaccine %in% c('dtp3','hepb3','hib3'),
         Year == rev_yr) %>%
  select(Country, ISOCountryCode, Year, Vaccine, WUENIC) %>%
  pivot_wider(., names_from = Vaccine, values_from = WUENIC) %>%
  # ensure missing vaccine columns exist
  { 
    df <- .
    for (v in vaccines) {
      if (!v %in% names(df)) df[[v]] <- NA_real_
    }
    df
  } %>%
  mutate(same = case_when(dtp3 == hepb3 & dtp3 == hib3 & hepb3 == hib3 ~ 1, TRUE ~ 0),
         both_differ = case_when(dtp3!=hib3 & dtp3!=hepb3 ~ 1, TRUE ~ 0),
         diff_dtp3_hepb3 = dtp3 - hepb3,
         diff_dtp3_hib3 = dtp3 - hib3,
         diff_hib3_hepb3 = hib3 - hepb3) %>%
  arrange(same) %>%
  filter(both_differ == 1)

# # cross-check wuenic
# check3 <- data %>%
#   filter(Vaccine %in% c('dtp3','hepb3','hib3'),
#          Year == rev_yr) %>%
#   select(Country, ISOCountryCode, Year, Vaccine, WUENIC) %>%
#   pivot_wider(., names_from = Vaccine, values_from = WUENIC) %>%
#   mutate(same = case_when(dtp3 == hepb3 & dtp3 == hib3 & hepb3 == hib3 ~ 1, TRUE ~ 0),
#          both_differ = case_when(dtp3!=hib3 & dtp3!=hepb3 ~ 1, TRUE ~ 0),
#          diff_dtp3_hepb3 = dtp3 - hepb3,
#          diff_dtp3_hib3 = dtp3 - hib3,
#          diff_hib3_hepb3 = hib3 - hepb3) %>%
#   arrange(same) %>%
#   filter(both_differ == 1)

# join on DTP-HIB-HEPB countries flag
check3 <- left_join(check3, sch_dtp_hib_hepb) %>%
  arrange(schedule) %>%
  filter(!is.na(schedule)) %>%
  mutate(check = "Cross-check DTP3, Hib3 and HepB3") %>%
  select(check, everything())


# cross-check country reported data
check3b <- data %>%
  filter(Vaccine %in% c('dtp3','hepb3','hib3'),
         Year == rev_yr) %>%
  select(Country, ISOCountryCode, Year, Vaccine, WUENIC, ReportedCoverage) %>%
  pivot_wider(., names_from = Vaccine, values_from = c(WUENIC, ReportedCoverage))

# check3c <- check3 %>%
#   left_join(., check3b[c('ISOCountryCode', 'ReportedCoverage_dtp3', 'ReportedCoverage_hepb3', 'ReportedCoverage_hib3')]) %>%
#   select(-same) %>%
#   mutate(hepb3_wuenic_reported_differ = case_when(hepb3 != ReportedCoverage_hepb3 ~ 1, TRUE ~ 0),
#          hib3_wuenic_reported_differ = case_when(hib3 != ReportedCoverage_hib3 ~ 1, TRUE ~ 0))
cols_to_join <- c(
  "ISOCountryCode",
  "ReportedCoverage_dtp3",
  "ReportedCoverage_hepb3",
  "ReportedCoverage_hib3"
)

check3c <- check3 %>%
  left_join(
    check3b %>% select(any_of(cols_to_join)),
    by = "ISOCountryCode"
  ) %>%
  select(-same) %>%
  mutate(
    hepb3_wuenic_reported_differ =
      if ("ReportedCoverage_hepb3" %in% names(.)) {
        as.integer(hepb3 != ReportedCoverage_hepb3)
      } else {
        NA_integer_
      },
    hib3_wuenic_reported_differ =
      if ("ReportedCoverage_hib3" %in% names(.)) {
        as.integer(hib3 != ReportedCoverage_hib3)
      } else {
        NA_integer_
      }
  )

## clean table ----
fin_check03 <- check3 %>%
  filter(same != 1) %>%
  select(ISOCountryCode, diff_dtp3_hepb3, diff_dtp3_hib3, diff_hib3_hepb3) %>%
  mutate(ch3_diff_dtp3_hepb3 = case_when(abs(diff_dtp3_hepb3) > 15 ~ paste0('>15pp diff (', diff_dtp3_hepb3,'pp)'),
                                         abs(diff_dtp3_hepb3) > 10 ~ paste0('>10pp diff (', diff_dtp3_hepb3,'pp)'),
                                         abs(diff_dtp3_hepb3) > 5 ~ paste0('>5pp diff (', diff_dtp3_hepb3,'pp)'),
                                         TRUE ~ '<5pp'),
         ch3_diff_dtp3_hib3 = case_when(abs(diff_dtp3_hib3) > 15 ~ paste0('>15pp diff (', diff_dtp3_hib3,'pp)'),
                                        abs(diff_dtp3_hib3) > 10 ~ paste0('>10pp diff (', diff_dtp3_hib3,'pp)'),
                                        abs(diff_dtp3_hib3) > 5 ~ paste0('>5pp diff (', diff_dtp3_hib3,'pp)'),
                                        TRUE ~ '<5pp'),
         ch3_diff_hib3_hepb3 = case_when(abs(diff_hib3_hepb3) > 15 ~ paste0('>15pp diff (', diff_hib3_hepb3,'pp)'),
                                         abs(diff_hib3_hepb3) > 10 ~ paste0('>10pp diff (', diff_hib3_hepb3,'pp)'),
                                         abs(diff_hib3_hepb3) > 5 ~ paste0('>5pp diff (', diff_hib3_hepb3,'pp)'),
                                         TRUE ~ '<5pp')) %>%
  select(-c(diff_dtp3_hepb3:diff_hib3_hepb3))