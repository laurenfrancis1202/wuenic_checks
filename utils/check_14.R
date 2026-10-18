## check 14: flag where MCV2 > MCV1 [MCV1 and MCV2 given in 2YL] ----
# mcv2 schedule
sch_mcv2 <- schedules %>%
  filter(vaccinecode %in% c("MEASLES","MMR","MR")) %>%
  # filter for second dose administered in second year of life
  filter(schedulerounds == 2) %>%
  filter(!grepl("Y", ageadministered)) %>%
  mutate(iso3c = tolower(iso3c)) %>%
  select(ISOCountryCode = iso3c, ageadministered)

# mcv1 schedule
sch_mcv1 <- schedules %>%
  filter(vaccinecode %in% c("MEASLES","MMR","MR")) %>%
  # filter for first dose administered in second year of life (from M12)
  filter(schedulerounds == 1) %>%
  filter(grepl("M1", ageadministered)) %>%
  mutate(iso3c = tolower(iso3c)) %>%
  select(ISOCountryCode = iso3c, ageadministered)

# countries in both lists
iso_both <- intersect(sch_mcv1$ISOCountryCode, sch_mcv2$ISOCountryCode)

check14 <- data %>%
  # filter for countries where MCV1 and MCV2 are given in 2YL
  filter(
    # ISOCountryCode %in% iso_both,
    Vaccine %in% c('mcv1','mcv2'),
    Year == max(Year)
    ) %>%
  select(ISOCountryCode, Year, Vaccine, WUENIC) %>%
  arrange(ISOCountryCode, Year, Vaccine) %>%
  pivot_wider(., names_from = Vaccine, values_from = WUENIC) %>%
  mutate(flag_yr = case_when(mcv2 > mcv1 ~ as.character(Year), TRUE ~ NA_character_),
         flag_diff = ifelse(!is.na(flag_yr), mcv2 - mcv1, NA_real_),
         flag = ifelse(!is.na(flag_yr), paste0(flag_yr, ' (', as.character(flag_diff), 'pp higher)'), NA_character_)) %>%
  filter(!is.na(flag_yr)) %>%
  # mutate(check = "MCV2 > MCV1 (MCV1 administered 2YL MCV2 administered Y02)") %>%
  mutate(check = "MCV2 > MCV1 (MCV2 administered Y02)") %>%
  select(check, everything()) %>%
  # filter for second dose administered in second year of life
  left_join(., sch_mcv2) %>%
  filter(!is.na(ageadministered)) %>%
  rename(mcv2_ageadministered = ageadministered)


## clean table ----
fin_check14 <- check14 %>%
  select(ISOCountryCode, ch14_mcv1_gt_mcv2 = flag) %>%
  distinct()
