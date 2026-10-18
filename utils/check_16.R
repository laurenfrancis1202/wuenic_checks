## check 16 :: MENGA > MCV1 but admin MENGA <= MCV1 ----
check16 <- data %>% 
  filter(Vaccine %in% c("mcv1", "menga"), 
         Year == rev_yr) %>%
  select(Country, ISOCountryCode, Year, Vaccine, WUENIC, AdministrativeCoverage) %>%
  pivot_wider(., names_from = Vaccine, values_from = c(WUENIC, AdministrativeCoverage)) %>%
  filter(!is.na(WUENIC_menga)) %>%
  mutate(flag = case_when(WUENIC_menga > WUENIC_mcv1 & AdministrativeCoverage_menga <= AdministrativeCoverage_mcv1 ~ 1,
                          TRUE ~ 0)) %>%
  filter(flag == 1) %>%
  # additional columnes
  mutate(check = "MENGA > MCV1 but admin MENGA <= MCV1") %>%
  select(check, ISOCountryCode, Year, 4:7, flag) %>%
  mutate(flag = "MENGA > MCV1 but admin MENGA <= MCV1",
         diff = WUENIC_menga - WUENIC_mcv1,
         flag = str_glue("{flag} (+{diff}%)")) %>%
  select(-diff)


## clean table ----
fin_check16 <- check16 %>%
  select(ISOCountryCode, ch16_wuenic_admin_disagree_mcv1_menga = flag) %>%
  distinct() 