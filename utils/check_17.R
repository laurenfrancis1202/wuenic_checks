## check 17 :: YFV > MCV1 but admin YFV <= MCV1 ----
check17 <- data %>% 
  filter(Vaccine %in% c("mcv1", "yfv"), 
         Year == rev_yr) %>%
  select(Country, ISOCountryCode, Year, Vaccine, WUENIC, AdministrativeCoverage) %>%
  pivot_wider(., names_from = Vaccine, values_from = c(WUENIC, AdministrativeCoverage)) %>%
  filter(!is.na(WUENIC_yfv)) %>%
  mutate(flag = case_when(WUENIC_yfv > WUENIC_mcv1 & AdministrativeCoverage_yfv <= AdministrativeCoverage_mcv1 ~ 1,
                          TRUE ~ 0)) %>%
  filter(flag == 1) %>%
  # additional columnes
  mutate(check = "YFV > MCV1 but admin YFV <= MCV1") %>%
  select(check, ISOCountryCode, Year, 4:7, flag) %>%
  mutate(flag = "YFV > MCV1 but admin YFV <= MCV1",
         diff = WUENIC_yfv - WUENIC_mcv1,
         flag = str_glue("{flag} (+{diff}%)")) %>%
  select(-diff)


## clean table ----
fin_check17 <- check17 %>%
  select(ISOCountryCode, ch17_wuenic_admin_disagree_mcv1_yfv = flag) %>%
  distinct() 