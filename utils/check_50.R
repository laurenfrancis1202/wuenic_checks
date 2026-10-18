## check 50: compare draft and final wuenic estimates (final only) ----
check50_draft <- read.csv(str_glue("{wd}/WUENIC Revision/{rev_yr} rev/wuenic_master/draft/wuenic-master_{rev_yr}rev.csv")) %>% 
  select(ISOCountryCode, Country, Vaccine, Year, WUENIC_draft = WUENIC, Rule_draft = Rule, Comment_draft = Comment)

check50_final <- read.csv(str_glue("{wd}/WUENIC Revision/{rev_yr} rev/wuenic_master/final/wuenic-master_{rev_yr}rev.csv")) %>% 
  select(ISOCountryCode, Country, Vaccine, Year, WUENIC_final = WUENIC, Rule_final = Rule, Comment_final = Comment)

check50 <- left_join(check50_draft, check50_final, by = c("ISOCountryCode", "Country", "Vaccine", "Year")) %>%
  mutate(difference = case_when(WUENIC_draft != WUENIC_final ~ 1,
                                TRUE ~ 0),
         diff = WUENIC_final - WUENIC_draft,
         diff_gt0 = case_when(abs(diff) > 0 ~ 1, TRUE ~ 0),
         diff_gt5 = case_when(abs(diff) > 5 ~ 1, TRUE ~ 0),
         diff_gt10 = case_when(abs(diff) > 10 ~ 1, TRUE ~ 0),
         diff_gt15 = case_when(abs(diff) > 15 ~ 1, TRUE ~ 0)) %>%
  filter(diff != 0 | diff_gt5 == 1 | diff_gt10 == 1 | diff_gt15 == 1) %>%
  arrange(desc(abs(diff))) %>%
  filter(
    diff_gt0 == 1,
         # Vaccine == "dtp3",
         Year == rev_yr)

# new vaccines since draft
new_since_draft <- anti_join(check50_final, check50_draft[c('Country','Vaccine','Year')])


## clean table ----
fin_check50 <- check50 %>%
  filter(
    # Vaccine == 'dtp3',
    Year == rev_yr) %>%
  select(ISOCountryCode, Vaccine, diff) %>%
  mutate(Vaccine = paste0('ch50_', Vaccine,'_final_vs_draft')) %>%
  mutate(ch1_diff_draft_final = case_when(abs(diff) > 15 ~ paste0('>15pp diff (', diff,'pp)'),
                                          abs(diff) > 10 ~ paste0('>10pp diff (', diff,'pp)'),
                                          abs(diff) > 5 ~ paste0('>5pp diff (', diff,'pp)'),
                                          TRUE ~ paste0('<=5pp diff (', diff,'pp)'))) %>%
  select(-diff) %>%
  pivot_wider(., names_from = Vaccine, values_from = ch1_diff_draft_final ) %>%
  distinct()