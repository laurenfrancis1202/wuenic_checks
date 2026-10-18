## check 5: cross-check => DTP3, MCV1 ----
# flag where mcv1 > dtp3 by >10pp
v1 <- "mcv1"
v2 <- "dtp3"
check5 <- compare_2vax_a(data, v1, v2) %>%
  mutate(check = "Cross-check DTP3 and MCV1") %>%
  select(check, everything()) 


## clean table ----
fin_check05 <- check5 %>%
  select(ISOCountryCode, diff) %>%
  mutate(ch5_diff_mcv1_dtp3 = case_when(diff > 0 ~ paste0('MCV1 > DTP3 (',diff,'pp)'),
                                        TRUE ~ NA_character_)) %>%
  select(-diff)