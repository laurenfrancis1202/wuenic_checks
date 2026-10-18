## check 8: cross-check => MCV1, MENGA ----
# flag where there are differences >10, >15
v1 <- "mcv1"
v2 <- "menga"
check8 <- compare_2vax_b(data, v1, v2)  %>%
  mutate(check = "Cross-check MCV1 and MENGA") %>%
  select(check, everything())


## clean table ----
fin_check08 <- check8 %>%
  select(ISOCountryCode, diff) %>%
  mutate(ch8_diff_mcv1_menga = case_when(abs(diff) > 15 ~ paste0('>15pp diff between MCV1 and MENGA (',diff,'pp)'),
                                         abs(diff) > 10 ~ paste0('>10pp diff between MCV1 and MENGA (',diff,'pp)'),
                                         abs(diff) > 5 ~ paste0('>5pp diff between MCV1 and MENGA (',diff,'pp)'),
                                         TRUE ~ NA_character_)) %>%
  select(-diff)