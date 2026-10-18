## check 6: cross-check => MCV1, YFV ----
# flag where there are differences >10%
v1 <- "mcv1"
v2 <- "yfv"
check6 <- compare_2vax_b(data, v1, v2) %>%
  mutate(check = "Cross-check MCV1 and YFV") %>%
  select(check, everything()) 


## clean table ----
fin_check06 <- check6 %>%
  select(ISOCountryCode, diff) %>%
  mutate(ch6_diff_mcv1_yfv = case_when(abs(diff) > 15 ~ paste0('>15pp diff between MCV1 and YFV (',diff,'pp)'),
                                       abs(diff) > 10 ~ paste0('>10pp diff between MCV1 and YFV (',diff,'pp)'),
                                       abs(diff) > 5 ~ paste0('>5pp diff between MCV1 and YFV (',diff,'pp)'),
                                       TRUE ~ NA_character_)) %>%
  select(-diff)
