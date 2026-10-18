## check 15: RCV1 != MCV1 ----
comment_rcv <- data_full %>% 
  filter(Vaccine == "rcv1") %>%
  select(ISOCountryCode, Year, Comment)

check15 <- data %>%
  filter(Vaccine %in% c("mcv1","rcv1")) %>% 
  select(ISOCountryCode, Year, Vaccine, WUENIC) %>%
  group_by(ISOCountryCode, Year) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count > 1) %>%
  select(-count) %>%
  pivot_wider(., names_from = Vaccine, values_from = WUENIC) %>%
  filter(mcv1 != rcv1)%>%
  left_join(., comment_rcv) %>%
  # remove instances we know of
  filter(!(grepl("First dose of rubella vaccine given with second dose|Rubella containing vaccine introduced|Rubella-containing vaccine was introduced|partially introduced|introduced during|introduced in|based on estimated MCV2| RCV was administered with MCV2", Comment))) %>%
  filter(!(ISOCountryCode == "idn" & Year == 2017)) %>%
  filter(!(ISOCountryCode == "chn" & Year <= 2015),
         !(ISOCountryCode == "pse" & Year <= 2010)) %>%
  mutate(flag = 1)


## clean table ----
fin_check15 <- check15 %>%
  mutate(flag = str_glue("RCV1 ({rcv1}) != MCV1 ({mcv1}), {Year}")) %>%
  select(ISOCountryCode, ch15_mcv1_ne_rcv1 = flag) %>%
  distinct()
