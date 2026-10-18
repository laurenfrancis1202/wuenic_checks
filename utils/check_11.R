## check 11: dtp3 increased and pcvc decreased ----
# flag changes >10%
check11 <- data %>%
  filter(Vaccine %in% c('dtp3', 'pcvc', 'rotac'),
         # filter(Vaccine %in% c('dtp3', 'pcvc'),
         Year %in% c(rev_yr-1, rev_yr)) %>%
  select(Country, ISOCountryCode, Year, Vaccine, WUENIC) %>%
  pivot_wider(., names_from = Year, values_from = WUENIC) %>%
  mutate(diff = .[[5]] - .[[4]],
         diff_abs = abs(diff)) %>%
  # filter where absolute difference > 2%
  filter(diff_abs > 10) %>%
  mutate(change =  case_when(diff > 0 ~ 'increase',
                             diff < 0 ~ 'decrease', 
                             diff == 0 ~ 'same',
                             TRUE ~ NA_character_)) %>%
  filter(change != 'same') %>%
  arrange(ISOCountryCode, Vaccine) %>%
  group_by(ISOCountryCode) %>%
  mutate(change_cnt = n_distinct(change)) %>%
  ungroup() %>%
  filter(change_cnt > 1) %>%
  mutate(change_desc = paste0(Vaccine, ' ', change, ' (', diff, ');'))

# check11_2 <- check11 %>%
#   select(ISOCountryCode, change, change_desc) %>%
#   pivot_wider(., names_from = change, values_from = change_desc) %>%
#   mutate(increase = gsub('c\\(', '', increase)) %>%
#   mutate(decrease = gsub('c\\(', '', decrease)) %>%
#   mutate(increase = gsub('\\"\\)', '', increase)) %>%
#   mutate(decrease = gsub('\\"\\)', '', decrease)) %>%
#   mutate(increase = gsub('\\"', '', increase)) %>%
#   mutate(decrease = gsub('\\"', '', decrease)) %>%
#   mutate(increase = gsub('\\,', '', increase)) %>%
#   mutate(decrease = gsub('\\,', '', decrease)) %>%
#   mutate(change_desc = paste0(increase, ' | ', decrease))  %>%


if (nrow(check11 > 0)) {
check11_2 <- check11 %>%
  select(ISOCountryCode, change, change_desc) %>%
  pivot_wider(names_from = change, values_from = change_desc) %>%
  # if either increase or decrease column exists, clean it up
  mutate(
    across(
      any_of(c("increase", "decrease")),
      ~ gsub('c\\(|"\\)|"|,', '', .x)
    )
  ) %>%
  # if either increase or decrease colunm exists, make a summary column
  mutate(
    change_desc = {
      has_inc <- "increase" %in% names(.)
      has_dec <- "decrease" %in% names(.)
      
      if (has_inc && has_dec) {
        paste0(.data[["increase"]], " | ", .data[["decrease"]])
      } else if (has_inc) {
        .data[["increase"]]
      } else if (has_dec) {
        .data[["decrease"]]
      }
    }
  ) %>%
  mutate(check = str_glue("DTP3 increased between {rev_yr-1} and {rev_yr} and other 3rd doses decreased etc")) %>%
  select(check, everything())


# clean table 
fin_check11 <- check11_2 %>%
  select(ISOCountryCode, ch11_increase_decrease = change_desc)
}