## check 18 :: admin coverage is the same, but WUENIC differs ----
# specify minimum difference you are interested in 
min_diff <- 3 

check18_long <- data %>%
  filter(
    Vaccine %in% c("dtp3", "pcvc", "rotac", "bcg", "hepbb", "mcv1", "menga", "yfv"),
    Year == rev_yr
  ) %>%
  mutate(
    vaccine_group = case_when(
      Vaccine %in% c("dtp3", "pcvc", "rotac") ~ "dtp3_pcvc_rotac",
      Vaccine %in% c("bcg", "hepbb") ~ "bcg_hepbb",
      Vaccine %in% c("mcv1", "menga", "yfv") ~ "mcv1_menga_yfv"
    )
  ) %>%
  select(
    Country,
    ISOCountryCode,
    Year,
    vaccine_group,
    Vaccine,
    WUENIC,
    admin_cvg = AdministrativeCoverage
  ) %>%
  filter(!is.na(admin_cvg), !is.na(WUENIC)) %>%
  arrange(ISOCountryCode, vaccine_group, admin_cvg, Vaccine)


if (nrow(check18_long) > 0) {
  
  check18 <- check18_long %>%
    group_by(ISOCountryCode, vaccine_group, admin_cvg) %>%
    filter(n_distinct(Vaccine) > 1) %>%
    mutate(
      min_wuenic = min(WUENIC, na.rm = TRUE),
      max_wuenic = max(WUENIC, na.rm = TRUE),
      diff_wuenic_abs = abs(max_wuenic - min_wuenic)
    ) %>%
    filter(diff_wuenic_abs >= min_diff) %>%
    summarise(
      vaccines = paste(Vaccine, collapse = ", "),
      wuenic_values = paste0(Vaccine, "=", WUENIC, collapse = ", "),
      diff_wuenic_abs = first(diff_wuenic_abs),
      .groups = "drop"
    ) %>%
    mutate(
      check = "For vaccines given at the same time: same admin cvg but different WUENIC"
    ) %>%
    select(check, everything())
  
  fin_check18 <- check18 %>%
    mutate(
      ch18_same_admin_diff_wuenic = paste0(
        "Same admin coverage (", admin_cvg,
        "), different WUENIC estimates for ",
        vaccines,
        " (", wuenic_values,
        "; abs diff=", diff_wuenic_abs, ")"
      )
    ) %>%
    select(ISOCountryCode, ch18_same_admin_diff_wuenic) %>%
    distinct()
}