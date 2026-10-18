## check 10 :: WUENIC is the same, but admin coverage differs ----
# Specify minimum difference in admin coverage between antigens with the same wuenic you are interested in
min_diff <- 3

check10_long <- data %>%
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
  filter(
    !is.na(admin_cvg),
    !is.na(WUENIC)
  ) %>%
  arrange(
    ISOCountryCode,
    vaccine_group,
    WUENIC,
    Vaccine
  )


if (nrow(check10_long) > 0) {
  
  check10 <- check10_long %>%
    # Group vaccines given at the same time that have the same WUENIC estimate
    group_by(
      ISOCountryCode,
      vaccine_group,
      WUENIC
    ) %>%
    # Need at least two different vaccines for comparison
    filter(
      n_distinct(Vaccine) > 1
    ) %>%
    # Calculate the difference in administrative coverage
    mutate(
      min_admin = min(admin_cvg, na.rm = TRUE),
      max_admin = max(admin_cvg, na.rm = TRUE),
      diff_admin_abs = abs(max_admin - min_admin)
    ) %>%
    # Keep only groups where admin differs by at least the specified threshold
    filter(
      diff_admin_abs >= min_diff
    ) %>%
    summarise(
      vaccines = paste(Vaccine, collapse = ", "),
      admin_values = paste0(
        Vaccine,
        "=",
        admin_cvg,
        collapse = ", "
      ),
      diff_admin_abs = first(diff_admin_abs),
      .groups = "drop"
    ) %>%
    mutate(
      check = "For vaccines given at the same time: same WUENIC but different admin cvg"
    ) %>%
    select(
      check,
      everything()
    )
  
  
  fin_check10 <- check10 %>%
    mutate(
      ch10_same_wuenic_diff_admin = paste0(
        "Same WUENIC estimate (",
        WUENIC,
        "), different admin coverage for ",
        vaccines,
        " (",
        admin_values,
        "; abs diff=",
        diff_admin_abs,
        ")"
      )
    ) %>%
    select(
      ISOCountryCode,
      ch10_same_wuenic_diff_admin
    ) %>%
    distinct()
}