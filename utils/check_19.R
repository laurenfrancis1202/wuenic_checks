## check 19: survey shows discrepancies between co-administered antigens while admin coverage is similar ----
# For each co-administered pair, flag country-years where:
#   1) both vaccines have a survey estimate and admin coverage is within ch19_admin_tol pp
#   2) the survey adjustment factors (CF = survey - admin) of the two vaccines differ by > ch19_cf_tol pp
#   3) WUENIC equals the survey for both vaccines (survey anchor year), and
#   4) WUENIC follows the admin trend: both vaccines carry a calibration rule ("C:")
#      for >= ch19_min_cal_yrs consecutive years immediately before OR after that year
#   5) (optional, ch19_same_visit = TRUE) the two vaccines are given at the same age
#      in the country's WIISE schedule (`schedules` object from the main script)
# Pairs: 1) DTP3, PCVC, ROTAC (all three pairings)  2) DTP1, IPV1

# parameters
ch19_admin_tol    <- 2     # admin coverage of the two vaccines within this many pp
ch19_cf_tol       <- 2     # flag where |CF vaccine 2 - CF vaccine 1| is greater than this
ch19_equal_tol    <- 0.5   # WUENIC counts as "= survey" within this many pp
ch19_min_cal_yrs  <- 2     # consecutive calibrated years either side of the survey year
ch19_cal_rule     <- "C:"  # rule prefix for calibrated estimates
ch19_years        <- NULL  # NULL = all years; e.g. c(rev_yr - 1, rev_yr) to restrict the flags
ch19_same_visit   <- FALSE  # TRUE = only pairs given at the same age in the schedule;
# FALSE = compare all pairs (visit column still shows the schedule result)

ch19_pairs <- tribble(
  ~v1,    ~v2,
  "dtp3", "pcvc",
  "dtp3", "rotac",
  "dtp3", "ipv1",
  "pcvc", "rotac",
  "dtp1", "ipv1"
)

# ages at which each vaccine is given, per country (from `schedules`) ----
# `schedules` = WIISE schedule, one row per vaccine code and dose (latest schedule
# year per country). The same schedule is applied to every year.
#   dtp1 / dtp3 : dose 1 / dose 3 of any DTP-containing code
#   ipv1        : dose 1 of any IPV-containing code (standalone IPV or combination)
#   pcvc, rotac : last dose (<= 3) of the PCV / rotavirus series
# Dose numbers are taken across codes (a country can use different DTP-containing
# codes for different doses), not per code.
ch19_ages <- schedules %>%
  mutate(
    ISO             = tolower(iso3c),
    ageadministered = str_squish(toupper(ageadministered))
  ) %>%
  filter(!is.na(ageadministered), ageadministered != "",
         !grepl("Y", ageadministered),     # primary-series ages in weeks / months only
         schedulerounds <= 3) %>%
  group_by(ISO) %>%
  filter(year == max(year)) %>%             # latest schedule per country
  ungroup() %>%
  group_by(ISO, vaccinecode) %>%
  mutate(last_round = schedulerounds == max(schedulerounds)) %>%
  ungroup() %>%
  mutate(
    is_dtp  = grepl("^DT", vaccinecode),
    is_ipv  = grepl("IPV", vaccinecode),
    is_pcv  = grepl("PCV", vaccinecode),
    is_rota = grepl("ROTA", vaccinecode)
  ) %>%
  {bind_rows(
    filter(., is_dtp,  schedulerounds == 1) %>% mutate(vaccine = "dtp1"),
    filter(., is_dtp,  schedulerounds == 3) %>% mutate(vaccine = "dtp3"),
    filter(., is_ipv,  schedulerounds == 1) %>% mutate(vaccine = "ipv1"),
    filter(., is_pcv,  last_round)          %>% mutate(vaccine = "pcvc"),
    filter(., is_rota, last_round)          %>% mutate(vaccine = "rotac")
  )} %>%
  distinct(ISOCountryCode = ISO, vaccine, ageadministered)

# per country: are the two vaccines given at the same age?
ch19_visit_fn <- function(v1, v2) {
  a <- ch19_ages %>% filter(vaccine == v1)
  b <- ch19_ages %>% filter(vaccine == v2)
  same <- inner_join(a, b, by = c("ISOCountryCode", "ageadministered"))$ISOCountryCode
  ages_txt <- function(d, nm) d %>% group_by(ISOCountryCode) %>%
    summarise(!!nm := paste(sort(unique(ageadministered)), collapse = "/"), .groups = "drop")
  tibble(ISOCountryCode = intersect(a$ISOCountryCode, b$ISOCountryCode)) %>%
    mutate(visit = if_else(ISOCountryCode %in% same, "Same visit", "Different visits")) %>%
    left_join(ages_txt(a, "age1"), by = "ISOCountryCode") %>%
    left_join(ages_txt(b, "age2"), by = "ISOCountryCode")
}

# find the admin / survey / rule columns in the master (names vary by file)
ch19_find_col <- function(pattern) {
  hit <- grep(pattern, names(data), ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0) stop("check 19: no column in `data` matches '", pattern, "'. ",
                             "Set ch19_col_* below to the right column name.")
  hit[1]
}
ch19_col_admin  <- ch19_find_col("^admin")
ch19_col_survey <- ch19_find_col("^survey")
ch19_col_rule   <- ch19_find_col("^rule")

# prep data
ch19_base <- data %>%
  filter(Vaccine %in% unique(c(ch19_pairs$v1, ch19_pairs$v2))) %>%
  transmute(
    Country, ISOCountryCode, Year, Vaccine,
    admin  = as.numeric(.data[[ch19_col_admin]]),
    survey = as.numeric(.data[[ch19_col_survey]]),
    wuenic = as.numeric(WUENIC),
    rule   = as.character(.data[[ch19_col_rule]])
  ) %>%
  distinct()

# one pair at a time: join the two vaccines by country-year and apply the rules
ch19_pair_fn <- function(v1, v2) {
  
  # vaccine 1
  a <- ch19_base %>% filter(Vaccine == v1) %>%
    select(Country, ISOCountryCode, Year, admin1 = admin, survey1 = survey, wuenic1 = wuenic, rule1 = rule)
  
  # vaccine 2
  b <- ch19_base %>% filter(Vaccine == v2) %>%
    select(ISOCountryCode, Year, admin2 = admin, survey2 = survey, wuenic2 = wuenic, rule2 = rule)
  
  # finds runs of consecutive calibrated years for one vaccine pair, and records how long each run is. The run length is used to test whether WUENIC follows admin on either side of a survey year.
  pr <- inner_join(a, b, by = c("ISOCountryCode", "Year")) %>%
    arrange(ISOCountryCode, Year) %>%
    group_by(ISOCountryCode) %>%
    mutate(
      # mark calibrated years (only includes when BOTH vaccines are calibrated that year, ie. rule C:, returns NA if both are not C:)
      calibrated = coalesce(startsWith(rule1, ch19_cal_rule) & startsWith(rule2, ch19_cal_rule), FALSE),
      # start a new run when something changes (it is TRUE ona country's first row or when calibrated differs from the previous row, or there's a gap in years)
      new_run    = row_number() == 1 | calibrated != lag(calibrated) | Year != lag(Year) + 1,
      run_id     = cumsum(new_run)
    ) %>%
    # count the years in each run
    group_by(ISOCountryCode, run_id) %>%
    mutate(run_len = if_else(calibrated, n(), 0L)) %>%
    ungroup()
  
  # length of the calibrated run ending the year before / starting the year after
  pr %>%
    left_join(pr %>% transmute(ISOCountryCode, Year = Year + 1, cal_run_before = run_len),
              by = c("ISOCountryCode", "Year")) %>%
    left_join(pr %>% transmute(ISOCountryCode, Year = Year - 1, cal_run_after = run_len),
              by = c("ISOCountryCode", "Year")) %>%
    mutate(
      cal_run_before = coalesce(cal_run_before, 0L),
      cal_run_after  = coalesce(cal_run_after, 0L),
      vaccine_pair   = paste(v1, "vs", v2),
      admin_diff     = admin1 - admin2,
      cf1            = survey1 - admin1,
      cf2            = survey2 - admin2,
      cf_diff        = cf2 - cf1,
      admin_similar  = !is.na(admin1) & !is.na(admin2) & abs(admin_diff) <= ch19_admin_tol,
      cf_diverges    = !is.na(cf_diff) & abs(cf_diff) > ch19_cf_tol,
      wuenic_is_svy  = !is.na(wuenic1) & !is.na(wuenic2) & !is.na(survey1) & !is.na(survey2) &
        abs(wuenic1 - survey1) <= ch19_equal_tol & abs(wuenic2 - survey2) <= ch19_equal_tol,
      follows_admin  = pmax(cal_run_before, cal_run_after) >= ch19_min_cal_yrs
    ) %>%
    left_join(ch19_visit_fn(v1, v2), by = "ISOCountryCode") %>%
    mutate(visit    = coalesce(visit, "Schedule unknown"),
           schedule = if_else(visit == "Schedule unknown", "schedule unknown",
                              as.character(str_glue("{v1} {age1}, {v2} {age2} = {str_to_lower(visit)}")))) %>%
    filter(admin_similar, cf_diverges, wuenic_is_svy, follows_admin,
           !ch19_same_visit | visit == "Same visit")
}

check19 <- purrr::pmap_dfr(ch19_pairs, ch19_pair_fn)

if (!is.null(ch19_years)) check19 <- check19 %>% filter(Year %in% ch19_years)

check19 <- check19 %>%
  mutate(
    result = as.character(str_glue(
      "{vaccine_pair} {Year} (admin diff = {admin_diff}; survey {survey1}/{survey2}; CF diff = {cf_diff}; given: {schedule})"
    ))
  ) %>%
  select(Country, ISOCountryCode, Year, vaccine_pair,
         admin1, admin2, admin_diff, survey1, survey2, cf1, cf2, cf_diff,
         wuenic1, wuenic2, rule1, rule2, cal_run_before, cal_run_after, visit, schedule, result) %>%
  arrange(ISOCountryCode, vaccine_pair, Year)

if (nrow(check19) > 0) {
  
  # export tab: one row per country - vaccine pair - year
  check19_2 <- check19
  
  # clean table (joined into the summary): one row per country - vaccine pair - year
  fin_check19 <- check19 %>%
    transmute(ISOCountryCode, ch19_cf_diverge = result)
}