library(tidyverse)
library(readxl)
library(openxlsx)
library(here)
library(janitor) # clean_names()
library(writexl)
library(purrr)
library(stringr)

# Purpose:  Data checks to be run at the end of the draft and final estimates
# Author:   Lauren Francis
# Created:  11 December 2023
# Updates:  
#          - 17/04/26: Added cross-check of combination DTP1-IPV1 and DTP3-IPVC
#          - 11/05/26: Added cross-check of where admin coverage is the same but WUENIC differs
#          - 15/05/26: Can run on the full data without having to export single country files
#          - 10/08/26: Updated check 10 to be the reverse of check 18, using admin coverage rather than doses to perform the check

# requires the wuenic-master file

# hardcode vars
rev_yr         <- 2025
latest_master  <- "2026-06-25"
type           <- "dummy"         # draft, final
check_type     <- "full"       # full or country
remove_ok      <- "no"           # yes, no

# set file paths
USERNAME     <- Sys.getenv("USERNAME")
wd           <- str_glue("C:/Users/{USERNAME}/OneDrive - UNICEF/Immunization")
utils        <- str_glue("wuenic-data-checks/utils")
masterfolder <- str_glue("wuenic_master/{type}")        # contains wuenic_master/iso3c_outputs
exportfolder <- str_glue("wuenic-data-checks")          # to export checks


## input ----
if (check_type == "full") {
  file_path <- str_glue("{masterfolder}/wuenic-master_{rev_yr}rev_{latest_master}.csv")
  wuenic_master <- read.csv(file_path) 
  
} else {
  folder_path <- str_glue("{masterfolder}")  # folder path
  # Read and combine all files
  csv_files <- list.files(path = folder_path, pattern = "_output\\.csv$", full.names = TRUE) # list all CSV files ending with _output.csv
  wuenic_master <- csv_files %>%
    map_df(read.csv)
}

# # date master/country file was produced
# production_date <- unique(wuenic_master$ProductionDate)

# region ref
reg_ref_master <- read_csv(str_glue('{wd}/reference_docs/region_ref/regional-groups_{rev_yr + 1}-release.csv')) %>%
  mutate(iso3c = tolower(iso3c)) %>%
  filter(wuenic == 'wuenic')


## country codes ----
reg_ref   <- reg_ref_master %>% select(iso3c)  # ref df
iso_codes <- unique(wuenic_master$ISOCountryCode) %>% sort()  # list
# iso_codes <- c("zmb")


## funcs ----
# function a: compare two vaccines and flags where one vaccine is greater than the other by 10% or more (5% or more if a gavi country)
compare_2vax_a <- function(df, v1, v2) {
  df %>% 
    filter(Vaccine %in% c(v1, v2),  # use the vaccine in quotations marks
           Year == rev_yr) %>%
    select(Country, ISOCountryCode, Year, Vaccine, WUENIC) %>%
    pivot_wider(., names_from = Vaccine, values_from = WUENIC) %>%
    mutate(diff = !!sym(v1) - !!sym(v2),   # remove quotation marks from vaccine names
           !!paste0(v1, "_gt_", v2) := case_when(diff > 0 ~ 1, TRUE ~ 0)) %>%
    arrange(desc(diff)) %>%
    # filter(diff >= 10)
    # flag rows that to filter for
    mutate(filter = case_when(ISOCountryCode %in% gavi_list & diff >= 5 ~ 1,
                              diff >= 10 ~ 1, 
                              TRUE ~ 0)) %>%
    filter(filter == 1)
}

# function b: compare two vaccines and flags where there are differences of >10pp and >15pp (>5pp for gavi countries)
compare_2vax_b <- function(df, v1, v2) {
  df %>% 
    filter(Vaccine %in% c(v1, v2), 
           Year == rev_yr) %>%
    select(Country, ISOCountryCode, Year, Vaccine, WUENIC) %>%
    pivot_wider(., names_from = Vaccine, values_from = WUENIC) %>%
    mutate(diff = !!sym(v1) - !!sym(v2),
           diff_gt5 = case_when(ISOCountryCode %in% gavi_list & abs(diff) > 5 ~ 1, TRUE ~ 0),
           diff_gt10 = case_when(abs(diff) > 10 ~ 1, TRUE ~ 0),
           diff_gt15 = case_when(abs(diff) > 15 ~ 1, TRUE ~ 0)) %>%
    filter(diff_gt5 == 1 | diff_gt10 == 1 | diff_gt15 == 1) %>%
    arrange(desc(abs(diff)))
  
}

# function: compare two antigens that are part of a combination vaccine (eg. hexa) 
compare_combination_2vax <- function(df, v1, v2) {
  df %>% 
    filter(Vaccine %in% c(v1, v2), 
           Year == rev_yr) %>%
    select(Country, ISOCountryCode, Year, Vaccine, WUENIC) %>%
    pivot_wider(., names_from = Vaccine, values_from = WUENIC) %>%
    mutate(diff = !!sym(v1) - !!sym(v2)) 
}

# identify if a specific vaccine exists in the data frame
v_exists <- function(v) {
  as.integer(
    data_full %>% 
      filter(Vaccine == v) %>% 
      nrow() > 0
  )
}


## WIISE Mart setup ----
source(str_glue("{utils}/wiise-funcs.R"))  #Read in the wiise API function
tkn <- get_token(tkn_type = "prod")
base_url <- "https://extranet.who.int/xmart-api/odata/WIISE/"


## grab WIISE vaccine schedules -----
odta_qry <- str_glue("?$filter=(YEAR ge {rev_yr-4})")

wiise_dta <- fetch_wiise(
  url = base_url, 
  tbl = "AD_SCHEDULES", 
  qry = odta_qry, 
  token = tkn
) %>% 
  .[['value']] %>% 
  as_tibble() %>%
  clean_names()

schedules <- wiise_dta %>%
  filter(geoarea == 'NATIONAL',
         is.na(targetpop)) %>%
  select(iso3c = country, year, vaccinecode, schedulerounds, ageadministered, sourcecomment) %>%
  # filter for schedule in latest available year
  group_by(iso3c) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(iso3c = tolower(iso3c)) %>%
  inner_join(., reg_ref) 


## country lists ----
# opv-using countries
opv_list <- reg_ref_master %>%
  select(iso3c, opv_using) %>%
  filter(!is.na(opv_using)) %>%
  pull(iso3c) %>%
  sort()

# list of dtp-ipv combination vaccines
dtp_ipv_vaccine_list <- c(
  'DTAPHEPBIPV',
  'DTAPHIBHEPBIPV',
  'DTAPHIBIPV',
  'DTPHIBHEPBIPV',
  'DTPHIBHEPIPV',
  'DTPHIBIPV',
  'DTWPHEPBIPV',
  'DTWPHIBHEPIPV',
  'DTWPHIBIPV',
  # 'DTAPIPV',
  # 'DTWPIPV',
  'DTWPHIBHEPBIPV'
)

# dtp1-ipv1 combination countries
comb_dtp1_ipv1_list <- schedules %>%
  filter(vaccinecode %in% dtp_ipv_vaccine_list,
         schedulerounds == 1) %>%
  pull(iso3c) %>%
  unique() %>%
  sort()

# dtp3-ipvc combination countries
comb_dtp3_ipvc_list <- schedules %>%
  filter(vaccinecode %in% dtp_ipv_vaccine_list,
         schedulerounds == 3) %>%
  pull(iso3c) %>%
  unique() %>%
  sort()

# gavi countries
gavi_list <- reg_ref_master %>%
  select(iso3c, gavi) %>%
  filter(gavi == "gavi57") %>%
  pull(iso3c) %>%
  sort()


## start loop ----
# In full mode, run the checks once using the full master file.
# In country mode, run the checks separately for each country and export one file per country.
loop_ids <- if (check_type == "full") "0ALL" else iso_codes

for(x in loop_ids) {
  
  if (check_type == "full") {
    data_full <- wuenic_master
  } else {
    data_full <- wuenic_master %>%
      filter(ISOCountryCode == x)
  }
  
  # date master/country file was produced
  production_date <- unique(data_full$ProductionDate) %>%
    sort() %>%
    paste(collapse = "; ")
  
  
  ## vaccine exists flag ----
  # flag for if specific vaccines exist in df (1/0)
  yfv_exists    <- v_exists("yfv")
  ipv1_exists   <- v_exists("ipv1")
  ipvc_exists   <- v_exists("ipvc")
  mcv2_exists   <- v_exists("mcv2")
  menga_exists  <- v_exists("menga")
  rcv1_exists   <- v_exists("rcv1")
  rotac_exists  <- v_exists("rotac")
  
  
  ## check 0: duplicates ----
  # identify if there is more than one row for country-vaccine-year
  dups <- data_full %>%
    mutate(check = "Duplicates") %>%
    select(check, ISOCountryCode, Vaccine, Year, WUENIC) %>%
    group_by(ISOCountryCode, Vaccine, Year) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    filter(count > 1)
  
  # remove dups
  data <- data_full %>% select(-c(Comment)) %>% distinct()
  
  ## check 1: compare country-vaccine estimates, current yr vs previous yr [vaccines with > 2 years of data] ----
  source(str_glue("{utils}/check_01.r"))
  
  ## check 2: compare coverage between current and previous revision ----
  source(str_glue("{utils}/check_02.r"))
  
  ## check 3: cross-check => DTP3, Hib3, HepB3 ----
  source(str_glue("{utils}/check_03.r"))
  
  ## check 4: cross-check => PCVc and RotaC against DTP3 [vaccines with > 2 years of data] ----
  # flag difference of >10%
  source(str_glue("{utils}/check_04.r"))
  
  ## check 5: cross-check => DTP3, MCV1 ----
  # flag where mcv1 > dtp3 by >10pp
  source(str_glue("{utils}/check_05.r"))
  
  ## check 6: cross-check => MCV1, YFV ----
  # flag where there are differences >10%
  if (yfv_exists == 1) {
    source(str_glue("{utils}/check_06.r"))
  }
  

  ## check 7b: cross-check => DTP3, IPV1 (OPV-using countries) ----
  # dtp3 and ipv1 should be similar in some countries, particularly in countries that administer OPV
  if (ipv1_exists == 1) {
    source(str_glue("{utils}/check_07b.r"))
  }
  
  ## check 7c: cross-check => DTP1, IPV1 (DTP-IPV combination using countries) ----
  # dtp1 and ipv1 should be the same in countries that administer hexa
  if (ipv1_exists == 1 & x %in% comb_dtp1_ipv1_list) {
    source(str_glue("{utils}/check_07c.r"))
  }
  
  ## check 7d: cross-check => DTP3, IPVC (DTP-IPV combination using countries) ----
  # dtp3 and ipvc should be the same in countries that administer hexa
  if (ipvc_exists == 1  & x %in% comb_dtp3_ipvc_list) {
    source(str_glue("{utils}/check_07d.r"))
  }
  
  
  
  ## check 8: cross-check => MCV1, MENGA ----
  if (menga_exists == 1) {
    # flag where there are differences >10, >15
    source(str_glue("{utils}/check_08.r"))
  }
  
  ## check 9: compare admin denoms current yr vs prev yr ----
  # flag changes in target population of >10%
  source(str_glue("{utils}/check_09.r"))
  
  ## check 10 :: WUENIC is the same, but admin coverage differs ----
  source(str_glue("{utils}/check_10.r"))
  
  ## check 11: dtp3 increased and pcvc decreased ----
  # flag changes >10%
  source(str_glue("{utils}/check_11.r"))
  
  ## check 12: flag where DTP3 > DTP1 ----
  source(str_glue("{utils}/check_12.r"))
  
  ## check 13: flag where IPVC > IPV1 ----
  if (ipvc_exists == 1) {
    source(str_glue("{utils}/check_13.r"))
  }
  
  ## check 14: flag where MCV2 > MCV1 [rev_yr] ----
  if (mcv2_exists == 1) {
    source(str_glue("{utils}/check_14.r"))
  }
  
  ## check 15: RCV1 != MCV1 ----
  if (rcv1_exists == 1) {
    source(str_glue("{utils}/check_15.r"))
  }
  
  ## check 16 :: MENGA > MCV1 but admin MENGA <= MCV1 ----
  if (menga_exists == 1) {
    source(str_glue("{utils}/check_16.r"))
  }
  
  ## check 17 :: YFV > MCV1 but admin YFV <= MCV1 ----
  if (yfv_exists == 1) {
    source(str_glue("{utils}/check_17.r"))
  }
  
  
  ## check 18 :: admin coverage is the same, but WUENIC differs ----
  source(str_glue("{utils}/check_18.r"))
  
  
  ## check 50: compare draft and final wuenic estimates (final only, flag any differences) ----
  # compare estimates
  # flag new vaccines since draft
  if (type == "final") {
    source(str_glue("{utils}/check_50.r"))
  }
  
  
  ## checks summary flags ----
  # join
  countries <- data %>% select(ISOCountryCode) %>% distinct() %>% arrange(ISOCountryCode)
  
  fin_check <- ls(pattern = "^fin_check[0-9]+") %>%
    keep(exists) %>%                 # only fin_check dfs that exist
    map(get) %>%
    keep(~ nrow(.x) > 0) %>%
    reduce(left_join, .init = countries) %>%
    distinct()
  
  if (type == "final" && exists("fin_check50") && nrow(fin_check50) > 0) {
    fin_check <- fin_check %>%
      left_join(fin_check50, by = "ISOCountryCode") %>%
      distinct()
  }
  
  
  ## finalise table ----
  # Check whether there are any actual flag values.
  # Do not use ncol(fin_check) because, in full mode, country columns mean ncol() > 1 even when there are no flags.
  flag_cols <- setdiff(names(fin_check), "ISOCountryCode")
  has_check_flags <- length(flag_cols) > 0 && fin_check %>%
    select(all_of(flag_cols)) %>%
    summarise(across(everything(), ~ any(!is.na(.x)))) %>%
    unlist(use.names = FALSE) %>%
    any()
  has_dup_flags <- nrow(dups) > 0
  has_flags <- has_check_flags || has_dup_flags
  
  # long version
  if (has_flags) {
    if (has_check_flags) {
      fin_check_long <- fin_check %>%
        pivot_longer(-c(ISOCountryCode), names_to = "check", values_to = "result") %>%
        filter(!is.na(result)) %>%
        mutate(check_desc = case_when(grepl("ch1_", check) ~ str_glue("Vaccine coverage in {rev_yr} vs {rev_yr-1} (>10% diff, current revision)"),
                                      grepl("ch2_", check) ~ str_glue("Coverage in current vs previous revision (change >10%)"),
                                      grepl("ch3_", check) ~ str_glue("Cross-check DTP3, Hib3 and HepB3"),
                                      grepl("ch4_", check) ~ str_glue("Cross-check PCVC and RotaC against DTP3 (differences >10%)"),
                                      grepl("ch5_", check) ~ str_glue("Cross-check DTP3 and MCV1 (differences >10%)"),
                                      grepl("ch6_", check) ~ str_glue("Cross-check MCV1 and YFV (differences >10%)"),
                                      grepl("ch7_", check) ~ str_glue("Cross-check DTP1 and IPV1 (differences >10%)"),
                                      grepl("ch7b_", check) ~ str_glue("Cross-check DTP3 and IPV1 (OPV countries) (differences >10%)"),
                                      grepl("ch7c_", check) ~ str_glue("Cross-check combination DTP1 and IPV1"),
                                      grepl("ch7d_", check) ~ str_glue("Cross-check combination DTP3 and IPVC"),
                                      grepl("ch8_", check) ~ str_glue("Cross-check MCV1 and MENGA (differences >10%)"),
                                      grepl("ch9_", check) ~ str_glue("Admin denominator in {rev_yr} vs {rev_yr-1} (change >10%)"),
                                      grepl("ch10", check) ~ str_glue("Same WUENIC but admin coverage differs"),
                                      grepl("ch11_", check) ~ str_glue("DTP3 increased between {rev_yr-1} and {rev_yr} and other 3rd doses decreased etc (changes >10%)"),
                                      grepl("ch12_", check) ~ str_glue("DTP3 > DTP1"),
                                      grepl("ch13_", check) ~ str_glue("IPVC > IPV1"),
                                      grepl("ch14_", check) ~ str_glue("MCV2 > MCV1 (MCV2 administered Y02)"),
                                      grepl("ch15_", check) ~ str_glue("RCV1 != MCV1"),
                                      grepl("ch16_", check) ~ str_glue("WUENIC and admin disagree mcv1 menga"),
                                      grepl("ch17_", check) ~ str_glue("WUENIC and admin disagree mcv1 yfv"),
                                      grepl("ch18_", check) ~ str_glue("Same admin coverage but WUENIC differs"),
                                      grepl("ch50_", check) ~ str_glue("Draft and final differ"))) %>%
        select(ISOCountryCode, check_desc, check, result) %>%
        distinct()
    } else {
      fin_check_long <- tibble(
        ISOCountryCode = character(),
        check_desc = character(),
        check = character(),
        result = character()
      )
    }
    
    
    # clean up table ::
    # produce vaccine column
    fin_check_long <- fin_check_long %>%
      # extract the string before the first "_" to split into check number and vaccine
      separate(check, into = c("check_number", "vaccine"), sep = "_", remove = FALSE) %>%
      # clean vaccine column
      mutate(vaccine = case_when(check_number %in% c("ch1","ch50") ~ vaccine, 
                                 check_number == "ch15" ~ "mcv1 and rcv1",
                                 check_number == "ch3" ~ str_remove(check, "^[^_]*_[^_]*_"),
                                 TRUE ~ NA_character_)
             ) %>%
      mutate(
        vaccine = case_when(
          check_number == "ch3" ~ gsub("_", ", ", vaccine),
          TRUE ~ vaccine
        )
      ) %>%
      select(ISOCountryCode, check_number, check_desc, vaccine, result, check) 
    
    
    # further clean up vaccine column
    fin_check_long <- fin_check_long %>%
      # extract the string (ie. vaccines) after "diff_"
      mutate(vax = str_extract(check, "(?<=diff_).*$")) %>%
      mutate(
        vaccine = ifelse(check_number == "ch4", vax, vaccine),
        vaccine = gsub("_", " vs ", vaccine),
        vax = str_extract(result, "(?<=\\|).*$"),
        vaccine = ifelse(check_number %in% c("ch2","ch9"), vax, vaccine),
        # extract the vaccines for ch10 and ch18 from the result
        vaccine = ifelse(check_number %in% c("ch10", "ch18"), str_extract(result, "(?<=for ).*(?= \\()"), vaccine),
        result = ifelse(check_number!="ch11", str_remove(result, "\\|.*"), result)
      ) %>%
      select(-c(vax, check))
    
    # add a result detail column
    fin_check_long2 <- fin_check_long %>%
      mutate(
        vaccine = str_trim(vaccine, side = "left"),
        
        result_detail = case_when(
          check_number %in% c("ch11", "ch15", "ch10", "ch18") ~ result,
          TRUE ~ str_remove_all(str_extract(result, "\\(.*?\\)"), "[()]")
        ),
        
        abs_diff = str_match(result_detail, "abs diff\\s*=\\s*(-?\\d+\\.?\\d*)")[, 2],
        wuenic_diff = str_match(result_detail, "WUENIC diff\\s*=\\s*(-?\\d+\\.?\\d*)")[, 2],

        result = case_when(
          check_number %in% c("ch10", "ch18") & !is.na(abs_diff) ~ paste0("abs diff = ", abs_diff),
          check_number == "ch18" & !is.na(wuenic_diff) ~ paste0("WUENIC diff = ", wuenic_diff),
          check_number == "ch10" ~ paste0("Admin diff = ", abs_diff),
          check_number %in% c("ch10", "ch18") ~ result,
          check_number %in% c("ch11", "ch15") ~ NA_character_,
          TRUE ~ str_replace(result, "\\(.*?\\)", "")
        ),
        
        result_detail = case_when(
          check_number %in% c("ch10", "ch18") ~ str_replace(result_detail, "for .*?(?= \\()", ""),
          TRUE ~ result_detail
        )
      ) %>%
      select(-abs_diff, -wuenic_diff)
    
    
    # # remove the MCV2 > MCV1 check for now
    # fin_check_long <- fin_check_long %>%
    #   filter(check_number != "ch14") %>%
    #   distinct()
    
    # add check for iso-vaccine-year-dups
    dups_final <- dups %>%
      mutate(check_number = "ch0",
             check_desc = "Duplicates (iso-vaccine-year)",
             vaccine = Vaccine,
             result = "Duplicates",
             result_detail = paste0(ISOCountryCode,"-",Vaccine,"-",Year,": ", WUENIC,"%")) %>%
      select(ISOCountryCode, check_number, check_desc, vaccine, result, result_detail) %>%
      distinct()
    
    fin_check_long <- rbind(fin_check_long, dups_final) %>%
      arrange(ISOCountryCode, check_number) %>%
      left_join(., reg_ref_master[c("iso3c", "country", "gavi")], by = c("ISOCountryCode"="iso3c")) %>%
      mutate(flags = "flags") %>%
      select(gavi, ISOCountryCode, country, flags, everything())
    
    # In full mode, keep countries with no flags in the summary.
    # Without this, pivot_longer() + filter(!is.na(result)) drops all no-flag countries.
    no_flag_countries <- countries %>%
      filter(!ISOCountryCode %in% fin_check_long$ISOCountryCode) %>%
      mutate(flags = "no flags") %>%
      left_join(reg_ref_master[c("iso3c", "country", "gavi")], by = c("ISOCountryCode" = "iso3c")) %>%
      select(gavi, ISOCountryCode, country, flags)
    
    fin_check_long <- bind_rows(fin_check_long, no_flag_countries) %>%
      arrange(ISOCountryCode, flags, check_number)
    
  } else {
    fin_check_long <- fin_check %>% mutate(flags = "no flags") %>%
      left_join(., reg_ref_master[c("iso3c", "country", "gavi")], by = c("ISOCountryCode"="iso3c")) %>%
      select(gavi, ISOCountryCode, country, flags)
  }
  
  # add production date so can filter for the latest countries run if needed
  fin_check_long <- fin_check_long %>%
    mutate(production_date = production_date) %>%
    select(production_date, everything())
  
  
  # for final, join on notes from previous checks to avoid duplication of efforts assessing checks
  if (type == "final") {
    draft_checks <- read_excel(str_glue("{exportfolder}/draft/01_wuenic_checks_{rev_yr}rev_draft_0ALL.xlsx")) %>%
      select(ISOCountryCode, check_number, check_desc, vaccine, comment, comment_detail)
    
    fin_check_long <- left_join(fin_check_long, draft_checks) %>% distinct() %>%
      mutate(comment = ifelse(flags == "no flags", "ok", comment))
  }
  
  # remove dup rows
  fin_check_long <- fin_check_long %>% distinct()
  
  
  # prep export ----
  # make empty dfs for the ones that don't exist
  checks <- c(
    "check6", 
    # "check7_2", 
    "check7b", "check7c_2","check7d_2","check8", "check10",
    # "check10_3", 
    "check11_2", "check13", "check14", "check15", "check16", "check17", "check18"
    # "fin_check10_3"
  )
  
  for (chk in checks) {
    if (!exists(chk)) {
      assign(chk, data.frame())
    }
  }
  
  # list dfs to export
  export <- list(
    'summary' = fin_check_long,
    'ch0' = dups,
    'ch1' = check1,
    'ch2' = check2,
    'ch3' = check3c,
    'ch4' = check4,
    'ch5' = check5,
    'ch6' = check6,
    # 'ch7' = check7_2,
    'ch7b' = check7b,
    'ch7c' = check7c_2,
    'ch7d' = check7d_2,
    'ch8' = check8,
    'ch9' = check9,
    'ch10' = check10,
    'ch11' = check11_2,
    'ch12' = check12,
    'ch13' = check13,
    'ch14' = check14,
    'ch15' = check15,
    'ch16' = check16,
    'ch17' = check17,
    'ch18' = check18
  )
  
  
  ## export :: country-specific [formatted .xlsx] ----
  # Only export individual country workbooks when running country checks.
  if (check_type == "country") {
    
    wb <- createWorkbook()  # create a workbook
    
    # Define a header style: light blue fill and bold font
    header_style <- createStyle(fontColour = "#000000", fgFill = "#D9E1F2", halign = "left", textDecoration = "bold")
    
    # Loop through each element in the export list and add a worksheet
    for (sheet_name in names(export)) {
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet = sheet_name, x = export[[sheet_name]], headerStyle = header_style, withFilter = TRUE)
      
      freezePane(wb, sheet = sheet_name, firstRow = TRUE) # freeze first row
    }
    
    # Add final elements if type is "final"
    if (type == "final") {
      if (exists("check50")) {
        addWorksheet(wb, "ch50")
        writeData(wb, "ch50", check50, headerStyle = header_style, withFilter = TRUE)
        freezePane(wb, "ch50", firstRow = TRUE)
      }
      
      if (exists("new_since_draft")) {
        addWorksheet(wb, "new_vax_since_draft")
        writeData(wb, "new_vax_since_draft", new_since_draft, headerStyle = header_style, withFilter = TRUE)
        freezePane(wb, "new_vax_since_draft", firstRow = TRUE)
      }
    }
    
    # Save workbook
    saveWorkbook(wb, str_glue("{exportfolder}/{type}/01_wuenic_checks_{rev_yr}rev_{type}_{x}.xlsx"), overwrite = TRUE)
    
  }
}


# save df with country-specific results
final_result <- fin_check_long
name_use <- paste0("final_result_", x, sep="")
assign(name_use, final_result, envir = .GlobalEnv)

# # remove objects with "check" in the name before the next country loop.
# Keep final objects needed for the final workbook tabs.
objects_to_remove <- ls(pattern = "check")
if (type == "final") {
  objects_to_remove <- setdiff(objects_to_remove, c("check50", "fin_check50"))
  
  final_result_all <- fin_check_long
}
rm(list = objects_to_remove)

# } else if (type == "final") {
#   final_result_all <- fin_check_long
# }
## end loop ----

## export :: all countries [formatted .xlsx] ----
# append all country files
# Get all objects starting with "final_result_" and combine into one df
dfs_to_bind <- mget(ls(pattern = "^final_result_"))
large_df <- bind_rows(dfs_to_bind) %>% distinct()

# if there is an existing outputs, join on the comment about the dq check
if (type == "draft") {
  file_path2 <- str_glue("wuenic-data-checks/draft/01_wuenic_checks_{rev_yr}rev_draft_0ALL.xlsx")
  
  if (file.exists(file_path2)) {
    
    temp <- read_excel(file_path2)
    # filter(comment == "ok")
    
    large_df <- large_df %>%
      left_join(
        temp[c("gavi","ISOCountryCode", "check_number","check_desc", "vaccine", "result_detail", "comment", "comment_detail")],
        by = c(
          # "production_date", # do not include because when we re-run all countries, the comment column will not join as production date will change
          "gavi","ISOCountryCode", "check_number","check_desc", "vaccine", "result_detail")
      )
    
    # if it doesn't exist, add comment columns
  } else {
    large_df <- large_df %>%
      mutate(comment = NA_character_,
             comment_detail = NA_character_)
  }
  
  # if there are no flags, put ok in the comment
  large_df <- large_df %>% mutate(comment = ifelse(flags == "no flags", "ok", comment)) %>%
    distinct()
  
  # if a file already existed, export it as _v0
  write.xlsx(temp, str_glue("{exportfolder}/{type}/01_wuenic_checks_{rev_yr}rev_{type}_0ALL_v0.xlsx"))
}






# create workbook
wb <- createWorkbook()

# Define a header style: light blue fill and bold font
header_style <- createStyle(fontColour = "#000000", fgFill = "#D9E1F2", halign = "left", textDecoration = "bold")

# Add main combined sheet
addWorksheet(wb, "all_countries")
writeData(wb, sheet = "all_countries", x = large_df, headerStyle = header_style, withFilter = TRUE)
freezePane(wb, sheet = "all_countries", firstRow = TRUE)

# Add final elements if type is "final"
if (type == "final") {
  if (exists("check50")) {
    addWorksheet(wb, "ch50")
    writeData(wb, "ch50", check50, headerStyle = header_style, withFilter = TRUE)
    freezePane(wb, "ch50", firstRow = TRUE)
  }
  
  if (exists("new_since_draft")) {
    addWorksheet(wb, "new_vax_since_draft")
    writeData(wb, "new_vax_since_draft", new_since_draft, headerStyle = header_style, withFilter = TRUE)
    freezePane(wb, "new_vax_since_draft", firstRow = TRUE)
  }
}

# Save workbook
saveWorkbook(wb, str_glue("{exportfolder}/{type}/01_wuenic_checks_{rev_yr}rev_{type}_0ALL.xlsx"), overwrite = TRUE)




## EXTRA ----
# see what vaccines are missing (ie. no estimates) by country
temp2 <- wuenic_master %>% select(ISOCountryCode, Vaccine) %>% distinct()
# uniquev <- unique(temp2$Vaccine)
uniquev <- c("bcg","dtp1","dtp3","hepb3","hepbb","hib3","ipv1","ipvc","mcv1","mcv2","menga","pcvc","rcv1","rotac","yfv")
all_combinations <- expand.grid(
  ISOCountryCode = unique(temp2$ISOCountryCode),
  Vaccine = uniquev
)
# find which combinations are missing in df
missing_vaccines <- anti_join(all_combinations, temp2, by = c("ISOCountryCode", "Vaccine"))
# total number of countries missing each vaccine
missing_vaccines2 <- missing_vaccines %>% group_by(Vaccine) %>% summarise(n = n())