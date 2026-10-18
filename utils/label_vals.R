label_vals <- function(df, col_to_label, new_col_name) {
  
  col_to_label_quo <- enquo(col_to_label) 
  
  df %>% 
    mutate(
      !!ensym(new_col_name) := case_when(
        abs(!!col_to_label_quo) < 100 ~ "<100",
        abs(!!col_to_label_quo) < 200 ~ "<200",
        abs(!!col_to_label_quo) < 500 ~ "<500",
        abs(!!col_to_label_quo) < 1000 ~ "<1,000",
        abs(!!col_to_label_quo) >= 1000 ~ as.character(format(round(!!col_to_label_quo, -3), big.mark = ",")),
        TRUE ~ NA_character_
        ) %>% 
        str_squish()
      )
}