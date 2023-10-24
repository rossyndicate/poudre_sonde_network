# add pass/fail column and apply this to every df
add_verification_column <- function(df) {
  df <- df %>%
    mutate(verification = case_when(
      # are there other conditions where a data point should automatically fail?
      # str_detect(flag, "outside sd range") ~ "fail",
      str_detect(flag, "missing data") ~ "fail",
      # need to add more situations where data points fail
      is.na(flag) ~ "pass",
      TRUE ~ NA))
  return(df)
}
