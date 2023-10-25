# Add a verification column to the data frame and automatically fail data points that meet certain criteria.
# @param df A dataframe with a `flag` column.
# @return A dataframe with a `verification` column.
# @examples
# add_verification_column(df = all_data_flagged$`archery-Actual Conductivity`)
# add_verification_column(df = all_data_flagged$`boxelder-Temperature`)

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
