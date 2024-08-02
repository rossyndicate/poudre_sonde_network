#' @title Add flags to the `flag` column of a dataframe based on large swaths of suspect data.
#' @description
#' "24hr anomaly flag" is added if more than 50% of the data points in a 24 hour window are flagged.
#' "anomaly window" flag is added if the point is included in a 24hr anomaly.
#' @param df A data frame with a `flag` column.
#' @return A data frame with a `flag` column that has been updated with the relevant calculated seasonal range flags.
#' @examples
#' add_range_flags(df = all_data_flagged$`archery-Actual Conductivity`)
#' add_range_flags(df = all_data_flagged$`boxelder-Temperature`)


add_suspect_flag <- function(df) {

  # these are the flags that we don't want to perform this exercise across
  auto_cleaned_flag_string <- "sonde not employed|missing data|site visit|sv window|sonde burial|sensor biofouling|depth calibration malfunction|sensor malfunction"

  # Define a function to check if a given 2-hour window has >= 50% fails
  check_3_hour_window_fail <- function(x) {
    sum(x) / length(x) >= 0.5
  }

  df_test <- df %>%
    #add_column_if_not_exists(column_name = "auto_cleaned_flag_binary", default_value = 0) %>%
    dplyr::mutate(auto_cleaned_flag_binary = ifelse(is.na(auto_cleaned_flag) | grepl(auto_cleaned_flag_string, auto_cleaned_flag) | auto_cleaned_flag == "suspect data", 0, 1)) %>%
    dplyr::mutate(over_50_percent_fail_window_right = zoo::rollapply(auto_cleaned_flag_binary, width = 8, FUN = check_3_hour_window_fail, fill = NA, align = "right")) %>%
    dplyr::mutate(over_50_percent_fail_window_center = zoo::rollapply(auto_cleaned_flag_binary, width = 8, FUN = check_3_hour_window_fail, fill = NA, align = "center")) %>%
    #dplyr::mutate(over_50_percent_fail_window_left = zoo::rollapply(auto_cleaned_flag_binary, width = 8, FUN = check_3_hour_window_fail, fill = NA, align = "left")) %>%
    dplyr::mutate(auto_cleaned_flag = as.character(ifelse(is.na(auto_cleaned_flag) &
                                             (over_50_percent_fail_window_right == TRUE | over_50_percent_fail_window_center == TRUE),
                                              "suspect data", auto_cleaned_flag)))

  return(df_test)
}


