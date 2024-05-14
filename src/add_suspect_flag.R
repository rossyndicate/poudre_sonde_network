#' @title Add flags to the `flag` column of a dataframe based on large swaths of suspect data.
#' 
#' @description
#' "24hr anomaly flag" is added if more than 50% of the data points in a 24 hour window are flagged.
#' "anomaly window" flag is added if the point is included in a 24hr anomaly.
#' 
#' @param df A data frame with a `flag` column.
#' 
#' @return A data frame with a `flag` column that has been updated with the relevant calculated seasonal range flags.
#' 
#' @examples
#' add_range_flags(df = all_data_flagged$`archery-Actual Conductivity`)
#' add_range_flags(df = all_data_flagged$`boxelder-Temperature`)
#' 
#' @seealso [flag_all_data()]
 
add_suspect_flag <- function(df) {

  flag_string <- "sonde not employed|missing data|site visit|sv window|suspect data" # include the flag added by this function

  # Define a function to check if a given 3-hour window has >= 50% fails
  check_3_hour_window_fail <- function(x) {
    sum(x) / length(x) >= 0.5
  }

  df_test <- df %>%
    dplyr::mutate(flag_binary = ifelse((is.na(flag) | grepl(flag_string, flag)), 0, 1)) %>%
    #arrange(timestamp) %>%
    dplyr::mutate(over_50_percent_fail_window = ifelse(is.na(over_50_percent_fail_window),
                                                zoo::rollapply(flag_binary, width = 12, FUN = check_3_hour_window_fail, fill = NA, align = "right"),
                                                over_50_percent_fail_window)) %>%
    add_flag(over_50_percent_fail_window == TRUE, "suspect data")

  return(df_test)

}
