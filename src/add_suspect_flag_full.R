# Add flags to the `flag` column of a dataframe based on large swaths of suspect data.
# "24hr anomaly flag" is added if more than 50% of the data points in a 24 hour window are flagged.
# "anomaly window" flag is added if the point is included in a 24hr anomaly.
# @param df A dataframe with a `flag` column.
# @return A dataframe with a `flag` column that has been updated with the relevant large anomaly flags.
# @examples
# add_large_anomaly_flags(df = all_data_flagged$`archery-Actual Conductivity`)
# add_large_anomaly_flags(df = all_data_flagged$`boxelder-Temperature`)

add_suspect_flag_full <- function(df) {

  flag_string <- "sonde not employed|missing data|site visit|sv window|suspect data"

  # Define a function to check if a given 3-hour window has >= 50% fails
  check_3_hour_window_fail <- function(x) {
    sum(x) / length(x) >= 0.5
  }

  df_test <- df %>%
    dplyr::mutate(flag_binary = ifelse((is.na(flag) | grepl(flag_string, flag)), 0, 1)) %>%
    #arrange(timestamp) %>%
    dplyr::mutate(over_50_percent_fail_window = zoo::rollapply(flag_binary, width = 12, FUN = check_3_hour_window_fail, fill = NA, align = "right")) %>%
    add_flag(over_50_percent_fail_window == TRUE & !grepl("suspect data", flag), "suspect data") # is this flagging the correct data?

  return(df_test)

}
