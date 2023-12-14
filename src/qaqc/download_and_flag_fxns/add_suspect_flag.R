# Add flags to the `flag` column of a dataframe based on large swaths of suspect data.
# "24hr anomaly flag" is added if more than 50% of the data points in a 24 hour window are flagged.
# "anomaly window" flag is added if the point is included in a 24hr anomaly.
# @param df A dataframe with a `flag` column.
# @return A dataframe with a `flag` column that has been updated with the relevant large anomaly flags.
# @examples
# add_large_anomaly_flags(df = all_data_flagged$`archery-Actual Conductivity`)
# add_large_anomaly_flags(df = all_data_flagged$`boxelder-Temperature`)

add_suspect_flag <- function(df) {

  # cases that should result in 0 in flag_binary column:
  # only "24hr anomaly"
  # only "anomaly window"
  # only "24hr anomaly" and "anomaly window"
  # only "missing data"
  # only "sonde not employed" and "missing data"
  # only "missing data" and "anomaly window"
  # only "missing data" and "24hr anomaly"
  # only "sonde not employed", "missing data", and "anomaly window"
  # only "site visit", "missing data", "24hr anomaly", and "anomaly window"
  # only "sv window", "missing data", "24hr anomaly", and "anomaly window"
  # flag_string <- "^(24hr anomaly|anomaly window|24hr anomaly;\\nanomaly window|
  #                 missing data|sonde not employed;\\nmissing data|
  #                 missing data;\\nanomaly window|missing data;\\n24hr anomaly|
  #                 sonde not employed;\nmissing data;\nanomaly window|
  #                 missing data;\n24hr anomaly;\nanomaly window|
  #                 site visit;\nmissing data;\n24hr anomaly;\nanomaly window|
  #                 sv window;\nmissing data;\n24hr anomaly;\nanomaly window)$"

  flag_string <- "sonde not employed|missing data|site visit|sv window"

  # Define a function to check if a given 6-hour window has >= 50% fails
  check_3_hour_window_fail <- function(x) {
    sum(x) / length(x) >= 0.4
  }

  df_test <- df %>%
    mutate(flag_binary = ifelse((is.na(flag) | grepl(flag_string, flag)), 0, 1)) %>%
    #arrange(timestamp) %>%
    mutate(over_50_percent_fail_window = zoo::rollapply(flag_binary, width = 12, FUN = check_3_hour_window_fail, fill = NA, align = "right")) %>%
    add_flag(over_50_percent_fail_window == TRUE, "suspect data") # is this flagging the correct data?

return(df_test)

}
