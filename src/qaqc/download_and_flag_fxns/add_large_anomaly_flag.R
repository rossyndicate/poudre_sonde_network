# Add flags to the `flag` column of a dataframe based on large swaths of suspect data.
# "24hr anomaly flag" is added if more than 50% of the data points in a 24 hour window are flagged.
# "anomaly window" flag is added if the point is included in a 24hr anomaly.
# @param df A dataframe with a `flag` column.
# @return A dataframe with a `flag` column that has been updated with the relevant large anomaly flags.
# @examples
# add_large_anomaly_flags(df = all_data_flagged$`archery-Actual Conductivity`)
# add_large_anomaly_flags(df = all_data_flagged$`boxelder-Temperature`)

add_large_anomaly_flag <- function(df) {

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
  flag_string <- "^(24hr anomaly|anomaly window|24hr anomaly;\\nanomaly window|
                  missing data|sonde not employed;\\nmissing data|
                  missing data;\\nanomaly window|missing data;\\n24hr anomaly|
                  sonde not employed;\nmissing data;\nanomaly window|
                  missing data;\n24hr anomaly;\nanomaly window|
                  site visit;\nmissing data;\n24hr anomaly;\nanomaly window|
                  sv window;\nmissing data;\n24hr anomaly;\nanomaly window)$"

  df <- df %>%
    mutate(flag_binary = ifelse((is.na(flag) | str_detect(flag, flag_string)), 0, 1),
           roll_bin = data.table::frollsum(flag_binary, n = 97, align = 'center', na.rm = F, fill = NA_real_)) %>%
    add_flag((roll_bin >= (97*0.5)), "24hr anomaly") # we can tweak the lower limit

  for (i in 1:48) {
    df <- df %>%
      add_flag((!str_detect(flag, "anomaly window") & lag(str_detect(flag, "24hr anomaly"), n = i)), "anomaly window") %>%
      add_flag((!str_detect(flag, "anomaly window") & lead(str_detect(flag, "24hr anomaly"), n = i)), "anomaly window")
  }
  return(df)
}
