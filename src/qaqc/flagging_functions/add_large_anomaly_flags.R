# Adding flags regarding large swaths of suspect data.

# Takes a rolling window of data points (24 hours worth of data points), and if
# more than a quarter of that data has been flagged, the point in the center of that
# rolling window will be flagged with "24hr anomaly flag".

# After a point has been determined to fall in the center of a large data anomaly,
# it will check if that point itself is part of another 24hr anomaly.
# This is done by looking for "24hr anomaly flags" within the 48 points behind and ahead
# of it. If that flag is detected within that window, an "anomaly window" will also
# be added to that point.

# "24hr anomaly flag" and "anomaly window" flags do not count toward the total for the
# "24hr anomaly flag"

add_large_anomaly_flags <- function(df) {

  # should we not be counting flags that have only missing data and nothing else?
  # cases that should result in 0 in flag_binary column:
  # only "24hr anomaly"
  # only "anomaly window"
  # only "24hr anomaly" and "anomaly window"
  # only "missing data"
  # only "sonde not employed" and "missing data"
  # only "missing data" and "anomaly window"
  # only "missing data" and "24hr anomaly"
  flag_string <- "^(24hr anomaly|anomaly window|24hr anomaly;\\nanomaly window|
                  missing data|sonde not employed;\\nmissing data|
                  missing data;\\nanomaly window|missing data;\\n24hr anomaly)$"

  df <- df %>%
    # str_detect to make sure we are not counting this same flag towards next count
    # add missing data to stringr
    mutate(flag_binary = ifelse((is.na(flag) | str_detect(flag, flag_string)), 0, 1),
           roll_bin = data.table::frollsum(flag_binary, n = 97, align = 'center', na.rm = F, fill = NA_real_)) %>%
    add_flag((roll_bin >= (97*0.5)), "24hr anomaly") # we can tweak the lower limit

  for (i in 1:48) {
    df <- df %>%
      add_flag((is.na(flag) | !str_detect(flag, "anomaly window") & lag(str_detect(flag, "24hr anomaly"), n = i)), "anomaly window") %>%
      add_flag((is.na(flag) | !str_detect(flag, "anomaly window") & lead(str_detect(flag, "24hr anomaly"), n = i)), "anomaly window")
  }
  return(df)
}
