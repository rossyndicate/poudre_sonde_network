new_add_suspect_flag <- function(df) {

  flag_string <- "sonde not employed|missing data|site visit|sv window|suspect data" # include the flag added by this function

  # Define a function to check if a given 3-hour window has >= 50% fails
  check_3_hour_window_fail <- function(x) {
    sum(x) / length(x) >= 0.5
  }

  df_test <- df %>%
    mutate(flag_binary = ifelse((is.na(flag) | grepl(flag_string, flag)), 0, 1)) %>%
    #arrange(timestamp) %>%
    mutate(over_50_percent_fail_window = ifelse(is.na(over_50_percent_fail_window),
                                                zoo::rollapply(flag_binary, width = 12, FUN = check_3_hour_window_fail, fill = NA, align = "right"),
                                                over_50_percent_fail_window)) %>%
    add_flag(over_50_percent_fail_window == TRUE & !grepl("suspect data", flag), "suspect data") # is this flagging the correct data?

  return(df_test)

}
