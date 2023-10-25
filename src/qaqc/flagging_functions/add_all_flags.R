add_all_flags <- function(df) {

  df <- df %>%
    add_field_flags %>%
    add_range_flags %>%
    # slope violation flag
    add_flag((slope_ahead >= m_slope_behind_99 | slope_behind >= m_slope_behind_99), "slope violation") %>%
    # SD flag
    add_flag((mean <= rollavg - (3 * m_sd_1090) | mean >= rollavg + (3 * m_sd_1090)), "outside sd range") %>%
    # For data that repeats
    add_flag((mean == front1 | mean == back1), "repeated value") %>%
    # Missing data flag
    add_flag(is.na(mean), "missing data") %>%
    # large anomaly flag
    add_large_anomaly_flags %>%
    # add public facing mean column
    mutate(mean_public = ifelse(is.na(flag), mean, NA)) %>%
    relocate(mean_public, .after = "mean")

}
