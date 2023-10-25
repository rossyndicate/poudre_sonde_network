# Add all flags to a dataframe from the flagging functions. Add additional single line flags here. Add public facing mean column.
  # "slope violation" flag is added if the slope is greater than the 99th percentile of the slope behind.
  # "outside sd range" flag is added if the mean is outside of the 10th and 90th percentile of the rolling average.
  # "repeated value" flag is added if the mean is the same as the value in front or behind.
  # "missing data" flag is added if the mean is NA.
  # `mean_public` column is added to the dataframe with the public facing mean.
# @param df A dataframe with a `flag` column.
# @return A dataframe with a `flag` column that has been updated with the relevant flags.
# @examples
# add_all_flags(df = all_data_flagged$`archery-Actual Conductivity`)
# add_all_flags(df = all_data_flagged$`boxelder-Temperature`)

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
