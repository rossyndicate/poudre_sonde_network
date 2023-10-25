# Add flags to the `flag` column of a dataframe based on field notes.
  # "sonde not employed" flag is added if the sonde was not employed at the site.
  # "site visit" flag is added if the last site visit date is the same as the date of the data.
  # "sv window" flag is added if the last site visit date is within 3 days of the date of the site visit.
# @param df A dataframe with a `flag` column.
# @return A dataframe with a `flag` column that has been updated with the relevant field note flags.
# @examples
# add_range_flags(df = all_data_flagged$`archery-Actual Conductivity`)
# add_range_flags(df = all_data_flagged$`boxelder-Temperature`)

add_field_flags <- function(df) {
  df <- df %>%
    add_flag(sonde_employed == 1, "sonde not employed") %>%
    add_flag(as.character(last_site_visit) == as.character(DT_round), "site visit") %>%
    add_flag(lag(str_detect(flag, "site visit"), n = 3), "sv window") %>%
    add_flag(lag(str_detect(flag, "site visit"), n = 2), "sv window") %>%
    add_flag(lag(str_detect(flag, "site visit"), n = 1), "sv window")
  return(df)
}
