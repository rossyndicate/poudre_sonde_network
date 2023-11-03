# Add flags to the `flag` column of a dataframe based on field notes.
## "sonde not employed" flag is added if the sonde was not employed at the site.
## "site visit" flag is added if the last site visit date is the same as the date of the data.
## "sv window" flag is added if the last site visit date is within 3 days of the date of the site visit.
# @param df A dataframe with a `flag` column.
# @return A dataframe with a `flag` column that has been updated with the relevant field note flags.
# @examples
# add_range_flags(df = all_data_flagged$`archery-Actual Conductivity`)
# add_range_flags(df = all_data_flagged$`boxelder-Temperature`)

add_field_flag <- function(df) {

  df <- df %>%
    # flag when sonde was not employed in the river
    add_flag(sonde_employed == 1, "sonde not employed") %>%
    # flag when sonde was handled in a site visit
    add_flag(as.character(last_site_visit) == as.character(DT_round), "site visit")
    ## This should add the sv window in the same way as the large anomaly window
    # Add flags for the next 45 minutes after a site visit
    for (i in 1:3) {
      df <- df %>%
        add_flag(lag(str_detect(flag, "site visit"), n = i), "sv window") %>%
        add_flag(lead(str_detect(flag, "site visit"), n = i), "sv window")
    }
  return(df)

}
