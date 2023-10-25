# Adding flags related to field notes
# site visits (and site visit window)
# sonde not employed
add_field_flags <- function(df) {
  df <- df %>%
    add_flag(sonde_employed == 1, "sonde not employed") %>%
    add_flag(as.character(last_site_visit) == as.character(DT_round), "site visit") %>%
    add_flag(lag(str_detect(flag, "site visit"), n = 3), "sv window") %>%
    add_flag(lag(str_detect(flag, "site visit"), n = 2), "sv window") %>%
    add_flag(lag(str_detect(flag, "site visit"), n = 1), "sv window")
  return(df)
}
