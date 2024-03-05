flag_all_data <- function(data, require = NULL) {
  flagged_data <- data %>%
    add_field_flag() %>%
    add_spec_flag() %>%
    add_seasonal_flag() %>%
    add_na_flag() %>%
    add_repeat_flag() %>%
    add_suspect_flag() %>%
    add_malfunction_flag()
    # mutate(mean_public = ifelse(is.na(flag), mean, NA))
  return(flagged_data)
}
