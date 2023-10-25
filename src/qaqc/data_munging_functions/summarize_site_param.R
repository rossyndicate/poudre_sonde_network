# Function to summarize site-parameter combinations
summarize_site_param <- function(site_arg, parameter_arg, api_data) {

  # filter deployment records for the full join
  site_field_notes <- field_notes %>%
    filter(site == site_arg)

  # filtering the data and generating results
  summary <- tryCatch({
    api_data %>%
      filter(site == site_arg & parameter == parameter_arg) %>%
      select(-name) %>%
      distinct() %>%
      group_by(DT_round) %>% # site & parameter does not need to be here anymore
      # to do: preserve values used with nest()
      summarize(mean = as.numeric(mean(value, na.rm = T)),
                diff = abs(min(value, na.rm = T) - max(value, na.rm = T)),
                n_obs = n()) %>%
      ungroup() %>%
      arrange(DT_round) %>%
      # pad the dataset so that all 15-min timestamps are present
      pad(by = "DT_round", interval = "15 min") %>%
      mutate(DT_join = as.character(DT_round),
             site = site_arg,
             parameter = parameter_arg,
             flag = NA) %>%
      left_join(filter(dplyr::select(site_field_notes, sonde_employed, last_site_visit, DT_join, site, visit_comments, sensor_malfunction, cals_performed)),
                by = c('DT_join', 'site')) %>%
      # the mutate dt_round is similar to what was done prior to tz stuff... lmk if this looks weird -j
      mutate(DT_round = as_datetime(DT_join, tz = "MST")) %>%
      # Use fill() to determine when sonde was in the field, and when the last site visit was.
      fill(c(sonde_employed, last_site_visit, sensor_malfunction))
  },

  error = function(err) {
    # error message
    cat("An error occurred with site ", site_arg, " parameter ", parameter_arg, ".\n")
    cat("Error message:", conditionMessage(err), "\n")
    flush.console() # Immediately print the error messages
    NULL  # Return NULL in case of an error
  })

  return(summary)
}
