# Summarize site parameter data from the API and field notes data frames.
#' @param site_arg A site name.
#' @param parameter_arg A parameter name.
#' @param api_data A dataframe with the munged API data.
#' @return A dataframe with summary statistics for a given site parameter data frame.
#' @examples
# summarize_site_param(site_arg = "archery", parameter_arg = "Actual Conductivity", api_data = api_data)
# summarize_site_param(site_arg = "boxelder", parameter_arg = "Temperature", api_data = api_data)

summarize_site_param <- function(site_arg, parameter_arg, api_data) {

  # filter deployment records for the full join
  site_field_notes <- field_notes %>%
    filter(site == site_arg)

  # filtering the data and generating results
  summary <- tryCatch({
    api_data %>%
      # summarize the data
      filter(site == site_arg & parameter == parameter_arg) %>%
      distinct() %>%
      group_by(DT_round) %>% 
      summarize(mean = as.numeric(mean(value, na.rm = T)),
                diff = abs(min(value, na.rm = T) - max(value, na.rm = T)),
                n_obs = n()) %>%
      ungroup() %>%
      arrange(DT_round) %>%
      # pad the dataset so that all 15-min timestamps are present
      pad(by = "DT_round", interval = "15 min") %>%
      # join the field notes
      mutate(DT_join = as.character(DT_round),
             site = site_arg,
             parameter = parameter_arg,
             flag = NA) %>% # maybe we don't want to do this here
      left_join(filter(dplyr::select(site_field_notes, sonde_employed, last_site_visit, DT_join, site, visit_comments, sensor_malfunction, cals_performed)),
                by = c('DT_join', 'site')) %>%
      mutate(DT_round = as_datetime(DT_join, tz = "MST")) %>%
      # Use fill() to determine when sonde was in the field, and when the last site visit was.
      fill(c(sonde_employed, last_site_visit, sensor_malfunction)) %>%
      # format the data by adding all the columns that are missing from the flagged data
      mutate(
        mean_public = NA,
        front1 = NA,
        back1 = NA,
        rollmed = NA,
        rollavg = NA,
        rollsd = NA,
        slope_ahead = NA,
        slope_behind = NA,
        rollslope = NA,
        month = NA,
        year = NA,
        y_m = NA,
        season = NA,
        m_mean01 = NA,
        m_mean99 = NA,
        m_slope_behind_01 = NA,
        m_slope_behind_99 = NA,
        t_mean01 = NA,
        t_mean99 = NA,
        t_slope_behind_01 = NA,
        t_slope_behind_99 = NA,
        t_sd_0199 = NA,
        # verification = NA
        # add column that signifies that this is new incoming data
        historical_data = FALSE
      ) %>%
      relocate(mean_public, .after = "mean")
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
