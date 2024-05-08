#' @title Summarize site parameter data from the HydroVu API and field notes data frames.
#' 
#' @description
#' A function that summarizes site parameter data from the API and field notes data frames.
#' 
#' @param site_arg A site name.
#' 
#' @param parameter_arg A parameter name.
#' 
#' @param api_data A dataframe with the munged API data.
#' 
#' @param notes The munged field notes
#' 
#' @param require An upstream dependency (i.e., alternative to `{necessary object}`).
#' 
#' @return A dataframe with summary statistics and field notes for a given site parameter data frame.
#' 
#' @examples
# summarize_site_param(site_arg = "archery", parameter_arg = "Actual Conductivity", api_data = incoming_data_collated_csvs)
# summarize_site_param(site_arg = "boxelder", parameter_arg = "Temperature", api_data = incoming_data_collated_csvs)

summarize_site_param <- function(site_arg, parameter_arg, api_data, notes, require = NULL) {

  # filter deployment records for the full join
  site_field_notes <- notes %>%
    dplyr::filter(site == site_arg)

  # filtering the data and generating results
  summary <- tryCatch({
    api_data %>%
      # summarize the data
      dplyr::filter(site == site_arg & parameter == parameter_arg) %>%
      dplyr::distinct() %>%
      dplyr::group_by(DT_round) %>%
      dplyr::summarize(mean = as.numeric(mean(value, na.rm = T)),
                       diff = abs(min(value, na.rm = T) - max(value, na.rm = T)),
                       n_obs = n()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(DT_round) %>%
      # pad the dataset so that all 15-min timestamps are present
      padr::pad(by = "DT_round", interval = "15 min") %>%
      # join the field notes
      dplyr::mutate(DT_join = as.character(DT_round),
                    site = site_arg,
                    parameter = parameter_arg,
                    flag = NA) %>% 
      dplyr::left_join(dplyr::filter(dplyr::select(site_field_notes, sonde_employed,
                                                   last_site_visit, DT_join, site,
                                                   visit_comments, sensor_malfunction,
                                                   cals_performed)),
                       by = c('DT_join', 'site')) %>%
      # join the metaparameter dfs
      dplyr::left_join(site_metaparams_list[[site_arg]], by = "DT_join") %>%
      dplyr::mutate(DT_round = lubridate::as_datetime(DT_join, tz = "MST"))
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
