combine_hist_inc_data <- function(incoming_data_list, historical_data_list) {

  # Get the matching index names
  matching_indexes <- intersect(names(incoming_data_list), names(historical_data_list))

  # Get the last 3 hours of the historically flagged data
  last_3_hours <- map(historical_data_list,
                      function(data) {
                        data %>%
                          filter(DT_round >= (max(DT_round) - hours(3)))
                        })

  # bind all_data_summary and last_3_hours together
  combined_hist_inc_data <- map(matching_indexes, function(index) {
    bind_rows(last_3_hours[[index]], incoming_data_list[[index]]) %>%
        filter(!(duplicated(DT_round) & !historical_flagged_data_1)) %>%
        arrange(DT_round) %>%
        mutate(historical_flagged_data_2 = FALSE)
    }) %>%
    set_names(matching_indexes) %>%
    keep(~ !is.null(.))

  return(combined_hist_inc_data)
}

