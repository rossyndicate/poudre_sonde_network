update_historical_flag_list <- function(new_flagged_data, historical_flagged_data){

  # Get the matching index names
  matching_indexes <- intersect(names(new_flagged_data), names(historical_flagged_data))

  # bind new_flagged_data and historical_flagged_data together
  updated_historical_flag_list <- map(matching_indexes, function(index) {
    bind_rows(historical_flagged_data[[index]], new_flagged_data[[index]]) %>%
      filter(!(duplicated(DT_round) & !historical_flagged_data_2)) %>%
      arrange(DT_round) %>%
      mutate(historical_flagged_data_2 = TRUE)
      }) %>%
      set_names(matching_indexes) %>%
      keep(~ !is.null(.))

  # need to make sure that when we are doing this we are not getting rid of dfs in the RDS list

  return(updated_historical_flag_list)

}
