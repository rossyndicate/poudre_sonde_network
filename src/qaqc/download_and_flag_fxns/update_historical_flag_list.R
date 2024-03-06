update_historical_flag_list <- function(new_flagged_data, historical_flagged_data){

  # Get the matching index names
  matching_indexes <- intersect(names(new_flagged_data), names(historical_flagged_data))

  # bind new_flagged_data and historical_flagged_data together
  updated_historical_flag_list <- map(matching_indexes, function(index) {

    old <- historical_flagged_data[[index]] %>%
      filter(DT_round < ymd_hms(max(DT_round) - hours(3), tz = "MST"))%>%
      mutate(last_site_visit = force_tz(last_site_visit, tzone = "MST")) #%>%

    bind_rows(old, new_flagged_data[[index]]) %>%
      arrange(DT_round) %>%
      select(-historical)
  }) %>%
    set_names(matching_indexes) %>%
    keep(~ !is.null(.))

  # need to make sure that when we are doing this we are not getting rid of dfs in the RDS list

  return(updated_historical_flag_list)

}
