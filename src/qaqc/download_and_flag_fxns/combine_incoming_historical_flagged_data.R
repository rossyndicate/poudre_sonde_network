combine_hist_inc_data <- function(incoming_data_list, historical_data_list) {

  # Get the matching index names
  matching_indexes <- intersect(names(incoming_data_list), names(historical_data_list))

  # Get the last 3 hours before the newest pull of data
  # last_3_hours <- map(incoming_data_list,
  #                     function(data) {
  #                       data %>%
  #                         filter(DT_round <= (min(DT_round))) %>%
  #                         mutate(DT_back = DT_round - hours(3)) %>%
  #                         select(site, parameter, DT_round, DT_back)
  #                     }) %>%
  #   bind_rows()



  # Get the last 3 hours of the historically flagged data
  last_3_hours <- map(historical_data_list,
                      function(data) {
                        data %>%
                          filter(DT_round >= ymd_hms(max(DT_round) - hours(3), tz = "MST")) %>%
                          mutate(historical = TRUE,
                                 last_site_visit = force_tz(last_site_visit, tzone = "MST")) #%>%
                          # rename(cleaner_flag_old = cleaner_flag,
                          #        over_50_percent_fail_window_old = over_50_percent_fail_window,
                          #        flag_old = flag)
                      })

  # bind all_data_summary and last_3_hours together
  combined_hist_inc_data <- map(matching_indexes, function(index) {
    last_3_hours[[index]] %>%
      select(-c(t_mean01,
                t_mean99,
                t_slope_behind_01,
                t_slope_behind_99,
                t_sd_0199)) %>%
      bind_rows(., mutate(incoming_data_list[[index]], historical = FALSE)[-1,]) %>%
      fill(c(sonde_employed, last_site_visit, sensor_malfunction)) #%>%
    #filter(!(duplicated(DT_round) & !historical_flagged_data_1)) %>%
    #arrange(DT_round) #%>%

  }) %>%
    set_names(matching_indexes) #%>%
  #keep(~ !is.null(.))

  return(combined_hist_inc_data)
}

